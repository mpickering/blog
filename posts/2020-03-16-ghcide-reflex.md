---
title: An IDE implemented using reflex
---

Around this point last year I set out to reimplement a lot of the backend of
`haskell-ide-engine` in order to make it more easily usable with a wide variety
of build tools. This effort was largely a success and my branch was merged just
before Christmas thanks to the extensive help of Zubin Duggal, Fendor, Alan Zimmerman,
Luke Lau and Javier Neira. The main result was an IDE based on the `hie-bios` library
which abstracts the interface to the different build tools so the IDE itself
doesn't have to worry about how to set up the GHC session.

Since then, the situation is vastly different with the focus now turning to
`ghcide` and `hls`. `ghcide` is generally faster and more robust than `haskell-ide-engine` because
it reimplements certain parts of the GHC API which allow for finer grain recompilation
checking. The future extension, `hls`, will extend `ghcide` with support for code
formatters and other diagnostics. I have found implementing extensions to
`ghcide` much easier and more robust. Both `ghcide` and `hls` are built on top of
`hie-bios`.

At Munihac 2019, Neil Mitchell gave a [presentation](https://www.youtube.com/watch?v=cijsaeWNf2E) where he described the
motivation for `ghcide` and a general description of the architecture.
In his talk, he describes how you can think of an IDE as a dependency graph, which
was greeted by an audience heckle suggesting an FRP library could be used to
implement the IDE. The current implementation is based on shake, which has similar
properties to an FRP library but with some crucial differences.

The pull-based model of shake does not scale well to large code bases.
All requests scale linearly with the number of dependencies which means that even
requests such as hovering can take upwards of 1s on a module with a large number
of transitive dependencies. A 1s hover response time was enough to get me interested
and after attempting to [improve the performance](https://github.com/digital-asset/ghcide/pull/384)
I decided that without a fundamental rewrite, the situation could not be improved.

So spurred on by the heckle and the desire for subsecond reponse times
it was time to put the money where my mouth was and attempt to reimplement
the backend using `reflex` instead of `shake`. Reflex is push-based
which means once the network is constructed changes propagate from input events
rather than being pulled from samples. This seemed like a better model for an IDE.

What did I imagine the primary benefits to this project would be?

* I wanted to prove it was possible.
* Using the push-based model means that requests such as hovering can return
  instantly rather than checking to see if any dependencies have updated.
* Handlers for LSP requests can be written in the same language as the functions
  which computed the module graph.

In short, I [now have an IDE](https://github.com/mpickering/ghcide/tree/reflex) which works and is completely implemented using reflex
which gives you a point to be able to evaluate the costs and benefits to both approaches.

In this post I will describe some of the basic abstractions which I implemented
using `reflex` which gives writing the IDE a similar feel. The rest of this post
is aimed at people who are already familiar with `reflex` and goes into
a reasonable amount of detail about specific things to do with reflex and design
decisions I had to make.

<!--more-->

# Implementation

An early goal of the implementation was to try to reuse as much code as possible
from `ghcide`. The end result was that I could reuse almost all the code for the
rule definitions but had to rewrite a lot of the code which dealt with input
events. Therefore there are two main parts to the implementation: the specification
of rules and the interpretation of rules into a reflex network.

## Step 1: What is a rule?

The program is structured by rules, there is one rule type for each of the different
stages of the compilation pipeline. The user provides definitions for these
rules and then the rules are combined to form the reflex network.

```haskell
data RuleType a where
  GetFileContents :: RuleType (FileVersion, Maybe T.Text)
  GetParsedModule :: RuleType ParsedModule
  GetLocatedImports :: RuleType LocatedImports
  GetSpanInfo :: RuleType SpansInfo
  GetDependencyInformation :: RuleType DependencyInformation
  GetDependencies :: RuleType TransitiveDependencies
  GetTypecheckedModule :: RuleType TcModuleResult
  ReportImportCycles :: RuleType ()
  GenerateCore :: RuleType (SafeHaskellMode, CgGuts, ModDetails)
  GenerateByteCode :: RuleType Linkable
  GhcSession :: RuleType HscEnvEq
  GetHiFile :: RuleType HiFileResult
  GetModIface :: RuleType HiFileResult
  IsFileOfInterest :: RuleType Bool
```

A `RuleType` is a per-module rule, therefore for each module we can ask to
get the parsed module for that module and a bunch of other information. As a first approximation, the result of
each rule will be stored in a `Dynamic`.

The monad which is used for defining rules is called `ActionM`, inside the
`ActionM` monad you can do two things.

1. Run IO actions using `liftIO`.
2. Request the value of existing rules, using `use` or `use_`.

`use` is a function which allows you to ask what the current value of a specific
rule is.

```haskell
use :: _ => RuleType a
         -> NormalizedFilePath
         -> ActionM t m (Maybe a)
```

Whenever `use` is invoked in a rule definition, a dependency is added on the value
was was sampled. If the value changes in future, the rule will run again and
the result recomputed.

Rule definition therefore end up looking a lot like the original shake
rule definitions.

```haskell
generateByteCodeRule :: WRule
generateByteCodeRule =
    define GenerateByteCode $ \file -> do
      deps <- use_ GetDependencies file
      (tm : tms) <- uses_ GetTypecheckedModule (file: transitiveModuleDeps deps)
      session <- hscEnv <$> use_ GhcSession file
      (_, guts, _) <- use_ GenerateCore file
      liftIO $ generateByteCode session [(tmrModSummary x, tmrModInfo x) | x <- tms] tm guts
```

The bytecode rule will rerun if the dependencies of the file change, the
result of typechecking changes, the current session changes or the generated
core changes.

### Defining rules

Once the body of a rule definition is defined, there are several ways to specify
the definition. The simplest is `define`, which does not implement early cut-off
or external triggering. There are other variants which enable both of these features.

```haskell
define :: RuleType a -> (forall t . C t => NormalizedFilePath -> ActionM t (HostFrame t) a) -> WRule
```

Once a rule is defined, like in shake, you put them all in a list and pass
them into the function which creates the reflex network.


## Representing a node in the network

Each rule is implemented by an `MDynamic`, which is a refined `Dynamic` which
implements early cut-off and lazy initialisation. Early cut-off means that
the dynamic will only fire if the value is updated to a new value.
Lazy initialisation means that the dynamic will only be populated after it has
been demanded once.

```haskell
newtype MDynamic t a = MDynamic { getMD :: Dynamic t (Early (Thunk a)) }
```

The combination of both of these features means that less events are propagated
in the network, something we really want to avoid in order to avoid running
expensive IO computations.

### Early Cut-off

Early cut-off is implemented by using the `Early` wrapper.

```haskell
data Early a = Early (Maybe BS.ByteString) Int a
```

The data type stores a hash of the current value and an integer which indicates the
number of times the value has been updated (this is used for debugging).

The value in the `Early` is only updated if either there is no hash or the hash
of the new value is different to the hash of the old value.

```haskell
early :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (Maybe BS.ByteString, a) -> m (Dynamic t (Early a))
early d = scanDynMaybe (\(h, v) -> Early h 0 v) upd d
  where
    -- Nothing means there's no hash, so always update
    upd (Nothing, a) (Early _ n _) = Just (Early Nothing (n + 1) a)
    -- If there's already a hash, and we get a new hash then update
    upd (Just h, new_a) (Early (Just h') n _) = if h == h'
                                                  then Nothing
                                                  else (Just (Early (Just h') (n + 1) new_a))
    -- No stored, hash, just update
    upd (h, new_a) (Early Nothing n _)   = Just (Early h (n + 1) new_a)
```

Most rules do not use the early cut-off functionality and hence the hash is always
`Nothing`.

### Lazy initialisation

Without proper care, when the state for a module is initialised all the
information about that module will be computed despite the fact most of it will
never be used. For example, you will not need the core for most modules but in early
versions of the project the core was always produced because on the first run
of the rule, it was observed to depend on the typechecked module and hence was updated
when the typechecked module was updated.

In order to solve this we implement the `Thunk` data type which has three distinct
states:

```haskell
data Thunk a = Value a | Awaiting | Seed (IO ()) deriving Functor
```

A thunk either contains a value, is awaiting a value to be provided to it
or is inactive. All thunks start out as inactive and are activated by calling
the `IO` action contained within the `Seed` constructor.

When a thunk is sampled, it is activated if it has never been activated before.

```haskell
sampleThunk :: (Reflex t, MonadIO m, MonadSample t m) => Dynamic t (Thunk a) -> m (Maybe a)
sampleThunk d = do
  t <- sample (current d)
  case t of
    Seed start -> liftIO start >> return Nothing
    Awaiting   -> return Nothing
    Value a    -> return (Just a)
```

It is also important to implement a version of the `improvingMaybe` combinator
to avoid propagating a lot of updates in the case when the dynamic is repeatedly
updated to an `Awaiting` value. So a thunk can step from a `Seed` to an `Awaiting`
and from an `Awaiting` to a `Value` but never back again.


```haskell
-- Like improvingMaybe, but for the Thunk type
improvingResetableThunk  ::  (MonadFix m, MonadHold t m, Reflex t, MonadIO m, MonadSample t m) => Dynamic t (Thunk a) -> m (Dynamic t (Thunk a))
improvingResetableThunk = scanDynMaybe id upd
  where
    -- ok, if you insist, write the new value
    upd (Value a) _ = Just (Value a)
    -- Wait, once the trigger is pressed
    upd Awaiting  (Seed {}) = Just Awaiting
    upd _ _ = Nothing
```

It will be good in future to allow resetting thunks in order to implement garbage collection.
It is probably that we want to allow reseting from a `Just` back to a `Nothing`
in order to avoid stale information in the network.

## Step 2: What is a global variable?

There is also a global rule type for parts of the IDE state which are not
dependent on a specific module.

```haskell
data GlobalType a where
  GetHscEnv :: GlobalType SessionMap
  GhcSessionIO :: GlobalType GhcSessionFun
  GetEnv :: GlobalType HscEnv
  GetIdeOptions :: GlobalType IdeOptions
  OfInterestVar :: GlobalType (HashSet NormalizedFilePath)
  FileExistsMapVar :: GlobalType FileExistsMap
  GetVFSHandle :: GlobalType VFSHandle
  GetInitFuncs :: GlobalType InitParams
  IdeConfigurationVar :: GlobalType IdeConfiguration
  GetPositionMap :: GlobalType PositionMap
```

Module rules can depend on global rules in the same manner as per-module rules.
The
interface for defining a global rule is slightly different to a local rule because
the global variables are usually directly populated from events. For example,
the `OfInterestVar` is modified by the user opening and closing files in their
editer and hence it is defined as the combination of these two events.

```haskell
addIdeGlobal :: GlobalType a -> (forall t . C t => (ReaderT (REnv t) m (Dynamic t a))) -> WRule

ofInterestVar :: WRule
ofInterestVar =
  addIdeGlobal OfInterestVar $ do
    e1 <- withNotification <$> getHandlerEvent didOpenTextDocumentNotificationHandler
    e2 <- withNotification <$> getHandlerEvent didCloseTextDocumentNotificationHandler
    upd <- logAction Info (fmapMaybe check e1)
    upd2 <- logAction Info (fmapMaybe check2 e2)
    foldDyn ($) S.empty (mergeWith (.) [upd, upd2])
  where
      check (DidOpenTextDocumentParams TextDocumentItem{_uri, _version}) =
        whenUriFile _uri Nothing $ \file -> Just (add file, "Opened text document: " <> getUri _uri)


      check2 (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) =
        whenUriFile _uri Nothing $ \file -> Just (remove file, "Closed text document:" <> getUri _uri)
      add file = S.insert file
      remove file = S.delete file

```

A global is defined in an environment with the other global dynamics and must
return a dynamic which is created by combining them together.

## Definition 3: What is an unit action?

The third type of definition is the unit action. Unit actions are useful for
parts of your program which don't contribute any state in the form of definitions.
For example, hooking up diagnostics to the output, responding to hover requests, logging
and progress notifications.

Unit actions are defined using the `unitAction` function.

```haskell
unitAction :: (forall t . C t => BasicM t (BasicGuestWrapper t) ())
           -> WRule
```

A unit action is an action which only operates in a reader environment where it
can depend on the value of other dynamics but must eventually return unit.
For example, in a unit action you can create a local dynamic which combines
different dynamics from the global state together before outputting the result
to the user. This is how diagnostics are implemented before being fed into the
function which sends output back to the language client.

# Evaluating the rule specification

Once we have a list of module rules, global rules and actions, they are combined
together in order to form the reflex network. Each global rule
is evaluated and turned into a dynamic, module rules are used to define the per-module
state when we discover a new file and finally actions are all evaluated to
connect additional parts of the network together.

## Evaluating a rule

The heart of the implementation is in how the rules report their dependencies
in the form of an `Event`, which is then used in order to trigger the action in future.
This is elegantly expressed recursively in five lines. The result of the call
to performAction is `Event t (IdeResult a, [Event t EType])`, which is
the separated using `splitE` before the dependency events are combined with
`mkDepTrigger` and then used in order to define `rebuild_trigger`.

```haskell
rule = mdo
    -- The event which will trigger a rebuild
    let rebuild_trigger = (fmap (\e -> leftmost [user_trig', start_trigger, e]) deps')
    act_trig <- switchHoldPromptly start_trigger rebuild_trigger
    -- When the trigger fires, run the rule
    pm <- performAction renv (act f) act_trig
    -- Separate the dependencies from the actual result
    let (act_res, deps) = splitE pm
    let deps' = pushAlways mkDepTrigger deps
    ...
```

The use of `switchHoldPromptly` is absolutely key to the implementation. It is
imperative that if in the same frame a dependency fires then we need to immediately
rerun the rule. The network is left in an inconsistent state is the simpler
`switchHold` is used.

Further processing to the returned result is performed to convert it into an
`MDynamic` which is then stored in the state.

## A note about the module state

The per-module state is a pair of a map from the rule type to an `MDynamic`
and an event which reports diagnostics for that module.

```haskell
data ModuleState t = ModuleState
      { rules :: DMap RuleType (MDynamic t)
      , diags :: Event t (NL.NonEmpty DiagsInfo) }
```

The state for all modules is stored in a map from the filepath to one
of these module state records.

```haskell
type ModuleMap t = Incremental t (PatchMap NormalizedFilePath (ModuleState t))
```

Using a `Dynamic` or `Incremental` here is important because it means values of the
map can be altered as the network is evaluated. For our use-case
as we do not know the dependencies of a module until we have parsed the module header.

So when a module rule is attempted to be sampled, there are in fact two
possible modes of failure which we can recover from.

1. Either the module has never been seen before, so we should initialise the
   module state for this module.
2. The value for the rule has not been computed yet, so we should recompute the
   rule when it is available.

In order to report that a sample failed for the first reason, the module map is
paired with an action which can be called to trigger the event which adds a new
module to the map.

```haskell
data ModuleMapWithUpdater t =
  MMU {
    getMap :: ModuleMap t
    , updateMap :: [(D.Some RuleType, NormalizedFilePath)] -> IO ()
    }
```

The second situation is dealt with by the `Thunk` mechanism described
in the previous situation.

Note: There is a place where using `batchOccurences` is very useful because
during the initialisation of the network, this trigger can be called hundreds of times
and it is much more efficient to collect together as many updates as possible.

```haskell
map_update' <- fmap concat <$> batchOccurrences 0.1 map_update
```

# Interaction with the language client

In the global environment as well as the global variables as defined by rules,
there is a collection of events which correspond to external events.

* One event which fires after the language server is initialised, this populates a
  few global dynamics.
* A record of events which fire whenever the server recieves a notification of request
  from the client. For example, when the user opens or modifies a file, the event fires.

As part of an action definition it is possible to also provide an additional
event trigger, constructed from these events, which causes the rule to fire.
For example, when a file is saved, the rule which parses a file fires again which causes
the changes to propagate through the network.

Global variables are typically constructed by holding these notification events.
It is a much nicer model in my opinion than the style previously found in `ghcide`
where there where some variables were mutated in the handlers and the whole shake
graph invalidated.

Note: The way this handler record is constructed by leveraging the `barbies` library
is [interesting](https://github.com/mpickering/ghcide/blob/reflex/src/Development/IDE/Core/Reflex/Service.hs#L257) in its own right.

# Evaluation

I found it important to be able to inspect certain properties of my network
during the implementation process. In particular, there were situations where actions
were running more than I expected so I wanted to analyse what was causing each rule to fire.
There is unfortunately not an existing framework built into reflex for this but it was possible
to instrument the application to get some good information.

I started by defining a data type which enumerates the different possible ways a rule can fire.

```haskell
-- EType is mainly used for debugging why an event is firing too often.
data EType = DepTrigger (D.Some RuleType, NormalizedFilePath)
              | MissingTrigger NormalizedFilePath
              | StartTrigger
              | UserTrigger
      deriving Show
```

Then each event which could contribute to an action firing is tagged with one
of these constructors. When the event fires I used `traceEvent` in order to
output both the action which was firing and the reason for it. Then by capturing
this log and using standard unix commands it was possible to analyse situations
where things were happening more often than not.

This was the method where I realised it was necessary to use `headE` in order to
make sure the `StartTrigger` event would only fire one time.

# What's next?

So we've achieved our goal of proving the implementation is possible but there
are still a few places the implementation could be improved. I have also not
extensively tested the branch, it is likely there are some bugs to do with
stale information.

### Progress Reporting

It isn't clear to me how to implement progress reporting for the IDE at the moment.
All changes to the system are driven by push events, which means that when an event
fires the amount of work which will be done can not be determined. This is
compounded by the fact reflex is a monadic FRP library so how much is left to do
depends on the result of running the rules.

### Better Profiling

It would be good to have a profiling mode like shake's profiling mode so the
effect of each input event could be analysed in detail. At the moment there
is nothing in the reflex ecosystem which can help with this analysis.

### Asynchronous Actions

It would be very beneficial if the rules could run in separate threads because
currently the whole application blocks whilst IO actions are being computed.
The usage of `MonadSample` is not currently compatible with using `performEvent`
asynchronously.

### Dynamic Rule Registration

For my own sanity, I decided to use a fixed set of rules, as defined by `RuleType`
in my implementation rather than a dynamic map of rules, as implemented in shake.
I have considered a few types going for the dynamic map approach, as it would also
be useful for plugins but it has been a low priority for the proof of concept
implementation.

# Conclusion

I had a great time implementing this fork, my second extensive rewrite of
a Haskell IDE. I'm looking forward to rewriting an IDE again next year.

## Related Links

* [Project Branch](https://github.com/mpickering/ghcide/tree/reflex)
* Reddit discussion











