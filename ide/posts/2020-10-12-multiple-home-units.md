---
posttitle: Multiple Home Units for GHC - GSoC Aftermath
author: Fendor
date: 2020-10-12
github: fendor
---

It has been a great summer for Haskell IDEs. Multiple successful Google Summer of Code projects and lots of contributions to Haskell Language Server. Additionally, Haskell IDE Engine has finally been put to rest! Lots of news, lots to talk about.

In this blogpost, I will tell you a bit about my own Google Summer of Code project, in the scope of which I tackled bringing multiple home units to GHC. We start by talking about why we chose this project in the first place.
This is potentially nothing new for people that read the initial proposal, so you might want to skip over the first part.
Then we are going to talk about how far we got in the project itself, what works, what doesn't, how does it work, and what is left to do.
Finally, a word or two about my experience in this Google Summer of Code, what I liked, and whether I would recommend it.

<!--more-->

## Motivation

As explained in on of my previous [blog posts](https://mpickering.github.io/ide/posts/2020-05-15-multiple-components.html), my motivation for this project was to improve the tooling situation for IDEs as well as build tools such as `cabal` and `stack`. What am I talking about in particular?

Take this example:
```bash
# Initialise a cabal project
$ cabal init --libandexe --source-dir=src \
   --application-dir=app --main-is=Main.hs -p myexample
# Open a repl session for the executable
$ cabal repl exe:myexample
[1 of 1] Compiling Main             ( app/Main.hs, interpreted )
Ok, one module loaded.
Main λ> main
Hello, Haskell!
someFunc
Main λ>
# Keep the session open
```

We keep this interactive session open, since we want to interactively develop our package!
Now, if we modify the library module `src/MyLib`, for example change the implementation of `someFunc` from
```haskell
someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

to

``` haskell
someFunc :: IO ()
someFunc = putStrLn "not just someFunc"
```
we would like to reflect this change in our interactive repl session. However, executing the following:
``` bash
Main λ> :r
Ok, one module loaded.
Main λ> main
Hello, Haskell!
someFunc
```

Hm. Not what we would have hoped for. What happened?

Essentially, the problem is that we opened only an interactive session for the executable component. GHC is not aware that other libraries' definitions and source files might change, so it is not even checking whether something has changed! Therefore, changes in our library will not be visible, until we completely restart our interactive repl session.

So, can't we just open both components? Library and executable in the same session? Well, let's try it:
``` bash
$ cabal repl exe:myexample lib:myexample
cabal: Cannot open a repl for multiple components at once. The targets
'myexample' and 'myexample' refer to different components..

The reason for this limitation is that current versions of ghci do not support
loading multiple components as source. Load just one component and when you
make changes to a dependent component then quit and reload.
```

That did not work. Turns out, such a feature is not implemented.

Maybe you know about this issue in `cabal` and are now suggesting use `stack`, where this actually works:

```bash
$ stack init  --resolver lts-14.21 # or insert your lts of choice
$ stack ide targets
myexample:lib
myexample:exe:myexample
$ stack repl myexample:lib myexample:exe:myexample
[1 of 2] Compiling MyLib
[2 of 2] Compiling Main
Ok, two modules loaded.
*Main MyLib λ>
```

Awesome, so end of story? Not quite. `Stack` accomplishes this feat by essentially merging the compilation options for the library and executable together into a single `GHCi` invocation. However, it is not hard to come up with situations where merging of `GHC` options does not yield the desired behaviour.

For a slightly contrived example, imagine you define a module `Data.Map` private to your library, while your executable depends on the `containers` library which exposes the module `Data.Map`. Importing now `Data.Map` in your `app/Main.hs` leads to the following behaviour: on `stack build`, everything works as expected, but if you do `stack repl`, suddenly you get import errors or type errors! It actually tries to load your local `Data.Map`, which is not what you would expect at all! This behaviour occurs, because when `stack` merges the options, `Data.Map` from our local package and `src/Main.hs` are part of the same `GHCi` session, therefore it is assumed, that we want to use the local module.

So, in short, the behaviour we desire can hardly be implemented without support from `GHC`, if at all.
Mitigating this requires support for *multiple home units* in `GHC`.

My project was to implement this feature for `GHC` followed by patching the relevant tools `cabal` and `stack` and, if there was still time left, to make `ghcide` use the new capabilities as a tech-preview.

## Multiple Home Units

First things first, what even is a home unit? Up until before I started working on the project, there was practically no mention of a home unit in `GHC`.
But recently, this has been changed, and this is what the documentation says now:

> The home unit is the unit (i.e. compiled package) that contains the module we are compiling/typechecking.

Short and concise, that's how we like it. So, a *unit* is a set of modules that we can compile. For example, a library is a unit and so is the executable. Now, the *home unit* is the unit that `GHC` is currently compiling in an invocation, e.g. when you perform a simple compilation `ghc -O2 Main.hs`. In this case `Main.hs` is our singleton set of *home modules* and `-O2` are the compilation options for our *home unit*.

We want to lift the restriction of only being able to have a single *home unit* in `GHC`. The main part of the change happens in the `HscEnv` record which is used for compiling single modules as well as storing the linker state. The part we are interested in looks like this:

```haskell
data HscEnv
  = HscEnv {
        hsc_dflags :: DynFlags,
        ...
        hsc_HPT    :: HomePackageTable,
        ...
```

We are only interested in the two fields `hsc_dflags` and `hsc_HPT`, where the former contain compilation information, such as optimisations, dependencies, include directories, etc... and the latter is a set of already compiled *home modules*. To support multiple home units, we basically need to maintain more than a single tuple of these two, ideally an arbitrary amount of these.
This is accomplished by adding new types:
```haskell
type UnitEnv = UnitEnvGraph InternalUnitEnv

data UnitEnvGraph v = UnitEnvGraph
  { unitEnv_graph :: !(Map UnitId v)
  , unitEnv_currentUnit :: !UnitId
  }

data InternalUnitEnv = InternalUnitEnv
  { internalUnitEnv_dflags :: DynFlags
  , internalUnitEnv_homePackageTable :: HomePackageTable
  }
```
**Note: Names are subject to bikeshedding**
As you can see, `InternalUnitEnv` is basically just a tuple of `hsc_dflags` and `hsc_HPT`, but it might be extended in the future.
The *important* new type is `UnitEnv` which describes a mapping from `UnitId` to `InternalUnitEnv` and represents our support for multiple home units. It is backed by `UnitEnvGraph` which is parameterised by the actual values, mainly for `Traversable`, `Foldable` and `Functor` instances. Why is it named graph you may ask? Because a home unit might have a dependency on another home unit! Just take our previous example, the simple cabal project, where we have an executable and a library. If we want to have both of these units in a repl session, then the executable and the library are home units, where the former depends on the latter. So, we not only maintain an indefinite amount of home units, we also need to make sure that there are no cyclic dependencies between our home units.
The field `unitEnv_currentUnit` indicates which home unit we are currently "working" on. It is helpful to maintain some form of backwards compatibility, e.g. now `hsc_dflags` and `hsc_HPT` are no longer record fields (since there is no longer a single `DynFlags` or `HomePackageTable`) but functions, and they use `unitEnv_currentUnit` to retrieve the appropriate `DynFlags` and `HomePackageTable`.
These are the core changes. Now, only the rest of `GHC` needs to be compatible with it! Luckily, guided by the type-system, changes are mostly mechanical and not very interesting. But how does the user interact with it? How can we now load multiple components into the same repl session?

### Usage

The existing command line interface of `GHC` does not suffice to capture the new features satisfactorily.
Therefore, we introduce two modes for ghc:

```bash
ghc -unit @unitA -unit @unitB ... -unit @unitZ
```

This might look a bit weird to you, but no worries, it is actually quite simple:
the argument `@unitA` uses response file syntax, where `unitA` is a filepath that contains all the compilation options necessary to compile the home unit.
We can produce all sorts of compiler artefacts, such as `.hie` or `.hi` files, which can be used for producing binaries.

A current limitation is, that each unit must supply the `-no-link` argument, to avoid reading information from disk. Unfortunately, this limitation is currently unlikely to be lifted, since it violates the separation of Cabal and GHC.

For using multiple home units in `GHCi`, we can use a similar invocation:

```bash
ghc --interactive -unit @unitA -unit @unitB ... -unit @unitZ
```

which drops us into an interactive session.

Now we come to the tricky part: What definitions should be in scope? Should be the definitions of all home units be in scope at the same time? What happens if you have multiple definitions with the same name? What about module name conflicts? So, there are a lot of open questions.
At the time of this writing, we decided to follow a more stateful approach: We choose one home unit that is currently *active*, e.g. every `GHCi` query is "relative" to the active home unit. To illustrate this, take the following example:

<p align="center">
![Home unit dependency structure where `UnitA` is active](https://i.imgur.com/nAG8Ukr.png)
</p>

Assume we have three home units, `UnitA`, `UnitB` and `UnitC`, where `UnitA` has a dependency on the other two. We say that `UnitA` is *active*, and so its dependencies (including `UnitB` and `UnitC`) are in scope in our interactive session. However, the dependencies of `UnitB` or `UnitC` are **not** in scope.
When we switch the active home unit to `UnitC`:

<p align="center">
![Home unit dependency structure where `UnitC` is active](https://i.imgur.com/0QK9l9m.png)
</p>

then **only** the dependencies of `UnitC` are in scope. In particular, no definition from `UnitA` or `UnitB` is in scope.

As we all know, state is bad. Why are we choosing stateful behaviour for the implementation then? The first and most important reason is, to have something to show that is easy to implement. Secondly, in my opinion it is the more intuitive behaviour. We also examined other ideas:

* Avoid state by making all home units "active" all the time.
    * Besides the implementation being way more complex, it felt non-trivial to know which definition is actually being invoked, what home unit a certain module comes from, etc...
    * There is also a more practical concern: If you have two home units that have no dependencies on each other, then there is nothing to stop them from having a dependency on the same library... with a different version. If we wanted to import now a module from this dependency, which of the two versions should we pick? This is non-trivial to answer.
* Don't bother with it.
    * For one of the main motivations, which is simplifying IDEs, we don't need `GHCi` support, so, just don't implement it. Seems like a reasonable course of action (don't introduce half-baked features), but it would be a dissappointment for the average user.

However, in the end, the opinion of the community matters the most, and you are welcome to add your personal input to that matter in the [Merge Request](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3950).

### Project Limitations

Unfortunately, there are two open issues that can not be solved within this project and must be taken care of in subsequent work.

#### Module visibility

Cabal packages usually define private and public modules, and other packages can depend on the latter. Without multiple home units, this does not concern the interactive session, as it is only possible to open a single component and this component can import and use its private modules. Therefore, there was no need for `GHC` to have an explicit notion of private/public modules and it just assumes that all modules are public. However, with multiple home units, the following situation might arise:

<p align="center">
![Module Visibility example](https://i.imgur.com/nWqbIRR.png)
</p>

The important issue is that module `D` might depend on module `C`, although `C` is private to Unit `Y` and it should not be possible for Unit `X` to depend on the private modules of Unit `Y`. Therefore, we might accept programs that are not valid for a cabal package. However, we do not expect real-world problems as long as tools such as cabal make sure a package only imports public modules.

A potential way to solve this issue, is to make `GHC` understand module visibility. In particular, we would need to extend the command line interface to specify the visibility of a module and dependency resolution would need to detect invalid imports. In theory, this should not be difficult, as `GHC` already understands when a user imports private modules from external packages and provides helpful error messages.

#### Package Imports

The language extension `PackageImports` is used to resolve ambiguous imports from different units. In particular, to help disambiguate importing two modules with the same name from two different units.

Example:

```haskell
{-# LANGUAGE PackageImports #-}
import "foo" Data.Foo
import "bar" Data.Foo
```

It uses the package name for the disambiguation. The problem is that the `package-name` is read from disk (or more precisely, from the package database) and for home units, there is no such information on disk, therefore this feature cannot work at the moment.

Here is one idea how to solve this:
A home unit needs to have its unit-id specified over a command line interface flag and it usually looks like `-this-unit-id <unit-name>-<version>-<hash>`. I say usually, because there is currently no standard way to specify the unit-id and thus no way to get the name of the package. However, if we are to create a naming-scheme for unit ids, we could obtain the package-name from it, but such a proposal is out of scope for this project.

### What's next?

I hope, you now have an overview of what this project is about and what some of the changes might entail for end-users.

Although I consider this project a success, I am not quite satisfied with the results. In my opinion, I did not reach enough of my goals, and I plan to continue with some of the work until it is finally done.
There is still some work left before the proposed changes can be merged into `GHC`. Some of this work is mainly writing tests and updating documentation, but we need discuss `GHCi` and its interaction with multiple home units.

To make this whole project easier to merge, some of its changes have been split into smaller MRs, which have been recently merged into `GHC`:

* [Move file target parsing from `ghc/Main` to library](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3897/pipelines)
* [Add mainModuleNameIs and demote mainModIs](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3974/pipelines)

The main MR, that I eventually want to get merged, can be found at:

* [WIP: Implement Multiple Home Units for GHC](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3950)

In order to let `cabal` and `stack` users benefit from multiple home units, the tools themselves still need to be patched. This turned out to be way harder than hoped, since `Cabal` (the library, not the executable) is designed with single components in mind (what this means exactly is out of scope for this post. In short, we don't have the relevant information available for multiple components at the same time). As a work-around, to actually test the `GHC` changes on real projects, we patched `cabal` to support multiple components in the same repl session... by completely ignoring the clean separation between `cabal` and `Cabal`. This renders the patch virtually useless, as it can never be merged into mainline `cabal`.

Maybe `stack` is easier to patch but unfortunately I ran out of time before being able to even look into this part of my project.

Patching `ghcide` to compile with `GHC HEAD` proved to be quite the adventure. The Hackage overlay `head.hackage` is immensely helpful but the documentation is a bit lacking for first time contributors, and some migrations for relevant packages (such `hie-bios` and `ghc-check`) were quite complex. While `ghcide` now compiles with `GHC HEAD` (from two months ago), time ran out before multiple home units could be used as a tech-preview.

## Conclusion

Alright, this was a long blog post. It still does not cover everything that happened during the summer, only the main parts or at least what I consider to be the most interesting parts.

During this summer, I had the opportunity to work on `GHC`, learn a lot about the open-source process and as a bonus, I got to present the project at the ICFP Haskell Implementors Workshop. It was insanely exciting! However, more than once I felt like I was not going to make it, that everything would be a catastrophic failure and I should never have attempted to work on a project where I am completely out of my depth. It got worse, when I realised that I would not even meet half of my goals for the project.
But in the end, it seems like I made it, thanks to my helpful mentors, and I am really happy that I took this amazing opportunity.
Now I am really proud of having preservered and emerged with a better understanding of something that seemed so out of reach when starting this project.

As a sendoff, I can whole-heartedly recommend to everyone to participate in Google Summer of Code at Haskell.org!