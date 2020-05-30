---
posttitle: Weekly Update and Multiple Components
author: Fendor
date: 2020-05-15
github: fendor
---

This is the second blog post in this year's series of IDE blog posts. In this blog post I will first present the progress we made during the last week, what we have been working on, what has been implemented, and what has been fixed. In the second section I will give some insights on why Haskell IDE Engine is in the retirement process. It explains the shortcomings of its structure and why ghcide supersedes it. In the last section, I will explain the motivation for implementing multiple home packages for GHC and how it, hopefully, will benefit IDEs and every-day users of GHC.

[![Jump To Instance Definition](https://s6.gifyu.com/images/JumpToInstance.gif)](https://gifyu.com/image/nkjS)

<!--more-->

## This week

A week has passed since the introduction to the state of IDEs in Haskell. A lot has happened in this one week and I am here to tell you about a few things in detail!

### Support for GHC 8.10.1

Maintaining an IDE is a lot of work and we couldn't do it without the work of
volunteers. Part of the maintenance work is updating dependencies and fixing
any errors that might occur! This week, as during countless weeks before that,
Javier Neira (\@jneira) took care of a lot of maintenance work such as
responding to issues, fixing bugs and updating Haskell Language Server to be
compatible with GHC 8.10.1!  Updating Haskell Language Server to support a new
GHC version is a huge effort, as it has plenty of dependencies and all of these
need to be updated in order to support the newer version.

### References

**Disclaimer**
This feature hasn't been merged, yet, due to issues with older GHC versions.

Modern IDEs need to be capable of showing you all references to a function,
where it is implemented and intelligent code search in general. It should not
be necessary to query the function name throughout the whole project to find
the implementation. While code-actions such as "Go to Definition" and "Go to
Type Definition" have been working for some time now, searching for all
references of an identifier has not been implemented as of yet.

But this is bound to change soon! Zubin Duggal developed a working implementation of finding references in a project based on [hiedb](https://github.com/wz1000/hiedb). `hiedb` is powered by [HIE](https://gitlab.haskell.org/ghc/ghc/-/wikis/hie-files) files, which are basically very verbose compiler artefacts that can be used to query references, the type of an expression, and a lot of other things (see projects such as [weeder](https://hackage.haskell.org/package/weeder)).

Below is a working example of how these HIE files can be used:

[![Find References](https://s6.gifyu.com/images/References.gif){width=100%}](https://gifyu.com/image/nkzg)

It allows developers to look for all usages of a function or type and to see all usages of a parameter or definition within a function!

### Jump to Instance Definition

**Disclaimer**
This feature is not yet merged into GHC, but will probably ship with GHC 8.12.

It is well known that you can jump to function definitions in Haddock. For
functions that are part of a type-class, you jump to the
type-class declaration. However, that is usually not what you want.  Thus, Zubin Duggal has
implemented a new feature that will allow you to jump to the specific instance
definition used by an overloaded function.

[![Jump To Instance Definition](https://s6.gifyu.com/images/JumpToInstance.gif)](https://gifyu.com/image/nkjS)

This is a really useful feature and will make it easier to explore a code base!
Moreover, it can make the IDE experience smoother for developers. LSP supports
returning multiple locations for go-to definition requests which we could exploit
to give the user a choice about where to jump to.

### First step towards `Open Telemetry` support

We are happy to announce that the first [Pull Request](https://github.com/mpickering/ghcide/pull/26) of Michalis (\@mpardalos) has been merged to Haskell Language Server!

It is a small step compared to the big task ahead to provide telemetry data of IDEs such as the Haskell Language Server. The final goal is to gain insight into the performance of big projects and optimising bottlenecks. To accomplish this, [Open Telemetry](https://opentelemetry.io/) will be used to emit relevant performance information. Very recently, the [opentelemetry](https://hackage.haskell.org/package/opentelemetry) package was added to Hackage by Dmitry Ivanov (\@ethercrow) which will aid the task.

### Memorable Bug-Fixes

I will now highlight some of the most memorable bugs we fixed this week, when working on the Haskell Language Server.

#### Completions

Since the last blog post, we were informed that there were no completions being provided by the Haskell Language Server, although there are completions when using ghcide.
What happened here? Why did Haskell Language Server not use the completion system of ghcide?

The answer was, the completion plugin had not been enabled. Thank you \@korayal for providing the [fix](https://github.com/haskell/haskell-language-server/pull/107) to this issue!

As an interesting side-note, the completion system of ghcide is almost the same as the one from Haskell IDE Engine! It was ported to ghcide (with some tweaks) and now found its way back into Haskell Language Server.

#### Bad Interface Files

A particularly nasty bug occured when cached interface files were out of sync.
The details of how a project is loaded are described later, but for now it suffices to know that packages are loaded only if one of its modules is loaded.
If multiple packages are loaded into the same Language Server session, the cached interface files reflect that fact. This does not cause any problem in the same session, but if it is restarted and a single package loaded, the cache files still try to refer to the package that is not currently loaded. This leads to the  whole session not being able to be loaded correctly and then displaying a sea of red squiggly lines until the previous session state is restored, e.g. other packages are loaded into the IDE.

This is obviously not the desired behaviour and, luckily, the fix was rather simple: We needed to isolate the cached interface files based on the packages that were currently loaded!
This means that we maintain a set of cached interface files for every subset of loaded local packages. To illustrate this, imagine you have three packages A, B and C.
The package A depends on B and C. When we load only the package A, we cache interface files so that subsequent loads of the package are faster. When we additionally load the package B, we cache interface files for B and new interface files for A. If we now load C, we have to re-cache the interface files again for A and B and generate new ones for C. In total, we now have three different sets of cache files for A, two for B and one for C. Overall, six sets of cached interface files have been created.
When restarting the session and only opening the package A we will now use interface files for A for which no other package has been loaded.
This is arguably a bit inefficient, but we expect to be able to reduce the number of cached interface files in the future.

We fixed the described bug at the cost of using more storage for cached interface files.

## The sunset of Haskell IDE Engine

In this section I am going to briefly explain some of the terminology which is often used in the IDE context.

* Component: A component is a set of modules that form a single unit of code. For example, the library of a project is a component, the test-suite is a component and the executable is one as well.
* Package: A package may consist of multiple components. It usually can be described using a single `.cabal` file.
* Packages in GHC: packages in GHC resemble components, seeing as they are a set of modules that can be compiled with given options.

As you may know, you can open a single component with `cabal` in a ghci session, via `cabal repl`. However, when you load an executable component which depends on the library component and make any changes to this component, you have to restart the whole session in order for your changes to take effect.
This is rather inconvenient for local development but it turns out that Haskell IDE Engine works rather similarly. It loads a single component, which it can update just fine, but if you try to load a second one, a separate session (in a so-called `HscEnv`) is created which is separated from the first component. Thus, you may modify each component individually, but updates from one component will not be reflected on the other one until the whole Server is restarted. This is a major flaw in Haskell IDE Engine, one that is almost impossible to fix with the current structure. It would require to copy substantial parts of GHC into Haskell IDE Engine, and heavily modify them in order to implement this somewhat basic feature of updating the executable when the library is changed. In the end, this flaw sealed the fate of Haskell IDE Engine. Out of the ashes, ghcide rose, integrating some parts of GHC, such as parsing and typechecking a module and gaining better control and performance.

However, at the time of this writing, mainline ghcide does not support loading multiple components at the same time. So, was everything for nothing and Haskell IDEs will never get their happy ending? Not at all! It is merely not merged yet!
Haskell Language Server provides support for loading multiple components at once by using a fork of [ghcide](https://github.com/mpickering/ghcide) created by Matthew Pickering (\@mpickering). Therefore, we can continue on our quest, implementing feature after feature until Haskell Language Server is a full-blown, highly-performant, reliable IDE?
Unfortunately, there is still a catch to this story.

## Reliable Multi Component Support

The current implementation of multiple component support is somewhat... hacky. In the previous section, we described how Haskell IDE Engine supports loading multiple components and how this is lacking for a modern IDE. In Haskell Language Server, we are essentially tricking the compiler into thinking that all these components, libraries, executables and so on, are part of the same GHC package, each with a different set of compilation options. While this works fairly well in practice, it is complex and error-prone code. It is also a lot of work to maintain in the Language Server, e.g. supporting multiple GHC versions, keeping it performant, not introducing nasty bugs with new changes and adding new features, such as supporting [backpack](https://gitlab.haskell.org/ghc/ghc/-/wikis/backpack).

This is where my Google Summer Of Code project comes into play: this multiple component support ought to be taken care of by GHC itself! The Language Server should not be responsible of maintaining this chunk of code.
Technically, the aim is to support multiple home packages in GHC and my work will be based on [existing efforts](https://gitlab.haskell.org/ghc/ghc/merge_requests/935) of John Ericson (\@Ericson2314) who is going to mentor me in this project, alongside Matthew Pickering and Zubin Duggal. If this project succeeds, not only will it be easier for IDEs to support multiple components in general, there is also direct benefit for the whole community! The issue that you can only load a single component into the same `ghci` session without resorting to complex tricks will be solved by this too. Not only will this improve usability of `ghci` for every user, different tools such as `ghcid` could be updated as well.
