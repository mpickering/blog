---
posttitle: Haskell Language Server 0.2.0
author: Fendor
date: 2020-07-17
github: fendor
---

We are back for another update of this summer's Haskell IDE efforts.
Quite some time has passed since our last update, so we have a lot to tell you about!
We are going to speak about what is new in the latest release and what other new features are already waiting in the pipeline.

## Release Haskell Language Server 0.2.0

At the start of this month we released a new version of Haskell Language Server!
The ChangeLog is [huge](https://github.com/haskell/haskell-language-server/blob/master/ChangeLog.md#02) since the last release was quite some time ago!
While a lot of these changes are minor, e.g. fix some bug, bump some dependency version, there are also new features!
Most of the new features are added to the project [ghcide](https://github.com/digital-asset/ghcide) which we rely on, so they actually dont show up in the ChangeLog.

### Delete unused function definition

There is now a new code-action which allows deletion of unused top-level bindings! To trigger this code-action, you need to enable the warning `-Wunused-top-binds` in your project. For `cabal` and `stack`, you can enable it for your library by modifying your `*.cabal` file:

```
library
   ...
   ghc-options: -Wunused-top-binds
```

Note, that this warning is implied by `-Wall`, which you should always use to compile your project!

A big thank you to \@serhiip for implementing this nice code-action!

### Add Typeclass Constraint to function declaration

Another awesome addition that will especially help newcomers: Add a missing typeclass constraint!

Take this imaginary example:

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

equal :: Tree a -> Tree a -> Bool
equal (Leaf x) (Leaf y) = x == y
equal (Node x y) (Node m n) = equal x y && equal m n
```

We essentially just want to check that our trees are structurally identical, but, unfortunately, we forgot to add the constraint `Eq a` to the head of our function definition.

The fix is just two clicks away:

![A cute GIF about adding a typeclass constraint](https://i.imgur.com/TfOqBgI.gif)

Thanks to \@DenisFrezzato, who implemented this feature.

### Various performance improvements

This section is a bit vague, but going into more details would be out of the scope of this blogpost.

Here is a brief summary:

* A name cache for `*.HIE` files has been added. This ought to power future features such as `Go to Type Definition`, `Go to References` and similar.
* Avoid excessive retypechecking of TH codebases.
* Performance improvements for GetSpanInfo.

Also, not to forget all the performance improvements from previous blogposts that have been merged into upstream repositories step by step.

With all of these, the overall experience ought to be a little bit smoother than before.

## Upcoming Features

It is always a bit tricky to talk about new features before they are released. There can always be last minute changes or delays and everyone is let down if a feature isn't in the next release. This is frustrating for us too!

Nevertheless, I will tease some upcoming improvements, with a disclaimer, that we cannot promise that the features will make it into the next release.

### Prebuilt Binaries

This has been a long requested feature! The first issue I can find about it was created for Haskell IDE Engine in January 2019. Back then, Haskell IDE Engine was facing a number of road blocks, such as data-files that are not easily relocatable and reliance on packages such as `ghc-paths` which compile important run-time information into the binary. Piece by piece, these issues have been resolved by patching upstream libraries, using alternative APIs and querying the run-time information at, well, run-time. Major changes to [hie-bios](https://github.com/mpickering/hie-bios/) were necessary in order to make it possible to find the information we care about.

Now we are close to being able to offer pre-built binaries for Windows, macOS and Linux.

A natural extension of this will be to make it possible to download these binaries from your editor extension. This is also in the making, although, for now, only for the vscode extension `vscode-hie-server`.
With prebuilt binaries, we hope to make the setup experience for newcomers easier and faster, without the need to compile everything from scratch, which can take hours and hours.

As the cherry on top, we plan to integrate these pre-built binaries with the successful tool [ghcup](https://www.haskell.org/ghcup/). This will improve the tooling story for Haskell and setting up from zero to a full-blown development environment will be a matter of minutes.

### Simple Eval Plugin

A new plugin called "Eval" will be added soon to the Haskell Language Server! Its purpose is to automatically execute code in haddock comments to make sure that example code is up-to-date to the actual implementation of the function. This does not replace a proper CI, nor [doctest](https://github.com/sol/doctest#readme), but it is a simple quality of life improvement!

For example, assume the following piece of code:

```haskell
module T4 where

import Data.List (unwords)

-- >>> let evaluation = " evaluation"
-- >>> unwords example ++ evaluation
example :: [String]
example = ["This","is","an","example","of"]
```

Executing these haddock code-examples by hand is a bit tedious. It is way easier to just execute a _code lens_ and see the result.
With the "Eval" plugin, it is as easy as a single click to produce the relevant output:

![Evaluate Haddock code comment](https://i.imgur.com/raaTFKr.gif)

And as promised, changes to any of the relevant definitions are picked up and we can update our haddock example:

![Update Haddock code comment](https://i.imgur.com/G6WbAQd.gif)
