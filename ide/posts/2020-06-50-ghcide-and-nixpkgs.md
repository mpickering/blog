---
title: Getting ghcide into nixpkgs
author: Malte Brandy
date: 2020-06-05
github: maralorn
---

This is a guest entry by maralorn an enthusiastic ghcide user.

A few weeks ago I got ghcide into nixpkgs, the package set of the package
manager nix and the distribution nixos. Mind you, that was not a brave act of
heroism or dark wizardry. Once I grasped the structure of the nixpkgs Haskell
ecosystem, it was actually pretty easy. In this post I wanna share my
experience and tell you what I learned about the nixpkgs Haskell infrastructure
and ghcide.
It took me quite a while to collect the knowledge lain out below, so I gather
it might be useful for someone else. But feel free to skip the parts that
aren‘t of interest to you. This is a very explanatory post which is probably
already much too long so I won‘t go to much into the details of actual nix
code. If you just want to know how to use ghcide in a nix environment jump to
the end of the post or just consult the README of ghcide.

<!--more-->

## What’s the problem?

Haskell development tooling setup is infamous for being brittle and hard to
setup. Every other day when someone asks on reddit or in the `#haskell`
channel, inescapably there will come at least one answer of the form "It’s not
worth the pain. Just use ghcid." I guess one point of this blog series is that
this does not have to be the case anymore.

But what were the reasons for this resignation? One is certainly that `ghcid`
is a really great and easy to use tool. But I think it‘s clear that a well done
language server can leverage you much further and to me `ghcide` has already
proven this.

### Compile your project and ghcide with the same ghc!

One source of frustration is likely that succesfully setting up a language
server that is deeply interwoven with `ghc` like `ghcide` has one very
important requirement. **You need to compile `ghcide` with the same `ghc`
(version) as your project.** This shouldn‘t be hard to achieve nowadays - I’ll
show how to do it if you use `nix` in this blogpost and I assume it‘s the
default in other setups - but if you fail to meet this requirement you are in
for a lot of trouble.

So why exactly do we need to use "the same ghc" and what does that even mean?
Frankly I am not totally sure. I am not a `ghcide` developer. I guess sometimes
you can get away with some slight deviations. But the general recommendation is
to use the same ghc version. I can tell you three situations that will cause
problems or have caused problems for me:

1. Using another `ghc` release. E.g. using `ghcide` compiled with `ghc` 8.6 on
   a `ghc` 8.8 project.
2. Using the same `ghc` release but with a patch to `ghc`. This happened to me
   while using the `obelisk` framework which uses a modified `ghc`.
3. Using the same `ghc` version but linked against different external libraries
   like `glibc`. This can happen when different releases of nixpkgs are
   involved.

To sum up, both ghcs should come from the same source and be linked against the
same libraries. Your best bet is to use the same binary. But that is not
necessary.

### How to actually get a ghcide on nix

So consider the following situation. You are using nix to develop your Haskell
project. To be more specific: To compile your project you use ghc installed
from nixpkgs. This probably means that you have a `default.nix` and a
`shell.nix` file in your project.

So what are your ways to get a ghcide?

1. Use the `ghcide-nix` derivation.
2. Install `haskellPackages.ghcide` from nixpkgs.
3. Compile one yourself using cabal.

So I was developing an obelisk project and wanted to use ghcide. The first way
was the recommend one in the `ghcide` README, but sadly that didn‘t work
because (as I learned later from Elliot Cameron) the obelisk project uses a
patched ghc. No luck here. We actually tried to pass the patched `ghc` into the
`ghcide-nix` derivation, but `haskell.nix` refuses to work with a not
`haskell.nix` ghc. So this was getting complicated quickly.
2. Was also not an option, since `haskellPackages.ghcide` was marked broken in
   nixpkgs and ignoring the flag just gave me tons of version mismatch errors.
   So that left option three: Compiling with cabal myself. That worked-ish,
   showing me that it‘s at least possible to get ghcide to run, but it is a
   very unclean and brittle solution.

After deciding that one and three can‘t help me I resolved to figure out how to
fix the second approach.

## The nixpkgs Haskell ecosytem and dependency resolution

So I was wondering: Why can cabal get the dependencies right and nixpkgs can‘t?
So for those of you who don‘t know: Let me digress on a short exploration of
the nixpkgs Haskell ecosystem.

### Haskell dependency resolution in general

Dependency resolution problems have a long history in Haskell. But today there
are two solutions that both work quite well in general.

1. Specify upper and lower bounds for every dependency in your cabal file and
   let cabal figure out a build plan. The times of cabal hell are over and this
   works quite well. Notably this is the way ghcide is supposed to be compiled
   in general.
2. Pin a stack LTS release for your dependencies and pin the version for
   packages not on stackage.

Now solution two is in some sense less complex to use, because at compile time
you don‘t need to construct a build plan. Of course, as I said, today cabal can
do this for you very smoothly, which is why I personally prefer the first
   approach.

### Haskell in nixpkgs - pkgs.hackagePackages

But how does nixpkgs do it? Well basically solution two. Everyday a cronjob
pulls a list of **all packages from a pinned stack LTS release** and creates a
derivation for every one of them. It also pulls **all other packages from
hackage** and creates a derivation for the **latest released version** of them.
(This happens on the haskell-updates branch of nixpkgs which get‘s normally
merged into nixpkgs master i.e. unstable once per week.) So then, you ask, how
does cabal2nix do dependency resolution? Well the short form is, it doesn‘t.
What I mean by that is: It completely ignores any version bounds given in a
cabal file or a pinned stack LTS release. It will just **take the one version
of every dependency that is present in nixpkgs** by the method I told you
above.

When I first learned about this I thought this was ludicrous. How can someone
device a not-solution like this? This is prone to fail. And indeed it does. A
large number of packages just don‘t build this way. Concretely the build will
either fail at compile time or more often cabal will complain that it can‘t
create a build plan. What that actually means: cabal says the one build plan we
provided it with is invalid because it does not match the given version bounds.
duh. So that packages get automatically marked broken after hydra, the nixos
build server, fails to build them. And oh boy, there are a lot of Haskell
packages broken in nixpkgs.

This is obviously very frustrating and so I vented my frustration in the nixos
discourse. I clearly hadn‘t understood a lot of what I just explained at that
time, so in retrospect definitely not one of my proudest moments.

Now what would be a suitable alternative to this for nixpkgs? Tough to say. We
could try to use some solution like the go, rust or node ecosystem and check in
a build plan for every package. Actually that can be a nice solution and if you
are interested in that you should definitely checkout the `haskell.nix`
infrastructure.
But that really does not go well together with providing all of hackage in
nixpkgs. For starters having every version of every Haskell package in nixpkgs
would already be very verbose. And it would lead to tons of different builds
for the same package even with the same version, one for every different valid
build plan of dependencies. And still if nixpkgs would only build the build
plans necessary to build every package once, your project would probably use
a different build plan and therefore you would need to compile a lot of
dependencies by yourself and couldn‘t use the precompiled derivations from
nixpkgs.

There can probably be said a lot more about this. But I have accepted that the
chosen solution in nixpkgs actually has a lot of advantages (mainly fewer
compilation work for everyone) and I actually haven‘t encountered a package I
couldn‘t get to build with nixpkgs. The truth is the best guess build plan
nixpkgs provides us with is normally not very far away from a working build
plan. And it actually is a reasonable build plan. As a Haskell developer I
think it is a good rule of thumb to always make your project work with the
newest versions of all dependencies on hackage. And then it‘s very likely that
your package will also work in nixpkgs.

Above I complained that a lot of Haskell packages are broken in nixpkgs. In
truth, all commonly used packages work and most other packages are very easy to
fix.

### How to fix a broken package in nixpkgs

So what can we do to fix a broken package on nixpkgs?

1. Often the error was actually fixed by an upstream version bound change, so
   you can always just try to compile the package. If it works make a PR
   against nixpkgs to remove the broken flag.
2. Often the problem is that the package can actually build with the supplied
   build plan but cabal doesn‘t believe us. So we can do a "jailbreak" and just
   tell cabal to ignore the version constraints. We don‘t do this by default
   because even if the package builds, it might now have changed semantics
   because of a change in a dependency. So a jailbreak package should be tested
   and reported upstream so that the cabal restrictions of that package can get
   fixed.
3. If those two don‘t help we can still override the build plan manually to use
   different versions of the dependencies, not the ones provided by nixpkgs by
   default.

## How to fix ghcide in nixpkgs

And the third option is what needed to be done for ghcide.

There were the following problems on nixpkgs-20.03:

* `hie-bios` was broken because of failing tests. Test fails during nix builds
  are very often false positives, so I disabled the tests.
* `ghcide` needed `regex-tdfa` and `haddock-library` newer than in the
  stack-lts. So I just used newer versions of those two libraries. This was not
  necessary on the haskell-updates branch because it uses a new enough stack
  lts release.
* `ghcide` pins the version of `haskell-lsp` and `haskell-lsp-types`. This will
  probably be the reason why maintaining `ghcide` in nixpkgs will always be a
  little bit of manual work because, it would have to be by chance _exactly_
  the `haskell-lsp` version from the stack lts release, to work without manual
  intervention.

So in summary only very few lines of code were needed to get `ghcide` to work.
If you are curious look at the commit. It

* enables the generation of `haskell-lsp` and `haskell-lsp-types` 0.19.
* uses those packages as dependencies for `ghcide`
* disables test for `hie-bios`
* and marks `ghcide` and `hie-bios` as unbroken.

### What if I can‘t upstream my package or am stuck with an older nixpkgs version?

Now I haven‘t told my story quite in order. I actually first went for another
solution, because I needed ghcide to work with my reflex-platform i.e obelisk
project. Obelisk uses a pinned nixpkgs version _and_ a patched ghc. So what I
did was putting the overrides I describe above as overrides into my projects
`default.nix`. That‘s always a nice way to first figure out how to fix a
dependency, but of course you help a lot more people if you find a way to
upstream the fixes into nixpkgs.  Because the nixpkgs version in
reflex-platform was so old I had to manually create some of the packages with a
function called `callHackageDirect`. It’s kinda the last way out, put it is
very flexibel and should be enough to solve most dependency issues. If nothing
else helps, create a build plan with cabal and reproduce it by hand with nix
overrides.

### ghcide in obelisk projects

That‘s actually what I did for ghcide in obelisk. Those overrides have now been
integrated into obelisk, so that we now have `ghcide` support in obelisk.
Enable one flag in the `default.nix` and you are good to go. Elliot Cameron who
integrated the changes, urges me to stress that this support is quite
experimental. But my experience with it is great so far. (Actually ironically
there was a time when `ghcide` worked perfectly, but `ghcid` was broken
somehow.)

So that was a lot of yadada. If someday you meet a broken Haskell package in
nixpkgs you now hopefully know why and how to fix it. Or at least that fixing
it is probably not hard and you should give it a shot.

## Using ghcide with nix

Now, when you want to use `ghcide` with nix you have two options. Either
`haskellPackages.ghcide` from nixpkgs or `ghcide-nix` which uses the
`haskell.nix` ecosystem. I will describe both solutions and their pros and
cons, from my point of view.

### haskellPackages.ghcide

First make sure you are on a new enough version of nixpkgs. You can try
installing `ghcide` user or system wide, with e.g. `nix-env -iA
haskellPackages.ghcide` or via your `configuration.nix` on nixos. But that has
a greater danger of being incompatible with the `ghc` you are using in your
specific project.
The less brittle and more versatile way is to configure `ghcide` in your
projects `shell.nix`. You probably already have a list with other dev tools you
use in there, like `with haskellPackages; [ hlint brittany ghcide ]`. Just add
`ghcide` in that list and you are good to go.

#### Pros

* Easy to setup
* Builds ghcide with the same ghc binary as your project, so no danger of
  incompatabilities between ghc and ghcide.

#### Cons

* We only have released versions of ghcide in nixpkgs. If you use
  nixpkgs-stable it might not even be the last release.
* When you use another `ghc` version than the default in your nixpkgs version,
  nix will compile ghcide on your computer because it isn‘t build by hydra.
  (But build times are totally fine.)

### ghcide-nix

You can import the `ghcide-nix` repo as a derivation and install the ghcide
from there. Consult the README for more details.

#### Pros

* Cached binaries for all supported `ghc` versions via cachix.
* Always a recent version from the ghcide master branch.
* Definitely recommended when you are already using the `haskell.nix`
  infrastructure for your project.

#### Cons

* Danger of incompatibilities, when your nixpkgs version and the pinned one of
  `ghcide-nix` don‘t match.
* Not compatible with a patched ghc, which is not build for the `haskell.nix` infrastructure.
* Larger nix store closure.

## Summary

Thank you for following me this long. I hope I have illuminated a bit the
situation with getting Haskell packages and `ghcide` specifically to run under
nixpkgs. My point here is, if you know a bit about the structure as I have
written above it should not be that hard.
Installing `ghcide` for sure isn’t anymore. It even works in fairly custom
special case development situations like obelisk. So my recommendation is, set
it up right now, you won‘t want to work without it anymore.

One other important point is, that the Haskell maintainers in nixpkgs are quite
fast and very friendly. So you can definitely get help if you have troubles
with getting a package into nixpkgs. (Remember: They are all already there. You
just need to fix them.)

Of course in this post I have touched a lot of topics, which could all use more
concrete how-to explanations, and I am far from an expert on all of them. So
hit me up if you think something is amiss and if you don‘t understand something
feel free to contact me and maybe we can clarify something.  I personally am
definitely looking forward to the first official release of
haskell-language-server and I am sure we can land it in nixpkgs quickly.

## Other Updates
