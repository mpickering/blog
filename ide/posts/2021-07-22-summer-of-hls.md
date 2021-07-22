---
posttitle: Summer of HLS
author: Fendor
date: 2021-07-22
github: fendor
---

Greetings!

This summer I am honoured to be able to work on HLS and improve its ecosystem!
The project consists of three sub-goals: Bringing HLS/GHCIDE up-to-speed with recent GHC developments, improving the very delicate and important loading logic of GHCIDE, and bringing a proper interface to cabal and stack to query for build information required by an IDE.

But before you continue, I'd like to thank the people who made this project possible! You know who it is? It is you! Thanks to your donations to [Haskell Language Server OpenCollective](https://opencollective.com/haskell-language-server) we accumulated over 3000 USD in the collective, making it possible for me to dedicate the whole summer to working on this project. Additionally, I'd like to thank the [Haskell Foundation](https://haskell.foundation/), with whom the [Haskell IDE Team](https://haskell.foundation/affiliates/) is affiliated, for their generous donation.
So, **thank you**!

Alright, let's jump into action, what do we want achieve this summer?

## GHC and GHCIDE

When GHC 9.0 was released, HLS had no support for it for almost three months and there is no work-in-progress PR for GHC 9.2. A big part of the migration cycle is caused by the module hierarchy re-organisation and changes to GHC's API. Because of that, it has taken a long time to migrate a large part of the ecosystem.

Haskell Language Server is big. In fact, so big that having every plugin and dependency updated immediately is close to impossible without having an entire team dedicated to upgrading for multiple weeks. However, the main features of the IDE are implemented in GHCIDE (the power-horse of Haskell Language Server). It has fewer features and fewer external dependencies. As such, contrary to HLS, upgrading GHCIDE within a reasonable amount of time after a GHC release *is* possible. Thus, we want to port GHCIDE to be compatible with GHC 9.2 alpha and lay the foundation to publish GHCIDE to Hackage.

Achieving this goal has clear advantages: an IDE for people who use the latest GHC version. However, it additionally helps developers in migrating their own project to newer GHC versions, since GHCIDE provides a convenient way to discover where an identifier can be imported from.

## Multiple Home Units

For a summary and some motivation on what this project is all about see this [blog post](https://mpickering.github.io/ide/posts/2020-10-12-multiple-home-units.html).

As a TLDR: it stabilises HLS' component loading logic and furthermore, enables some long-desired features for cabal and stack, such as loading multiple components into the same GHCi session.

## Cabal's Show-Build-Info

If you know of the so-called `show-build-info` command in cabal, you might chuckle a bit.
At least four authors (including myself) have already attempted to merge `show-build-info` for `cabal-install`. It was never finished and merged though.

However, implementing this feature would benefit HLS greatly, as it entails that HLS can eagerly load all components within a cabal project, e.g. provide type-checking and goto definitions for all components.
In particular, this would help the Google Summer of Code project adding [symbolic renaming](https://summerofcode.withgoogle.com/organizations/5997747863289856/#5971774971314176) support to HLS. Symbolic renaming can only properly function if all components of a project are known but currently, for stack and cabal projects, HLS has no way of finding all components and loading them. `show-build-info` solves this issue for cabal and there are plans to add a similar command for stack.

## Summary

I am happy to continue contributing to the HLS ecosystem and excited for this summer! Now I hope you are as excited as me. I will keep you all updated on new developments once there is some presentable progress.
