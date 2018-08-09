---
title: Fixing the missing `ar` error with a development build of GHC (on NixOS)
---

On NixOS, if you build GHC from source using Make or hadrian and try to use it
with cabal then you will be greeted with the following error:

```
cabal: The program 'ar' is required but it could not be found.
```

The way to fix this problem is to run cabal in an environment
with the `binutils-unwrapped` package available. You can then
use cabal as normal.

```
nix-shell -p binutils-unwrapped
```




