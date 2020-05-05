---
title: A Tip for Profiling GHC
---

GHC developers are often in a situation where we want to profile a new change to GHC to
see how it affects memory usage or runtime performance. In this post I will
describe quite an ergonomic way of profiling any merge request without having
to build the branch yourself from source or in any special mode. We'll download
the bindist from GitLab CI and then compile a simple GHC API application which
models the compilation pipeline which we can profile.

<!--more-->

## Step 1: Enter an environment with the bindist

I recently wanted to profile one of Sebastian Graf's great patches, seeing
as he put up a merge request on GitLab, CI built his patch and produced a bindist
for a large number of platforms. This included a Fedora bindist which can be passed to my tool [`ghc-head-from`](https://mpickering.github.io/posts/2019-06-11-ghc-artefact.html)
in order to enter an environment with his patched version of GHC available.

I found the URL for the bindist for his patch by navigating through the GitLab
interface.

```
ghc-head-from https://gitlab.haskell.org/ghc/ghc/-/jobs/289084/artifacts/raw/ghc-x86_64-fedora27-linux.tar.xz
```

Once it has finished downloading and installed, which takes a surprising amount of
time, the patched version of GHC will be available.

```
> ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.11.0.20200324
```

## Step 2: The simple GHC API program

Now we need the program we are going to profile. This is a simple GHC
API program which will read arguments from a file called `args` and then
just compile the modules as specified by the arguments.


```haskell
module Main where

import Lib
import GHC as G
import GHC.Driver.Session as G
import GHC.Driver.Session
import SrcLoc as G

import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import System.Mem
import Control.Concurrent
import Outputable

initGhcM :: [String] -> Ghc ()
initGhcM xs = do
    df1 <- getSessionDynFlags
    let cmdOpts = ["-fforce-recomp"] ++ xs
    (df2, leftovers, warns) <- G.parseDynamicFlags df1 (map G.noLoc cmdOpts)
    setSessionDynFlags df2
    ts <- mapM (flip G.guessTarget Nothing) $ map unLoc leftovers
    setTargets ts
    pprTraceM "Starting" (ppr ts)
    void $ G.load LoadAllTargets

main :: IO ()
main = do
    xs <- words <$> readFile "args"
    let libdir = "/nix/store/c7113gcm42jjjzpgygfmmrivdhrxgvvk-ghc-8.11.0.20200324/lib/ghc-8.11.0.20200324"
    runGhc (Just libdir) $ initGhcM xs
```

In the program you need to set the `libdir` to the `libdir` for the version
of `ghc` we just downloaded.

```
> ghc --print-libdir
/nix/store/c7113gcm42jjjzpgygfmmrivdhrxgvvk-ghc-8.11.0.20200324/lib/ghc-8.11.0.20200324
```

The `args` file contains a list of arguments that you would normally pass to GHC.
The wrapper is then compiled as normal, passing both the `-package` and `-prof`
flags.

```
ghc Profile.hs -package ghc -prof
```

## Step 3: The program you want to profile

Say for this example we want to profile a single compilation of `Cabal`, how
do we know what options we should pass to the wrapper program in order to
perform the compilation? The easiest way to work this out is to
ask `cabal` to compile the project and then copy the arguments it uses to
invoke GHC. So in the locally cloned `Cabal` repository, we can compile it
like normal and pass the `-v2` flag to get `cabal` to print the options it will
use to call GHC.

```
cabal v2-build -v2 Cabal | tee args
```

Then open the `args` file and delete everything apart from the arguments for the
final call to `ghc`. The final file should contain a single line with just the
options you want to pass to GHC to compile the project.

By using `cabal` to get the arguments it will also build any necessary dependencies
for us.

Note: You might need to fix some of the include paths if you are running the
executable in a different directory.


## Step 4: Running the profile

So now we have the program to profile and something to compile, we can profile
using any of the normal profiling modes.

```
-- Run a time profile
./Profile +RTS -p -l-au
-- Run a heap profile
./Profile +RTS -hy -l-au
```

Then you can use [`hs-speedscope`](https://mpickering.github.io/posts/2019-11-07-hs-speedscope.html)
to view the time profile or [`eventlog2html`](https://mpickering.github.io/eventlog2html/) to
view the heap profile. You will observe that the simplifier is very slow.

# Conclusion

The main disadvantage of this approach is that you can't add any cost centres into
the build. GHC comes with a limited number of hand written cost centres but not
covering a lot of functions.

It would be nice in future to automate some of these steps to make it even
more seamless to profile a specific MR.

