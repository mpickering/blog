---
title: Packaging a Haskell library for artefact evaluation using nix
---

This year I packaged two artefacts for the ICFP artefact evaluation process.
This post explains the system I used to make it easy to produce the docker images
using nix. I hope this documentation will be useful for anyone else submitting a Haskell
library for evaluation.

The end result will be an `artefact.nix` file which is used to build
a docker image to submit. It will be an entirely reproducible process as we will
fix the versions of all the dependencies we use.

<!--more-->

## The structure of the artefact

In this example, I am going to package the artefact from the paper ["Generic Deriving
of Generic Traversals"](https://dl.acm.org/citation.cfm?id=3236780). The artefact was a Haskell library and an executable which
ran some benchmarks. The resulting artefact will be a docker image which contains:

1. The source code of the library
2. The source code for the benchmarks
3. The executable of the benchmarks
4. An environment where it is possible to compile the library and benchmarks

To start with, I will assume that we have placed the source code and benchmarks
code in our current directory. We will add the rest of the files

```
>>> ls
generic-lens-1.0.0.1/
benchmarks/
```


## Step 1: Pinning nixpkgs

The most important step of the whole process is to "pin" our version of nixpkgs to
a specific version so that anyone else trying to build the image will use the
same versions of all the libraries and system dependencies.

Once we have established a commit of nixpkgs that out package builds with. We
can use `nix-prefetch-git` in order to create `nixpkgs.json` which will provide
the information about the pin.

```
nix-prefetch-git --rev 651239d5ee66d6fe8e5e8c7b7a0eb54d2f4d8621 --url https://github.com/NixOS/nixpkgs.git > nixpkgs.json
```

Now we have a file, `nixpkgs.json` which specifies which version of nixpkgs we
should use.

We then need to load this file. Some boilerplate, `nixpkgs.nix`, will do that
for us.

```
opts:
let
   hostPkgs = import <nixpkgs> {};
   pinnedVersion = hostPkgs.lib.importJSON ./nixpkgs.json;
   pinnedPkgs = hostPkgs.fetchFromGitHub {
     owner = "NixOS";
     repo = "nixpkgs";
     inherit (pinnedVersion) rev sha256;
   };
in import pinnedPkgs opts
```

`nixpkgs.nix` will be imported in `artefact.nix` and will determine precisely
the version of all dependencies we will use.


## Step 2: Using `dockerTools`

Now we have specified the set of dependencies we want to use we can go
about starting to build our docker image. Nixpkgs provides a convenient set of
functions called `dockerTools` in order to create docker images in a declarative
manner. This is the start of our `artefact.nix` file.

```
let
  pkgs = import ./nixpkgs.nix { };
in
with pkgs;
let
    debian = dockerTools.pullImage
      { imageName = "debian"
      ; imageTag = "9.5"
      ; sha256 = "1jxci0ph7l5fh0mm66g4apq1dpcm5r7gqfpnm9hqyj7rgnh44crb"; };
in
dockerTools.buildImage {
  name = "generic-lens-artefact";

  fromImage = debian;

  contents = [  bashInteractive
                glibcLocales
             ];

  config = {
    Env = ["LANG=en_US.UTF-8"
           "LOCALE_ARCHIVE=${glibcLocales}/lib/locale/locale-archive"];
    WorkingDir = "/programs";
  };
}
```

This is the barebones example we'll start from. We firstly import `nixpkgs.nix`
which defines the package set we want to use. Our docker file will be based on
`debian`, and so we use the `dockerTools.pullImage` function to get this base image.
The `imageName` comes from docker hub and the `imageTag`
indicates the specific tag.

This image is our base image when calling `dockerTools.buildImage`.
For now, we add the basic packages `bashInteractive` and `glibcLocales`, in the next step
we will add the specific contents that we need for our artefact.

Setting the `LANG`
and `LOCALE_ARCHIVE` env vars is important for Haskell programs as otherwise you can
run into strange encoding errors.

This is a complete image which can already be build with `nix-build artefact.nix`.
The result will be a `.tar.gz` which can be loaded into docker and run as normal.


## Step 3: Including the artefact

First we'll deal with making the executable itself available on the image.
Remember that the source code the the benchmarks, which is a normal Haskell package,
is located in `benchmarks/`.

We need to tell nix how to build the benchmarks. The standard way to do this is to
use `cabal2nix` to generate a package specification which we will pass to `haskellPackages.callPackage`.

```
cabal2nix benchmarks/ > benchmarks.nix
```

This will produce a file which looks a bit like

```
{ mkDerivation, base, criterion, deepseq, dlist, dump-core
, generic-lens, geniplate-mirror, haskell-src, lens, mtl, one-liner
, parallel, plugin, random, stdenv, syb, transformers, uniplate
, weigh
}:
mkDerivation {
  pname = "benchmarks";
  version = "0.1.0.0";
  src = ./benchmarks;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base criterion deepseq dlist dump-core generic-lens
    geniplate-mirror haskell-src lens mtl one-liner parallel plugin
    random syb transformers uniplate weigh
  ];
  license = stdenv.lib.licenses.bsd3;
}
```

Now we will add the executable to the docker image. A new definition is created
in the let bindings and then we add the executable to the `contents` of the image.

```
run-benchmarks = haskellPackages.callPackage ./benchmarks.nix {};
```

So now our `contents` section will look like:

```
  contents = [  bashInteractive
                glibcLocales
                run-benchmarks
                ];
```

When we build this image, the executable will be available on the path by
default. In our case, the user will type `bench` and it will run the benchmarks.

## Step 4: Including the source files

The next step is to add the source files to the image. To do this we use a the
`runCommand` script to make a simple derivation which copies some files into the
right place.


```
benchmarks-raw = ./benchmarks;
benchmarks =
  runCommand "benchmarks" {} ''
  mkdir -p $out/programs
  mkdir -p $out/programs/benchmarks
  cp -r ${benchmarks-raw}/* $out/programs/benchmarks
'';
```

All the derivation does is copy the directory into the nix store at a specific
path. We then just add this to the `contents` list again and also do the same
for the library itself and the README.

```
  contents = [  bashInteractive
                glibcLocales
                run-benchmarks
                benchmarks
                readme
                library];
```

Now once we build the docker image, we'll have the executable `bench` available
and also a file called `README` and two folders containing the library code
and benchmarks code.

## Step 5: An environment to build the source code

Finally, we need to do two more things to make it possible to build the source
programs in the container.

Including `cabal-install` in the contents is the first so that we can use
`cabal` in the container.

```
  contents = [  bashInteractive
                glibcLocales
                run-benchmarks
                benchmarks
                readme
                library
                cabal-install ];
```

The second is much less obvious, we need to make sure that the necessary dependencies
are already installed in the environment so that someone can just use `cabal build`
in order to build the package. The way to achieve this is to modify the `benchmarks.nix`
file and change `isLibrary` to `true`.

```
-  isLibrary = false;
+  isLibrary = true;
```

This means that all the build inputs for the benchmarks are propagated to the
container so all the dependencies for the benchmarks will be available to
rebuild them again.

## Complete `artefact.nix`

Here's the complete `artefact.nix` that we ended up with. We also generated
`nixpkgs.json`, `nixpkgs.nix` and `benchmarks.nix` along the way.

```
let
  pkgs = import ./nixpkgs.nix {};
in
with pkgs;
let
    debian = dockerTools.pullImage
      { imageName = "debian"
      ; imageTag = "9.5"
      ; sha256 = "1y4k42ljf6nqxfq7glq3ibfaqsq8va6w9nrhghgfj50w36bq1fg5"; };

    benchmarks-raw = ./benchmarks;
    benchmarks =
      runCommand "benchmarks" {} ''
        mkdir -p $out/programs
        mkdir -p $out/programs/benchmarks
        cp -r ${benchmarks-raw}/* $out/programs/benchmarks
      '';

    library-raw = ./generic-lens-1.0.0.1;
    library =
      runCommand "benchmarks" {} ''
        mkdir -p $out/programs
        mkdir -p $out/programs/library
        cp -r ${library-raw}/* $out/programs/library
      '';

    readme-raw = ./README;
    readme =
      runCommand "readme" {} ''
        mkdir -p $out/programs
        cp ${readme-raw} $out/programs/README
      '';

    run-benchmarks = haskellPackages.callPackage ./benchmarks.nix {};

in
dockerTools.buildImage {
  name = "generic-lens-artefact";


  fromImage = debian;

  contents = [  bashInteractive
                cabal-install
                glibcLocales
                run-benchmarks
                benchmarks
                readme
                library];

  config = {
    Env = ["LANG=en_US.UTF-8"
           "LOCALE_ARCHIVE=${glibcLocales}/lib/locale/locale-archive"];
    WorkingDir = "/programs";
  };
}
```

## Conclusion

Hopefully this tutorial will be useful for anyone having to package a Haskell
library in future. Each artefact is different so you'll probably have to modify
some of the steps in order to make it work perfectly for you.
It's also possible that the `dockerTools` interface will change but it should
be possible to modify the examples here to adapt to any minor changes. If you're
already using nix, you probably know what you're doing anyway.

## Related Links

* [Quick and Easy Nixpkgs Pinning - Vaibhav Sagar](https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/)
* [`dockerTools` documentation](https://nixos.org/nixpkgs/manual/#sec-pkgs-dockerTools)
* [Reproducible Environments With Nix - Blair Archibald](https://www.software.ac.uk/blog/2017-10-05-reproducible-environments-nix)
* Reddit comments








