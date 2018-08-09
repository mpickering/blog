---
title: Using a development version of GHC with nix
---


Most documentation for using nix with Haskell concentrates on quite simple use
cases such as how to modify or add new dependencies. What about when you want
to do something more complicated? In this post we look at combining
modifications from different sources.
The goal is to create a package set which can
be used to compile packages with HEAD.

<!--more-->

The main problem which we are going to solve is that many packages need
modifying in order to compile with HEAD. There is already a nix package which
builds
a development version of the compiler and a corresponding package set but the
utility is limited as many packages will fail to compile.

For example, at the time of writing this post,
if we try and build the `primitive` package, the build fails due to a
missing `Semigroup` instance.

If you are impatient, here is the [complete example](https://gist.github.com/mpickering/209354a6affe2883ca07ffc4f4084af6) we are going to come up with.
We create a new top-level attribute `new-ghcHEAD` which we can use
to build packages with `ghcHEAD`.

```nix
let
  nixpkgs = import <nixpkgs> {};
  patchRepo =
    nixpkgs.fetchFromGitHub {
      owner = "mpickering";
      repo = "head.hackage";
      rev = "767dcf11b367ccff4a9fcd55df9c2432cd485fbe";
      sha256 = "1cdrcw7grpc2dyxnb7a5bg9mi1h7qnblcibi91s348034zf7a0vj";};

  patchDir = "${patchRepo}/patches";
  patchScript = "${patchRepo}/scripts/overrides.nix";
in
self: super:
{
  haskellPatches = self.callPackage patchScript
              { patches = patchDir; };

  new-ghcHEAD =
    let ghcPackageOverrides = self.callPackage self.haskellPatches {};
        localOverrides      =
          sel: sup: { haskell-src-exts
                        = sel.callHackage "haskell-src-exts" "1.20.1" {};
                      mkDerivation
                        = drv: sup.mkDerivation (drv //
                             { jailbreak = true; doHaddock = false;});
                     };

    in self.haskell.packages.ghcHEAD.extend
         (self.lib.composeExtensions localOverrides ghcPackageOverrides);
}
```


# A simple overlay

We are going to use an overlay in order to define the new package set.
An overlay is the idiomatic way to extend and change the top-level nixpkgs
set.  It is a function which takes two arguments, the result of performing the
modification
and the old unmodified package set. It returns a set of attributes which augment
the top-level attribute set.

```nix
self: super: {
  my-cool-new-attribute = ...;
}
```

As such `my-cool-new-attribute` will be in `self` but not in `super`. If we
want to use `my-cool-new-attribute` in another attribute we can refer to it
as `self.my-cool-new-attribute`.

When this overlay is installed (see below), we can use `my-cool-new-attribute`
like any top-level attribute defined in `nixpkgs`. The overlay mechanism surpasses
the old `packageOverrides` mechanism which used to be how to modify `nixpkgs`.

For example, we could enter an environment with `my-cool-new-attribute` available
with `nix-shell -p my-cool-new-attribute` as if it were defined in the main
`nixpkgs` repo.

There are two ways to easily install an overlay.

1. Pass it as an argument when importing nixpkgs.\
   ```
   let pkgs = import <nixpkgs> {overlays = [overlay1 overlay2 ..]};
   in ...
    ```
2. Place the overlay in `~/.config/nixpkgs/overlays/`

More details about using overlays can be found in the [user manual](https://nixos.org/nixpkgs/manual/#sec-overlays-install).


# Extending a package set

We'll start from a simple template which demonstrates the concept of extending
a Haskell package set like you might find in other documentation.
We start from the `ghcHEAD` package set (`haskell.packages.ghcHEAD`) and then
call the `extend` attribute with the extension function.

The extension function takes two arguments, the extended haskell package
set and the original haskell package set much like an overlay.
`ghcPackageOverrides` is the identity extension function in this example
which performs no modifications.

```nix
self: super: {
  new-ghcHEAD =
    let ghcPackageOverrides = sel: sup: {};
    in self.haskell.packages.ghcHEAD.extend ghcPackageOverrides ;
}
```

If we then want to fix a specific version of a package we can modify
`ghcPackageOverrides` like so:

```nix
self: super: {
  new-ghcHEAD =
    let ghcPackageOverrides =
          sel: sup: {
            haskell-src-exts =
              sup.callHackage "haskell-src-exts" "1.20.1" {};
          };
    in self.haskell.packages.ghcHEAD.extend ghcPackageOverrides;
}
```

When using the `new-ghcHEAD` package set, we will now install
`haskell-src-exts-1.20.1` rather than `haskell-src-exts-1.19.1` if it is needed
as a dependency.

## More extensions

Manually redefining each package to get it to build is tedious. Instead,
we're going to use the patches from [`head.hackage`](https://github.com/hvr/head.hackage)  in order to generate a lot of
modifications automatically. What we need are the patches in `head.hackage/patches` which enable packages to build with the bleeding edge version of
the compiler.

In `head.hackage/scripts` there are the necessary files in order to use the
patches with nix. `overrides.nix` is a wrapper which runs a haskell executable
in order to generate a nix expression explaining the package modifications. The
result of the derivation is a file which we can then import into our overlay
directly.

### Using a local copy of `head.hackage`

You will probably want to clone `head.hackage` locally so that you can add and
modify patches as necessary to get your project to compile.

In that case, instead of specifying a url, we just specify the correct paths
for the packaging script and patch directory.

For example:

```nix
self: super: {
  haskellPatches = self.callPackage <path-to-overrides.nix>
              { patches = <path-to-patch-dir>; };

  new-ghcHEAD =
    let ghcPackageOverrides = self.callPackage self.haskellPatches {};
    in self.haskell.packages.ghcHEAD.extend ghcPackageOverrides;
}
```

### Using a remote copy of `head.hackage`

If you don't want to maintain a local clone of the repo then you can also
fetch it from wherever you like. For example, here I fetch a specific branch
from my clone of the repo which contains some nix specific patches.

```nix
let
  nixpkgs = import <nixpkgs> {};
  patchRepo = nixpkgs.fetchFromGitHub {
    owner = "mpickering";
    repo = "head.hackage";
    rev = "767dcf11b367ccff4a9fcd55df9c2432cd485fbe";
    sha256 = "1cdrcw7grpc2dyxnb7a5bg9mi1h7qnblcibi91s348034zf7a0vj";};

  patchDir = "${patchRepo}/patches";
  patchScript = "${patchRepo}/scripts/overrides.nix";
in
self: super:
  {
  haskellPatches = self.callPackage patchScript
              { patches = patchDir; };

  new-ghcHEAD =
    let ghcPackageOverrides = self.callPackage self.haskellPatches {};
    in self.haskell.packages.ghcHEAD.extend ghcPackageOverrides;
}
```

## Combining `head.hackage` modifications with local modifications

Unfortunately, this is not quite enough as it is sometimes necessary to further
modify packages in order to get them to compile. The most obvious way is that
a lot of packages have restrictive version bounds but compile fine with newer
versions of libraries. So, in order to get around this we preemptively need
to jailbreak all packages.

Jailbreaking is a process which removes all version bounds from a package.
This is safe to do as each package set only contains one version of
each dependency anyway.

We introduce a new extension function `localOverrides` which will contain our
specific modifications which we don't want to include in `head.hackage`.
We will first only contain one specific modification which chooses
a new version of `haskell-src-exts`.


```nix
self: super: {
  haskellPatches = self.callPackage <path-to-overrides.nix>

              { patches = <path-to-patch-dir>; };

  new-ghcHEAD =
    let ghcPackageOverrides = self.callPackage self.haskellPatches {};
        localOverrides      =
          sel: sup:
            { haskell-src-exts
                = sel.callHackage "haskell-src-exts" "1.20.1" {};
            };
    in self.haskell.packages.ghcHEAD.extend
        (self.lib.composeExtensions localOverrides ghcPackageOverrides);
}
```

We need to use `composeExtensions` so that the attributes we locally modify
are then also modified by `head.hackage` incrementally.

### Modifying `mkDerivation`

Finally, we will modify `mkDerivation` in order to jailbreak packages and not
run `haddock` in order to speed up the builds. `mkDerivation` is the
function which takes a Haskell package description and turns it into a nix
derivation. It's the function which when given the what to build, says
precisely *how* to build it.


```nix
self: super: {
  haskellPatches = self.callPackage <path-to-overrides.nix>
              { patches = <path-to-patch-dir>; };

  new-ghcHEAD =
    let ghcPackageOverrides = self.callPackage self.haskellPatches {};
        localOverrides      =
          sel: sup: {
            haskell-src-exts
              = sel.callHackage "haskell-src-exts" "1.20.1" {}
            mkDerivation
              = drv: sup.mkDerivation (drv //
                  { jailbreak = true; doHaddock = false;});
          };

    in self.haskell.packages.ghcHEAD.extend
        (self.lib.composeExtensions localOverrides ghcPackageOverrides);
}
```

## Testing

We can now test our new attribute using `nix-shell`. For example, let's try to
provision a version of the compiler with `primitive` available.

```
> nix-shell -p "new-ghcHEAD.ghcWithPackages (ps: [ps.primitive])"
...
```

Success!

# Conclusion

Thanks to Csongor Kiss, Alessio Zakaria, Sarah Brofeldt and Vaibhav Sagar
for comments on a draft.

## Related Links



----------------------------------------------------------
# Appendix

## Addendum: What do I do if a patch from `head.hackage` doesn't apply?

There are some situations where patches from `head.hackage` fail to apply
properly. The usual cause is that patches in the `head.hackage`
repo apply to unmodified tarballs but the nix build process tries to apply
patches after patching the cabal file to take into account package revisions.

The workaround for this is to just create a new nix-specific patch in your local
directory. :)

## Addendum: How do I make a patch to contribute to `head.hackage`?

```
cabal get haskell-src-exts-1.20.1 <--pristine>
cd haskell-src-exts-1.20.1
git init
git add .
<Make the simple change>
git diff > <patchDir>/haskell-src-exts-1.20.1.patch
```

Use the `--pristine` flag if you intend to contribute the patch upstream!
This will fetch the package without applying cabal file revisions.

## Addendum: How does `overrides.nix` work?

`overrides.nix` is a simple derivation which packages a single haskell script.
The haskell script scans a directory and generates the correct overrides for
the given patches. The wrapper script then stores the overrides and copies the
patch files into the nix store.

```nix
{ stdenv
  , haskell
  , patches  # A directory containing patch files used to build packages
             # it can either be a local directory or fetched from the web
}:
let
  ghc = haskell.packages.ghc822.ghcWithPackages (ps: with ps;
          [ ]);
in

stdenv.mkDerivation {

  name = "hs-generate-overrides-0.1";

  src = ./generate-nix-overrides.hs;

  preUnpack = ''mkdir hs-generate-overrides'';
  buildInputs = [ ghc ];

  unpackCmd = ''
    cp $curSrc ./hs-generate-overrides
    cp -r ${patches} ./hs-generate-overrides/patches
    sourceRoot=hs-generate-overrides;
  '';

  buildPhase = ''
    ghc $src -o generate
    ./generate $script/patches patches > patches.nix
  '';

  outputs = ["out" "script"];

  installPhase = ''
    cp patches.nix $out
    ensureDir $script/patches
    cp -r patches $script/patches
  '';

}
```

## Addendum: How is jailbreaking different to cabal's `--allow-newer`?

The goal of jailbreaking and `--allow-newer` are the same, to remove restrictive
bounds on packages.

Jailbreaking achieves this by parsing and rewriting the cabal
file to remove nearly all bounds. This fits better into the nix ecosystem where
there is exactly one version of each package anyway so additional version bounds
are irrelevant.

`--allow-newer` influences the solver in order to pick a package set as if the
package didn't have any upper bounds.

## Addendum: Why are you using `extend` rather than `override`.

`override` is a general nix mechanism to override arguments to a function.
In the haskell package set case, one of the arguments to the package set is
called `overrides`, which is a function explaining how we want to modify the
package set further.

However, there is one big downside to using this mechanism, it is not
possible to repeatedly override the package set with more and more extensions
as each time you call `override` you *replace* the argument.

For more explanation of the difference see [this issue](https://github.com/NixOS/nixpkgs/issues/25887) on the nixpkgs issue tracker.


## Addendum: What to do if `callHackage` doesn't find the latest version of a package

If `callHackage` doesn't find a package which you know is on hackage then it
is because the `all-cabal-hashes` attribute is too old. In order to override it,
you can simply add a new line to your overlay with an updated copy.

For example, this version is from `2018-01-05`:

```
all-cabal-hashes = super.fetchurl {
  url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/e1089e56e666c2a0fe82f840d3cc7f49b9c9fe9b.tar.gz";
  sha256 = "0qbzdngm4q8cmwydnrg7jvipw39nb1mjxw95vw6f789874002kn1";
};
```

## Addendum: How to update the ghcHEAD derivation

If you want to use a more recent version of HEAD then you have to update
the ghcHEAD derivation. It can be found at `development/compilers/ghc/head.nix`.

You then need to modify three things.

1. Correct the version of the compiler by modifying the `version` argument.
2. Set `rev` to the git commit identifier of the revision you want to build.
3. Correct the `sha256` attribute by running `nix-prefetch-git` and copying the
   resulting hash.

    ```
    nix-shell -p nix-prefetch-git
     --run "nix-prefetch-git --fetch-submodules git://git.haskell.org/ghc.git <rev>"
    ```

