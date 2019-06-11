---
title: Making use of GHC bindists built by GitLab CI
---

The new GHC GitLab CI infrastructure builds hundreds of different commits
a week. Each commit on `master` is built, as well as any merge requests; each
build produces an bindist which can be downloaded and installed on the
relevant platform.

[`ghc-artefact-nix`](https://github.com/mpickering/ghc-artefact-nix)
provides a program `ghc-head-from` which downloads and
enters a shell providing an artefact built with GitLab CI.

<!--more-->

## Using `ghc-artefact-nix`

You can install `ghc-head-from` using [`NUR`](https://github.com/nix-community/NUR).

```
nix-shell -p nur.repos.mpickering.ghc-head-from
```


There are three modes of
operation.

### Grab a recent commit from `master`

```
ghc-head-from
```

### Grab a merge request

```
ghc-head-from 1107
```

### Grab a specific bindist (for example, from a branch or fork)

```
ghc-head-from https://gitlab.haskell.org/ghc/ghc/-/jobs/98842/artifacts/raw/ghc-x86_64-fedora27-linux.tar.xz
```

The URL you provide has to be a direct link to a `fedora27` bindist.

## Technical Details

The bindist is downloaded from the (very flaky) CDN and  patched to remove
platform specific paths. The `fedora27` job is used because it
is built using `ncurses6` which works better with nix.

### Using an artefact in a nix expression

The [`old-ghc-nix`](https://github.com/mpickering/old-ghc-nix) repo provides a
`mkGhc` function which can be used in a nix expression to create an attribute for
a specific bindist. It is also packaged using `NUR`.

```
nur.repos.mpickering.ghc.mkGhc
  {  url = "https://gitlab-artifact-url.com"; hash = "sha256"; ncursesVersion = "6"; }
```

The `ncursesVersion` attribute is important to set for `fedora27` jobs as the
function assumes that the bindist was built with `deb8` which uses `ncurses5`.

If you plan on using the artefact for a while then make sure you click the "keep"
button on the artefact download page as otherwise it will be deleted after
a week. This is very useful if you are developing a library against an
unreleased version of the compiler and want to make sure all your collaborators
are using the same version of GHC.



