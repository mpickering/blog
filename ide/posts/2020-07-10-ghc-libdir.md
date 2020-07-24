---
posttitle: Haskell Language Server, binaries and installation
author: Luke Lau
date: 2020-07-24
github: bubba
---

If you've ever had to install *haskell-ide-engine* or *haskell-language-server*,
you might be aware that it is quite a lengthy process. There are several reasons
for this, two of the most significant being:

- Both *haskell-ide-engine* and *haskell-language-server* act as a kitchen sink
  for plugins. These plugins all depend on the corresponding tool from Hackage,
and as a result they end up pulling in __a lot__ of transient dependencies.
- The GHC API that powers the underlying *ghcide* session only works on projects
  that match __the same__ GHC version as it was compiled with. This means that
in order to support multiple GHC versions (which is quite common with Stack
projects that can define a specific version of GHC) the install script needs to
build a binary of *haskell-ide-engine*/*haskell-language-server* __for every
single GHC version__ that is supported.

The latter is the purpose of the *hie-wrapper*/*haskell-language-server-wrapper*
executable. The `install.hs` script will install binaries for every version
under `haskell-language-server-8.6.5`, `haskell-language-server-8.8.3`, etc. The
wrapper is then used in place of the language server, and will detect what
version of GHC the project is using and launch the appropriate version of
*hie*/*haskell-language-server*.

Building all these different binaries with several different versions of
GHC means you need to build every single dependency multiple times over, leading
to some hefty build times and chewing through a lot of disk space. On top of
this, installation from source is the _only_ supported installation method so
far. 
This isn't great for newcomers or for those who just want to setup a working IDE
in a pinch. So many of us have spent the past month hard at work trying to
improve the installation story.

# Static binaries

One obvious solution would be to just provide static binaries. This has been a
[long
running discussion](https://github.com/haskell/haskell-ide-engine/issues/1068),
that dates all the way back to *haskell-ide-engine*. When we say static
binaries, we mean a binary that has no dynamically linked libraries. You can
test that by running `ldd`:

```
$ ldd haskell-language-server-wrapper
    linux-vdso.so.1 (0x00007fff185de000)
    libz.so.1 => /usr/lib/libz.so.1 (0x00007faa3a937000)
    libtinfo.so.5 => /usr/lib/libtinfo.so.5 (0x00007faa3a8d2000)
    librt.so.1 => /usr/lib/librt.so.1 (0x00007faa3a8c7000)
    libutil.so.1 => /usr/lib/libutil.so.1 (0x00007faa3a8c2000)
    libdl.so.2 => /usr/lib/libdl.so.2 (0x00007faa3a8bc000)
    libpthread.so.0 => /usr/lib/libpthread.so.0 (0x00007faa3a89a000)
    libgmp.so.10 => /usr/lib/libgmp.so.10 (0x00007faa3a7f7000)
    libm.so.6 => /usr/lib/libm.so.6 (0x00007faa3a6b2000)
    libc.so.6 => /usr/lib/libc.so.6 (0x00007faa3a4eb000)
    /lib64/ld-linux-x86-64.so.2 => /usr/lib64/ld-linux-x86-64.so.2 (0x00007faa3a9ab000)
```

That's a lot of linked libraries, all of which will need to be available on the
users machine if we were just to ship the binary like that. With cabal-install,
we can statically link these with just `cabal build
--enable-exeutable-static`:

```
$ ldd haskell-language-server-Linux-8.10.1 
	not a dynamic executable
```

However one big caveat is that this only works on Linux. macOS doesn't really
have a notion of truly static binaries, since the system libraries are only
provided as dylibs. The best we can do is just ensure that the only dynamically
linked libraries are the ones already provided by the system, which it looks
like it was in the first place!

```
$ otool -L haskell-language-server
haskell-language-server:
	/usr/lib/libncurses.5.4.dylib (compatibility version 5.4.0, current version 5.4.0)
	/usr/lib/libiconv.2.dylib (compatibility version 7.0.0, current version 7.0.0)
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1281.100.1)
	/usr/lib/libcharset.1.dylib (compatibility version 2.0.0, current version 2.0.0)
```

# Static binaries that actually work on other machines

Unfortunately, making a static binary is one thing, but having that binary be
portable is another. If we want the binary to run on other machines outside of
the host, it can't rely on data-files, which some [plugins such as *hlint*
used](https://github.com/haskell/haskell-ide-engine/issues/1143). And the same
goes for any `libexec` binaries, which *cabal-helper* took advantage of.

Once these were [taken care of
upstream](https://github.com/ndmitchell/hlint/pull/824), we then had to deal
with GHC library directory: This is a directory that comes with your GHC
installation, typically in something like `/usr/local/lib/ghc-8.10.1/` or in
`~/.stack/programs/x86_64-osx/ghc-8.10.1/lib/ghc-8.10.1/` for Stack. Inside it
contains all the compiled [core
libraries](https://wiki.haskell.org/Library_submissions#The_Libraries), as well
as the actual ghc executable itself: Your `/usr/bin/ghc` is most likely a script
that just launches the binary in the library directory!

```bash
#!/bin/sh
exedir="/usr/local/lib/ghc-8.10.1/bin"
exeprog="ghc-stage2"
executablename="$exedir/$exeprog"
datadir="/usr/local/share"
bindir="/usr/local/bin"
topdir="/usr/local/lib/ghc-8.10.1"
executablename="$exedir/ghc"
exec "$executablename" -B"$topdir" ${1+"$@"}
```

Either way, *ghcide*/*haskell-language-server* use the GHC API, which [needs to
know where this directory
is](https://hackage.haskell.org/package/ghc-8.10.1/docs/GHC.html#v:runGhc) to do
it's job:

```haskell
runGhc :: Maybe FilePath -- ^ The path to the library directory
                         -- Sometimes called the top_dir
       -> Ghc a
       -> IO a
```

The most common way to get the path to the library directory is through the
[*ghc-paths*](https://github.com/simonmar/ghc-paths/blob/master/Setup.hs)
package, which uses [some custom `Setup.hs`
magic](https://github.com/simonmar/ghc-paths/blob/master/Setup.hs) to work out
where the library directory is, for the GHC **that is being used to compile the
program**. It bakes in the paths at compile time, which means it's portable if
we share the source and build it on other systems. But if we build it on
one system where the library directory is at `/usr/local/lib/ghc-8.10.1` for
example, then when **distributing the binary** to another system it will still
try looking for the old path which resides on a completely different machine!
For example, if GHC was installed via ghcup on the other system, then the
library directory would reside at `~/.ghcup/ghc/8.10.1/lib/`: a very different
location.

So if we want to be able to distribute these binaries and have them actually run
on other systems, *ghc-paths* is out of the question. This means that we have to
somehow get the library directory oureslves **at runtime**. Thankfully, the
`ghc` executable has a handy command for this:

```
$ ghc --print-libdir
/usr/local/lib/ghc-8.10.1
```

We could just call this directly. But what if you had a Cabal project,
configured with `cabal configure -wghc-8.8.3` whilst the `ghc` on your PATH was
version 8.10.1? Then the library directories would have mismatching verisons!
What we can do instead however is:

```
$ cabal exec ghc -- --print-libdir
Resolving dependencies...
/usr/local/lib/ghc-8.8.3
```

And consider even the case for Stack, where it downloads GHC for you. Well, we
can do the same thing as Cabal:

```
$ stack exec ghc -- --print-libdir
/Users/luke/.stack/programs/x86_64-osx/ghc-8.6.5/lib/ghc-8.6.5
```

These commands are tool specific, so it only made perfect sense to put this
logic into [*hie-bios*](https://hackage.haskell.org/package/hie-bios), our
library for interfacing and setting up GHC sessions with various tools. Now
there's an API for cradles to specify how to execute the `ghc` executable that
they use when building themselves.

# Automated builds with GitHub Actions

The build process is automated across a wide range of platforms and GHC versions
on GitHub Actions, which gets triggered everytime a release is made. Previously
setting up Haskell on Travis CI/CircleCI/AppVeyor used to be pretty fiddly, but
the [setup-haskell](https://github.com/actions/setup-haskell) action for GitHub
has made dramatic strides recently. In just a few lines of yaml we can setup a
rather hefty build matrix for all the verisons we support:

```
  build:
    runs-on: ${{ matrix.os }}

    strategy:
      fail-fast: false
      matrix:
        ghc: ['8.10.1', '8.8.3', '8.8.2', '8.6.5', '8.6.4']
        os: [ubuntu-latest, macOS-latest, windows-latest]
        exclude:
          - os: windows-latest
            ghc: '8.8.3' # fails due to segfault
          - os: windows-latest
            ghc: '8.8.2' # fails due to error with Cabal
```

<p align="center">
<img alt="Now that's what I call a build matrix" src="/ide/images/github-actions-matrix.png" height="300"/>
</p>

Unfortunately the story of Haskell on Windows is a bit hairy as usual, so there
were a few bumps that needed worked around. The biggest and most annoying one by
far was hitting the `MAX_PATH` limit for file paths whenever we tried to build
the `haskell-language-server-wrapper` executable. Admittedly this is a rather
long name for a binary, but a combination of the fact that GitHub actions
checks out the source directory in
`D:\a\haskell-language-server\haskell-language-server` and how Cabal's
per-component builds nest build products crazy deep meant that we we're
constantly going over the rather stringent 260 character limit:

```
Linking D:\a\haskell-language-server\haskell-language-server\dist-newstyle\build\x86_64-windows\ghc-8.10.1\haskell-language-server-0.1.0.0\x\haskell-language-server-wrapper\build\haskell-language-server-wrapper\haskell-language-server-wrapper.exe ...
55
realgcc.exe: error: D:\a\haskell-language-server\haskell-language-server\dist-newstyle\build\x86_64-windows\ghc-8.10.1\haskell-language-server-0.1.0.0\x\haskell-language-server-wrapper\build\haskell-language-server-wrapper\haskell-language-server-wrapper-tmp\Paths_haskell_language_server.o: No such file or directory
56
`gcc.exe' failed in phase `Linker'. (Exit code: 1)
```

We tried several things including

- Enabling the [LongPathsEnabled registry
  key](https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#enable-long-paths-in-windows-10-version-1607-and-later)
to disable this restriction. But it turns out it was already on the entire time
and [GHC/GCC aren't using the right Windows API calls](https://gitlab.haskell.org/ghc/ghc/issues/13373)
- Checking out the code in a different root directory, but it doesn't seem to be
  possible with GitHub actions
- Squashing the build directory with just `--build-dir=b` – still left us 2
  characters over the limit!
- Disabling per-component builds – just fails with another build error

But at the end of the day, the only reliable solution was just to rename
`haskell-language-server-wrapper` to something shorter whilst building:

```
- name: Shorten binary names
  shell: bash
  run: |
    sed -i.bak -e 's/haskell-language-server/hls/g' \
               -e 's/haskell_language_server/hls/g' \
               haskell-language-server.cabal
    sed -i.bak -e 's/Paths_haskell_language_server/Paths_hls/g' \
               src/**/*.hs exe/*.hs
```

There's still some sporadic issues with [Cabal on
Windows](https://github.com/haskell/cabal/issues/6986) and GitHub Actions having
infrastructure outages so the builds aren't 100% flake free yet, but it does
provide a rather large build matrix with generous amounts of parallelism. [You
can check out the first release with binaries
here.](https://github.com/haskell/haskell-language-server/releases/tag/0.2.1)

# The new Haskell Visual Studio Code extension

So you can download the binaries and manually put them on your path, which is
fine and dandy, but at the end of the day the ultimate goal was to make the
process of setting up a Haskell environment as easy as possible for newcomers.
So now the Visual Studio Code now takes full advantage of these binaries by
*automatically downloading them*.

It first downloads the wrapper, which it can use to detect what GHC version the
project is using. Then once it knows what GHC your project needs, it downloads
the appropriate `haskell-language-server` for the matching GHC and platform
before spinning it up. That way you only need the binaries for the GHC versions
you are using, and the extension will automatically download the latest binaries
whenever a new version of `haskell-language-server` is released. The video below
shows it in action:

<video controls alt="Visual Studio Code demo" style="width: 100%" src="/ide/images/vscode-demo.mov" />

Hopefully this one click install will help users get setup a lot more quickly,
but it is worth noting that at either GHC, Cabal or Stack still need to be
installed depending on the project. They're needed for both the aforementioned
GHC library directory, as well as building dependencies etc. (But someday in the
near future, we might be able to automatically download these tools as well!)

In the coming weeks the Visual Studio Code extension, previously known as
`vscode-hie-server`, will be hosted under the Haskell organisation and become
just "Haskell" in the extension marketplace. This should give a new unified,
official front for the language server, which is a labour of love of the entire
community: The road to these static binaries was the work of many contributors
across ghcide, hie-bios and haskell-language-server. Thanks to Javier Neira for
ironing out all the kinks on Windows, Brian McKenna and amesgen for making the
binaries truly static on Linux, and [all those who helped test it
out](https://github.com/haskell/haskell-language-server/issues/158).

