---
title: "Nix scaffolding for running Haskell plugins"
---

I've been all about writing [source plugins](http://mpickering.github.io/posts/2018-06-11-source-plugins.html) recently but have been dissatisfied
with how rough it is to use them practically. In particular, I am writing
plugins which don't change the semantics of my programs but are useful for
debugging. I only sometimes want to run them and don't want them to appear as
dependencies at all on Hackage. It needs to be easy to apply them to my own and
other people's packages.

Of course, the solution was to leverage my knowledge of nix to wrap up
everything into a nice interface. The key to the interface is a new
function `haskell.lib.addPlugin` which augments an existing package with a plugin.

```
addPlugin dump-core-plugin either
```

This invocation will now also output an HTML representation of the core program
of the `either` package by using the `dump-core` plugin.

This post will explain this interface in more detail but will not get into
the gritty implementation details. The implementation can be found in the
[`haskell-nix-plugin`](https://github.com/mpickering/haskell-nix-plugin) repo.

<!--more-->

# Two uses of plugins

There are two kinds of plugins:

1. A plugin which modify your program so should always run. For example, the
  [`ghc-typelits-natnormalise`](https://hackage.haskell.org/package/ghc-typelits-natnormalise) plugin which solves constraints containing type
  level numbers.
2. A plugin which collects information or debugging information which is
   optional. For example, the [`dump-core`](https://hackage.haskell.org/package/dump-core) plugin which outputs an HTML rendering
   of the core of your program.

Even though you only sometimes want to run plugins in the second category,
the convenient way to involves several steps:

1. Add the plugin package to your `build-depends`.
2. Pass a specific option `-fplugin=DumpCore` by modifying `ghc-options`.

If you want to package your library correctly, you then need to hide
this information behind a cabal flag. However, this is still undesirable as
a package like `dump-core` is only going to be used by developers but the flag
now appears in the public interface. Further to this, I can't run
my plugin on someone else's package without modifying the cabal file.

What we really want is a way to take an existing package and to augment it
to run the plugin. In order to do this, we define a function `addPlugin`
which takes a plugin and a package as an argument and then compiles the package
whilst running the plugin. For example, if we want to inspect the core of
the `either` package, we could specify this like so.

```
addPlugin dump-core-plugin either
```

If we then build this modified package, there will be a new output which
has the same name as the plugin which contains the output of the plugin. So
in this case, we will find the relevant HTML by inspecting the `DumpCore`
attribute. It will also be symlinked to `result-DumpCore`.

```
either-core = (addPlugin dump-core-plugin either).DumpCore
```

There are three new elements to the nixpkgs API.

1. A new function `haskell.lib.addPlugin` which adds a plugin to a package.
2. A new attribute `haskell.plugins` which is parameterised by
a Haskell package set and contains a set of plugins.
3. A new `with*` function, `haskellPackages.withPlugin` which takes a function
  expecting two arguments, the first being a set of plugins for that package set
  and the second being a list of packages for that package set. The
  result of the function should be a Haskell package.

## The plugin set

The `haskell.plugins` attribute is a set of plugins parameterised by a normal
Haskell package set. It is designed in this manner so the same plugin definitions
can be used with different compilers.

```nix
hp:
{
dump-core = { ... };
graphmod-plugin = { ... };
}
```

Each attribute is a different plugin which we might want to use with our program.


## A plugin

A plugin is a Haskell package which provides the plugin with four additional
attributes which describe how to run it. For example, here is the definition
for the `dump-core` plugin.

```nix
dump-core = { pluginPackage = hp.dump-core ;
              pluginName = "DumpCore";
              pluginOpts = (out-path: [out-path]);
              pluginDepends = [];
              finalPhase = _: ""; } ;
```

`pluginPackage`
: The Haskell package which provides the plugin.

`pluginName`
: The module name where the plugin is defined.

`pluginOpts`
: Additional options to pass to the plugin. The path where it places its output
is passed as an argument.

`pluginDepends`
: Any additional system dependencies the plugin needs for the finalPhase.

`finalPhase`
: An action to run in the `postBuild` phase, after the plugin has run. The output
directory is passed as an argument.

In most cases, `pluginDepends` and `finalPhase` can be omitted (they then take
these default values) but they are useful for when a plugin emits information
as it compiles each module which is then summarised at the end.

An example of this architecture is the [`graphmod-plugin`](https://github.com/mpickering/graphmod-plugin). As each module is
compiled, the import information is serialised. Then, at the end we read all
the serialised files and create a dot graph of the module import structure.
Here is how we specify the final phase of the plugin:

```nix
graphmod = { pluginPackage = hp.graphmod-plugin;
             pluginName = "GraphMod";
             pluginOpts = (out-path: ["${out-path}/output"]);
             pluginDepends = [ nixpkgs.graphviz ];
             finalPhase = out-path: ''
                graphmod-plugin --indir ${out-path}/output > ${out-path}/out.dot
                cat ${out-path}/out.dot | tred | dot -Tpdf > ${out-path}/modules.pdf
              ''; } ;
```

The first three fields are standard, however we now populate the final two
arguments as well. We firstly add a dependency on `graphviz` which we will
use to render the module graph and then specify the invocations needed
to firstly summarise and then render the information.

In this architecture, the plugin package provides a library interface which
exposes the plugin and an executable which is invoked to collect the information
output by the plugin. This is what the call to `graphmod-plugin` achieves.

## `withPlugin`

We also provide the `withPlugin` attribute which supplies both the
plugins and packages already applied to a specific package set. The reason
for this is that **a plugin and a package must be both compiled by the same
compiler**. Thus, unrestricted usage of `addPlugin` can lead to confusing errors
if the plugin and package are compiled with different compilers.
The `withPlugin` attribute ensures that the versions align
correctly.

```
core-either =
  haskellPackages.withPlugin
    (plugins: packages: addPlugin plugins.dump-core packages.either)
```

## How can I use it?

This infrastructure is provided as an overlay. Install the overlay as you would
normally, one suggested method can be see in the [`example.nix`](https://github.com/mpickering/haskell-nix-plugin/blob/master/example.nix) file.

```
let
  plugin-overlay-git = builtins.fetchGit
    { url = https://github.com/mpickering/haskell-nix-plugin.git;}  ;
  plugin-overlay = import "${plugin-overlay-git}/overlay.nix";
  nixpkgs = import <nixpkgs> { overlays = [plugin-overlay]; };
in ...
```

## Conclusion

So far, I have not package many plugins in this manner but as source plugins
are released in GHC 8.6 I expect to want to use plugins more and more regularly.


### Related Links











