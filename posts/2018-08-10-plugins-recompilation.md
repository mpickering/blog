---
title: Specifying how a plugin affects recompilation
---

Plugins have existed for a long time in GHC. The first plugins were
implemented in [2008](http://blog.omega-prime.co.uk/2008/06/15/compiler-plugins-for-ghc-the-first-week/) by Max Bolingbroke. They enabled users to
modify the optimisation pipeline. They ran after desugaring and hence were
called ["core" plugins](http://mpickering.github.io/ghc-docs/build-html/users_guide/extending_ghc.html#core-plugins-in-more-detail). Later, Adam Gundry implemented what I shall
refer to as ["constraint solver" plugins](http://adam.gundry.co.uk/pub/typechecker-plugins/) which allow users to provide
custom solver logic to solve additional constraints. Recently, Boldizsár Németh
has extended the number of extension points again with a set of plugins which
can inspect and modify the syntax AST. Plugins can run after parsing, renaming
or type checking and hence are called ["source" plugins](http://mpickering.github.io/ghc-docs/build-html/users_guide/extending_ghc.html#source-plugins).

The idea behind plugins was great - a user can extend the compiler in their
own specific way without having to modify the source tree and rebuild the
compiler from scratch. It is far more convenient to write a plugin than to use
a custom compiler. However, if a user wants to use a plugin, they will find
that every module where the plugin is enabled is always recompiled, even if the
source code didn't change at all. Why is this? Well, a plugin can do anything,
it could read the value from a temperature sensor and insert the
room temperature into the program. Thus, we would always need to recompile
a module if the temperature reading changed as it would affect what our program did.

However, there are also "pure" plugins, whose output is only affected
by the program which is passed as in input. For these plugins, if the source
code doesn't change then we don't need to do any recompilation.

This post is about a new metadata field which I added to the `Plugin`
data type which specifies how a plugin should affect recompilation.
This feature will be present in GHC 8.6.

<!--more-->

# Controlling Recompilation

The `Plugin` data type is a record which contains a field for each
of the different types of plugin. There is now also a new field `pluginRecompile`
which specifies how the plugin should affect recompilation.

```haskell
data Plugin {
  installCoreToDos :: CorePlugin
  , tcPlugin :: TcPlugin
  , parsedResultAction
    :: [CommandLineOption] -> ModSummary -> HsParsedModule
                                         -> Hsc HsParsedModule

  ... omitted fields

  , pluginRecompile :: [CommandLineOpts] -> IO PluginRecompile
}
```

This function will be run during the recompilation
check which happens at the start of every module compilation.
It returns a value of the `PluginRecompile` data type.

```haskell
data PluginRecompile = PurePlugin
                     | ImpurePlugin
                     | MaybeRecompile Fingerprint
```

There are three different ways to specify how a plugin affects
recompilation.

1. `PurePlugin` which means that it doesn't contribute
anything to the recompilation check. We will only recompile a module if
we would normally recompile it.
2. `ImpurePlugin` which means that should always recompile a module.
  This is the default as it is backwards compatible.
3. `MaybeRecompile`, we compute a `Fingerprint` which we add to the
  recompilation check to decide whether we should recompile.

## Library Functions

The `Plugins` interface provides some library functions for common configurations.

We might want to use `impurePlugin` when our plugin injects some additional
impure information into the program such as the result of reading a webpage.

```haskell
impurePlugin :: [CommandLineOpts] -> IO PluginRecompile
impurePlugin _ = return ImpurePlugin
```

The `purePlugin` function is useful for static analysis tools which don't modify
the source program at all and just output information. Other plugins which
modify the source program in a predictable manner such as the `ghc-typelits-natnormalise`
plugin should also be marked as pure.


```haskell
purePlugin :: [CommandLineOpts] -> IO PluginRecompile
purePlugin _args = return NoForceRecompile
```

If you have some options which affect the output of the plugin then you might
want to use the `flagRecompile` option which causes recompilation if any of the
plugin flags change.

```haskell
flagRecompile :: [CommandLineOption] -> IO PluginRecompile
flagRecompile =
  return . MaybeRecompile . fingerprintFingerprints
    . map fingerprintString . sort
```

The nature of this interface is that it is sometimes necessary to be
overly conservative when specifying recompilation behaviour. For example,
you can't decide on a per-module basis whether to recompile or not. Perhaps
the interface could be extended with this information if user's found it
necessary.

# Conclusion

There is now a simple mechanism for controlling how plugins should affect
recompilation. This solves one of the major problems that large scale usage
of plugins has faced. Using a plugin on a 1000 module code base was impractical
but now shouldn't impose any additional inconvenience.

# Related Links

* [GHC User's Guide](http://mpickering.github.io/ghc-docs/build-html/users_guide/extending_ghc.html#controlling-recompilation)
* [Reddit comments](https://www.reddit.com/r/haskell/comments/967o6k/specifying_how_a_plugin_affects_recompilation/)





