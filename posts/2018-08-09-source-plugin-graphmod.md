---
title: "Reimplementing graphmod as a source plugin: graphmod-plugin"
---

You may have heard about [source plugins](http://mpickering.github.io/ghc-docs/build-html/users_guide/extending_ghc.html#source-plugins) by now. They allow you to modify
and inspect the compiler's intermediate representation. This is useful for
extending GHC and performing static analysis of Haskell programs.

In order to test them out, I reimplemented the [`graphmod`](https://github.com/yav/graphmod) tool as a source
plugin. `graphmod` generates a graph of the module structure of your package.
Reimplementing it as a source plugin makes the implementation more robust.
I implemented it as as a type checker plugin which runs after type checking has finished. The result: [`graphmod-plugin`](https://github.com/mpickering/graphmod-plugin)

![An example of the structure of the `aeson` package](https://raw.githubusercontent.com/yav/graphmod/master/screenshots/aeson1.dot.png)


<!--more-->

# Architecture

The plugin runs once at the end of type checking for each module.
Therefore, if we want to collate information about multiple modules, we must
first serialise the information we want and then once all the modules have
finished compiling collect all serialised files and process the information.

We will therefore first define a plugin which extracts all the import information
from one module before defining the suitable executable which collects all the
import information and produces the final output graph.

`graphmod-plugin` consists of a library which exports the plugin and
an executable which is then invoked to render the information.
Here is how to directly use the two in tandem:

```bash
# Run the plugin on the source file
ghc -fplugin=GraphMod -fplugin-opt:GraphMod:/output/dir

# Collect the information which was produced
graphmod-plugin --indir /output/dir > modules.dot
```

Once the dot file has been generated, you can use the normal graphviz utilities
to render the file.

```bash
cat modules.dot | tred | dot -Tpdf > modules.pdf
```

`tred` removes transitive edges from the graph before we render the graph as a pdf.

# The plugin

A type checker plugin is a function of the following type:

```haskell
sourcePlugin :: [CommandLineOpts] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
```

The `TcGblEnv` is the output of the type checker, it contains all the type checked
bindings in addition to lots of other useful information.
We are interested in just the imports, these
are located in the `tcg_rn_imports` field.

```haskell
tcg_rn_imports :: [LImportDecl GhcRn]
```

An `LImportDecl GhcRn` is a data type which contains information about
each import.

```haskell
-- GHC data types
type LImportDecl pass = Located (ImportDecl pass)

data ImportDecl pass
  = ImportDecl {
      ideclExt       :: XCImportDecl pass,
      ideclSourceSrc :: SourceText,
      ideclName      :: Located ModuleName, -- ^ Module name.
      ideclPkgQual   :: Maybe StringLiteral,  -- ^ Package qualifier.
      ideclSource    :: Bool,          -- ^ True <=> {-\# SOURCE \#-} import
      ideclSafe      :: Bool,          -- ^ True => safe import
      ideclQualified :: Bool,          -- ^ True => qualified
      ideclImplicit  :: Bool,          -- ^ True => implicit import (of Prel
ude)
      ideclAs        :: Maybe (Located ModuleName),  -- ^ as Module
      ideclHiding    :: Maybe (Bool, Located [LIE pass])
                                       -- ^ (True => hiding, names)
    }
```

Along with the module name, there is lots
of meta information about other aspects of the import such as whether it was
qualified and so on. Our plugin will take this information and convert
it into the format expected by the existing `graphmod` library.

```haskell
# Graphmod data types
data Import = Import { impMod :: ModName, impType :: ImpType }

data ImpType = NormalImp | SourceImp
```

The `graphmod` `Import` data type is a simplified version of `ImportDecl`.
It's straightforward to extract the information we need. Notice how much
simpler this approach is than the approach taken in the original library
which uses a lexer to try to identify textually the position of the imports.

```haskell
convertImport :: ImportDecl GhcRn -> GraphMod.Import
convertImport (ImportDecl{..}) =
  GraphMod.Import { impMod = convertModName (ideclName)
                  , impType = if ideclSource
                                then GraphMod.SourceImp
                                else GraphMod.NormalImp
                  }

convertModName :: Located ModuleName -> GraphMod.ModName
convertModName (L _ mn) = GraphMod.splitModName (moduleNameString mn)
```

Notice that it is also possible to extend the `GraphMod.Import` data type
to contain new information easily. In the previous implementation this would be
much more effort as the lexing approach is fragile.

# Serialisation

Once we have gathered this information we need to serialise it and write
it to disk so that once we have compiled all the modules we can deserialise it
and render the final graph.

As we are using GHC, we can use the same serialisation machinery as GHC
uses to write interface files. Of course, you are free to use whatever
serialisation library you like but there are already instances defined for
GHC specific types. We won't need any of them in this example but they
can be useful. The `writeBinary` function takes a value serialisable
by the `GHC.Binary` class and writes it to the file.


```haskell
import Binary

initBinMemSize :: Int
initBinMemSize = 1024 * 1024

writeBinary :: Binary a => FilePath -> a -> IO ()
writeBinary path payload = do
  bh <- openBinMem initBinMemSize
  put_ bh payload
  writeBinMem bh path
```

We also needed to write some simple `Binary` instances by hand in order
to do the serialisation.

```haskell
instance Binary GraphMod.Import where
  put_ bh (GraphMod.Import mn ip) = put_ bh mn >> put_ bh ip
  get bh = GraphMod.Import <$> get bh <*> get bh
instance Binary GraphMod.ImpType where
  put_ bh c =
    case c of
      GraphMod.NormalImp -> putByte bh 0
      GraphMod.SourceImp -> putByte bh 1
  get bh = getByte bh  >>= return . \case
                      0 -> GraphMod.NormalImp
                      1 -> GraphMod.SourceImp
                      _ -> error "Binary:GraphMod"
```

# Plugin Description

Once we have these parts, we can assemble them into the final plugin.
We first get the imports out of `tcg_rn_imports` and then convert them
using `convertImport`. We then write this information to a uniquely named
file in the output directory which
is passed as an argument to the plugin.


```haskell
-- The main plugin function, it collects and serialises the import
-- information for a module.
install :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
install opts ms tc_gbl = do
    let imps = tcg_rn_imports tc_gbl
        gm_imps = map (convertImport . unLoc) imps
        outdir = mkOutdir opts
        path = mkPath outdir (ms_mod ms)
        gm_modname = getModName ms
    liftIO $ do
      createDirectoryIfMissing False outdir
      writeBinary path (gm_modname, gm_imps)
    return tc_gbl
```

`mkPath` tries to come up with a unique name for a module by using the `moduleUnitId`.
The file name doesn't matter particularly as long as it's unique. We could
instead write this information to a database or to a file handle. Writing to
disk is just a convenient method of serialisation.

```haskell
mkPath :: FilePath -> Module -> FilePath
mkPath fp m
  = fp </> (moduleNameString (moduleName m) ++ (show (moduleUnitId m)))
```

Then, we define the plugin by making a definition called `plugin` and
overriding the `typeCheckResultAction` field and the `pluginRecompile` field.
`purePlugin` means that the result of our plugin
only depends on the contents
of the source file rather than any external information. This means that we
don't need to recompile the module every time just because we are using a plugin.

```haskell
-- Installing the plugin
plugin :: Plugin
plugin = defaultPlugin  {
  typeCheckResultAction = install
  , pluginRecompile = purePlugin
}
```

Now that our module exports an identifier of type `Plugin` called `plugin` we
are finished defining the plugin part of the project.

# The finaliser

Once all the modules have finished compiling. They will have written
their information to a file in a certain directory that we can now
inspect to create the dot graph.

We define an executable to do this. The executable takes the directory
of the files as an argument, reads all the files and then processes them
to produce the graph.

In the `collectImports` function, we first read the directory from
a command line argument. Then we find all the files in this directory and
read their contents into memory. We use the helper function `readImports`
which uses functions from the `Binary` module to read the serialised files.
Finally, we build the graph using all the import information and
then pass the graph we have built to the existing `graphmod` backend.

```haskell
collectImports :: IO ()
collectImports = do
  raw_opts <- getArgs
  let (fs, _ms, _errs) = getOpt Permute options raw_opts
      opts = foldr ($) default_opts fs

      outdir = inputDir opts
  files <- listDirectory outdir

  usages <- mapM (readImports outdir) files

  let graph = buildGraph opts usages
  putStr (GraphMod.make_dot opts graph)


readImports :: FilePath -> FilePath -> IO Payload
readImports outdir fp = do
  readBinMem (outdir </> fp) >>= get
```

The `buildGraph` function builds an in memory representation of the module
graph. There is a node for each module and an edge between modules if one
imports the other. We finally mimic the original `graphmod` tool and output the
representation of the graph on `stdout`. This can then be piped to `dot`
in order to render the graph.

# Running the plugin with nix

By far the most convenient way to run the plugin is with nix. This
gets around the problem of having to run the finaliser after compiling
the plugin. We use the [`haskell-nix-plugin`](http://mpickering.github.io/posts/2018-06-24-haskell-nix-plugins.html) infrastructure in order to do this.

The information required to run the plugin consists of information about
the plugin package but also an additional, optional, final phase which runs
after the module has finished compiling.

```nix
graphmod =
  { pluginPackage = hp.graphmod-plugin;
    pluginName = "GraphMod";
    pluginOpts = (out-path: ["${out-path}/output"]);
    pluginDepends = [ nixpkgs.graphviz ];
    finalPhase = out-path: ''
      graphmod-plugin --indir ${out-path}/output > ${out-path}/out.dot
      cat ${out-path}/out.dot | tred | dot -Gdpi=600 -Tpng > ${out-path}/modules.png
      ''; } ;
```

I will add this definition to the `plugins.nix` file in `haskell-nix-plugin`
once ghc-8.6.1 is released.

We then would use the `addPlugin` function in order to run the plugin on a
package. In order to get the module graph we inspect the `GraphMod` output.

```nix
(addPlugin graphmod hp.aeson).GraphMod
```

Running this script on `aeson` produces [this](https://i.imgur.com/baiyyuc.jpg)
quite large image which shows the whole module graph.

A complete example `default.nix` can be found in the [repo](https://github.com/mpickering/graphmod-plugin/blob/master/default.nix).

# Conclusion

We have described one way in which one can structure a plugin. There are
probably other ways but this seems ergnomic and convenient. Hopefully others
will find this quite detailed summary and reference code useful to build upon.

Writing plugins is quite similar to modifying GHC itself so if you need help,
the best place to ask is either on the [`ghc-devs` mailing list](https://mail.haskell.org/mailman/listinfo/ghc-devs) or on #ghc on freenode.

# Related Links

* [`graphmod-plugin`](https://github.com/mpickering/graphmod-plugin)
* [Reddit comments](https://www.reddit.com/r/haskell/comments/95r8uj/reimplementing_graphmod_as_a_source_plugin/)






