---
title: A guide to GHC's parser entry points
---

GHC 7.10 exposes several new entry points to the parser. Unfortunately, the
documentation about what each of these do and how to use them is lacking. THere
is a small code snippet at the top of the
[Parser](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc/Parser.html)
module which is the start of an explanation but there are still quite a few
missing pieces before you can use them to their full power. In this post I will
demonstrate a minimum example to get you going.

= Entry Points =

In total, there are now 11 entry points which each parses a `String` into a GHC
AST element. For the purposes of this post we will concentrate on `parseModule`
which parses an entire Haskell source file to a [`Located (HsModule
RdrName)`haskell](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc/HsSyn.html#t:HsModule).

= Imports =

> -- GHC Imports
> import qualified Parser as GHC
> import qualified Lexer as GHC
> import qualified StringBuffer as GHC
> import qualified DynFlags as GHC
> import qualified SrcLoc as GHC
> import qualified FastString as GHC
> import qualified HeaderInfo as GHC
> import qualified RdrName as GHC
> import qualified HsSyn as GHC
> import qualified GHC as GHC hiding (parseModule)
> import qualified Outputable as GHC
>
> -- External
> import GHC.Paths
> import System.IO.Unsafe

A large number of imports are needed to get us off the ground.

> runParser :: GHC.P a -> GHC.DynFlags -> String -> GHC.ParseResult a
> runParser parser flags str = GHC.unP parser parseState
>     where
>       location = GHC.mkRealSrcLoc (GHC.mkFastString "<interactive>") 1 1
>       buffer = GHC.stringToStringBuffer str
>       parseState = GHC.mkPState flags buffer location


>
> parseFile :: GHC.DynFlags -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.RdrName))
> parseFile = runParser GHC.parseModule

We start with much the same function present in the documentation. The type
signature leaves one pressing question, what are `DynFlags` and how can I get
some?

The `DynFlags` keeps track of all options which are set during the compilation
pipeline. As a result, it's rather unweildy to deal with when we are only
concerned with a very small fraction of the options present in it.

>
> getDynFlags :: IO GHC.DynFlags
> getDynFlags =
>   GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
>     GHC.runGhc (Just libdir) GHC.getSessionDynFlags
>

Once we've worked out how to get a copy of the `DynFlags`, we need to set them
to the correct options. Confusingly, the way to do this is to use
`getOptions` from `HeaderInfo` to parse the top of the file before passing

> getExtensions :: String -> GHC.DynFlags
> getExtensions str =
>   let dflags0  = unsafePerformIO getDynFlags
>       src_opts = GHC.getOptions dflags0 (GHC.stringToStringBuffer str) "<interactive"
>       (dflags1, _, _) = unsafePerformIO $ GHC.parseDynamicFilePragma dflags0 src_opts
>   in dflags1

>
> runParseModule :: String -> Either String (GHC.Located (GHC.HsModule GHC.RdrName))
> runParseModule contents =
>   case parseFile finalDFlags contents of
>     GHC.PFailed ss m ->
>       Left $ "Parse failure at: " ++  ghcShow (GHC.ppr ss) ++ "\n"
>               ++ ghcShow m
>     GHC.POk _ parseResult  ->
>       Right parseResult
>   where
>     finalDFlags :: GHC.DynFlags
>     finalDFlags = getExtensions contents
>     ghcShow :: GHC.SDoc -> String
>     ghcShow = GHC.showSDoc finalDFlags


