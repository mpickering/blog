---
title: "Announcing ghc-exactprint: A New Foundation For Refactoring Tools"
---

Over the last few months Alan Zimmerman and myself have been working on what
will hopefully become a new foundation for refactoring Haskell programs. We
have called this foundation `ghc-exactprint` and it is now available on
[hackage](https://hackage.haskell.org/package/ghc-exactprint) to coincide with the release of GHC 7.10.2.

Thompson and Reinke originally described 22 refactorings in their
[catalogue](http://www.cs.kent.ac.uk/projects/refactor-fp/publications/refactoring-draft-catalogue.pdf)
of functional refactorings which motivated the development of HaRe. Missing
from their list is the identity transformation, in the context of refactoring
this is perhaps the most important. When applying any kind of change to small
segment of your project it is undesirable for the tooling to modify another
part of your file unexpectedly. The identity refactoring seems to be the
easiest of all but it is important that any refactoring tool behaves in this
way.

The biggest challenge of the last few months was being able to perform the
identity refactoring whilst retaining a suitable representation to be able to
easily transform programs. Now that we believe that `ghc-exactprint` is robust,
the challenge is to build the promised refactoring tools upon it.

<!--more-->

# Representation

If your only concern is to perform the identity transformation then perhaps the
representation you choose is not very important. One can easily choose to
operate directly on the source files for instance. The representation becomes
much more important when considering other refactoring operations.

When deciding on a representation, you have to take into account more
complicated transformations such as renamings, insertions and deletions which
are made much easier by manipulating an AST. Most current source manipulations
are based on `haskell-src-exts` which provides a separate parser  and as a
result lacks support for many modern language extensions. We instead chose to
base `ghc-exactprint` directly on the GHC parser so that we don't have to worry
(much) about adding support for future language extensions and secondly,
working with the GHC AST directly is essential if you want to perform *type-aware*
refactorings.

We work in two stages.

After receiving the output of the GHC parser, the first stage converts all the
absolute source positions into relative positions.

```haskell
not True = False
```

For example, in the preceding declaration, using absolute source positions we
might describe the the declaration starts at `(0,0)`, the first argument starts
at `(0,4)` and the RHS of the definition starts at `(0,12)`. Using relative
positioning like in the library, we instead describe the position of each
element relative to the position of the previous element. For example, `True`
is 0 lines and 1 column further than `not` and `False` is 0 lines and 1 column
further than `=`.

To keep track of this information we introduce a separate data structure which
we call *annotations*. This data structure keeps track of the relative
positions of all AST elements and keywords as well as any necessary information which
is not included in the AST in order to exactly reproduce the original source file.

We hope this approach will make it easy to perform transformations without
worrying too much about where exactly everything will end up. With this style,
replacing expressions is easy as we don't have to worry about updating anything
at all, just by replacing the expression in the AST produces the correct
output.

Secondly, we perform the reverse of this transformation and produce a source file given an AST and the previously worked out relative positions. Thus a refactoring becomes two things:

1. A direct modification of the AST.
2. A modification of the associated annotations.

# Results

Much of the last 10 months was spent modifying GHC and `ghc-exactprint` in
order to be able roundtrip any source file successfully. The library has
been through many iterations with varying levels of success but finally has
reached an understandable and usable core.  After testing the program on around
50,000 source files from Hackage, we can say with confidence that at least the
foundations are in place for further tooling to be built.

A glance at the [test
suite](https://github.com/alanz/ghc-exactprint/tree/master/tests/examples) will
show some particularly tricky examples. The IOHCC also provided a good source
of test material..

```haskell
import Data.Char
e=181021504832735228091659724090293195791121747536890433

u(f,m)x=i(m(x),       [],let(a,b)=f(x)       in(a:u(f,m)b))
(v,h)=(foldr(\x(y    )->00+128*y+x)0,u(     sp(25),((==)"")))
p::(Integer,Integer )->Integer      ->     Integer    --NotInt
p(n,m)x     =i(n==0 ,1,i(z n             ,q(n,m)x,    r(n,m)x))
i(n,e,d     )=if(n) then(e)              else  (d)    --23+3d4f
(g,main     ,s,un)= (\x->x,             y(j),\x->x*x,unlines)--)
j(o)=i(take(2)o==   "e=","e="++t        (drop(4-2)o),i(d>e,k,l)o)
l=un.map (show.p      (e,n).v.map(      fromIntegral{-g-}.ord)).h
k=co.map(map(chr       .fromIntegral    ).w.p(d,n).   read).lines
(t,y)=(\ (o:q)->              i(o=='-'  ,'1','-' ):   q,interact)
q(n,m)x=   mod(s(    p(        div(n)2, m{-jl-})x)    )m--hd&&gdb
(r,z,co)    =(\(n,   m)x->mod(x*p(n-1,  m)x)m,even    ,concat)--6
(w,sp)=(    u(\x->(   mod(x)128,div(x   )128),(==0    )),splitAt)

d=563347325936+1197371806136556985877790097-563347325936
n=351189532146914946493104395525009571831256157560461451
```


Replacements, deletions and rearrangements are very easy using this framework. Most of the time they can be specified by generic traversals (with any generics library) The last piece of the puzzle is to make it just as easy to add new items into the AST. A challenge that I and Alan Zimmerman will now begin to work on.


## Future Work

* I have used this tooling to write a [tool](https://github.com/mpickering/apply-refact) which automatically applies hlint suggestions.
* HaRe is currently being rewritten to use this new machinery.
* We are looking at using pattern synonyms in order to smooth over the rough corners of the GHC API.

If you are interested in using the tooling then we are both active in `#haskell-refactorer`.

## Caveats

There are a few things which remain difficult to process.

* CPP - I think that every Haskell tool has difficultly dealing with CPP and we are no exception.
* Minimal Pragmas - The GHC AST performs normalisation on complicated Minimal Pragmas which means that some positional information is lost. This will be fixed for GHC 7.12.
* Unicode Syntax  - The lexer doesn't differentiate between unicode characters and their ascii equivalents. For the most part we can work around this by comparing the length of symbols. The exception is ★, which has the same length as it's ascii equivalent * which means that ★'s are unfortunately lost on conversion.

## A simple insertion

This simple example inserts a type signature making sure to move comments to the right place. We hope to build a higher level interface on top of this low level manipulation.

Along with the changes that Alan made to GHC to keep track of all the locations
of keywords during parsing there have been two other improvements to enable this work.

1. Andrew Gibiansky added some more parser entry points which have proved invaluable.
2. All the datatypes in the GHC AST now derive Data and Typeable instances which enables generic traversals.

The following example takes advantage of the former of these two additions.

### Input

```haskell
-- test.hs
module Foo where

baz "one" = 1
baz "two" = 2
```

### Program
```haskell
{-# LANGUAGE NamedFieldPuns #-}
module InsertSignature where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import qualified Data.Map as Map

import qualified HsSyn as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC

type Module = GHC.Located (GHC.HsModule GHC.RdrName)

main :: IO ()
main = do
  Right (as, m) <- parseModule "test.hs"
  (finalAs, finalM) <- addSignature "baz" "baz :: String -> Int" as m
  putStrLn $ exactPrintWithAnns finalM finalAs


addSignature :: String -- ^ Function to add a signature for
             -> String -- ^ Type signature
             -> Anns
             -> Module
             -> IO (Anns, Module)
addSignature funid tsig as (GHC.L l m) = do
  -- Parse new AST element
  Right (sigAnns, sig) <- withDynFlags (\d -> parseDecl d "template" tsig)
  let (before, (bind: after)) = break findFunBind (GHC.hsmodDecls m)
      -- Add new annotations to the map
      newAs = Map.union as sigAnns
      -- Modify the annotations to
      -- 1. Retain the original spacing
      -- 2. Make sure that comments are placed correctly.
      Just Ann{annEntryDelta, annPriorComments} = Map.lookup (mkAnnKey bind) newAs
      finalAnns = Map.adjust (\sigAnn -> sigAnn { annEntryDelta = annEntryDelta
                                                , annPriorComments = annPriorComments })
                             (mkAnnKey sig)
                  . Map.adjust (\bindAnn -> bindAnn { annEntryDelta = DP (1, 0)
                                                    , annPriorComments = [] })
                               (mkAnnKey bind) $ newAs

      finalMod = m { GHC.hsmodDecls = before ++ [sig, bind] ++ after }

  return (finalAnns, GHC.L l finalMod)


  where
    findFunBind :: GHC.LHsDecl GHC.RdrName -> Bool
    findFunBind (GHC.L _ (GHC.ValD b@(GHC.FunBind {})))
      | showGhc (GHC.unLoc (GHC.fun_id  b)) == funid = True
    findFunBind _ = False
```

### Result

```haskell
module Foo where

baz :: String -> Int
baz "one" = 1
baz "two" = 2
```

