---
title: Plugins
---

This page provides an index of different resources about GHC plugins.

## Blog posts

* [phadej - 2018-09-13 - Regular expressions of types](http://oleg.fi/gists/posts/2018-09-13-regular-expressions-of-types.html)
* [mpickering - 2018-08-10 - Specifying how a plugin affects recompilation](http://mpickering.github.io/posts/2018-08-10-plugins-recompilation.html)
* [mpickering - 2018-08-09 - Reimplementing graphmod as a source plugin](http://mpickering.github.io/posts/2018-08-09-source-plugin-graphmod.html)
* [phadej - 2018-07-06 - Idiom brackets via source plugin](http://oleg.fi/gists/posts/2018-07-06-idiom-brackets-via-source-pluging.html)
* [mpickering - 2018-06-24 - Nix scaffolding for running Haskell plugins](http://mpickering.github.io/posts/2018-06-24-haskell-nix-plugins.html)
* [mpickering - 2018-06-11 - Source Plugins: Four ways to build a typechecked Haskell expression](http://mpickering.github.io/posts/2018-06-11-source-plugins.html)
* [nboldi - 2018-04-14 - Proposal 0017: Source Plugins](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0017-source-plugins.rst)
* [nomeata - 2018-02-02 - The magic "Just do it" type class](https://www.joachim-breitner.de/blog/735-The_magic_%E2%80%9CJust_do_it%E2%80%9D_type_class)
* [Tweag I/O - 2017-09-22 - GHC compiler plugins in the wild: typing Java](https://www.tweag.io/posts/2017-09-22-inline-java-ghc-plugin.html)
* [christiaanb - 2016-08-17 - Solving custom operations in KnownNat constraints](https://qbaylogic.com/blog/2016/08/17/solving-knownnat-custom-operations.html)
* [christiaanb - 2016-08-10 - Solving GHC's KnownNat constraints](https://qbaylogic.com/blog/2016/08/10/solving-knownnat-constraints-plugin.html)
* [Adam Gundry - 2015-07-17 - A Typechecker Plugin for Units of Measure](http://adam.gundry.co.uk/pub/typechecker-plugins/)
* [Eric Seidel - 2014-12-04 - (Ab)using Compiler Plugins to Improve Embedded DSLs](https://galois.com/blog/2014/12/abusing-compiler-plugins-improve-embedded-dsls/)
* Max Bolingbroke - Compiler Plugins For GHC [1](http://blog.omega-prime.co.uk/2008/06/15/compiler-plugins-for-ghc-the-first-week/)[2](http://blog.omega-prime.co.uk/2008/06/23/compiler-plugins-for-ghc-week-two/)[34](http://blog.omega-prime.co.uk/2008/07/05/compiler-plugins-for-ghc-weeks-three-and-four/)[5](http://blog.omega-prime.co.uk/2008/07/14/compiler-plugins-for-ghc-week-five/)[6](http://blog.omega-prime.co.uk/2008/07/22/compiler-plugins-for-ghc-week-six/)
* [Max Bolingbroke - 2008-04-24 - The Summer Of Code, or Compiler Development for the Masses](http://blog.omega-prime.co.uk/2008/04/24/the-summer-of-code-or-compiler-development-for-the-masses/)


## Tutorial plugins

[`plugin-constraint`- mpickering](https://github.com/mpickering/plugin-constraint)

: How to interact with the contraint solve and generate `HsExpr GhcTc`.

[`hashtag-coerce` - mpickering](https://github.com/mpickering/hashtag-coerce)

: How to perform a simple static analysis using `syb` to traverse the syntax tree.

[`ghc-typelits-gcd` - christiaanb](https://github.com/christiaanb/ghc-typelits-gcd)

: An example of implementing a constraint solver plugin

## Source plugins

[`hlint-source-plugin`- ocharles](https://github.com/ocharles/hlint-source-plugin)

: Runs `hlint` during compilation and reports errors are though
they are GHC errors.

[`assert-explainer` - ocharles](https://github.com/ocharles/assert-explainer)

: Instruments an assertion to print the value of its failed parts when it fails.

[`what-it-do` - ocharles](https://github.com/ocharles/what-it-do)

: Rewrites `do` expressions to trace all binds.

[`smuggler` - kowainik](https://github.com/kowainik/smuggler)

: Plugin which removes unused imports automatically.

[`lift-plugin` - mpickering](https://github.com/mpickering/lift-plugin)

: An experimental plugin which introduces an operator which acts like an overloadable
bracket.

[`graphmod-plugin` - mpickering](https://github.com/mpickering/graphmod-plugin)

: A reimplementation of [`graphmod`](https://github.com/yav/graphmod) as a source plugin.

[`idioms-plugins` - phadej](https://github.com/phadej/idioms-plugins)

: Implementation of idiom brackets using a parser plugin.

[`kleene-type` - phadej](https://github.com/phadej/kleene-type)

: Implementation of a syntax for HLists and solver for a custom `MatchI` constraint
which implements a custom type family for regular expressions.

[`record-dot-preprocessor` - ndmitchell](https://github.com/ndmitchell/record-dot-preprocessor)

: Record dot syntax: Access record fields with `record.field`.


## Core plugins

[`dump-core` - yav](https://github.com/yav/dump-core)

: A pretty printer which renders HTML for GHC's internal representation

[`HerbiePlugin` - mikeizbicki](https://github.com/mikeizbicki/HerbiePlugin)

: GHC plugin that improves Haskell code's numerical stability

[`sbvPlugin` - LeventErkok](http://hackage.haskell.org/package/sbvPlugin)

: Prove properties about Haskell programs using SBV/SMT.

[`ghc-justdoit` - nomeata](https://github.com/nomeata/ghc-justdoit)

: Plugin which synthesises a definition from a type, like `djinn`.

[`inspection-testing` - nomeata](https://github.com/nomeata/inspection-testing)

: Embed assertions about the intermediate code and have them checked by GHC.

[`ghc-srcspan-plugin` - gridaphobe](https://github.com/gridaphobe/ghc-srcspan-plugin)

: Generic GHC Plugin for annotating Haskell code with source location data.

[`concat` - conal](https://github.com/conal/concat)

: A plugin which overloads Haskell programs so they can be interpreted in
different ways.

[`unroll-plugin` - Max Bolingbroke](https://github.com/batterseapower/unroll-plugin)

: A plugin which unrolls loops based on user annotations.


## Constraint solver plugins

[`uom-plugin` - adamgundry](https://github.com/adamgundry/uom-plugin/)

: Support for solving constraints mentioning units of measure.

[`ghc-typelits-extra` - christiaanb](https://github.com/clash-lang/ghc-typelits-extra)

: Extra type-level operations on `GHC.TypeLits.Nat`, solved using a plugin.

[`type-nat-solver` - yav](https://github.com/yav/type-nat-solver)

: Solve numerical constants using an SMT solver

[`coxswain` - nfrisby](https://github.com/nfrisby/coxswain)

: A GHC type checker plugin for row types

[`thoralf` - Divesh-Otwani](https://github.com/Divesh-Otwani/the-thoralf-plugin)

: A plugin that solves constraints using Z3.



