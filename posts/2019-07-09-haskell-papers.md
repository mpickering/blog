---
title: Two new Haskell Symposium papers
---

This year I was lucky to have both my papers accepted for the Haskell Symposium.
The first one is about the problematic interaction of Typed Template Haskell
and implicit arguments and the second, a guide to writing source plugins.
Read on the abstracts and download links.

<!--more-->

### Multi Stage Programming in Context
Matthew Pickering, Nicolas Wu, Csongor Kiss ([PDF](../papers/multi-stage-programs-in-context.pdf))

<div class="blockquote">

> Cross-stage persistence is an essential aspect of multi-stage programming that allows a value defined in one stage to be available in another. However, difficulty arises when implicit information held in types, type classes and implicit parameters needs to be persisted. Without a careful treatment of such implicit information—which are pervasive in Haskell—subtle yet avoidable bugs lurk beneath the surface.

> This paper demonstrates that in multi-stage programming care must be taken when representing quoted terms so that important implicit information is not discarded. The approach is formalised with a type-system, and an implementation in GHC is presented that fixes problems of the previous incarnation.

</div>

### Working with Source Plugins
Matthew Pickering, Nicolas Wu, Boldizsár Németh ([PDF](../papers/working-with-source-plugins.pdf))

<div class="blockquote">

> A modern compiler calculates and constructs a large amount of information about the programs it compiles. Tooling authors want to take advantage of this information in order to extend the compiler in interesting ways. Source plugins are a mechanism implemented in the Glasgow Haskell Compiler (GHC) which allow inspection and modification of programs as they pass through the compilation pipeline.

> This paper is about how to write source plugins. Due to their nature–they are ways to extend the compiler–at least basic knowledge about how the compiler works is critical to designing and implementing a robust and therefore successful plugin. The goal of the paper is to equip would-be plugin authors with inspiration about what kinds of plugins they should write and most importantly with the basic techniques which should be used in order to write them.

</div>
