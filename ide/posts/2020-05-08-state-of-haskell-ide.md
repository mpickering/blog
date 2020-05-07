---
title: The State of Haskell IDE
author: Luke Lau
date: 2020-05-08
github: bubba
---

It is an exciting time for Haskell tooling. As many might be aware, the past
year has seen a number of significant changes to the ecosystem, with one of the
most noticeable ones being [the marriage of ghcide and haskell-ide-engine](https://neilmitchell.blogspot.com/2020/01/one-haskell-ide-to-rule-them-all.html). We now
have contributors from both projects working towards a unified vision of a language
server, the *haskell-language-server*, which aims to be the de-facto IDE for
veteran and newcomer Haskellers alike:
A full-fledged LSP server that provides diagnostics, code completion,
navigation and more, works with both your Cabal and Stack projects, and scales
from tiny scripts all the way up to huge codebases like GHC.

*haskell-language-server* does not do this on its own however. It builds upon a
whole ecosystem of tools to drive all of the underlying IDE features. How they
all interconnect can be a bit overwhelming at first, so this post will break it
down and take a look at them individually.


<p align="center">
![An overview of the current ecosystem](/ide/images/tooling-graph.png){ width=400px }
</p>

<!--more-->

### ghcide
Powering the language server under the hood is
[ghcide](https://github.com/digital-asset/ghcide).
Based on tooling built for DAML, it is the core piece of infrastructure that talks to GHC to parse and typecheck your code.
One of its many clever innovations is the use of the Shake build system to keep an in-memory cache of modules, file contents and other computations (e.g. code completions). Tooling built on top of it can then query it to get responsive, up-to-date information about the code it is working with.
ghcide is also a language server itself, so if you need a lightweight IDE without any fancy trimmings then it will slot right in with any LSP client.

### hie-bios
In order for ghcide to set up the GHC session, it needs to know what set of flags to pass to it.
Unfortunately, this is not just as simple as `-Wall -O1` --- if you ever run `cabal build -v` you will find that there are loads of flags passed to GHC, a lot of them related to whatever packages your project depends on.
[hie-bios](https://github.com/mpickering/hie-bios) takes care of this by querying the underlying build tool, such as Stack, Cabal or Hadrian, delegating it the work of figuring out the flags and building any package dependencies that might be needed.
By specifying a `hie.yaml` file in your project's root directory, you can specify one or more *cradles*, where each cradle represents some component to build with a specific build tool.

### cabal-helper
You can leave out the `hie.yaml` file, and hie-bios will try its best to infer which components to build with which tools.
However this can get fairly complicated and hairy quickly.
haskell-ide-engine and haskell-language-server use [cabal-helper](https://github.com/DanielG/cabal-helper.git) to query more information about your project to help with this.

### haskell-lsp
haskell-lsp provides the transport between the client
and the server using the Language Server Protocol. It keeps track of a lot of
bookkeeping within the protocol, like request IDs and client/server
capabilities, and also provides a virtual file system that mirrors edits coming
in from the text editor, before they have saved the actually document. Having
this mirror is pretty useful for external tools that need to be run on physical
files rather than in-memory text buffers. Its sister library,
haskell-lsp-types, provides type definitions for the actual specification, so if
you want to do your own thing you don't need to rewrite the data types and
parsing all over again.

### lsp-test
[lsp-test](https://github.com/bubba/lsp-test) is a testing framework for LSP servers, used by ghcide, HIE and haskell-language-server. It acts as a language client
that can be programmed to send messages to servers, and assert that the right
messages are received back. It can also be used to recreate certain
scenarios: [useful for hunting down memory
leaks](https://lukelau.me/haskell/posts/leak/).

### haskell-ide-engine
The [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine) is a
language server that faithfully served as a focal point for a whole suite of
tools. It's been a relatively long-running project: It predates the existence of
LSP! It originally used ghc-mod as its backend before switching to
hie-bios, and it provides a bunch of logic for extracting code completion and
symbols etc. out of the GHC API.  It also has a plugin system which allows external
tools to easily interface with LSP. So built into HIE, Floskell, Ormolu and
Brittany can provide formatting whilst GHC, Liquid Haskell and HLint provide
diagnostics, all through the same interface.

### GHC `.hie` files
A lot of the work on ghcide and haskell-ide-engine has driven new features and functionality upstream into GHC.
One such example is [`.hie` files](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html). These are generated with the `-fwrite-ide-flag` during  compilation, and like a `.hi` file they contain additional information about some compiled module. However `.hie` files contain lots of information specifically useful for tooling, such as the type of expressions or where things are defined, hence the acronym for *haskell information extended* --- not to be
confused with haskell-ide-engine!
Work is underway to use these files in ghcide to provide much more accurate code
navigation, completion and type definitions.

### haskell-language-server
[haskell-language-server](https://github.com/haskell/haskell-language-server) now aims to concentrate the efforts behind ghcide and
haskell-ide-engine. It uses the powerful core of ghcide and HIE's approach of
plugins to integrate an ecosystem worth of tools. Eventually there will be
enough tools integrated that users will be able to configure what tools they
want to use for each job. Floskell for formatting, or Ormolu?
And more importantly, because it builds upon so many components,
haskell-language-server will receive any improvements made downstream. This
division of labour allows contributors to focus on the individual problems that need tackled, whilst benefiting the whole ecosystem.

### Looking forward
We are now entering the *renaissance* of Haskell tooling. This summer
there are 3 Google Summer of Code projects and a Tweag open source fellowship
all working on tooling, as well as the continued efforts of many contributors.

[Fendor](https://github.com/fendor) is working on supporting compilation of multiple *home-packages* at
once within GHC. In short this will allow tooling to work with multiple
components inside a package simultaneously. [Luke Lau](http://github.com/bubba) (that's me) is improving the
implicit discovery of cradles in hie-bios by connecting together the [Cabal
`show-build-info` command](), which will give a much more reliable Cabal setup
in the absence of `hie.yaml` files. [Zubin Duggal](http://github.com/wz1000) will be focusing on the haskell-language-server, fleshing out its features and taking advantage of GHC's `.hie` files.
And [Michalis Pardalos](https://github.com/mpardalos) is integrating
[OpenTelemetry](https://opentelemetry.io) with the language servers, so that we
can
instrument and profile how they perform on the vast heterogeneous array of LSP clients that they
may be used with.

By the end of the summer we will have built a robust language server which works
on any Haskell project of any size and built with any build system.

There are a lot of exciting projects in the pipeline, and we will
be posting about them here every Friday. In the meantime, come chat with us over at
[\#haskell-ide-engine](irc://irc.freenode.net/haskell-ide-engine) on Freenode, clone some projects and help us build the
ultimate Haskell IDE.

