---
title: Grokking the continuation monad
---

Out of all the standard monad instances, the continuation monad is the one which people find the most difficult to grasp. Being no different, this led to me reading what seemed to be every blog post and wikipedia entry on them in order to get a better understanding of the idiomatic usage of the powerful construct. This post brings together all this literature.

<!--more -->

# Hackage

https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Cont.html

Wiki

Papers

http://www.cs.indiana.edu/~sabry/papers/monadicDC.pdf

Blogs

http://www.haskellforall.com/2012/12/the-continuation-monad.html

http://www.haskellforall.com/2014/04/how-continuation-monad-works.html

Stack overflow

There are a plethora of questions on Stack Overflow asking about `Cont` and `ContT` but I will limit myself to commenting on a few of the more lucid examples.

http://stackoverflow.com/questions/25365900/trying-to-apply-cps-to-an-interpreter/25382352#25382352
