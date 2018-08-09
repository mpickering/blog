---
title: Inlining and Specialisation
---

The inliner and specialiser are the two parts of the optimiser which are crucial
to writing performant functional programs. They ensure that we can write programs
at a high-level of abstraction which are simplified when eventually used with concrete
arguments.

The inliner's job is to replace a function with its definition. This removes
one layer of indirection and most importantly allows other optimisations to
fire. The specialiser is important for optimising code which uses type classes.
Type classes are desugared into dictionary passing style but the specialiser
removes a layer of indirection by creating new functions with the relevant
dictionaries already supplied.

This document will explain the basics of these two parts of the
optimiser and some user-facing options which can be used to control them.


<!--more-->

# What does the `INLINABLE` pragma do?

Top-level definitions can be marked `INLINABLE`.

```haskell
myComplicatedFunction :: (Show a, Num a) => ...
myComplicatedFunction = ...

{-# INLINABLE myComplicatedFunction #-}
```


This causes exactly two things
to happens.

1. The function's (exact) definition is included in the interface file for the module.
2. The function will be specialised at use sites -- even across modules.

Note that GHC is no more keen to inline an `INLINABLE` function than any other.


# What does the `INLINE` pragma do?

The `INLINE` pragma can be applied to top-level definitions. It behaves like
the `INLINABLE` pragma but makes GHC very keen to inline the function.

```haskell
mySimpleFunction :: ...
mySimpleFunction = ...

{-# INLINE mySimpleFunction #-}
```

It is a sledgehammer and without care you can make the compiler take a long
time and produce a lot of code. Most "ticks exhausted" panics are due to library
authors misusing `INLINE` pragmas.

Liberally sprinkling all your definitions with `INLINE` is likely make the compiler
take a very long time to compile your program. It is not beneficial to inline every
function, inlining a function which is not optimised further only increases
overall code size without improving performance.

One situation where it is useful to use an `INLINE` pragma is when the definition
of the function contains functions which are mentioned in `RULES`. In this case,
it is essential that the optimiser is quite aggressive so that the `RULES`
can fire.


# Optimised vs unoptimised unfoldings

GHC will decide to include some small unfoldings in interface files. When it does
this, it first optimises the definitions so that they are not repeatedly optimised
at each use site after being inlined. Unfoldings included by `INLINE` or `INLINABLE`
are unoptimised so that they interact better with `RULES`.


# What is an interface file?

An interface file stores all information about a module which is needed by other
modules.

The key to cross-module inlining and specialisation is making sure that
we have the definitions of functions we want to inline at hand.
Information is only passed between modules by interface files, therefore we
must include the unfoldings of definitions in interface files if we want
to inline them across modules.

The extension for interface files is `.hi`, you can see what's in an
interface file by using the `--show-iface` flag.

# What is an unfolding?

The *unfolding* of a function `f` is what `f` is replaced by when it is inlined.
This is usually the definition of `f`.

# When are unfoldings included in interface files?

Not all definitions are included in interface files by default, doing so
might create quite large files.
There's no point including an unfolding of very large definitions which
we will never inline in other modules.

Unfoldings end up in interface files in three ways:

1. GHC decides to include unfoldings of small functions by default which it
		knows it will inline later.
2. Functions marked as `INLINE` or `INLINABLE` are included in interface files.
3. Compiler flags such as `-fexpose-all-unfoldings` include all unfoldings of
		all definitions in a module unless they are marked as `NOINLINE`.

# What is specialisation?

Specialisation is the process of removing typeclass dictionary arguments
by creating a new type-specialised definition for an overloaded function. Once
specialised, dictionary methods can be easily inlined which usually creates more
efficient code.

For example, if we define the overloaded function `foo`

```haskell
foo :: Show a => a -> a -> Bool
foo x y = show x == show y
```

the following core definition will be produced:


```haskell
foo = \ @a $dShow x y ->
    eqString (show $dShow x) (show $dShow y)
```

There are now 4 parameters to `foo`, the first argument is a type (denoted by `@`),
the second argument is the dictionary for `Show` (denoted by the `$d` prefix) and
the last two are the arguments `x` and `y` of the original definition.

The class constraint is translated into a dictionary. Each time a class
method is used, we must dynamically lookup which definition to use
in the supplied class dictionary.

If we know which type `a` is instantiated with, we can specialise the definition of `foo` and
produce much better code.

```haskell
qux :: Bool -> Bool -> Bool
qux = foo @Bool
```

Using `foo` at a specific type produces a new definition `foo_$sfoo` which is defined as:

```haskell
foo_$sfoo :: Bool -> Bool -> Bool
foo_$sfoo = foo @Bool $fShowBool
```

Further optimisations then inline `foo` and then the dictionary selector
`show` which produces the following more direct program.

```haskell
foo_$sfoo =
  \ x y ->
    case x of {
      False ->
        case y of {
          False -> foo4;
          True -> foo3
        };
      True ->
        case y of _ {
          False -> foo2;
          True -> foo1
        }
    }
```

## When does specialisation occur?

Specialisation occurs when an overloaded function is called at a specific type.
The specialised definition is placed in the module where the call happens but also exported
so that it can be reused if there is another upstream call-site where
specialisation would take place.

By default, functions are not specialised across modules.


There are two ways to make functions specialise across modules:

1. Marking a function as `INLINABLE` or `INLINE`.
2. Using the flag `-fspecialise-aggressively` when compiling the client module.
	 An unfolding must still be available to perform specialisation.

Further to this, observe that for specialisation to occur across modules, the unfolding must
be made available in interface files.

Notice this subtle point, the `INLINABLE` pragma guarantees the precise conditions
for a function to be specialised across modules.

# How do I use the `SPECIALISE` pragma?

The `SPECIALISE` pragma is used to create a specialised copy of an overloaded
function even if it is not used with that type in the module.

```haskell
module A where

class C ...

foo :: C a => a -> a

{-# SPECIALISE foo :: Text -> Text #-}
```

This example will create a new function, `foo_$sfoo :: Text -> Text` which will be used whenever
`foo` is applied to a `Text` value even in modules which import `A`.

This is useful to prevent GHC creating many copies of the same specialised function
if you have a very flat module structure.


# What is a loop-breaker?

In general, if we were to inline recursive definitions without care
we could easily cause the simplifier to diverge. However, we still
want to inline as many functions which appear in mutually recursive blocks
as possible.
GHC statically analyses each recursive groups of bindings and chooses
one of them as the *loop-breaker*. Any function which is marked as a loop-breaker
will never be inlined. Other functions in the recursive group are free to be
inlined as eventually a loop-breaker will be reached and the inliner will stop.

Note: Do not apply `INLINE` pragmas to loop-breakers, GHC will never inline a
loop breaker regardless of which pragma you attach. In fact, with a debugging
compiler, core lint will warn about using an `INLINE` pragma on a loopbreaker.

Loop-breakers are discussed in detail in section 4 of [Secrets of the Glasgow Haskell Compiler inliner](https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf).

# How does GHC choose a loop-breaker?

GHC uses a heuristic to decide which definitions it would be least beneficial
to inline and to choose those as loop breakers. For example,
it is very beneficial to inline simple expressions and dictionary selector
functions so they are given high scores. Discounts are also available if an
unfolding is available thus marking a definition as `INLINABLE` or `INLINE`
will usually cause GHC to not choose it.

# Which flags can I use to control the simplifier and inliner?

`-fspecialise-aggressively` removes the restrictions about which functions
are specialisable. Any overloaded function will be specialised with this flag.
This can potentially create lots of additional code.

`-fexpose-all-unfoldings` will include the (optimised) unfoldings of all functions
in interface files so that they can be inlined and specialised across modules.

Using these two flags in conjunction will have nearly the same effect as marking
every definition as `INLINABLE` apart from the fact that the unfoldings for `INLINABLE`
definitions are *not* optimised.

# Will GHC ever inline recursive definitions with static arguments?

Sometimes people ask if GHC is smart enough to unroll a recursive definition
when given a static argument. For example, if we could define `sum` using direct
recursion:

```haskell
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
```

A user might expect `sum [1,2,3]` to be optimised to 6. However,
GHC will not inline `sum` because it is a self-recursive definition and hence
a loop-breaker. The compiler is not smart enough to realise that repeatedly
inlining `sum` will terminate.

However, there is a trick that can be used in order to tell GHC that an argument
is truly static. We replace the value argument with a type argument.
Then by defining suitable type class instances, we can recurse on the
structure of the type as we would on a normal value. This time however,
GHC will happily inline each "recursive" call as each call to `sum` is at a
different type.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
module Sum where

import Prelude (Integer, (+))
import GHC.TypeLits

data Proxy x = Proxy

class Sum (xs :: [Nat]) where
  sum :: proxy xs -> Integer

instance Sum '[] where
  sum _ = 0

instance (KnownNat x, Sum xs) => Sum (x ': xs) where
  --sum :: Proxy (x ': xs) -> Int
  sum _ = natVal (Proxy @x) + sum (Proxy @xs)

main = sum (Proxy @'[1,2,3])
```

Inspecting the core we find that the definition of `main` is simplified
to the constant value `6`.


Note that this is slightly different to the [static argument transformation](http://blog.johantibell.com/2010/09/static-argument-transformation.html)  which
applies to a multi-parameter recursive functions where one of the arguments is the same
for each recursive call.
In this case, there are no arguments which remain constant across recursive calls.

This technique is due to Andres Löh.

# More Links

* [Secrets of the Glasgow Haskell Compiler inliner](https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf)
* [Optimizing Generics Is Easy!](https://dl.acm.org/authorize?297115)
* [GHC Users Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
* [Reddit Thread](https://www.reddit.com/r/haskell/comments/60h7a8/inlining_and_specialisation/?ref=share&ref_source=link)
* [GHC Wiki Inlining page](https://ghc.haskell.org/trac/ghc/wiki/Inlining)

# Acknowledgements

Thanks to Reid Barton, Ashok Menon and Csongor Kiss for useful comments on a draft.

