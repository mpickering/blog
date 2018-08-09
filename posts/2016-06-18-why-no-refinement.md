---
title: Why do pattern synonyms not cause type refinement?
---

Pattern synonyms can't (safely) cause any additional type refinement than
their definition dictates.
This means that they can't be used to provide a GADT-like interface
when the underlying representation is not a GADT.
The purpose of this note is to explain this restriction.

<!--more-->

The defining feature of GADTs is that the constructors can bind
dictionaries for constraints.

```haskell
data CaptureShow a where
  CaptureShow :: Show a => CaptureShow a
```

When the constructor is matched upon, the constraints are *provided*
to the local environment.

When the constraints are equality constraints, this causes
type refinement. We learn more about the result type from
performing the match.

We can use pattern synonyms to abstract GADTs. The second set of
constraints is the set of provided constraints.

```haskell
pattern MyCaptureShow :: () => Show a => CaptureShow a
pattern MyCaptureShow = CaptureShow
```

But, the set of provided constraints must be exactly those constraints
which the underlying constructor provides. This is different to *required*
constraints which can be more specific than needed.

Why is this the case? One might expect that if additional constraints were
specified then the pattern synonym could bind the needed
dictionaries when building and release them when matching. However,
**not all values which can be destructed with a pattern synonym must be
constructed with a pattern synonym**.

For example, we would be able to specify an unsatisfiable constraint in the
provided context.

```haskell
pattern Unsat :: () => ('True ~ 'False) => Int
pattern Unsat = 0
```

If we did the same in a GADT it would be impossible to construct such
a value, similary here we can't use `Unsat` to construct an `Int` as
we will never be able to satisfy the equality constraint. However,
if it were possible to define such a pattern synonym we would be
able to use it to match on `0`. Doing so would provide
the bogus constraint.

There is a more immediate reason why this will never work.
For type class constraints, the dictionaries must
be bound when the constructor is used to construct values. If the pattern
synonym is not used to construct the value then we can't conjure it up
out of thin air when we need it.

This wasn't obvious to me, which is why it is now written down.
Pattern synonym signatures are surprisingly tricky.

# Addendum

David Feuer and Edward Yang conspired to show that using `unsafeCoerce`
it was possible to provide additional type equalities. The key to
the approach is to use a dummy GADT which is used to actually do the refining.
Our more efficient representation is upcasted to this GADT, then by matching on
the constructor, we cause refinement. Here is Edward's
code:

```haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GhostBuster where

import GHC.TypeLits
import Unsafe.Coerce

newtype Vec a (n :: Nat) = Vec { unVec :: [a] }

-- "Almost" Vec GADT, but the inside is a Vec
-- (so only the top-level is unfolded.)
data Vec' a (n :: Nat) where
    VNil'  :: Vec' a 0
    VCons' :: a -> Vec a n -> Vec' a (n + 1)

upVec :: Vec a n -> Vec' a n
upVec (Vec [])     = unsafeCoerce VNil'
upVec (Vec (x:xs)) = unsafeCoerce (VCons' x (Vec xs))

pattern VNil :: () => (n ~ 0) => Vec a n
pattern VNil <- (upVec -> VNil') where
    VNil = Vec []

pattern VCons :: () => ((n + 1) ~ n') => a -> Vec a n -> Vec a n'
pattern VCons x xs <- (upVec -> VCons' x xs) where
    VCons x (Vec xs) = Vec (x : xs)

headVec :: Vec a (n + 1) -> a
headVec (VCons x _) = x

mapVec :: (a -> b) -> Vec a n -> Vec b n
mapVec f VNil = VNil
mapVec f (VCons x xs) = VCons (f x) (mapVec f xs)
```

If we were to change the definition of the nil case of `mapVec` to use `VCons`
instead then it wouldn't type check.






