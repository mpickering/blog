---
title: Replacing type classes with records affects optimisation
---

It is somewhat common to suggest using records instead of type classes for
particular domains for which type classes are not deemed idiomatic. However,
this suggestion should be taken with caution as the change will have consequences
on how your program is optimised. Using records instead of type classes can
lead to much slower programs.

<!--more-->

In order to provide an interface for propositional logic, we might provide
a type class which allows the constructors to be overloaded. We can
then provide a direct interpretation of `Prop` which evaluates an expression
to a truth value.

```haskell
class Prop r where
  or :: r -> r -> r
  and :: r -> r -> r
  true :: r
  false :: r


instance Prop Bool where
  or = (||)
  and = (&&)
  true = True
  false = False
```

However, one might be tempted to avoid using a type class and instead perform
the manual type class desugaring in order to be able to more easily modify
and extend an interpretation.

```haskell
data PropDict r = PropDict {
  dor :: r -> r -> r
  , dand :: r -> r -> r
  , dtrue :: r
  , dfalse :: r
  }

boolDict = PropDict {
  dor = (||)
  , dand = (&&)
  , dtrue = True
  , dfalse = False }
```

We can then use both versions in order to implement a helper function which
turns a list into a chain of disjunctions.

```haskell
ors :: Prop r => [r] -> r
ors [o] = o
ors (o:os) = o `or` ors os

dors :: PropDict r -> [r] -> r
dors _ [o] = o
dors pd (o:os) = dor pd o (dors pd os)
```

We can then instantiate each function by either supplying a type argument
or the dictionary directly in the latter case.

```haskell
test1 :: [Bool] -> Bool
test1 = ors @Bool

test2 :: [Bool] -> Bool
test2 = dors boolDict
```

What's the difference between these two versions? The process of optimisation is
different. In the first case, the overloading of `ors` will be eliminated by
**specialisation**. In the later case, the static argument will be eliminated
by **SpecConstr**. When we define these definitions in the same module as our
earlier definitions of `ors` and `dors` then
both definitions result in essentially the same code as each other. The
overhead is eliminated.

However, when we use `ors` or `dors` in another module, they behave quite differently.
If we mark `ors` as `INLINABLE` then it will also be specialised in other modules.
However, **SpecConstr does not work across module boundaries**. As a result,
`dors` will not be specialised on the static argument and the dictionary overhead
will remain.

## Links

* [Benchmarks](https://github.com/mpickering/rtcwrao-benchmarks)
* [Reddit Discussion](https://www.reddit.com/r/haskell/comments/85svhm/replacing_type_classes_with_records_affects/)

















