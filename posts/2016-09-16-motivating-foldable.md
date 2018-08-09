---
title: Motivating the Foldable Type Class
---


Something I have never seen articulated is why the `Foldable`
type class exists. It is lawless apart from the free theorems which
leads to ad-hoc definitions of its methods. What use is the abstraction if not
to enable us to reason more easily about our programs?
This post aims to articulate some justification stemming
from the universality of folds.


In brief, here is the argument.

1. For inductive data types, the fold is unique defined as a consequence of
   initiality.
2. The `Foldable` type class is a way to exploit this universality without
   having to define all of our data types as the fixed points of base functors.
3. The uneasiness comes from this impedence mismatch between points 1 and 2.

<!--more-->

To recall, the type class is meant to capture the intuitive
notion of a fold. Folds are a way of consuming a data type
by summarising values in a uniform manner before combining them together.

We now recall the basics of initial algebra semantics for polynomial data types.^[From now on when we say data type, we mean polynomial data type.]
We can represent all data types
as the fixed point of a polynomial functor.
In Haskell we can represent the fixed point by
the data type `Mu`.

```haskell
data Mu f = Mu { out :: (f (Mu f)) }

-- Inital algebra
in :: f (Mu f) -> Mu f
in = Mu
```

We then specify data types by first defining a control functor $F$ and then
considering the initial $F$-algebra for the functor. The initial $F$-algebra is
given by $(\mu F, in)$. The injection function $in$ wraps up one level of the recursion.
The projection function `out` strips off one layer of the recursion.^[Lambek's lemma
tells us that the two functions are each other's inverse.]

We can define the type of lists
by first specifying a control functor $ListF = 1 + (A \times \_)$ and
then defining lists in terms of this base functor and `Mu`.

```haskell
data ListF a r = Nil | Cons a r

type ListM a = Mu (ListF a)
```

We call types which are definable in this manner *inductive* types.

We do not usually define data types in this style as
programming directly with them is quite cumbersome as one must wrap
and unwrap to access the recursive structure.

However, defining data in this manner has some useful properties. The one that
we care about is that it is possible to define a generic fold operator for
inductive types.

```haskell
cata :: (f a -> a) -> Mu f -> a
cata f = f . fmap (cata f) . out
```

What's more, due to initiality, `cata f` is the unique function of this type.
We have no choice about how we define a fold operator after we have described how
to interpret the control functor.

Fleshed out in some more detail,
Given a functor $F$,
for any other algebra $(B, g : F B \to B)$ there exists a unique map $h$ to this algebra from $(\mu F, in)$.
Our definition of `cata` is precisely the way to construct this unique map.

# Real-World Haskell

Languages such as Haskell allow users to define data types in a more ad-hoc fashion by specifying the recursive
structure themselves rather than in terms of a base functor.

```haskell
data List a = Nil | Cons a (List a)
```

It can be easily seen that `Mu (ListF a) ~= List a` and thus we can exploit the
the uniqueness of the fold function and define a canonical fold function specialised to our newly
defined data type.

```haskell
foldr :: (a -> b -> b) -> b -> List a -> b
```

`foldr` is a specialisation of `cata` to lists. It is perhaps clearer to see the correspondence
if we rewrite the function to explicitly take a list algebra as it's first argument.

```haskell
data ListAlg a b = ListAlg { z :: () -> b , cons :: (a, b) -> b }

foldr :: ListAlg a b -> List a -> b
```

Specifying a function `ListF a b -> b` is precisely the same as specifying
`ListAlg a b` as it amounts to specifying functions $1 \to b$ and $a \times b \to b$.

So, for each data type we define we can specialise the `cata` operator in
order to define a canonical fold operator. However, the issue now is that
each one of our fold operators has a different type. It would be useful to still
be able to provide a consistent interface so that we can still fold any inductive
type. The answer to this problem is `Foldable`.

This highlights the essential tension with the `Foldable` type class. It exists
in order to be able to continue to define a generic fold operation but without the
cost of defining our data types in terms of fixed points and base functors.

# `Foldable`

The method `foldr` is the only method needed to define an instance of `Foldable`.^[
Notice that we are only able to fold type constructors of kind `* -> *`, this is an
arbritary choice, motivated by the fact that most containers which we wish to fold are
polymorphic in one way. (For example, lists and trees).]

```haskell
class Foldable f where
  foldr :: (a -> b -> b) -> b -> t a -> b
```

It turns out that `(a -> b -> b)` and a single constant `b` are sufficient for
specifying algebras for inductive types. Inductive types are built from polynomial
base functors so we can describe an algebra by first matching on the summand
and then iteratively applying the combining function to combine each recursive position.
If there are no recursive positions, we instead use the zero value `z`.

Defining the instance for lists is straightforward:

```haskell
instance Foldable [] where
  foldr _ z [] = z
  foldr f _ (x:xs) = f x (foldr f xs)
```

As another example, we consider writing an instance for binary trees which
only contain values in the leaves. It is less obvious then how to implement
`foldr` as the usual fold (`foldTree` below) has a different type signature.

```haskell
data Tree a = Branch (Leaf a) (Leaf a) | Leaf a

foldTree :: (b -> b -> b) -> (a -> b) -> Tree a -> b
```

We can do so by cleverly instantiating the result type `b` when performing the
recursive calls.

```haskell
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z t =
    case t of
      Leaf a -> f a z
      Branch l r -> foldr (\a c -> f a . c) id l
                   		(foldr f z r)

```

The first recursive call of `foldr` returns a function of type `b -> b` which tells
us how to combine the next recursive occurence. In this case there are only
two recursive positions but this process can be iterated to combine all the
recursive holes.^[Note that I didn't think of this definiton myself but arrived
at it purely calculationally from the definition of `foldMap` and `foldr`.]

The definition for `foldMap`
usually provides a more intuitive interface for defining instances but it is
harder to motivate which is why we prefer `foldr`.

```haskell
foldMapTree :: Monoid m => (a -> m) -> Tree a -> m
foldMapTree f (Leaf a) = f a
foldMapTree f (Branch l r) = foldMapTree f l <> foldMapTree f r
```

However, instances for inductive types defined in this uniform manner are
less powerful than folds induced by an $F$-algebra. The problem comes from
the fact that all the recursive holes much be combined in a uniform fashion.

The question I have, is it possible to define `middle` using the `Foldable`
interface?

```
data Tree3F a r = Leaf a | Branch r r r

type Tree3 a = Mu (Tree3F a)

middleFold :: Tree3F a a -> a
middleFold (Leaf a) = a
middleFold (Branch _ m _) = m

middle :: Tree3 a -> a
middle = cata middleFold
```


# Conclusion

The definition of `Foldable` is motivated by the well-understood theory of
inductive data types. The pragmatics of Haskell lead us to the seemingly quite
ad-hoc class definition which has generated much discussion in recent years.
The goal of this post was to argue that the class is better founded than
people think and to explain some of the reasons that it leads to some uncomfort.

My argument is not about deciding whether an ad-hoc definition
is lawful, it is explaining the motivation for the class in a way which also
explains the lawlessness. The class definition is a compromise because of the practicalities
of Haskell. The only way in which we can know a definition is sensible or not
is by inspecting whether the ad-hoc definition agrees with the canonical
definition given by `cata`.

# Appendix: Free Theorems of `foldr`

There are some free theorems for `foldr` which is natural in `a` and `b`.

```haskell
foldr :: (a -> b -> b) -> b -> t a -> b
```


Naturality in `b` amounts to, for all functions `g : b -> c`.

```haskell
g (foldr f z t) = foldr (\x y -> f x (g y)) (g z)
```

and naturality in `a` amounts to, for all functions `f : c -> a`.

```haskell
foldr f z . fmap h = foldr (\x y -> f (h x) y) z
```

These are included for completeness.


