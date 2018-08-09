---
title: Indexed Optics
---

What is an indexed optic? It is an optic which gives you access to an index
whilst performing updates.

It is a simple clear generalisation of a lens but
the implementation looks quite complicated. This is due to the
desire to reuse the same combinators for both non-indexed and indexed variants.
We we will start by explaining a simplified implementation of indexed optics
before the technique used in order to reuse the same combinators
as ordinary optics.

<!--more-->

# Simple Implementation

As a first approximation, we will augment the updating function with an
additional index which we will then subsequently refine.

```haskell
type PrimitiveIndexedTraversal i s t a b
  = forall f . Applicative f => (i -> a -> f b) -> (s -> f t)
```

Implementing optics by hand gives a good intuition for the types involved.

```haskell
pair :: PrimitiveIndexedTraversal Int (a, a) (b, b) a b
pair iafb (a0, a1) = (,) <$> iafb 0 a0 <*> iafb 1 a1
```

The implementation
is exactly the same as a normal traversals apart from we also pass an index
to each call of the worker function.
Note that we have a lot of choice about which indices we choose. We could have
indexed each field with a boolean or in the opposite order.
For lists, we need to use a helper
function which passes an index to each recursive call.

```haskell
list :: PrimitiveIndexedTraversal Int [a] [b] a b
list iafb xs = go xs 0
  where
    go [] _ = pure []
    go (x:xs) n = (:) <$> iafb n x <*> go xs (n+1)
```

There are all the usual combinators to work with indexed traversals as normal
traversals but one of the most useful ones to see what is going on is `itoListOf`
which converts an indexed traversal into a list of index-value pairs.

```haskell
itoListOf :: ((i -> a -> Const [(i, a)] b) -> s -> (Const [(i, a)] t))
          -> s -> [(i, a)]
itoListOf t s = getConst $ t (\i a -> Const [(i, a)]) s

> itoListOf pair (True, False)
[(0, True), (1, False)]
```

We monomorphise the argument so that we we don't have to use a variant
of `cloneTraversal` in order to work around impredicative types.

We can also turn an ordinary traversal into an indexed traversal by labelling
each element with the order in which we traverse it. In order to do so
we need to define an applicative functor which when traversed with will
perform the labelling ultimately returning an indexed traversal.

```haskell
newtype Indexing f s = Indexing { runIndexing :: (Int -> (Int, f s))}

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing fn) = Indexing (over (_2 . mapped) f . fn)

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing (\i -> (i, pure x))
  (Indexing fab) <*> (Indexing fa)
    = Indexing (\i ->
        let (i', ab) = fab i
            (i'', a) = fa i'
        in (i'', ab <*> a))
```

Then traversing with this applicative functor supplies the index to each
function call which we can pass to our indexed updating function.

```haskell
indexing :: Traversal s t a b -> PrimitiveIndexedTraversal Int s t a b
indexing t p s = snd $ runIndexing
  (t (\a -> Indexing (\i -> (i + 1, p i a) )) s) 0
```

A common pattern is to use `indexing` and `traverse` together to create indexed
traversals for `Traversable` functors. It is so common that it is given a special
name `traversed`.

```haskell
traversed :: Traversable t => PrimitiveIndexedTraversal (t a) (t b) a b
traversed = indexing traverse

> itoListOf traversed (Just 5)
[(0, 5)]
```

However, there are two problems with this representation.

1. Indexed optics do not compose with ordinary optics.
2. We need a different composition operator other than (`.`) in order to compose indexed optics together.

# Composing indexed and ordinary optics

Considering the first problem, in order to compose an indexed optic with an ordinary
optic using function composition we would need to be able to unify `i -> a -> b` with `s -> t`.

Given an ordinary optic `op` and an indexed optic `iop` with the following types:

```haskell
op : (a -> f b) -> (s -> f t)
iop : (i -> c -> f d) -> (u -> f v)
```

`op . iop` is the only composition which type checks. It yields an indexed
traversal which keeps track of the index of the inner component.


```haskell
> itoListOf (traverse . list) (True, [1,2,3])
[(0, 1), (1, 2), (2, 3)]
```

However, composition the other way around doesn't work and further with this
representation indexed optics do not compose together with `.`. In order
to compose indexed optics together with `.` we need to be able to unify the argument
and result type of the lens together. In order to do this, we abstract away from the the indexed
argument of the updating function for any `Indexable` profunctor.

```haskell
class Indexable i p where
  index :: p a b -> i -> a -> b
```

Using this class, the type for indexed traversals becomes:

```haskell
type IndexedTraversal i s t a b =
  forall f p . (Applicative f, Indexable i p) => p a (f b) -> s -> f t
```

There are instances for the newtype wrapped indexed functions which we were
using before

```haskell
newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance (Indexable i (Indexed i)) where
  index (Indexed p) = p
```

but also for `(->)` which ignores the index. This means that we can seamlessly use
optics with or without the index by instantiating `p` with either `Indexed` or `->`.

```haskell
instance (Indexable i (->)) where
  index f _ = f
```

Now `op . iop` yields an indexed traversal, `iop . op` forces us to instantiate
`p = (->)` and so yields a traversal which has forgotten the index.
Perhaps most surprisingly, the composition `iop . iop` type-checks as well, but
again we loose information as we are forced to instantiate `p = (->)` and thus forget about the
indexing of the outer traversal.

This is a double edged sword. Composing using `.` leads to more
code reuse as the same combinators can be used for both indexed and non-indexed
optics. On the other hand, composing indexed optics using `.` is nearly always the
wrong thing if you care about the indices.

# A different composition operator

Composing together indexed optics with the normal lens composition operator
`.` leads to unexpected results as the indices are not combined appropriately.
The index of the inner-most optic of the composition is preserved whilst the
outer indexing is thrown away. It would be more desirable to combine the indices
together in order to retain as much information as possible.

To that end we define `<.>` which can compose indexed optics together whilst
suitably combining their indices.

```haskell
(<.>) :: Indexable (i, j) p
      => (Indexed i s t -> r)
      -> (Indexed j a b -> s -> t)
      -> p a b -> r
(istr <.> jabst) p
  = istr (Indexed (\i s ->
      jabst (Indexed (\j a ->
        indexed p (i, j) a)) s))
```

The definition monomorphises the argument again in order to avoid inpredicativity
problems.

```haskell
> itoListOf (list <.> pair) [(1,2), (2, 3)]
[((0,0),1),((0,1),2),((1,0),2),((1,1),3)]
```

Generalisations are possible which combine indices in other ways but this
simple combination function highlights the essence of the approach.


# What are indexed optics useful for?

Now that is a question which I will have to defer to reddit comments. I couldn't
find many libraries which were using the indexing in interesting ways.


