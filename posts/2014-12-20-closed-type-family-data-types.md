---
title: Data Types à la Carte with Closed Type Families
---

By hitting that critical sweet spot of solving an interesting problem and having a catchy name, most know, if not understand Wouter Swierstra's data types à la carte.

As the name suggests, the à la carte approach involves composing together data types to form bigger, *custom* types along with functions where we can precisely specify the necessary pieces. The approach relies heavily on a clever trick with type classes which is reviewed below. With more modern GHC extensions, the implementation can be made much more explicit, this is what this post will explore.

For a general overview of the approach there is no better introduction than the original [functional pearl](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.4131).

<!--more-->

To recap, in order to ease writing composed data types, we define a type class `a :<: b` to express that `a` is a subtype of `b`. To do this we require three instances.

```haskell
-- Coproduct
data (f :+: g) e = Inl (f e) | Inr (g e)

class f :<: g where
  inj :: f a -> g a

instance f :<: f where
  inj = id

instance f :<: (f :+: g) where
  inj = Inl

instance (f :<: h) =>  f :<: (g :+: h) where
  inj = inj . Inr
```

Together these three instances model a linear search through our nested coproduct whilst picking out the correct injection.

Closed type families as introduced by Richard Eisenberg [-@Eisenberg2013] are on a simple level are restricted functions which act on types.
With this observation, it seems that they can be used to reimplement most type class tricks in a more explicit manner and indeed this is the case. Patrick Bahr [-@Bahr2014] was the first to explore this idea in connection with compositional data types, this post closely follows his exposition.

Patrick incrementally builds a solution to provide a more robust subtyping constraint. He proceeds in three stages.

1. A (near) faithful recreation of the semantics presented by Swierstra.
2. Strengthening to prevent ambiguous injections.
3. Strengthening to support complex injections.

Today I will talk about the first two stages, leaving the slightly more intricate third for another time.

# Recreation

Implementing the subtyping relation using type families brings out quite a subtle point which is implicit in the original pearl. The type class `:<:` has two purposes, the first is to check whether we are able to construct an injection from `f` to `g` by computing at the type level. The second is to work out the correction injection from `a` to `b` at the term level. Type families make this dependency explicit.

Thus, our first step will be to check whether such an injection exists from `f` to `g`.

As type class resolution operators without backtracking - we can't express any kind of branching computation in type class instances. This led to the convention where we were forced to make sure that `:+:` associated to the right. ^[For instance `a :<: (a :+: b) :+: c`  would not typecheck.] Hence, it was easy to think about our composed data types as lists of types (with `:+:` being a type level cons and `:<:` searching through the list). As type families allow us to backtrack, this restriction is needless. Instead it is much easier to think about our constructed data types as trees.

Our goal becomes to find a type family which searches these trees. To do this, we consider how we would do so at the term level.

```haskell
data Tree a = Tip a | Branch (Tree a) (Tree a

elem :: Eq v => a -> Tree a -> Bool
elem v (Tip x) = if v == x then True else False
elem v (Branch l r) = (elem v l) || (elem v r)
```

We can define a very similar type family which searches a composite coproduct at the type level. Note that we will make extensive implicit use of the `DataKinds` extension. For example in the following example, `True` and `False` are the promoted constructors.^[In fact, it is possible to promote simple functions using the [singletons](http://hackage.haskell.org/package/singletons) package by Richard Eisenberg.].

```haskell
type family Elem e f :: Bool where
  Elem e e = True
  Elem e (l :+: r) = Or (Elem e l) (Elem e r)
  Elem e f = False

type family Or a b :: Bool
  Or False False = False
  Or a b         = True
```

This is no use to us as we must also calculate the injection, to do this, we need to know the path we took to find the value in the tree. By introducing a new data type `Res` and `Crumbs` we can construct the trail necessary. The modified definition of `Elem` and `Or` follow naturally.

```haskell
data Crumbs = Here | L Crumbs | R Crumbs

data Res = Found Crumbs | NotFound

type family Elem e f :: Res where
  Elem e e         = Found Here
  Elem e (l :+: r) = Choose (Elem e l) (Elem e r)
  Elem e f         = NotFound

type family Choose e f :: Res where
  Choose (Found a) b = Found (L a)
  Choose a (Found b) = Found (R b)
  Choose a b         = NotFound
```

Again, this is very similar to the term-level definition but is more verbose thanks to the lack of type-level type classes.

Now we have the path to the type, we must also construct the injection. In fact, another type class is what we need but this time thanks to our additional work we don't need to rely on convention nor `OverlappingInstances`.

```haskell
class MakeInj (res :: Res) f g where
  mkInj :: Proxy res -> f a -> g a
```

Notice one difference here is that as we are going to explicitly use the computed type level path, we need someway to make this information accessible to our type class. One way to do this is to use a [proxy variable](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Proxy.html). At first glace it may seem useless to define a data type in this fashion but it allows us to pass around type information in exactly the way we want to here.


```haskell
instance MakeInj (Found Here) f f where
  mkInj _ = id

instance MakeInj (MakeInj (Found p) f l) => MakeInj (Found (L p)) f (l :+: r) where
  mkInj _ = Inl . mkInj (Proxy :: Proxy (Found p))

instance MakeInj (MakeInj (Found p) f r) => MakeInj (Found (R p)) f (l :+: r) where
  mkInj _ = Inr . mkInj (Proxy :: Proxy (Found p))
```

Notice how the proxy is used to direct the construction.

Finally we can define `:<:`.

```haskell
type f :<: g = MakeInj (Elem f g) f g
```

That is to say, `f` is a subtype of `g` if there is an injection from `f` to `g`. Notice that there is an injection if `Elem f g` is not `NotFound`.

Using closed type families might seem like more work but the approach is inherently more powerful. There is also something to be said about making our search strategy explicit. Without knowledge of the type class resolution mechanism it can be confusing why a certain subtyping would fail. With this implementation, even if unfamiliar with the nuances of type families, it is much clearer.

# Ambiguity

Another wart was the possibility of ambiguous injections if the same constructor was specified twice in a type signature. One example where it would be impossible to guarantee a consistent injection would be `Lit :+: Lit`. We want to disallow such signatures at compile time as they are always a programming error.

To do this we can extend the `Res` data type to include a new constructor `Ambiguous` and altering the definition of `Choose`.

```haskell
data Res = Found Crumbs | NotFound | Ambiguous

type family Choose e f :: Res where
    Choose (Found x) (Found y) = Ambiguous
    Choose Ambiguous x = Ambiguous
    Choose x Ambiguous = Ambiguous
    Choose (Found a) b = Found (L a)
    Choose a (Found b) = Found (R b)
    Choose a b = NotFound
```

The choice here to use a new constructor `Ambiguous` is stylistic, we could have easily reused the `NotFound` constructor. The reason for doing so is that the error messages produced by a fine grained result type are better. For example now attempting to compile the following expression results in an error which specifically mentions ambiguity.

```haskell
lit2 :: Expr (Lit :+: Lit)
lit2 = lit 2

No instance for (MakeInj 'Ambiguous Lit (Lit :+: Lit))
   arising from a use of ‘lit’
   In the expression: lit 2
   In an equation for ‘lit2’: lit2 = lit 2
Failed, modules loaded: none.
```

# Further Work

We'll wrap things up there for today but Patrick goes on to explore the possibility of further extending the subtyping relation to deal with more complicated injections. Even with the machinery he developed, intuitively correct subtyping relationships (such as  `Add :+: Mult :<: Add :+: Mult :+: Lit`) fail to hold.  To avoid any suspense, he shows how to further extend `:<:` to allow such relationships.

These ideas and more are implemented in the [compdata](https://hackage.haskell.org/package/compdata) package.

# References

* [Q&A with Richard Eisenberg](http://ezyang.tumblr.com/post/74438034022/richard-a-eisenberg-closed-type-families-with)






