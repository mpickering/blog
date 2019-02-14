---
title: Implementing Nested Quotations
---

Quotation is one of the key elements of metaprogramming. Quoting an expression
`e` gives us a representation of `e`.

```
[| e |] :: Repr
```

What this representation is depends on the metaprogramming framework and what
we can do with the representation depends on the representation. The most common
choice is to dissallow any inspection of the representation type relying on the
other primative operation, the splice, in order to insert quoted values into
larger programs.

The purpose of this post is to explain how to implemented nested quotations.
From our previous example, quoting a term `e`, gives us a term which represents
`e`. It follows that we should be allowed to nest quotations so that quoting
a quotation gives us a representation of that quotation.

```
[| [| 4 + 5 |] |]
```

However, nesting brackets in this manner has been disallowed in Template Haskell
for a number of years despite nested splices being permitted. I wondered why
this restriction was in place and it seemed that [no one knew the answer](https://mail.haskell.org/pipermail/ghc-devs/2019-January/016939.html). It turns out, there was no technical reason and implementing
nested brackets is straightforward once you think about it correctly.

<!--more-->

## Template Haskell

We will now be concrete and talk about how these mechanisms are implemented
in Template Haskell.

In Template Haskell the representation type of expressions is called `Exp`.
It is a [simple ADT](http://hackage.haskell.org/package/template-haskell-2.14.0.0/docs/Language-Haskell-TH-Syntax.html#t:Exp) which mirrors source Haskell programs very closely.
For example quoting `2 + 3` might be represented by:

```
[| 2 + 3 |] :: Exp
= InfixE (Just (LitE 5)) (VarE +) (Just (LitE 5))
```

Because `Exp` is a normal data type we can define its representation in the same
manner as any user defined data type. This is the purpose of the `Lift` type
class which defines how to turn a value into its representation.

```
class Lift t where
  lift :: t -> Q Exp
```

So we just need to implement `instance Lift (Q Exp)` and we're done. To do that
we implement a general instance for `Lift (Q a)` and then also an instance for
`Exp`.

```
instance Lift a => Lift (Q a) where
  lift qe = qe >>= \b' -> lift b' >>= \b'' -> return ((VarE 'return) `AppE` b'')
```

This instance collapses effects from building the inner code value into a single
outer layer. In order to make the types line up correctly, we have to insert a call
to `return` to the result of lifting the inner expression.

Instances for `Exp` and all its connected types are straightforward to define
and thankfully we can use the `DeriveLift` extension in order to derive them.

```
deriving instance Lift Exp
... 40 more instances
deriving instance Lift PatSynDir
```

It's now possible to write a useless program which lifts a boolean value twice
before splicing it twice to get back the original program.

```
-- foo = True
foo :: Bool
foo = $($(lift (lift True)))
```

Running this program with `-ddump-splices` would show us that when the first splice
is run, the code that is insert is the representation of `True`. After the
second splice is run, this representation is turned back into `True`.

## Cross Stage Persistance

If you use variables in a bracket the compiler has to persist their value from one
stage to another so that they remain bound and bound to the correct value
when we splice in the quote.

For example, quoting `x`, we need to remember that the `x` refers to the `x`
bound to the top-level which is equal to `5`.

```
x = 5

foo = [| x |]
```

If we didn't when splicing in `foo`, in another module, we would use whatever
`x` was in scope or end up with an unbound reference to `x`. No good at all.

For a locally bound variable, we can't already precisely know the value of
the variable. We will only know it later at runtime when the function is applied.

```
foo x = [| x |]
```

Thus, we must know for any value that `x` can take, how we construct its
representation. If we remember, that's precisely what the `Lift` class is for.
So, to correct this cross-stage reference, we replace the variable `x` with
a splice (which lowers the level by one) and a call to `lift`.

```
foo x = [| $(lift x) |]
```

### Nesting Brackets

The logic for persisting variables has to be extended to work with nested brackets.

```
foo3 :: Lift a => a -> Q Exp
foo3 x = [| [| x |] |]
```

In `foo3`, `x` is used at level 2 but defined at level 0, hence we must
insert two levels of splices and two levels of lifting to rectify the stages.

```
foo3 :: Lift a => a -> Q Exp
foo3 x = [| [| $($(lift(lift x))) |] |]
```

Now with nested brackets, you can also lift variables defined in future stages.

```
foo4 :: Q Exp
foo4 = [| \x -> [| x |] |]
```

Now `x` is defined at stage 1 and used in stage 2. So, like normal, we need to
insert a lift and splice in order to realign the stages. This time, just one splice
as we just need to lift it one level.

```
foo4 :: Q Exp
foo4 = [| \x -> [| $(lift x) |] |]
```

# Implementing Nested Brackets


## Implementing Splices

After renaming a bracket, all the splices inside the bracket are moved into
an associated environment.

```
foo = [| $(e) |]
=> [| x |]_{ x = e }
```

When renaming the RHS of `foo`, we replace the splice of `e` with a new variable
`x`, this is termed the "splice point" for the expression `e`. Then, a new binding
is added to the environment for the bracket which says that any reference to `x`
inside the bracket refers to `e`. That means when we make the representation
of the code inside the bracket, occurences of `x` are replaced with `e` directly
(rather than a representation of `x`) in the program.

The same mechanism is used for the implicit splices we create by instances of
cross-stage persistence.

```
qux x = [| x |]
        => [| $(lift x) |]
        => [| x' |]_{ x' = lift x }
```

The environment is special in the sense that it connects a stage 1 variable with
an expression at stage 0.

How is this implemented? When we see a splice we rename it and the write it
to a state variable whose scope is delimited by the bracket. Once the contents
of the bracket is finished being renamed we read the contents and use that as the
environment.

## Generalisation to n-levels

Nested splices work immediately with nested brackets. When there is a nested
bracket, the expression on the inside is first floated outwards into the
inner brackets environment.

```
foo n = [| [| $($(n)) |] |]
      => [| [| x |]_{x=$(n)} |]
      => [| [| x |]_{x = y} |]_{y = n}
```

Then it is floated again to the top-level leaving a behind a trail of bindings.

## Representing Quotes

Template Haskell represents renamed terms so that references remain constent
after splicing. As such, our representation of a quotation in the TH AST
should reflect the renamed form of brackets which includes the environment.

```
data Exp = ... | BrackE [(Var, Exp)] Exp | ...
```

The constructor therefore takes a list which is the environment mapping splice
points to expressions and a representation of the quoted expression.

It is invariant that there are no splice forms in renamed syntax as they are all
replaced during renaming into this environment form.

To represent a simple quoted expression will have an empty environment but
if we also use splices then these are included as well.

```
[| [| 4 |] |] => BrackE [] (representation of 4)

[| [| $(foo) |] |] => BrackE [(x, representation of foo)] (representation of x)
```

# Conclusion

Those are the details of implementing nested brackets, if you ever need to
for your own language. In the end, the patch was quite simple but it took quite
a bit of thinking to work out the correct way to propagate the splices and build
the correct representation.














