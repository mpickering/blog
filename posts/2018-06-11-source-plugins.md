---
title: "Source Plugins: Four ways to build a typechecked Haskell expression"
---

A source plugin allows users to modify and analyse code after typechecking.
They have access to all the features of the compiler to create and modify
bindings.

Sounds great right? GHC is all about creating and modifying bindings so
this should be a doddle. However, it isn't immediately obvious how to make
a typechecked binding using the provided API.

In this post, I'll demonstrate and explain four different ways of arriving
at a typechecked expression which can then be inserted into a program.
All these examples are packaged up into a complete plugin on my
[GitHub page](https://github.com/mpickering/plugin-constraint).

<!--more-->

## Constructing Expressions

The four examples which we order from the most direct to the most indirect.
Each has their advantages but in general, directly constructing an already
typechecked binding is the most risky but flexible whilst starting from a
Template Haskell quote is the safest but most rigid option. In the end
we will always end up with a `LHsExpr GhcTc` but we will start from four
different places.

* `LHsExpr GhcTc` - An already typechecked expression
* `LHsExpr GhcRn` - An already renamed expression
* `LHsExpr GhcPs` - A parsed expression
* `TH.Exp`        - A quoted expression

Each example will construct the simple expression `print ()`. This example
is chosen because it  demonstrates that we must be
concerned with polymorphism and type classes during the process.

## Typechecked Expression

Constructing a `LHsExpr GhcTc` by hand is quite annoying as you must
be knowledgeable about how the typechecker prepares a program. You must also
be quite explicit about types and the compiler won't tell you that you're
wrong unless you compiler with core lint enabled.

```haskell
-- Creates the already typechecked expression `print ()`
--
-- There is quite a lot of boilerplate and the compiler won't tell
-- you where you went wrong unless -dcore-lint is enabled.
mkNewExprTc :: TcM (LHsExpr GhcTc)
mkNewExprTc = do
  -- Get the `Id`s that we need, these ones helpfully are already defined
  -- in `PrelNames` and `TysWiredIn`.
  print_id    <- tcLookupId printName

  -- Generate the evidence for `Show ()` which we will pass to `print`
  (dict_var, showUnitEv) <- generateDictionary
  let
    rhs = mHsApp (mLHsWrap wrapper printExpr) unitExpr)

    printExpr = nlHsVar print_id
    unitExpr = nlHsDataCon unitDataCon

    -- How we are going to apply the necessary type arguments
    wrapper = mkWpLet showUnitEv <.> mkWpEvVarApps [dict_var] <.> mkWpTyApps [unitTy]
  return rhs
```

Notice that in particular, we have to be careful to construct and apply
a wrapper which will desugar to type and dictionary applications. We also
have to explicitly come up with the correct dictionary for the `print` function
and manually apply it ourselves. This approach gives us a lot of control but
leaves many fiddly places to go wrong.

## Renamed Binding

The second approach creates a `LHsExpr GhcRn` which we will then typecheck
using the normal typechecker. This has the advantage that we can be
direct about which names we mean but also have the safety that the compiler will
emit an error when the plugin is used if we made a mistake.

We still have to construct the return type of the expression to pass
into the typechecker. We use `lookupOccRn` to find the definition of `print`
which is in scope but can use `unitDataCon` directly without looking it up
as it is already wired into the compiled and defined in `PrelNames`.

```haskell
-- Creates a `LHsExpr GhcRn` which we then typecheck to turn into
-- a `LHsExpr GhcTc`. The compiler will raise an error to the user if you
-- made a mistake in constructing the term.
mkNewExprRn :: TcM (LHsExpr GhcTc)
mkNewExprRn = do
  -- The names we want to use happen to already be in PrelNames so we use
  -- them directly.
  let print_occ = mkRdrUnqual (mkVarOcc "print")
  print_name <- lookupOccRn print_occ
  let raw_expr = nlHsApp (nlHsVar print_name) (nlHsVar (dataConName unitDataCon))
  io_tycon <- tcLookupTyCon ioTyConName
  let exp_type = mkTyConApp io_tycon [unitTy]
  typecheckExpr exp_type raw_expr
```

## Parsed Binding

We sometimes don't want to specify exactly which names we mean or know which
renaming functions to use to find them. It's
easier to let the renamer do that for us. In this example, we construct
a `LHsExpr GhcPs` which we then pass into the renamer and typechecker. Therefore,
the compiler will warn us about out of scope variables or type errors in the
expression we have created like normal.

Instead of constructing `Name`s like before, we are constructing `RdrName`s.
These are conceptually easier to create as we must only consider the namespace
the name lives in rather than any other details. The renamer will fill in the
rest of the information.

```haskell
-- An example of how to construct a value from a `LHsExpr GhcPs`.
-- Making sure things are in the right namespace is sometimes a bit
-- awkward.
mkNewExprPs :: TcM (LHsExpr GhcTc)
mkNewExprPs  = do

  let
    print_occ = mkRdrUnqual (mkVarOcc "print")
    unit_occ = nameRdrName (dataConName unitDataCon)
    ps_expr = nlHsApp (nlHsVar print_occ)
                      (nlHsVar unit_occ)

  io_tycon <- tcLookupTyCon ioTyConName
  let exp_type = mkTyConApp io_tycon [unitTy]
  renameExpr ps_expr >>= typecheckExpr exp_type
```

Note, I initially tried to use a different definition of `unit_occ` which
didn't start from the wired in name but it proved tricky to get correct. This
is why we start from the more specific `Name` and turn it into a `RdrName`.
We are also relying more on the context of the module, if the user hasn't
imported `print` then the plugin will fail.

## Constructing a binding from Template Haskell

Still, explicitly typing out the syntax tree is quite onerous.
We would much prefer type out a Haskell program as normal and then splice it into
our program. We can achieve this by using a Template Haskell quasiquoter.

We first quote the expression we want to insert which creates a `TH.Exp` which
we can convert to a normal `HsExpr GhcPs` by using the same function that
GHC itself uses to deal with Template Haskell. Once we have the parsed expression
we can rename it and typecheck it as in the previous examples. This is all quite
convenient!

```haskell
liftQ :: Q a -> TcM a
liftQ = liftIO . runQ

-- Creates a `TH.Exp` using a quasiquoter before renaming
-- and typechecking to create an `LHsExpr`. This is convenient as you
-- don't have to write out the syntax.
-- p
mkNewExprTh :: TcM (LHsExpr GhcTc)
mkNewExprTh = do
  th_expr <- liftQ [| print () |]
  ps_expr <- case convertToHsExpr noSrcSpan th_expr of
    Left _err -> error "Bad expression"
    Right res -> return res

  io_tycon <- tcLookupTyCon ioTyConName
  let exp_type = mkTyConApp io_tycon [unitTy]
  renameExpr ps_expr >>= typecheckExpr exp_type
```

# Conclusion

In our experience, the most practical way of constructing expressions is to
use Template Haskell. A more in-depth example which creates a complicated
expression can be see in the [assert-explainer](https://github.com/ocharles/assert-explainer)
which I worked on with Ollie Charles at Zurihac 2018.


# Addendum: How can I interact with the constraint solver?

The `generateDictionary` is an example of how to generate some evidence.
It might need to be refined further but worked for simple tests.

# Addendum: How can I get the type of an `HsExpr GhcTc`?

To get the type of an expression, first desugar the expression and
then use `exprType` to find the type of the desugared variant. There
is an example in [assert-explainer](https://github.com/ocharles/assert-explainer/blob/master/plugin/AssertExplainer.hs#L289).

# Addendum: How can I combine together old and new program fragments

If you want to combine together pieces of new syntax
and old syntax then don't try to do it directly. Construct a lambda with
arguments for each of the pieces you want to insert. Finally, apply the
lambda to the existing expressions you want to insert.





