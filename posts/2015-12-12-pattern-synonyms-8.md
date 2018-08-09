---
title: Pattern Synonyms in GHC 8.0
---

There have been four small but significant improvements to pattern synonyms
which are going to appear in GHC 8.0.

This work closes up some holes which were left in the implementation of pattern
synonyms and should provide library authors with a new and flexible method of
abstraction.

More information about pattern synonyms can be found in the [GHC 8.0 user guide](http://mpickering.github.io/users_guide/glasgow_exts.html#pattern-synonyms).

<!--more-->

### Record Pattern Synonyms

The biggest update extends pattern synonyms to allow the construction of
pattern synonyms which behave like record data constructors.

Since GHC 7.8 you have been able to define prefix and infix pattern synonyms
which behave like normal data constructors. With the addition of record
pattern synonyms
most data constructors can be
replicated by pattern synonyms.[^1]

To make this clear, consider the data constructor `Just`. We can use this
constructor in two contexts, in a pattern match or in an expression context
to construct a value.

If we defined the pattern synonym `MyJust`, we can use it in precisely the same
contexts as `Just`.

```haskell
pattern MyJust :: a -> Maybe a
pattern MyJust a = Just a
```

Similarly, record data constructors can be used in seven contexts.

+---------------------------------------+--------------------------------------+
|Usage                                  |   Example                            |
+=======================================+======================================+
|As a constructor                       |   `zero = Point 0 0`                 |
+---------------------------------------+--------------------------------------+
|As a constructor with record syntax    |   `zero = Point { x = 0, y = 0}`     |
+---------------------------------------+--------------------------------------+
|In a pattern context                   |   `isZero (Point 0 0) = True`        |
+---------------------------------------+--------------------------------------+
|In a pattern context with record syntax|   `isZero (Point { x = 0, y = 0 }`   |
+---------------------------------------+--------------------------------------+
|In a pattern context with field puns   |   `getX (Point {x}) = x`             |
+---------------------------------------+--------------------------------------+
|In a record update                     |   `(0, 0) { x = 1 } == (1,0)`        |
+---------------------------------------+--------------------------------------+
|Using record selectors                 |   `x (0,0) == 0`                     |
+---------------------------------------+--------------------------------------+

Record pattern synonyms are defined as follows and can also be used in these
seven contexts.

```haskell
pattern MyPoint :: Int -> Int -> (Int, Int)
pattern MyPoint{x, y} = (x,y)
```

Projection functions, `x` and `y` are defined like record selectors for
ordinary constructors.

```haskell
x :: (Int, Int) -> Int
y :: (Int, Int) -> Int
```

Because we defined this pattern synonym,
tuples can now be updated with record update syntax.

```haskell
> (0,0) { x = 5 }
(5, 0)
```

### Bundling Pattern Synonyms

Since pattern synonyms are a lot like data constructors, they should be able to
be imported just like data constructors. To put it another way
a user should be unaware whether they
are using a pattern synonym or a data constructor.

However, before GHC 8.0, there
has been quite an awkward distinction between the two. Data constructors couldn't
be imported or exported separated from the type which they construct. On the other
hand, pattern synonyms could only be imported and exported individually by using
the `pattern` keyword. This meant that consumers had to be aware that whether
they were importing a pattern synonym or not! No good!

Now there are two ways which we can export the pattern synonym `P :: A`.

1. Separately, as before, by using the `pattern` keyword.
    ```haskell
    module Foo (pattern P) where
    ```
2. Bundled with the relevant type constructor
    ```haskell
    module Foo ( A(P) ) where
    ```
    or to export all of `A`'s constructors along with the pattern synonym `P`.
    ```haskell
    module Foo ( A(.., P) ) where
    ```

In this second case, if another module imports `Foo` then `P` can be imported
alongwith `A`.

```haskell
-- Will import P
import Foo (A (..))
```

or

```haskell
-- Will import P
import Foo (A (P))
```

#### An Example: `ErrorCall`

This problem reared its head in one of the first serious uses of pattern synonyms.
In GHC 8 a pattern synonym `ErrorCall` is introduced into the base library to
smooth over changes in the internal representation caused by Eric Seidel's
work on call stacks.

The datatype `ErrorCall` previously just had one synonymous constructor.

```haskell
data ErrorCall = ErrorCall String
```

After the [refactoring](https://phabricator.haskell.org/D861), the single
constructor was renamed to `ErrorCallWithLocation` but Eric wanted to smooth
over the transition by providing a pattern synonym which would behave much like
before.

```haskell
data ErrorCall = ErrorCallWithLocation String String

pattern ErrorCall :: String -> ErrorCall
pattern ErrorCall s <- ErrorCallWithLocation s _ where
  ErrorCall s = ErrorCallWithLocation s ""
```

However clients importing `ErrorCall(..)` found that despite the careful efforts
of the library author this change broke their code. The problem being that by default,
it is necessary to explicitly import pattern synonyms.

With this feature, we can now bundle the new `ErrorCall` pattern synonym in
the export list of the module so that users importing `ErrorCall(..)` will
also import the pattern synonym.

```haskell
module GHC.Exception ( ErrorCall(.., ErrorCall) ) where
```

### Pattern Synonym Signatures

Pattern synonyms can also have type
signatures. The syntax is very similar to normal type
signatures but there are two sets of constraints rather than the usual one which
correspond to "required" and
"provided" constraints.

```haskell
pattern P :: required => provided => type
```

In the common case that there are no provided constraints, it is possible to omit
the first set of constraints.

```haskell
pattern P :: required => type
```

and in the even more common case when there are no constraints, both can be omitted.

```haskell
pattern P :: type
````

*Required* constraints are constraints which are required in order to make a match.
For example, we could provide the quite silly pattern synonym which uses `show` to
check whether a pattern should match. As `show` is from the `Show` typeclass,
we have to add it to the required constraints.

```haskell
pattern IsTrue :: Show a => a
pattern IsTrue <- ((== "True") . show -> True)
```

*Provided* constraints are constraints which are made available on a successful match.
This usually occurs when matching on a GADT with an existential type.

In fact, it only makes sense for provided constraints to mention existentially
quantified type variables which explains why they are less often used.

```haskell
data T where
  MkT :: (Show b) => b -> T

pattern ExNumPat :: () => Show b => b -> T
pattern ExNumPat x = MkT x
```

Pattern synonym signatures aren't new for GHC 8.0 but the order of required
and provided constraints has been switched.[^2]

### Warnings for missing pattern synonym signatures

The final small change is that GHC can also warn about any pattern
synonym which doesn't have a type signature. The warning is turned on by the flag
`-fwarn-missing-pat-syn-sigs` and is also enabled by `-Wall`.



[^1]: There is one exception which is a datatype which has record constructors
     which share field names.
      `data A = B { a :: Int } | C { a :: Int, b :: Int }`

[^2]: In previous versions, provided constraints appeared before required
      constraints and if only one set was given then it was assumed to be the
      provided rather than required constraints.
