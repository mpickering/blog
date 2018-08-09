Consider a backtracking compilation, there are three things we might want to do: return a value, kill a branch or branch. We can represent these options using an ADT.

```
data BacktrackF a =
    Return a
  | Fail
  | Branch (BacktrackF a) (BacktrackF a)
```

Backtrack forms a functor (we explicitly define it below but the `DeriveFunctor` extension can also be used).

```
instance Functor BacktrackF where
  fmap f (Return a) = Return (f a)
  fmap f Fail = Fail
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
```

We also get a monad for free with the `Free` construction.

```
type Backtrack a = Free BacktrackF a
```

Now we can write programs using `Backtrack`. One such example might be the knapsack problem (the problem to choose elements from a set which sum to n).

```
knapsack :: Int -> [Int] -> Backtrack [Int]
knapsack w vs
  | ws < 0  = Free Fail
  | ws == 0 = return []
  | ws > 0  = do
                v <- select vs
                vs' <- knapsack (w - v) vs
                return (v:vs')

select :: [Int] -> Backtrack Int
select = foldr (Free Branch) (Free Fail) . map (Free Return)
```
