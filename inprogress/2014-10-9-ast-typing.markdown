---
title: The AST typing problem revisited
date: 2014-10-10
---

For me at least, ezyang's [blogpost](http://blog.ezyang.com/2013/05/the-ast-typing-problem/) provided an illuminating summary to the problems faced by compiler writers.

In fact, it is not a theoretical problem but a practical problem. It is very easy to define an ADT but once your codebase has reached 20 000 lines, it is a lot of work to extend your datatype with additional information. This is especially problematic if (like [pandoc](pandoc) does) you expose this data type to the user so that they can extend the functionality themselves. GHC 7.10 will provide a way to make such changes but still maintaining the exposed interface.

Type level fix
==============

Of all the solutions to the problem, it is clear that type-level fix is the most elegant. Unfortunately it has been historically difficult to retrofit such an interface, especially if you want to maintain code which works with previous versions. Fortunately, with GHC 7.10 it will become easy to maintain the interface whilst changing the underlying representation.

I will now demonstrate how to do so with a simple expression language and the `recursion-schemes` library.

We start with a simple expression language and evalation function.

~~~
data Exp = Const Int
         | Add Exp Exp

exp1 :: Exp
exp1 = Add (Const 5) (Const 6)

evaluate :: Exp -> Int
evaluate (Const n) = n
evaluate (Add x y) = evaluate x + evaluate y
~~~


~~~
data ExpF a = Const Int
            | Add a a
            deriving (Functor)

type Exp = Fix ExpF
~~~

Unfortunately, now our existing function won't work correctly as we instead need to match on `

~~~

Two Extensions
==============

Consider the scenario where you may want to leave "holes" in your data structure to fill in later with additional information not currently availible. This may motivate you to define your AST as follows.

@@@
data Hole

newtype HoleyExp = HoleyExp (Either Hole (ExpF HoleyExp))

hole = Left Hole

sumWithHole :: HoleyExp
sumWithHole = add (num 6) hole

fillHoles :: Int -> HoleyExp -> Exp
fillHoles n (= ?
@@@





Patterns
================

Now we have our new super-charged data type - let's get back to the original problem. There is already an evaluate function but unfortunately we can no longer use it without some 'not only tedious but abstraction breaking' pattern matching.

@@@
evaluate :: Exp -> Int
evaluate (Exp (Const n)) = n
evaluate (Exp (Add x y))  = evaluate x + evaluate y
@@@

Also, this definition doesn't get us any closer to being able to use our evaluate function with any 


[fixplate]: https://hackage.haskell.org/package/fixplate
[recurschemes]: https://hackage.haskell.org/package/recursion-schemes
[fho]: http://fho.f12n.de/posts/2014-05-07-dont-fear-the-cat.html
[patrickthompson]: http://patrickthomson.ghost.io/an-introduction-to-recursion-schemes/
[timphillipwilliams]: http://www.timphilipwilliams.com/slides.html
[comonadrsfieldguide]: http://comonad.com/reader/2009/recursion-schemes/
[stackoverflowrecursionschemes]: http://stackoverflow.com/questions/6941904/recursion-schemes-for-dummies
