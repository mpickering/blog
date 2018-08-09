---
title: How to write an update function
---

One of the first exercises when learning functional programming
is writing a function which updates a value in a data
structure by pattern matching.

It is only later that we learn that pattern matching on a data
constructor forces the value. For sum types, this is necessary
in order to evaluate which constructor we are dealing with but for
data types with one constructor we already know which constructor
we used to build the value.

This tension between writing functions using pattern matching and avoiding
unecessary evaluation is the reason for lazy pattern matching.
A pattern can be preceded with a `~` to indicate that it is an irrefutable
pattern

Lazy pattern matches are a way to write functions in a pattern
matching style without forcing the values.

In these examples we used `⊥` to demonstrate the strictness properties
of these functions. This is still relevant with normal programs which don't contain `⊥`s as one field of a data structure might contain a very expensive
function which we don't want to force unless necessary.
