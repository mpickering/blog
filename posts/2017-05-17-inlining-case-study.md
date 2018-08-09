---
title: "INLIN(E)ing: A case study"
---

Ollie Charles recently popped into #ghc to ask about a small program which
was taking a long time to compile. In fact, the compiler was taking so long
in the simplifier he had to increase the tick factor (a measure of how much work
the simplifier is allowed to do) to get compilation to finish. Oleg and I quickly
set to work working out what was going on in his program.

It turned out that a misplaced `INLINE` pragma was causing a lot of simplification
work to be duplicated. Removing the pragma allowed the compiler to operate
faster whilst producing the same code.

<!--more-->

# The Problem

When a lot of time is spent in the simplifier it is usually because the core
programs have grown quite large. Core programs can grow large for a number of reasons
but one of primary reasons is due to excessive inlining caused by `INLINE` pragmas.

The first tool we have at our disposal is `-ddump-simpl-stats` which outputs
a summary of each step the simplifier takes. Looking at this summary is a good
way to work out roughly where the problem lies.

In this case, the statistics file was quite [large](https://gist.github.com/ocharles/8d69c518b5d3169e8254b24205e03f65).
The first bit I always check is the "UnfoldingDone" section which details
how many times each definition has been inlined. Here is the relevant
snippet from the top of that section.

```
  14620 UnfoldingDone
    596 $
    574 contramapF
    546 $fNumInt_$c+
    485 $fStorableWord8_$cpoke
    485 castPtr
    485 $fStorableWord8_$calignment
    485 word8
    485 $s>$<
    485 castPtr1
    484 thenIO
    484 thenIO1
    484 ord
    484 $fBitsInt_$c.&.
    484 plusPtr
    484 $fStorableWord19
    463 char7
    331 $s>*<1
    331 pairF
    220 returnIO
    220 returnIO1
    220 $s>$<
    220 contramapB
```

The first thing to notice about these numbers is that there are groups of definitions
which have all been inlined the same number of times. This is indicative of
a misplaced `INLINE` pragma as a large unoptimised definition will then be inlined
many times and then simplified individually at each call site rather than once
at the definition site. Of particular suspicion is the large block of definitions
which are each inlined exactly 484 times.

By looking at the definitions of each of the identifiers in this list, we can then
work out what is going on. To cut to the chase, inspecting the definition of
`char7` from the `Data.ByteString.Builder.Prim.ASCII` module we can see where
a lot of the work is coming from.

```
-- | Encode the least 7-bits of a 'Char' using the ASCII encoding.
{-# INLINE char7 #-}
char7 :: FixedPrim Char
char7 = (\c -> fromIntegral $ ord c .&. 0x7f) >$< word8
```

The definition of `char7` is concise but composed of combinators which
will be keen to get inlined later on. The definitions of `ord`, `.&.` and `>$<`
are all small.

By using an `INLINE` pragma, the unoptimised unfolding is included in the
interface file so this complex definition will be inline verbatim into each call site.
We can inspect the unfolding by using the `--show-iface` flag on the `.hi` file for
the module.

```
8334ad079da5b638008c6f8feefdfa4a
   char7 :: FixedPrim Char
   {- HasNoCafRefs, Strictness: m, Inline: INLINE (sat-args=0),
      Unfolding: InlineRule (0, False, False)
                 ($s>$<
                    @ Char
                    @ Word8
                    (\ (c :: Char) ->
                     $ @ 'PtrRepLifted
                       @ Int
                       @ Word8
                       (\ (x :: Int) ->
                        case x of wild { I# x# -> W8# (narrow8Word# (int2Word# x#)) })
                       ($fBitsInt_$c.&. (ord c) (I# 127#)))
                   word8) -}
```

Which very closely resembles the source definition.

Removing the `INLINE` pragma we get a nice, small optimised definition
which crucially is still small enough that GHC inlines it at call sites.

```
5e7820a4ab4b18cf2032517105d2cc56
  char7 :: FixedPrim Char
  {- HasNoCafRefs, Strictness: m,
     Unfolding: (FP
                   @ Char
                   1#
                   char1
                     `cast`
                   (<Char>_R ->_R <Ptr Word8>_R ->_R Sym (N:IO[0] <()>_R)))      -}
```

Look! No calls to `>$<`, `.&.`, `ord` or any other complicated functions. We
have optimised the definition *once* at the definition site so that we don't
have to repeatedly do so at each call site. We didn't even need to look at the
program to spot the problem.

# Discussion

This is currently a problem because `INLINE` is used for two different reasons.

1. By library authors who use `RULES` where it is important to inline the literal
   rhs of a definition so that the rules reliably fire.
2. By library authors who want to inline definitions so that GHC's simplifier can
   work better across modules.

For the first case, the unoptimised unfoldings are important but for the second
this leads to a lot of duplicated work. In this case, I could see that
there were no rules defined which were relevant to the definition of `char7` so
I ruled out the first scenario. I then verified that GHC considered the optimised
version of `char7` small enough to include in interface files and inline by using
`--show-iface`. Ruling out both of these possibilities, it then seemed sensible
to remove the pragma.

It would be good to add a new pragma which instructs GHC to inline an *optimised*
unfolding across modules rather than the unoptimised version so that the second
scenario can be reliably achieved.

# Links

* [Reddit Comments](https://www.reddit.com/r/haskell/comments/6bojlj/inlineing_a_case_study/)



