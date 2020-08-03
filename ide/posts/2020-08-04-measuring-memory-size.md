---
posttitle: Measuring memory usage of Haskell values and patching GHC
author: Michail Pardalos
date: 2020-08-04
github: mpardalos
---

As part of my Google Summer of Code project to add instrumentation to ghcide, I needed to measure the size of Haskell values in memory. After getting blocked by a bug in a GHC primop I fell down a rabbit hole of learning about GHC's memory layout, C-- and making my first contribution to GHC.

<!--more-->

In this post I want to describe that journey and hopefully encourage some more people to consider contributing to GHC. I will be explaining the concepts that were new to me, however some will still be unfamiliar to some people. I encourage you to look for them in the excellent [GHC wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/home). Ctrl-f in the [table of contents](https://gitlab.haskell.org/ghc/ghc/-/wikis/index) will most likely get you a good explanation.

## The prompt

This all started when one of my GSoC mentors, Matthew Pickering, suggested that I use the [ghc-datasize](https:github.com/def-/ghc-datasize) library to measure the in-memory size of a large hashmap at the core of ghcide. This would be useful as memory usage is something we have discussed as a target for improvement on ghcide. This info would allow us to correlate actions in the editor with spikes in memory usage, including exactly what data is taking up space. We could also check whether entries in this HashMap are released when appropriate, for example, when a file is closed.

## Starting out

The library only provides one [module](https://hackage.haskell.org/package/ghc-datasize-0.2.2/docs/GHC-DataSize.html) with 3 functions. Of those, I only really needed `recursiveSize`. I set up a thread using the [async](https://hackage.haskell.org/package/async) library to regularly run the `recursiveSize` function on the hashmap in question and print the result, the memory size of the hashmap, to stdout. The code compiled --- a promising sign --- but upon starting ghcide, I was greeted by the following error, and no size measurements.
```
closurePtrs: Cannot handle type SMALL_MUT_ARR_PTRS_FROZEN_CLEAN yet
```
Looking at the `recursiveSize` function showed nothing that could throw this error. I decided to dig a bit deeper at the functions it was calling, namely `closureSize` and `getClosureData` (which came from the `ghc-heap` library). The fact that both of them were calling a primop, `unpackClosure#`, seemed suspicious. I decided to grep for the error in GHC. Sure enough, this is an error message printed by `heap_view_closurePtrs` , a C function in the RTS, which is then used by the `unpackClosure#` primop.

This function works on (*closures*)[https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects], the objects that GHC's RTS allocates on the heap to represent basically all Haskell values. When called on a haskell value, it inspects its closure's tag, which determines what kind of data the closure is holding (e.g. a Thunk, different kinds of arrays, data constructors, etc.). Based on that it extracts the all the pointers contained in it. Back in haskell-land, `unpackClosure#` returns the objects pointed at by those pointers.

Looking through GHC's git log showed that this function did not support any of the `SMALL_MUT_ARR_PTRS_*` closure types (corresponding to the `SmallArray#` and `SmallMutableArray#` types) until GHC 8.10. The library requires GHC 8.6 but looking at its code it seemed like it should work with a newer GHC. Given that the code is a single file, it seemed simple enough to just copy as-is into ghcide, which I did. Switching to GHC 8.10.1 stopped the error and showed some memory size measurements being printed. Success!

Except not so fast. Firstly, I was still getting a similar error:
```
closurePtrs: Cannot handle type TVar
and
closurePtrs: Cannot handle type TSO
```
It seemed to not stop the measuring thread, and was also repeated. Looking back at the code for `heap_view_closurePtrs` it seems these closure types are not supported even in GHC 8.10. When it encounters them, `unpackClosure#` simply returns empty arrays, meaning that `recursiveSize` then just stops recursing and returns 0. I decided to ignore this error for now as there was a bigger issue.

It *appeared* that I was only getting an output 4-5 times after the start of the program. I assumed that some exception was stopping the thread that was performing the measurement. I was wrong. After an embarrassingly long time trying to find what could be throwing an exception, I realised: it wasn't stopping, it was simply taking so long that I never saw the output because I just killed the program. Setting a recursion depth limit (having the measurement function stop recursing after a certain number of steps) made it keep on producing output confirming this hypothesis.

## This time it *is* a bug in the compiler (kind of)

The recursion limit was not an acceptable workaround, as we would be missing the majority of the structure being measured. I decided to optimise enough that I could remove the recursion limit. I will not go into detail on the optimisations I did. Suffice it to say, I replaced a list of visited closure addresses that was used for cycle detection with a HashSet. What *is* important is that this optimisation allowed me to remove the recursion limit and uncovered a bug. I started getting this output, which appeared to also be fatal for the measuring thread:
```
ghcide: internal error: allocation of 2243016 bytes too large (GHC should have complained at compile-time)
    (GHC version 8.10.1 for x86_64_unknown_linux)
    Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
```

My initial guess was that this was coming from the HashSet I had just added. I thought removing the recursion limit allowed it to get big enough to cause this error. However, some googling (and speaking with my GSoC mentor, Matthew) pointed to [this issue](https://gitlab.haskell.org/ghc/ghc/-/issues/12492) which appeared like a much more plausible explanation. To save you a click, this is a bug in the `unpackClosure#` primop, used by `getClosureData` in `ghc-heap`, triggered when it is used on a closure above a certain size.

## Patching to GHC

This is the point where I decided to take a go at fixing this bug in GHC. I was initially hesitant. I had no clue about GHC's internals at the time, but my mentors encouraged me to go ahead. The fact that this was a significant blocker for my project also helped.

### Nailing the bug

The first step was to make a minimal test case that would trigger the bug. I needed to make a closure *exactly* large enough to trigger this bug (but no bigger) and then call `unpackClosure#` on it. My first attempt used the [`newIOArray`](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-IOArray.html#v:newIOArray) function and then pattern matched on it to get the primitive array:

```haskell
main :: IO ()
main = do
  IOArray (STArray _ _ _ arr)  <- newIOArray (1 :: Int, 128893) (0::Int)
  let !(# !_, _, _ #) = unpackClosure# (unsafeCoerce# arr)
  return ()
```

The array size, which I found by experimentation, is the smallest that would trigger the bug. The call to `unpackClosure#` also needs to be pattern matched on, with those bang patterns, or the buggy code never runs (because of laziness).

I wanted to simplify this example the bit more, to the point of using primops exclusively. I thought that would also make it more obvious to someone reading the test what bug it is testing. This is the final test (and the one that is now in GHC's test suite):
```haskell
main :: IO ()
main = IO $ \s -> case newByteArray# 1032161# s of
  (# s', mba# #) -> case unpackClosure# (unsafeCoerce# mba# :: Any) of
    (# !_, _, _ #) -> (# s', () #)
```
The major changes here are the use of `newByteArray#` instead of `newIOArray` and explicitly constructing an `IO ()` instead of using do-notation. The latter is a necessary for the former since `newByteArray#` returns an unlifted array (`MutableByteArray#`). You can also see that the array size has changed, since it is now in bytes, not a number of `Int`'s. However you might also notice that $1032161$ is not *exactly* $8 * 128893$, which is what you would expect, since Ints should be 64 bits (8 bytes), but rather 1017 bytes more. I am unsure why this discrepancy is there but this was the minimum size that would trigger the bug.

### Making the fix

The information in this section might be inaccurate as I am new to all this. Please take it with a grain of salt.

I added this test case into GHC's tests. The next step was to fix the actual bug. `unpackClosure#`, like all primops, is written in C--, a language used as an internal representation in GHC. There is excellent information on it [in the GHC wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/cmm). There was a comment left on the ticket discussion that the problematic line had to be the following.
```c
ALLOC_PRIM_P (dat_arr_sz, stg_unpackClosurezh, closure);
```

The previous line makes an allocation of `dat_arr_sz` words, which is then used by the next line:
```c
dat_arr = Hp - dat_arr_sz + WDS(1);
```
Here `Hp` is the heap pointer. After calling `ALLOC_PRIM_P` it points to the top of the heap, and so we can get the pointer to the start of the array by subtracting the array size from the heap pointer.

According to the discussion on the issue, the fix to this would be to replace the use of `ALLOC_PRIM_P` with `allocate`. I was unsure of the right way to do this, but, thankfully, the `stg_newSmallArrayzh` function (mapping to the `newSmallArray#` primop) seemed to use `allocate` in essentially the same way as I needed. Copying the code from there and with some minor adaptations I got:
```c
("ptr" dat_arr) = ccall allocateMightFail(MyCapability() "ptr", BYTES_TO_WDS(dat_arr_sz));
if (dat_arr == NULL) (likely: False) {
    jump stg_raisezh(base_GHCziIOziException_heapOverflow_closure);
}
TICK_ALLOC_PRIM(SIZEOF_StgArrBytes, WDS(len), 0);
```

Running the full test suite confirmed that this fixed the bug, and also did not cause any regressions. Job done! I got my commit history cleaned up and marked the MR as ready. It is now in the merge queue for GHC 8.12! ([or is it 9.0?](https://mail.haskell.org/pipermail/ghc-devs/2020-July/019083.html)).

## Back to the main task

At this point I was done with patching GHC, however I still needed the patched code for ghcide, using ghc 8.10. As it turns out, it is actually possible to define your own primops, in C--, include them in a Cabal package, and import them from Haskell using `foreign import prim`, which is what I ended up using. I forked ghc-datasize [here](https://github.com/mpardalos/ghc-datasize), and adapted it to work with a custom primop (containing this fix).

At this point I went back to running ghcide and realized that I needed to urgently improve the performance of ghc-datasize. Even with the optimisations I had already made, each measurement took almost 2 minutes! I am still in the process of optimising this library in order to make it practical for use on ghcide. However, one interesting turn in this process is the fact that I ended up throwing away the code I had written for use in GHC.

The purpose of the buggy allocation was to create an array to copy the closure's data section into. I, however, had no use for this. I only needed the full size of the closure (in order to add to the final count) and its pointers (in order to recurse). Since I could get the closure size using `closureSize#` I could throw away the code dealing with the closure's data section, including what I had previously added. This gave a 3-6x improvement in my benchmarks.

Even with this improvement, there is still work to be done before we can use this on ghcide. Measuring the HashMap I mentioned before, now takes around a minute and 30 seconds. This is a problem as it's really hard to get valuable data on the running program if collection can only happen that rarely. This is something I am still working on and will hopefully write about once I've improved.

I have really enjoyed this process, especially since I go to understand so much about the internals of the language we all like using so much. Hopefully this post goes to show that taking your first steps in contributing to GHC can be very rewarding.
