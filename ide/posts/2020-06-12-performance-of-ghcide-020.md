---
posttitle: The performance of ghcide 0.2.0
author: Pepe Iborra
date: 2020-06-15
github: pepeiborra
---

You may have tried ghcide in the past and given up after running out of memory.
If this was your experience, you should be aware that ghcide v0.2.0 was released
[earlier this month][1] with a number of very noticeable efficiency improvements.
Perhaps after reading this post you will consider giving it another try.

In case you don't have time much, this is what the heap size looks like while
working on the Cabal project (230 modules) over different versions of ghcide:

![ghcide heap size while working on Cabal][4]

The graph shows that ghcide v0.2.0 is much more frugal and doesn't' leak. These improvements apply to haskell-language-server as well, which builds on a version of ghcide that includes these benefits.

## Background

A few months ago I started using ghcide 0.6.0.
It worked fine for small packages, but our codebase at work has a few
hundreds of modules and ghcide would happily grow >50GB of RAM. While the
development server I was using had RAM to spare, the generation 1 garbage collector pauses were multi-second and
making ghcide unresponsive. Thus one of my early contributions to the project
was to use [better default GC settings][5] to great effect.

Work to improve the situation started during the [Bristol Hackathon][6] that
Matthew Pickering organised, where some of us set to teach ghcide to leverage `.hi` and `.hie` files produced by GHC to reduce the memory usage. Version 0.2.0 is the culmination of those efforts, allowing it to handle much larger projects with ease.

In parallel, Matthew Pickering and Neil Mitchell spent some long hours chasing
and plugging a number of space leaks that were causing an unbounded memory
growth while editing files or requesting hover data. While there's probably still
some leaks left, the situation has improved dramatically with the new release.

## Benchmarks

One thing that became clear recently is that a benchmark suite is needed,
both to better understand the performance impact of a change and to prevent the
introduction of space leaks that are very costly to diagnose once they get in.

Usually the main hurdle with defining a benchmark suite is ensuring that the experiments do
reproduce faithfully the real world scenario that is being analysed.
Thankfully, the fantastic lsp-test package makes this relatively easy in this case.
An experiment in the new and shiny ghcide benchmark suite looks like follows:

<p>
<script src="http://gist-it.appspot.com/http://github.com/pepeiborra/ghcide/blob/144837aa90b75f8dab5a8e639369b5ca1fba6e66/bench/Main.hs?slice=103:106"></script>
</p>

Currently the benchmark suite has the following experiments covering the most common actions:

    - edit
    - hover
    - hover after edit
    - getDefinition
    - completions after edit
    - code actions
    - code actions after edit
    - documentSymbols after edit

The benchmark unpacks the Cabal 3.0.0.0 package (using Cabal of course), then
starts ghcide and uses Luke's lsp-test package to simulate opening
the `Distribution.Version` module and repeating the experiment a fixed number of times,
collecting time and space metrics along the way. It's not using criterion or anything
sophisticated yet, so there is plenty of margin for improvement. But at least it provides
a tool to detect performance regressions as well as to decide which improvements are
worth pursuing.

## Performance over time

Now that we have a standard set of experiments, we can give an answer to the question:

> How did the performance change since the previous release of ghcide?

Glad that you asked!

I have put together a little Shake script to checkout, benchmark and compare a set of git
commit ids automatically. It is able to produce simple graphs of memory usage over time too.
I have used it to generate all the graphs included in this blog post, and to gain insights
about the performance of ghcide over time that have already led to new performance improvements,
as explained in the section about interfaces a few lines below.

The script is currently still in [pull request][PR-histBench].

## Performance of past ghcide versions

The graph of live bytes over time shown at the beginning of the post was for the hover after edit
experiment. It's reproduced below for convenience.

![ghcide heap size while working on Cabal][4]

The graph shows very clearly that versions 0.0.5, 0.0.6 and 0.1.0 of ghcide
contained a roughly constant space leak that caused the huge memory usage
that caused many people including myself struggle with ghcide. As ghcide became
faster in v0.1.0 the leak became faster too, making the situation perhaps even worse.

The good news is that not only does v0.2.0 leak nearly zero bytes in this experiment, but
it also sits at a much lower memory footprint. Gone are the days of renting
EC2 16xlarge instances just to be able to run ghcide in your project!

The graph also shows that v0.1.0 became 2.5X faster than the previous version,
whereas v0.2.0 regressed in speed by a roughly similar amount.
I wasn't aware of this fact before running these experiments,
and I don't think any of the usual ghcide contributors was either.
By pointing the performance over time script to a bunch of commits in between the 0.1.0 and
0.2.0 version tags, it was relatively easy to track this regression down to the change that
introduced interface files. More details a few lines below.

## The HashMap space leak

The graph below compares the live bytes over time in the hover after edit experiment
before and after [Neil's PR][7] fixing the 'Hashable' instances to avoid a space leak on collisions.

![ghcide heap size before and after HashMap space leak fix][8]

The graph shows clearly that the fix makes a huge difference in the memory footprint, but back
when Neil sent his PR, the benefits were not so obvious. Neil said at the time:

> This patch and the haskell-lsp-types one should definitely show up in a benchmark, if we had one. Likely on the order of 2-10%, given the profiling I did, but given how profiling adjusts times, very hard to predict. Agreed, without a benchmark its hard to say for sure.

And then followed up with:

> Since I had the benchmark around I ran it. 9.10s, in comparison to 9.77s before.

That statement doesn't really illustrate the massive space benefits of this fix. A good benchmark suite
must show not only time but also space metrics, which are often just as important or even more.

## The switch to interfaces

The graph below shows the accumulated impact of the switch to interface files, which
was spread over several PRs for ease of review, using the get definition experiment
compared with the previous version:

![ghcide heap size before and after interface files (get definition)][9]

On a first impression this looks like a net win: shorter execution time and lower memory usage,
as one would expect from such a change. But when we look at another experiment, hover after edit,
the tables turn as the experiment takes almost twice as long as previously, while consuming
even more memory:

![ghcide heap size before and after interface files (hover after edit)][10]

We can explain the memory usage as the result of a space leak undoing any win from using interface files,
but there is no such explanation for the loss in performance.
This is a good reminder that one experiment is not enough, a good benchmark suite
must cover all the relevant use cases, or as many as possible and practical.

As it turns out, the switch to interfaces introduced a
serious performance regression in the code that collects the Haddocks for the set of spans
of a module, something that went completely undetected until now. Thankfully,
with the knowledge that better performance is available, it is much easier for any competent
programmer to stare at their screen comparing, measuring and bisecting until eventually
such performance is recovered. This [pull request][PR-ghc-module] to ghcide
does so for this particular performance regression.

## Conclusion

ghcide is a very young project with plenty of low
hanging fruit for the catch. With a benchmark suite in place, the
project is now in a better position to accept contributions without the
fear of incurring into performance regressions or introducing new space leaks.

If you are interested in joining a very actively developed Haskell project,
check the good first issue tags for [ghcide][11] and [haskell-language-server][12]
and send your contributions for review!

[1]: https://twitter.com/cocreature/status/1267837792064876545?s=20
[2]: https://github.com/pepeiborra/ghcide/blob/more-benchmark/bench/Hist/Main.hs
[3]: https://neilmitchell.blogspot.com/2020/05/fixing-space-leaks-in-ghcide.html
[4]: /ide/images/benchmarks/hoverAfterEdit-majorVersions.svg
[5]: https://github.com/digital-asset/ghcide/pull/329
[6]: https://mpickering.github.io/bristol2020.html
[7]: https://github.com/digital-asset/ghcide/pull/588
[8]: /ide/images/benchmarks/hoverAfterEdit-hashablePR.svg
[9]: https://raw.githubusercontent.com/pepeiborra/ghcide/bench-hist-dump/bench-hist/interface_files/getDefinition.diff.svg
[10]: https://raw.githubusercontent.com/pepeiborra/ghcide/bench-hist-dump/bench-hist/interface_files/hover_after_edit.diff.svg
[11]: https://github.com/digital-asset/ghcide/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22
[12]: https://github.com/haskell/haskell-language-server/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22
[PR-histBench]: https://github.com/digital-asset/ghcide/pull/629
[PR-ghc-module]: https://github.com/digital-asset/ghcide/pull/630
