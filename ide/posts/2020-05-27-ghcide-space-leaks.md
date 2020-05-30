---
posttitle: Fixing Space Leaks in Ghcide
author: Neil Mitchell
date: 2020-05-27
github: ndmitchell
---
# Fixing Space Leaks in Ghcide

This blog post was first posted on [Neil's own blog](http://neilmitchell.blogspot.com/2020/05/fixing-space-leaks-in-ghcide.html) and is cross-posted here.

Over the bank holiday weekend, I decided to devote some time to a possible [Shake build system](https://shakebuild.com/) performance issue in [Ghcide Haskell IDE](https://github.com/digital-asset/ghcide). As I started investigating (and mostly failed) I discovered a space leak which I eventually figured out, solved, and then (as a happy little accident) got a performance improvement anyway. This post is a tale of what I saw, how I tackled the problem, and how I went forward. As I'm writing the post, not all the threads have concluded. I wrote lots of code during the weekend, but most was only to experiment and has been thrown away - I've mostly left the code to the links. Hopefully the chaotic nature of development shines through.

## Shake thread-pool performance

I started with [a Shake PR](https://github.com/ndmitchell/shake/pull/751) claiming that simplifying the Shake thread pool could result in a performance improvement. Faster and simpler seems like a dream combination. Taking a closer look, simpler seemed like it was simpler because it supported less features (e.g. ability to kill all threads when one has an exception, some fairness/scheduling properties). But some of those features (e.g. better scheduling) were in the pursuit of speed, so if a simpler scheduler was 30% faster (the cost of losing randomised scheduling), that might not matter.

The first step was to [write a benchmark](https://github.com/ndmitchell/shake/pull/751#issuecomment-632634439). It's very hard to synthesise a benchmark that measures the right thing, but spawning 200K short tasks into the thread pool seemed a plausible start. As promised on the PR, the simpler version did indeed run faster. But interestingly, the simplifications weren't really responsible for the speed difference - switching from `forkIO` to `forkOn` explained nearly all the difference. I'm not that familiar with `forkOn`, so decided to micro-benchmark it - how long does it take to spawn off 1M threads with the two methods. I found two surprising results:

* The performance of `forkOn` was quadratic! A [GHC bug](https://gitlab.haskell.org/ghc/ghc/issues/18221) explains why - it doesn't look too hard to fix, but relying on `forkOn` is unusual, so its unclear if the fix is worth it.
* The performance of `forkIO` was highly inconsistent. Often it took in the region of 1 second. Sometimes it was massively faster, around 0.1s. A [StackOverflow question](https://stackoverflow.com/questions/61971292/ghc-forkio-bimodal-performance) didn't shed much light on _why_, but did show that by using the [`PVar concurrency primitive`](https://hackage.haskell.org/package/pvar/docs/Data-Primitive-PVar.html#t:PVar) it could be 10x faster. There is a [GHC bug tracking the issue](https://gitlab.haskell.org/ghc/ghc/issues/18224), and it seems as though the thread gets created them immediately switches away. There is a suggestion from Simon Peyton Jones of a heuristic that might help, but the issue remains unsolved.

My desire to switch the Shake thread-pool to a quadratic primitive which is explicitly discouraged is low. Trying to microbenchmark with primitives that have inconsistent performance is no fun. The hint towards `PVar` is super interesting, and I may follow up on it in future, but given the remarks in the GHC tickets I wonder if `PVar` is merely avoiding one small allocation, and avoiding an allocation avoids a context switch, so it's not a real signal.

At this point I decided to zoom out and try benchmarking all of Ghcide.

## Benchmarking Ghcide

The thread about the Shake thread pool pointed at [a benchmarking approach](https://github.com/digital-asset/ghcide/issues/503) of making hover requests. I concluded that making a hover request with no file changes would benchmark the part of Shake I thought the improved thread-pool was most likely to benefit. I used the Shake source code as a test bed, and opened a file with 100 transitive imports, then did a hover over the `listToMaybe` function. I know that will require Shake validating that everything is up to date, and then doing a little bit of hover computation.

I knew I was going to be running Ghcide a lot, and the Cabal/Stack `build` steps are frustratingly slow. In particular, every time around Stack wanted to unregister the Ghcide package. Therefore, I wrote a simple `.bat` file that [compiled Ghcide and my benchmark](https://gist.github.com/ndmitchell/11467985dbf1855e62035fa97248a585#file-test-bat) using `ghc --make`. So I could experiment quickly with changes to Shake, I pulled in all of Shake as source, not as a separate library, with an include path. I have run that benchmark 100's of times, so the fact it is both simple (no arguments) and as fast as I could get has easily paid off.

For the benchmark itself, I first went down the route of looking at the [replay functionality](https://hackage.haskell.org/package/lsp-test/docs/Language-Haskell-LSP-Test-Replay.html#v:replaySession) in [lsp-test](https://hackage.haskell.org/package/lsp-test). Sadly, that code doesn't link to anything that explains how to _generate_ traces. After asking on the [haskell-ide-engine IRC](https://webchat.freenode.net/?channels=haskell-ide-engine) I got pointed at both the existing functionality of [`resCaptureFile`](https://hackage.haskell.org/package/haskell-lsp-0.22.0.0/docs/Language-Haskell-LSP-Core.html#v:resCaptureFile). I also got pointed at the [vastly improved version in a PR](https://github.com/alanz/haskell-lsp/pull/247/files), which doesn't fail if two messages race with each other. Configuring that and running it on my benchmark in the IDE told me that the number of messages involved was tiny - pretty much an initialisation and then a bunch of hovers. Coding those directly in `lsp-test` was trivial, and so [I wrote a benchmark](https://gist.github.com/ndmitchell/11467985dbf1855e62035fa97248a585#file-benchmark-hs). The essence was:

```haskell
doc <- openDoc "src/Test.hs" "haskell"
(t, _) <- duration $ replicateM_ 100 $
    getHover doc $ Position 127 43
print t
```

Open a document. Send 100 hover requests. Print the time taken.

## Profiling Ghcide

Now I could run 100 hovers, I wanted to use the GHC profiling mechanisms. Importantly, the 100 hover requests dominates the loading by a huge margin, so profiles would focus on the right thing. I ran a profile, but it was empty. Turns out the way `lsp-test` invokes the binary it is testing means it kills it too aggressively to allow GHC to write out profiling information. I changed the benchmark to send a shutdown request at the end, then sleep, and changed Ghcide to abort on a shutdown, so it could write the profiling information.

Once I had the profiling information, I was thoroughly uniformed. 10% went in file modification checking, which [could be eliminated](https://github.com/digital-asset/ghcide/issues/583). 10% seemed to go to hash table manipulations, which seemed on the high side, but not too significant (turned out I was totally wrong, read to the end!). Maybe 40% went in the Shake monad, but profiling exaggerates those costs significantly, so it's unclear what the truth is. Nothing else stood out, but earlier testing when profiling `forkIO` operations had shown they weren't counted well, so that didn't mean much.

## Prodding Ghcide

In the absence of profiling data, I started changing things and measuring the performance. I tried a bunch of things that made no difference, but some things did have an impact on the time to do 100 hovers:

* Running normally: 9.77s. The baseline.
* Switching to `forkOn`: 10.65s. Suggestive that either Ghcide has changed, or the project is different, or platform differences mean that `forkOn` isn't as advantageous.
* Using only one Shake thread: 13.65s. This change had been suggested in one ticket, but made my benchmark worse.
* Avoid spawning threads for things I think will be cheap: 7.49s. A useful trick, and maybe one that will be of benefit in future, but for such a significant change a 25% performance reduction seemed poor.
* Avoid doing any Shake invalidation: 0.31s. An absolute lower bound if Shake cheats and does nothing.

With all that, I was a bit dejected - performance investigation reveals nothing of note was not a great conclusion from a days work. I think that other changes to Ghcide to [run Shake less](https://github.com/digital-asset/ghcide/pull/554) and [cache data more](https://github.com/wz1000/ghcide/tree/hiedb) will probably make this benchmark even less important, so the conclusion worsens - performance investigation of nothing of note reveals nothing of note. How sad.

But in my benchmark I did notice something - a steadily increasing memory size in process explorer. Such issues are pretty serious in an interactive program, and [we'd fixed several issues recently](https://github.com/digital-asset/ghcide/pull/557), but clearly there were more. Time to change gears.

## Space leak detection

Using the benchmark I observed a space leak. But the program is huge, and manual code inspection usually needs a 10 line code fragment to have a change. So I started modifying the program to do less, and continued until the program did as little as it could, but still leaked space. After I fixed a space leak, I zoomed out and saw if the space leak persisted, and then had another go.

The first investigation took me into the Shake Database module. I found that if I ran the Shake script to make everything up to date, but did no actions inside, then there was a space leak. Gradually commenting out lines (over the course of several hours) eventually took me to:

```haskell
step <- pure $ case v of
    Just (_, Loaded r) -> incStep $ fromStepResult r
    _ -> Step 1
```

This code increments a step counter on each run. In normal Shake this counter is written to disk each time, which forces the value. In Ghcide we use Shake in memory, and nothing ever forced the counter. The change was simple - replace `pure` with `evaluate`. This fix has been [applied to Shake HEAD](https://github.com/ndmitchell/shake/commit/8da74bab4a2466b52f8ddc50b75a56139eecb273).

## Space leak detection 2

The next space leak took me to the Shake database `reset` function, which moves all Shake keys from `Ready` to `Loaded` when a new run starts. I determined that if you didn't run this function, the leak went away. I found a few places I should have [put strictness annotations](https://github.com/ndmitchell/shake/commit/04b0fb349a5e8ff84c073f9751bcef11b3928570), and a function that [mutated an array lazily](https://github.com/ndmitchell/shake/commit/ddf5e2d2020decc44f08c2d5482b8941c5c6d816). I reran the code, but the problem persisted. I eventually realised that if you don't call `reset` then none of the user rules run either, which was really what was fixing the problem - but I committed the improvements I'd made even though they don't fix any space leaks.

By this point I was moderately convinced that Shake wasn't to blame, so turned my attention to the user rules in Ghcide. I stubbed them out, and the leak went away, so that looked plausible. There were 8 types of rules that did meaningful work during the hover operation (things like `GetModificationTime`, `DoesFileExist`, `FilesOfInterest`). I picked a few in turn, and found they all leaked memory, so picked the simple `DoesFileExist` and looked at what it did.

For running `DoesFileExist` I wrote a very quick "bailout" version of the rule, equivalent to the "doing nothing" case, then progressively enabled more bits of the rule before bailing out, to see what caused the leak. The bailout looked like:

```haskell
Just v <- getValues state key file
let bailout = Just $ RunResult ChangedNothing old $ A v
```

I progressively enabled more and more of the rule, but even with the whole rule enabled, the leak didn't recur. At that point, I realised I'd introduced a syntax error and that all my measurements for the last hour had been using a stale binary. Oops. I span up a copy of [Ghcid](https://github.com/ndmitchell/ghcid), so I could see syntax errors more easily, and repeated the measurements. Again, the leak didn't recur. Very frustrating.

At that point I had two pieces of code, one which leaked and one which didn't, and the _only_ difference was the unused `bailout` value I'd been keeping at the top to make it easier to quickly give up half-way through the function. Strange though it seemed, the inescapable conclusion was that `getValues` must somehow be fixing the space leak.

If `getValues` fixes a leak, it is a likely guess that `setValues` is causing the leak. I modified `setValues` to also call `getValues` and the problem went away. But, after hours of staring, I couldn't figure out why. The code of `setValues` read:

```haskell
setValues state key file val = modifyVar_ state $ \vals -> do
    evaluate $ HMap.insert (file, Key key) (fmap toDyn val) vals
```

Namely, modify a strict `HashMap` from [`unordered-containers`](https://hackage.haskell.org/package/unordered-containers), forcing the result. After much trial and error I determined that a "fix" was to add:

```haskell
case HMap.lookup k res of
    Nothing -> pure ()
    Just v -> void $ evaluate v
```

It's necessary to insert into the strict `HashMap`, then do a `lookup`, then evaluate the result that comes back, or there is a space leak. I duly [raised a PR to Ghcide](https://github.com/digital-asset/ghcide/pull/586) with the unsatisfying comment:

> I'm completely lost, but I do have a fix.

It's nice to fix bugs. It's better to have some clue why a fix works.

## Space leak in `HashMap`

My only conclusion was that `HashMap` must have a space leak. I took a brief look at the code, but it was 20+ lines and nothing stood out. I wrote a benchmark that inserted billions of values at 1000 random keys, but it didn't leak space. I puzzled it over in my brain, and then about a day later inspiration struck. One of the cases was to deal with collisions in the `HashMap`. Most `HashMap`'s don't have any collisions, so a bug hiding there could survive a very long time. I wrote a benchmark with colliding keys, and lo and behold, it leaked space. Concretely, it leaked 1Gb/s, and brought my machine to its knees. The benchmark inserted three keys all with the same hash, then modified one key repeatedly. I posted the [bug to the `unordered-containers` library](https://github.com/tibbe/unordered-containers/issues/254).

I also looked at the code, figured out why the space leak was occurring, and a potential fix. However, the fix requires duplicating some code, and its likely the same bug exists in several other code paths too. The `Lazy` vs `Strict` approach of `HashMap` being dealt with as an outer layer doesn't quite work for the functions in question. I took a look at the PR queue for `unordered-containers` and saw 29 requests, with the recent few having no comments on them. That's a bad sign and suggested that spending time preparing a PR might be in vain, so I didn't.

Aside: Maintainers get busy. It's no reflection negatively on the people who have invested lots of time on this library, and I thank them for their effort! Given [1,489 packages on Hackage](https://packdeps.haskellers.com/reverse/unordered-containers) depend on it, I think it could benefit from additional bandwidth from someone.

## Hash collisions in Ghcide

While hash collisions leading to space leaks is bad, having hash collisions at all is also bad. I augmented the code in Ghcide to print out hash collisions, and saw collisions between `("Path.hs", Key GetModificationTime)` and `("Path.hs", Key DoesFileExist)`. Prodding a bit further I saw that the `Hashable` instance for `Key` only consulted its argument value, and given most key types are simple `data Foo = Foo` constructions, they all had the same hash. The solution was to mix in the type information stored by `Key`. I changed to the definition:

```haskell
hashWithSalt salt (Key key) = hashWithSalt salt (typeOf key) `xor` hashWithSalt salt key
```

Unfortunately, that now gave hash collisions with different paths at the same key. I looked into the hashing for the path part (which is really an `lsp-haskell-types` `NormalizedFilePath`) and saw that it used an optimised hashing scheme, precomputing the hash, and returning it with `hash`. I also looked at the `hashable` library and realised the authors of `lsp-haskell-types` hadn't implemented `hashWithSalt`. If you don't do that, a generic instance is constructed which deeply walks the data structure, completely defeating the `hash` optimisation. A [quick PR fixes that](https://github.com/alanz/haskell-lsp/pull/248).

I also found that for tuples, the types are combined by using the `salt` argument. Therefore, to hash the pair of path information and `Key`, the `Key` `hashWithSalt` gets called with the `hash` of the path as its salt. However, looking at the definition above, you can imagine that both `hashWithSalt` of a type and `hashWithSalt` of a key expand to something like:

```haskell
hashWithSalt salt (Key key) = salt `xor` hash (typeOf key) `xor` (salt `xor` 0)
```

Since `xor` is associative and commutative, those two `salt` values cancel out! While I wasn't seeing complete cancellation, I was seeing quite a degree of collision, so I changed to:

```haskell
hashWithSalt salt (Key key) = hashWithSalt salt (typeOf key, key)
```

With that [fix in Ghcide](https://github.com/digital-asset/ghcide/pull/588), all collisions went away, and all space leaks left with them. I had taken this implementation of hash combining from Shake, and while it's not likely to be a problem in the setting its used there, [I've fixed it in Shake too](https://github.com/ndmitchell/shake/commit/297c60fa0c6b0d4e98f61b9cdb1359a409cda901).

## Benchmarking Ghcide

With the hash collisions reduced, and the number of traversals when computing a hash reduced, I wondered what the impact was on performance. A rerun of the original benchmark showed the time had reduced to 9.10s - giving a speed up of about 5%. Not huge, but welcome.

Several days later we're left with less space leaks, more performance, and hopefully a better IDE experience for Haskell programmers. I failed in what I set out to do, but found some other bugs along the way, leading to 9 PRs/commits and 4 ongoing issues. I'd like to thank everyone in the Haskell IDE team for following along, making suggestions, confirming suspicions, and generally working as a great team. [Merging the Haskell IDE efforts](https://neilmitchell.blogspot.com/2020/01/one-haskell-ide-to-rule-them-all.html) continues to go well, both in terms of code output, and team friendliness.
