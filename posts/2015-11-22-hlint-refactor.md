---
title: Automatically Apply HLint Suggestions
---

Users upgrading to [hlint 1.9.23](https://hackage.haskell.org/package/hlint-1.9.23)
will now be able to take advantage of the new
`--refactor` flag which by invoking the `refactor` executable supplied by
[`apply-refact`](https://hackage.haskell.org/package/apply-refact-0.1.0.0)
can automatically apply suggestions.

![example](http://i.imgur.com/5hiEZJo.gif)\

<!--more-->

# Getting Started

```
cabal install hlint
cabal install apply-refact
hlint --refactor myfile.hs
```

To take advantage of this flag users also need to install the `refactor`
executable which is provided by `apply-refact`. Users can directly invoke
`refactor` if they wish but it is easier to invoke it via hlint, passing any
extra options with the `--refactor-options` flag.

HLint will by default apply all suggestions without any prompts to the user and
output the result on stdout.

![example2](http://i.imgur.com/cmxQJiI.gif)\

If you're feeling cautious, the `-s` flag will prompt you before applying each
refactoring.

![sexample](http://i.imgur.com/pxygqXI.gif)\

If you're feeling brave, the `-i` flag will perform the refactoring in place.

![iexample](http://i.imgur.com/QJOGoHL.gif?1)\

The final option which is useful for tool writers is the `--pos` flag which
specifies the region to which hints should be applied to.

![posexample](http://i.imgur.com/vOmNwq0.gif?1)\


# Editor Integration

There are plugins availible for vim, emacs and atom. Moritz Kiefer has already
helped me with the emacs plugin, pull requests to clean up the other two plugins
would be greatly appreciated.

* [`hlint-refactor-vim`](https://github.com/mpickering/hlint-refactor-vim)
* [`hlint-refactor-atom`](https://github.com/mpickering/hlint-refactor-atom)
* [`hlint-refactor-mode`](https://github.com/mpickering/hlint-refactor-mode)

# Reporting Bugs

If you find that the program gives you an unexpected result then please report
a bug to the [issue tracker](https://github.com/mpickering/apply-refact).
Please also include the output of `hlint --serialise` and the input
file.

There are a few known problems with CPP and a few corner cases which are not
handled by the underlying library but for most users,
a pain-free experience is expected.

## Known Issues

* Tabs and trailing whitespace will be removed (this is by design and not going
to change). Line endings are changed to unix line endings.

* Not all hlint suggestions are supported. A notable exception being the
eta reduction refactoring as it proved quite difficult to implement correctly.

* Sometimes hlint suggests a refactoring which uses a name which isn't in scope.
The tool is quite dumb! It just takes whatever hlint suggests and applies it
blindly. It might be good to add an option to hlint to avoid spitting out
hints which do this.

