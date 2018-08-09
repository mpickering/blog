---
title: Circular Programming in the Wild
---

Footnotes have caused a lot of implementation pain for pandoc's developers over the last few years. Several unsatisfactory solutions have made their way into the code base but finally thanks to laziness we now have an elegant and robust solution to the problem.

<!--more-->

# Footnotes

Pandoc's document model represents footnotes directly in the AST. The following simple pandoc markdown file is parsed into this representation from which it can be rendered in any of the supported output formats.

```
Here is a footnote reference,[^1] and another.[^longnote]

[^1]: Here is the footnote.

[^longnote]: Here's one with multiple blocks.

> pandoc -f markdown -t native

```

The problem is that upon reading the footnote marker (`[^1]`) we need to fill in something which might appear later in the source file.

# Circularity

Previous attempts had explictly returned a function



