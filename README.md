# R-svg-interpreter

- Visualize an SVG file with base R graphics
- Written with [Cursor](https://cursor.com/home)
- Tested with `Rlogo.svg`
- Gradients not working

*Thanks to Barry Rowlingson for an [R-help message](https://stat.ethz.ch/pipermail/r-help/2025-June/481075.html) about drawing an SVG image in R. I used [that message as context](notes.md) for the chat with Cursor.*

## Usage

```r
source("svg_interpreter.R")
savePlot("Rlogo.png")
```

<div align="center">
  <img src="Rlogo.png" alt="R logo plotted in R" style="width:50%;"/>
</div>

## Chat history

- [Chat 1](chat1.md)

## Licenses

The code in this repository was authored by Jeffrey Dick with AI assistance and is distributed under the MIT license.

Text attributed to Barry Rowlingson is taken from the [R-help mailing list](https://stat.ethz.ch/mailman/listinfo/r-help). The [R posting guide](https://www.r-project.org/posting-guide.html) says:

> Posters should be aware that the R lists are public discussion lists and anything you post will be archived and accessible via several websites for many years.

The [R logo](https://www.r-project.org/logo/) carries this text:

> The R logo is © 2016 The R Foundation.
> You can distribute the logo under the terms of the Creative Commons Attribution-ShareAlike 4.0 International license (CC-BY-SA 4.0) or (at your option) the GNU General Public License version 2 (GPL‑2).
