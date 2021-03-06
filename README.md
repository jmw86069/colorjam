
# colorjam <img src="man/figures/colorjam_logo.png" width="133px" height="154px" align="right" style="padding-left:10px;background-color:white;" />

The goal of colorjam is to provide useful color manipulation functions
for R data visualization.

It includes methods to generate dynamic categorical colors of arbitrary
length, providing as much visual separation between adjacent colors as
currently feasible.

## Installation

To install colorjam, use the `devtools` package installer:

> devtools::install\_github(“jmw86069/colorjam”);

Note that colorjam Depends upon the “jamba” package for some
manipulations.

## Command reference

A full command reference is provided using the `pkgdown::build_site()`
function, and is available here:

[Full command reference](https://jmw86069.github.io/colorjam)

## Quick start with colorjam

For the examples below, two packages are loaded:

``` r
library(colorjam);
library(jamba);
```

### Categorical colors

First generate some basic categorical colors, `n=5` colors. We will use
the function `jamba::showColors()` to display the colors.

``` r
showColors(rainbowJam(5));
```

![](man/figures/README-cat5-1.png)<!-- -->

To illustrate the effect of increasing categorical colors, we will
create a list of categorical colors with increasing `n`.

``` r
colorList <- lapply(nameVector(c(5, 10, 20)), function(n){
   rainbowJam(n);
});
showColors(colorList,
   main="Dichromat color wheel (default colorjam)");
```

![](man/figures/README-cat_list_dichromat-1.png)<!-- -->

By default, colorjam uses a “warped” color wheel, which produces a
dichromat (color blind friendly) color wheel.

There are several custom color wheel “presets”, for example
red-yellow-blue color wheel can be used with `preset="ryb"`.

``` r
colorList1 <- lapply(nameVector(c(5, 8, 16)), function(n){
   rainbowJam(n, preset="ryb", hue_pad_percent=100, do_hue_pad=TRUE);
});
showColors(colorList1,
   main="Red-Yellow-Blue color wheel (previous default rainbowJam)");
```

![](man/figures/README-cat_list_ryb-1.png)<!-- -->

To view Red-Green-Blue colors using the default color wheel in base R,
supply `warpHue=FALSE` as shown below, or use `preset="rgb"` to define
the red-green-blue color wheel.

``` r
colorList2 <- lapply(nameVector(c(5, 10, 20)), function(n){
   rainbowJam(n, warpHue=FALSE);
});
showColors(colorList2,
   main="Red-Green-Blue color wheel (base R)");
```

![](man/figures/README-cat_list_rgb-1.png)<!-- -->

### Color-blending (new in version 0.0.16.900)

The color wheel red-yellow-blue is particularly effective for
color-mixing operations. The function `blend_colors()` has some useful
features:

  - Paint color mixing style (blue + yellow = green)
  - Able to mix more than two colors
  - Accounts for color transparency during mixing

The argument `do_plot=TRUE` will plot a visual summary of the mixing
results.

``` r
blent1 <- blend_colors(c("red", "blue"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-1.png)<!-- -->

``` r
blent2 <- blend_colors(c("gold", "blue"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-2.png)<!-- -->

``` r
blent3 <- blend_colors(c("gold", "red"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-3.png)<!-- -->

``` r

blent4 <- blend_colors(c("gold", "deeppink4"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-4.png)<!-- -->

``` r
blent5 <- blend_colors(c("red", "green4"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-5.png)<!-- -->

``` r
blent6 <- blend_colors(c("blue", "darkorange"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-6.png)<!-- -->

``` r

blent7 <- blend_colors(c("red", "gold", "blue"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-7.png)<!-- -->

``` r

blent8 <- blend_colors(c("red1", "red3", "blue"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-8.png)<!-- -->

``` r
blent9 <- blend_colors(c("red1", "blue1", "blue4"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-9.png)<!-- -->

``` r

blent10 <- blend_colors(c("blue", "ivory"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-10.png)<!-- -->

``` r
blent10 <- blend_colors(c("red", "blue", "ivory"), do_plot=TRUE);
```

![](man/figures/README-blend_colors1-11.png)<!-- -->

### Color-splitting

Another technique to expand a color palette is to split colors into a
gradient, using the function `color2gradient()`.

This technique is useful when assigning categorical colors to a primary
group, then splitting those colors by a sub-grouping.

``` r
colorSet <- rainbowJam(5);
colorSet4 <- color2gradient(colorSet, n=4);
showColors(list(colorSet=rep(colorSet, each=4),
   colorSet4=unname(colorSet4)),
   main="Color split into 4 additional subsets.");
```

![](man/figures/README-color_split-1.png)<!-- -->

The color gradient can be tuned to increase or reduce the contrast
between the light and dark colors:

``` r
colorSet <- rainbowJam(5);
colorSet4a <- color2gradient(colorSet,
   n=4,
   gradientWtFactor=1/4);
colorSet4c <- color2gradient(colorSet,
   n=4,
   gradientWtFactor=1);
colorSet4b <- color2gradient(colorSet,
   n=4,
   gradientWtFactor=2);
showColors(list(colorSet=rep(colorSet, each=4),
   `gradientWtFactor=1/4`=unname(colorSet4a),
   `gradientWtFactor=2/3\n(default)`=unname(colorSet4),
   `gradientWtFactor=1`=unname(colorSet4c),
   `gradientWtFactor=2`=unname(colorSet4b)),
   main="Colors adjusted with gradientWtFactor");
```

![](man/figures/README-color_split_wt-1.png)<!-- -->

### Assigning colors to groups

The function `group2colors()` takes a vector of group labels, and
assigns categorical colors using `rainbowJam()`. To illustrate the
process, each group will have a different number of replicates.

I use the helper function “makeNames()” which creates unique names for
each vector item. Typically the names represent sample identifiers of
some kind. The names are retained in the output color vector, which can
be helpful to ensure data is in the correct order during a sequence of
analysis steps.

``` r
groupLabels <- rep(c("Wildtype", "Knockout", "Treated"),
   c(3, 5, 4));
names(groupLabels) <- makeNames(groupLabels);
groupColors <- group2colors(groupLabels);
print(data.frame(groupLabels, groupColors));
#>             groupLabels groupColors
#> Wildtype_v1    Wildtype     #0091FF
#> Wildtype_v2    Wildtype     #0091FF
#> Wildtype_v3    Wildtype     #0091FF
#> Knockout_v1    Knockout     #D92029
#> Knockout_v2    Knockout     #D92029
#> Knockout_v3    Knockout     #D92029
#> Knockout_v4    Knockout     #D92029
#> Knockout_v5    Knockout     #D92029
#> Treated_v1      Treated     #FFB600
#> Treated_v2      Treated     #FFB600
#> Treated_v3      Treated     #FFB600
#> Treated_v4      Treated     #FFB600
showColors(groupColors);
```

![](man/figures/README-group_colors-1.png)<!-- -->

Colors are assigned in order, after sorting with `jamba::mixedSort()`,
which provides alphanumeric sorting. However, if the input vector is a
factor, the order of factor levels is maintained.

Sometimes it is helpful to split the colors by replicate using a color
gradient, using `jamba::color2gradient()`. This technique is helpful
when trying to make replicates visually distinct in a data
visualization.

``` r
groupColorsSplit <- group2colors(groupLabels,
   useGradient=TRUE);
showColors(groupColorsSplit);
```

![](man/figures/README-group_colors_split-1.png)<!-- -->

### ggplot2 color functions

There are a couple color functions useful with ggplot2.

  - scale\_color\_jam() defines categorical colors to the ggplot2
    `colour` property.
  - scale\_fill\_jam() defines categorical colors to the ggplot2 `fill`
    property.

These functions can be used directly in ggplot2 calls:

``` r
if (suppressPackageStartupMessages(require(ggplot2))) {
   dsamp <- diamonds[sample(nrow(diamonds), 1000),];
   d <- ggplot(dsamp, aes(carat, price)) +
      geom_point(aes(colour=cut, fill=cut), size=4, shape=21);
   
   d +
      scale_color_jam() +
      scale_fill_jam() +
      ggtitle("scale_color_jam()");
}
```

![](man/figures/README-ggplot_functions-1.png)<!-- -->

Some plots use `"fill"` and `"colour"` properties, where the `"colour"`
defines an outline. In this case, the colors can be adjusted to be
lighter or darker using the `darkFactor` argument. The effect is to make
the outline color slightly darker, and the fill color slightly brighter.

``` r
if (suppressPackageStartupMessages(require(ggplot2))) {
   d +
      scale_color_jam(darkFactor=1.5) +
      scale_fill_jam(darkFactor=-1.2) +
      ggtitle("Adjustment using 'darkFactor'");
}
```

![](man/figures/README-ggplot_functions_outline-1.png)<!-- -->

### Alternate ggplot2 theme

An alternative ggplot2 theme is provided, which by default does not use
the newspaper-grey background color.

``` r
if (suppressPackageStartupMessages(require(ggplot2))) {
   d +
      scale_color_jam(darkFactor=1.5) +
      scale_fill_jam(darkFactor=-1.2) +
      ggtitle("theme_jam()") +
      theme_jam()
}
```

![](man/figures/README-ggplot2_theme-1.png)<!-- -->

Notably, this function provides some common arguments which can be
customized.

  - `base_size` = The default font size in points.
  - `blankGrid` = boolean which removes all background grid lines,
    alternatively use `blankXgrid` or `blankYgrid` for control of either
    the x-axis or y-axis, respectively.

For example, when creating figures for presentation slides or other
documents, it can be helpful to make the font substantially larger than
would be comfortable on a computer screen.

``` r
if (suppressPackageStartupMessages(require(ggplot2))) {
   d +
      scale_color_jam(darkFactor=1.5) +
      scale_fill_jam(darkFactor=-1.2) +
      ggtitle("theme_jam()") +
      theme_jam(base_size=24)
}
```

![](man/figures/README-ggplot2_theme_figure-1.png)<!-- -->

### Naming colors

A small but useful function `closestRcolor()` takes a vector of colors,
and returns a vector of the closest R color name based upon the colors
defined by `colors()`.

The name is sometimes easier to remember and re-use than the hex format.

The function has an optional argument `showPalette=TRUE` which will plot
the original colors as well as the closest R color for comparison.

``` r
closestRcolor(rainbowJam(12),
   showPalette=TRUE);
```

![](man/figures/README-color_names-1.png)<!-- -->

    #>          #D92029          #FF9C74          #FF6300          #A34F00 
    #>     "firebrick3"    "lightsalmon"         "tomato"         "sienna" 
    #>          #00D9FF          #0084E9          #0061FF          #B8C1FF 
    #>     "turquoise2"    "dodgerblue2"     "royalblue2"   "lightskyblue" 
    #>          #956FFF          #7D24FA          #FF65FF          #D43FCD 
    #> "lightslateblue"        "purple2"        "orchid1"       "magenta3"

Of course, not all hex colors have a close match in the named R colors,
but the closest color is returned nonetheless.

### Fixed color gradients (new in version 0.0.19.900, as of 08-Apr-2021)

Two new fixed gradients were added, motivated by the need for
linear/sequential and divergent color gradients that are also color
blindness friendly.

The linear/sequential gradients in `jam_linear` use a white baseline
color, to distinguish them from divergent gradients. However the same
names are used in `jam_linear` and `jam_divergent`.

``` r
jamba::showColors(jam_linear)
```

![](man/figures/README-jam_linear_1-1.png)<!-- -->

The divergent color gradients in `jam_divergent` use a black background,
to be distinguished from the linear gradients. However they use the same
names, so that they can be paired as appropriate.

``` r
jamba::showColors(jam_divergent)
```

![](man/figures/README-jam_divergent_1-1.png)<!-- -->

The motivating example is showing genome sequence coverage heatmaps from
something like a ChIP-seq experiment. The coverage data itself would use
a linear gradient. If experimental data were compared to control data,
using subtraction for example, the resulting coverage would use a
divergent gradient with the same primary name as the coverage. An
example would be `"jam_linear$firebrick"` for coverage, and
`"jam_divergent$firebrick_skyblue"` for coverage difference.

See either the `"platjam" R package (jmw86069/platjam) or the main
package it extends from Bioconductor,`“EnrichedHeatmap”`, from the
author of the amazing package`“ComplexHeatmap”\`.

### Color gradient manipulations

Two new functions provide some interesting and useful features.

`twostep_gradient()` makes an “enhanced” linear/sequential gradient by
combining the visual effects of two color gradients together. This
technique is widely used, and famously demonstrated by Dr. Brewer in the
R package `RColorBrewer` in her use of linear color gradients that also
employ a subtle hue color shift to improve visible clarity between color
steps.

Essentially the function takes two colors, produces a linear gradient
for each individually, then blends them using a gradually increasing
weight along the steps. The argument `debug=TRUE` will create a plot
showing each individual color, then the final blended result. The
example below uses `"orange2"` and `"firebrick"` and produces a much
more visibly distinctive linear gradient.

``` r
ts1 <- twostep_gradient("orange2", "firebrick", n=11, debug=TRUE)
#>       w1    w2
#> 1  1.000 0.000
#> 2  1.000 0.000
#> 3  0.838 0.162
#> 4  0.686 0.314
#> 5  0.544 0.456
#> 6  0.414 0.586
#> 7  0.296 0.704
#> 8  0.192 0.808
#> 9  0.105 0.895
#> 10 0.037 0.963
#> 11 0.000 1.000
title("orange2 + firebrick");
```

![](man/figures/README-twostep_1-1.png)<!-- -->

``` r
ts2 <- twostep_gradient("aquamarine", "dodgerblue", n=11, debug=TRUE)
#>       w1    w2
#> 1  1.000 0.000
#> 2  1.000 0.000
#> 3  0.838 0.162
#> 4  0.686 0.314
#> 5  0.544 0.456
#> 6  0.414 0.586
#> 7  0.296 0.704
#> 8  0.192 0.808
#> 9  0.105 0.895
#> 10 0.037 0.963
#> 11 0.000 1.000
title("aquamarine + dodgerblue");
```

![](man/figures/README-twostep_2-1.png)<!-- -->

`make_jam_divergent()` is the next color gradient function in the chain,
intended to combine two linear gradients into one divergent gradient. It
is useful in that it accepts a single color, the name of a known color
gradient, or a vector of colors for each “half” of the divergent color
gradient. Oh and it can use a lite or dark baseline color.

The first use case is to combine the colors from `twostep_gradient()`:

``` r
ts1ts2 <- make_jam_divergent(list(ts2=ts2),
   list(ts1=ts1),
   n=21)
jamba::showColors(ts1ts2)
```

![](man/figures/README-div_1-1.png)<!-- -->

Another example re-creates a widely used yellow-black-purple color
gradient, by first expanding each linear gradient, then combines them.

For kicks, we use single colors to create a comparable “flat” one-step
divergent gradient, which is still quite nice.

``` r
gr1d <- twostep_gradient("slateblue1", "purple", debug=TRUE, lite=FALSE)
#>       w1    w2
#> 1  1.000 0.000
#> 2  1.000 0.000
#> 3  0.838 0.162
#> 4  0.686 0.314
#> 5  0.544 0.456
#> 6  0.414 0.586
#> 7  0.296 0.704
#> 8  0.192 0.808
#> 9  0.105 0.895
#> 10 0.037 0.963
#> 11 0.000 1.000
```

![](man/figures/README-div_2-1.png)<!-- -->

``` r
gr2d <- twostep_gradient("darkorange", "gold", debug=TRUE, lite=FALSE)
#>       w1    w2
#> 1  1.000 0.000
#> 2  1.000 0.000
#> 3  0.838 0.162
#> 4  0.686 0.314
#> 5  0.544 0.456
#> 6  0.414 0.586
#> 7  0.296 0.704
#> 8  0.192 0.808
#> 9  0.105 0.895
#> 10 0.037 0.963
#> 11 0.000 1.000
```

![](man/figures/README-div_2-2.png)<!-- -->

``` r
div12d <- make_jam_divergent(list(gr1d=gr1d), list(gr2d=gr2d))

div12dflat <- make_jam_divergent("purple", "gold", lite=FALSE)
jamba::showColors(list(
   flat=div12dflat[[1]],
   twostep=div12d[[1]]))
title("Comparison of flat 'purple-black-yellow' to twostep_gradient()")
```

![](man/figures/README-div_2-3.png)<!-- -->

It becomes more clear when using a light background. Note that
`jam_make_divergent()` does a fairly good job already with just one
color on each “side”.

``` r
gr1 <- twostep_gradient("slateblue", "purple", debug=TRUE)
#>       w1    w2
#> 1  1.000 0.000
#> 2  1.000 0.000
#> 3  0.838 0.162
#> 4  0.686 0.314
#> 5  0.544 0.456
#> 6  0.414 0.586
#> 7  0.296 0.704
#> 8  0.192 0.808
#> 9  0.105 0.895
#> 10 0.037 0.963
#> 11 0.000 1.000
```

![](man/figures/README-div_3-1.png)<!-- -->

``` r
gr2 <- twostep_gradient("gold", "darkorange", debug=TRUE)
#>       w1    w2
#> 1  1.000 0.000
#> 2  1.000 0.000
#> 3  0.838 0.162
#> 4  0.686 0.314
#> 5  0.544 0.456
#> 6  0.414 0.586
#> 7  0.296 0.704
#> 8  0.192 0.808
#> 9  0.105 0.895
#> 10 0.037 0.963
#> 11 0.000 1.000
```

![](man/figures/README-div_3-2.png)<!-- -->

``` r
div12 <- make_jam_divergent(list(gr1=gr1), list(gr2=gr2))
div12flat <- make_jam_divergent("purple", "gold")
jamba::showColors(list(
   flat=div12flat[[1]],
   twostep=div12[[1]]))
title("Comparison of flat 'purple-black-yellow' to twostep_gradient(),\nwith light baseline color")
```

![](man/figures/README-div_3-3.png)<!-- -->
