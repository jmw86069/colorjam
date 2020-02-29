---
output: github_document
---


# colorjam <img src="man/figures/colorjam_logo.png" width="133px" height="154px" align="right" style="padding-left:10px;background-color:white;" />

The goal of colorjam is to provide useful color manipulation functions
for R data visualization.

It includes methods to generate dynamic categorical colors of
arbitrary length, providing as much visual separation between
adjacent colors as currently feasible.

## Installation

To install colorjam, use the `devtools` package installer:

> devtools::install_github("jmw86069/colorjam");

Note that colorjam Depends upon the "jamba" package for some
manipulations.

## Command reference

A full command reference is provided using the `pkgdown::build_site()`
function, and is available here:

[Full command reference](https://jmw86069.github.io/colorjam)

## Quick start with colorjam

For the examples below, two packages are loaded:


```r
library(colorjam);
library(jamba);
```

### Categorical colors

First generate some basic categorical colors, `n=5` colors. We
will use the function `jamba::showColors()` to display the colors.


```r
showColors(rainbowJam(5));
```

![plot of chunk cat5](figure/cat5-1.png)

To illustrate the effect of increasing categorical colors,
we will create a list of categorical colors with increasing `n`.


```r
colorList <- lapply(nameVector(c(5, 10, 20)), function(n){
   rainbowJam(n);
});
showColors(colorList,
   main="Red-Yellow-Blue color wheel (default colorjam)");
```

![plot of chunk cat_list_ryb](figure/cat_list_ryb-1.png)

By default, colorjam uses a "warped" color wheel, which produces
Red-Yellow-Blue equally spaced. The effect is color additive,
which is beneficial when blending colors as well as selecting
categorical colors.

To view Red-Green-Blue colors using the default color wheel
in base R, supply `warpHue=FALSE` as shown below:


```r
colorList2 <- lapply(nameVector(c(5, 10, 20)), function(n){
   rainbowJam(n, warpHue=FALSE);
});
showColors(colorList2,
   main="Red-Green-Blue color wheel (base R)");
```

![plot of chunk cat_list_rgb](figure/cat_list_rgb-1.png)

### Color-splitting

Another technique to expand a color palette is to split colors
into a gradient, using the function `color2gradient()`.

This technique is useful when assigning categorical colors to a primary
group, then splitting those colors by a sub-grouping.


```r
colorSet <- rainbowJam(5);
colorSet4 <- color2gradient(colorSet, n=4);
showColors(list(colorSet=rep(colorSet, each=4),
   colorSet4=unname(colorSet4)),
   main="Color split into 4 additional subsets.");
```

![plot of chunk color_split](figure/color_split-1.png)

The color gradient can be tuned to increase or reduce the contrast
between the light and dark colors:


```r
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

![plot of chunk color_split_wt](figure/color_split_wt-1.png)

### Assigning colors to groups

The function `group2colors()` takes a vector of group labels,
and assigns categorical colors using `rainbowJam()`.
To illustrate the process, each group will have a different
number of replicates.

I use the helper function "makeNames()" which creates unique
names for each vector item. Typically the names represent sample
identifiers of some kind. The names are retained in the output
color vector, which can be helpful to ensure data is in the correct
order during a sequence of analysis steps.


```r
groupLabels <- rep(c("Wildtype", "Knockout", "Treated"),
   c(3, 5, 4));
names(groupLabels) <- makeNames(groupLabels);
groupColors <- group2colors(groupLabels);
print(data.frame(groupLabels, groupColors));
#>             groupLabels groupColors
#> Wildtype_v1    Wildtype     #FFE200
#> Wildtype_v2    Wildtype     #FFE200
#> Wildtype_v3    Wildtype     #FFE200
#> Knockout_v1    Knockout     #D0445E
#> Knockout_v2    Knockout     #D0445E
#> Knockout_v3    Knockout     #D0445E
#> Knockout_v4    Knockout     #D0445E
#> Knockout_v5    Knockout     #D0445E
#> Treated_v1      Treated     #FD8600
#> Treated_v2      Treated     #FD8600
#> Treated_v3      Treated     #FD8600
#> Treated_v4      Treated     #FD8600
showColors(groupColors);
```

![plot of chunk group_colors](figure/group_colors-1.png)

Colors are assigned in order, after sorting with `jamba::mixedSort()`,
which provides alphanumeric sorting. However, if the input vector is
a factor, the order of factor levels is maintained.

Sometimes it is helpful to split the colors by replicate using
a color gradient, using `jamba::color2gradient()`. This technique is
helpful when trying to make replicates visually distinct in a
data visualization.



```r
groupColorsSplit <- group2colors(groupLabels,
   useGradient=TRUE);
showColors(groupColorsSplit);
```

![plot of chunk group_colors_split](figure/group_colors_split-1.png)

### ggplot2 color functions

There are a couple color functions useful with ggplot2.

* scale_color_jam() defines categorical colors to the ggplot2 `colour`
property.
* scale_fill_jam() defines categorical colors to the ggplot2 `fill`
property.

These functions can be used directly in ggplot2 calls:


```r
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

![plot of chunk ggplot_functions](figure/ggplot_functions-1.png)

Some plots use `"fill"` and `"colour"` properties, where the `"colour"`
defines an outline. In this case, the colors can be adjusted to be
lighter or darker using the `darkFactor` argument. The effect is to
make the outline color slightly darker, and the fill color slightly
brighter.


```r
if (suppressPackageStartupMessages(require(ggplot2))) {
   d +
      scale_color_jam(darkFactor=1.5) +
      scale_fill_jam(darkFactor=-1.2) +
      ggtitle("Adjustment using 'darkFactor'");
}
```

![plot of chunk ggplot_functions_outline](figure/ggplot_functions_outline-1.png)

### Alternate ggplot2 theme

An alternative ggplot2 theme is provided, which by default does
not use the newspaper-grey background color.


```r
if (suppressPackageStartupMessages(require(ggplot2))) {
   d +
      scale_color_jam(darkFactor=1.5) +
      scale_fill_jam(darkFactor=-1.2) +
      ggtitle("theme_jam()") +
      theme_jam()
}
```

![plot of chunk ggplot2_theme](figure/ggplot2_theme-1.png)

Notably, this function provides some common arguments which can
be customized.

* `base_size` = The default font size in points.
* `blankGrid` = boolean which removes all background grid lines,
alternatively use `blankXgrid` or `blankYgrid` for control of
either the x-axis or y-axis, respectively.

For example, when creating figures for presentation slides or
other documents, it can be helpful to make the font substantially
larger than would be comfortable on a computer screen.


```r
if (suppressPackageStartupMessages(require(ggplot2))) {
   d +
      scale_color_jam(darkFactor=1.5) +
      scale_fill_jam(darkFactor=-1.2) +
      ggtitle("theme_jam()") +
      theme_jam(base_size=24)
}
```

![plot of chunk ggplot2_theme_figure](figure/ggplot2_theme_figure-1.png)

### Naming colors

A small but useful function `closestRcolor()` takes a vector of colors,
and returns a vector of the closest R color name based upon the
colors defined by `colors()`.

The name is sometimes easier to remember and re-use than the hex format.

The function has an optional argument `showPalette=TRUE` which will
plot the original colors as well as the closest R color for comparison.


```r
closestRcolor(rainbowJam(12),
   showPalette=TRUE);
```

![plot of chunk color_names](figure/color_names-1.png)

```
#>               1               2               3               4 
#>    "violetred3"    "indianred1"   "lightsalmon"       "tomato2" 
#>               5               6               7               8 
#>        "orange"       "orange3"    "olivedrab4"     "seagreen3" 
#>               9              10              11              12 
#>          "cyan"    "dodgerblue"    "steelblue1" "mediumpurple1"
```

Of course, not all hex colors have a close match in the named R colors,
but the closest color is returned nonetheless.

