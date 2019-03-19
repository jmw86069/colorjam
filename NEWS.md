# colorjam version 0.0.11.900

## changes

* `scale_fill_jam()`, `scale_color_jam()`, and `jam_pal()` have
an argument `alpha` to control alpha transparency, on a scale of
0 (transparent) to 1 (non-transparent.)

# colorjam version 0.0.10.900

## changes

* `closestRcolor()` was updated to handle new behavior from `col2hcl()`
which by default does not assign names to unnamed vectors.
* `closestRcolor()` now has `method` argument to define the distance
method (see `stats::dist()`); added optional "LUV" color model. In some
cases "LUV" has greater sensitivity albeit with less accuracy. In other
words for large color vectors, "LUV" may produce the most non-repeated
colors at cost of accuracy, while "hcl" tends to be more accurate but
sometimes snaps two similar colors to the same closest R color.
Future work should probably handle greyscale colors separately.

# colorjam version 0.0.9.900

## changes

* `vals2colorLevels()` was updated to handle edge cases where input
values had no numeric range.

# colorjam version 0.0.8.900

## new functions

* `vals2colorLevels()` converts a numeric vector into a color gradient,
optionally divergent around a baseline, optionally applying the color
warp using `jamba::warpRamp()` and a `lens` adjustment value.

# colorjam version 0.0.7.900

## changes

* Added more function prefixes "jamba::" to avoid handle the inconsistent
import implementation in R.

## new functions

* `matrix2heatColors()` to apply color gradient to each column in a matrix,
where each column has its own color scale.

# colorjam version 0.0.5.900

## additions

* A README.Rmd file was created with a basic walkthrough for colorjam.
* DESCRIPTION was changed to move "jamba" to the "Depends:" field.
Guidance from R Package Development and R central docs is cryptic on
this point.

# colorjam version 0.0.4.900

## new functions

* `theme_jam()` which provides a Jam-specific ggplot2 default.
* `scale_color_jam()`, `scale_fill_jam()`, and `jam_pal()` provide
categorical colors and fills, respectively, using `rainbowJam()`.

## bug fixes and enhancements

* Updated DESCRIPTION to include proper "Remotes" entry pointing
to the Github jamba package.
* Updated `closestRcolor()` to fix a small bug with name handling
of the output.

# colorjam version 0.0.3.900

## new functions

* `group2colors()` takes a vector of group labels and assigns
categorical colors, by default using `rainbowJam()` but which
can be substituted with other color functions as needed. It
maintains order of factor levels, otherwise uses `jamba::mixedSort()`
to order unique labels before assigning colors.
* `closestRcolor()` finds the closest named R color from
`colors()` and returns that name. It can also be given a custom
color vector, and will return the closest color for each color
in the input list.
* `rainbowJam()` is the key categorical color function for
the JAM package suite. It uses Red-Yellow-Blue color wheel,
and uses a pattern of alternating Chroma (color saturation) and
Luminance (visible brightness) to maximize the difference between
adjacent colors.

