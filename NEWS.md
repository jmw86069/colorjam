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

