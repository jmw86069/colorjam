# colorjam version 0.0.16.900

## new functions

* `blend_colors()` performs paint color mixing, very close
to subtractive color blending with some modifications to
account for red-yellow-blue color wheel. It can mix more
than two colors, and accounts for color transparency.
This function currently performs better than any other
color mixing function I am aware of in R -- judged by
the tendency to return what is "expected" in more
cases than not.
* `mean_angle()` takes a vector of angles in degrees,
optionally with weights, and returns the average angle
along with the new radius. Internally it takes the
average unit vector (scaled by weights if supplied).

## changes to existing functions

* `rainbowJam()` was modified to clean up the internal workflow.
Specifically, the argument `preset` is more prominent,
making it easy to call `rainbowJam(5, preset="ryb")` for
example. The hue padding was also modified to reduce most
cases to zero padding -- this padding added separation
between the first and last color hues in a sequence,
to prevent them from being too similar when the `Cvals`
and `Lvals` sequence was not optimal.
* `rainbowJam()` new argument `phase` allows shifting
the `Cvals`,`Lvals` sequence by steps, or to reverse the
sequence, in order to create more varied color sets.
* `h2hw()` and `hw2h()` functions have new argument
`preset` which calls `h2hwOptions()` and uses the appropriate
color wheel. This change makes it easier to convert
color hues with `h2hw(60, preset="ryb")` to convert
default RGB yellow (hue=60) to RYB yellow (hue=120).
* `closestRcolor()` argument `preset` defaults to `ryb`,
to avoid using `dichromat` for closest-color calculations.

# colorjam version 0.0.15.900

## changes to existing functions

* `rainbowJam()` arguments `Lvals` and `Cvals` were
manually adjusted based upon initial usage and feedback.
* `h2hwOptions()` was refactored to have a cleaner workflow.
It explicitly defines `getOptions()` in default argument
values, which can be replace by defining `preset`. There
is new argument `default_preset` so the first time
this function is called, it knows which preset to use
for initial values.

## bug fixes

* `group2colors()` fixed a longstanding bug where
input values that contain `""` would return `NA`
with `NA` name. This occurs from
referencing a vector using name `""` which R forces
to return `NA`. Instead `group2colors()` now uses
`match(x, names(colors))` which works properly.

# colorjam version 0.0.14.900

## more changes to rainbowJam()

There will almost certainly be more changes, after
using the updated `rainbowJam()` for a while and
experiencing the cascade effects. For now, I had
to make some changes, to force me to continue making
more changes as needed.

After using the updated `rainbowJam()` for a few months,
a few things became clear:

* Red-orange-yellow as the first three colors, is not terrible,
but has substantial problems when trying to split
into light-dark variations. The dark-yellow and light-orange
were nearly identical. (Also R is not great at keeping the hue
for orange consistent when adjusting luminance. Nobody will
hear this feedback, that's okay.)
* The use of green and variations of green, are problematic
with color-blind viewers, making the whole function
not ideal, if a substantial user base doesn't see beautiful
colors at the end.
* Ultimately there were too many cases where I would
call `rainbowJam(n + 20)` and hand-pick colors from the set.
That is almost exactly the problem I was originally trying
to solve with `rainbowJam()`, which means the function
was failing.
* Last is a small thing, `rainbowJam()` padded the end of
the hue sequence to avoid having similar first and last color,
with identical C,L values. When that happened, it added an
aggressive hue pad so the colors would still differ. Long
story short, there weren't enough beautiful purples and pinks.

Overall changes:

* `h2hwOptions()` defines custom color wheels, by adjusting
the hue from rgb to any non-linear sequence. It has new argument
`preset` with some named shortcut options: `"rgb"` the R default;
`"ryb"` the previous red-yellow-blue, which still includes green;
`"dichromat"` new option that removes green altogether, and spaces
the remaining hues based upon my perceived consistent visible
distinctiveness between steps. I tried to adjust for effects
simulated by `dichromat::dichromat()` for the three types
it provides.
* `h2hwOptions()` new default is `preset="dichromat"`! Substantial
change, no more green colors. To change back call `h2hwOptions(preset="ryb")`
or `h2hwOptions(preset="rgb")`. *shudder*
* `rainbowJam()` argument `Cvals` was uniformly increased by 30. It
turns out the conversion from HCL already handles values too high,
by favoring luminance over chroma -- meaning when we request a certain
brightness, we get that brighness even at the expense of lower
chroma (saturation.) Thus most of the work is done by the `Lvals`
luminance, and the chroma should generally be as high as feasible
with few adjustments.
* `rainbowJam()` by default does not pad the last hue color. Instead
the `preset="dichromat"` default in `h2hwOptions()` adjusts the
hues from 300 to 360 (which were almost nearly identical pink anyway)
to take up less of the color wheel.
* `rainbowJam()` flipped the 2nd and 3rd values in `Lvals`. Even when
calling `rainbowJam(4, preset="ryb2")`, it won't give the same results
as before this update, without also changing the 2nd and 3rd values in
`Lvals`. I realize, only I care.


# colorjam version 0.0.13.900

## Substantial changes to rainbowJam()

* `rainbowJam()` was refactored to address longstanding
critique that the colors were too muddy. New logic
is present in `rainbowJam()` and the previous function
is available with `rainbowJam_v1()` for backward
compatibility. The new colors are much brighter and
will need testing over time to evaluate the effects on
downstream uses. That said, the colors are so much
improved, it seems reasonable to replace the previous
function for now.

## Other changes

* The R files were slightly refactored to split functions into
different files.

## New function

* `color_pie()` is a simple function to display colors in
pie chart form, which is helpful to assess the first and
last color in a rainbow color ramp. In fact, `color_pie()`
might be moved into `jamba::showColors()` as an optional
output format. Current challenge is how to label each ring,
when the input is a list of color vectors.

# colorjam version 0.0.12.900

## changes

* `theme_jam()` was updated to make the major and minor grid
lines a lighter shade of gray, so they have less interference
with text labels on a plot panel.

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

