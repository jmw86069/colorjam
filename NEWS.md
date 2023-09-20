# colorjam 0.0.26.900

* `.onLoad()` to add default `options(colorjam.preset="dichromat2")`,
and which can be customized.
* README.Rmd was rewritten to match recent updates.

## changes to existing functions

* `sort_colors()`

    * argument `byCols=NULL` changed to `byCols=c("H", "C", "L")` so
    the default behavior will sort colors by hue, previously the
    default behavior was not to sort colors, which seemed counter
    to the default purpose of the function.

* new preset `"hcl_to_hsl"` intended for internal use to convert HCL hue
to HSL hue.
* `launchColorjamShiny()`

    * now hides presets: `"none"`, `"hcl_to_hsl"`

## new functions

* `remap_colorjam_preset()`

    * experimental, intended to remap colors from one preset to another
    * it works fairly well, but honestly it may have marginal utility

* `hcl_to_hsl_hue()`,`hsl_to_hcl_hue()`

    * conversion to the hue in HCL and HSL color spaces.

# colorjam 0.0.25.900

Added MIT license and copyright.

## general updates

* `dichromat` added as package dependency. Respect.
* `cli` was added as a package dependency (in v24 actually),
to be consistent with recommended R package messaging.
* Each `preset` now includes `direction` and `default_step`, optional
attribute `"description"` as a label.

    * after testing it soon became clear that each `preset` is designed with
    a `step` in mind, so the appropriate `default_step` should be stored
    * also the `direction` was previously deduced by the `h1`,`h2` values,
    however this too is unnecessary and should be encoded in the `preset`
    to avoid errors.

* For my own benefit, I made a simple R-shiny app:

    * visualize colors from `rainbowJam()`
    * adjust the number of colors `n`, `preset`, `step`
    * plotly to adjust the `preset` control points, creating a new color wheel
    * subset specific colors for direct visual comparison

* I used the R-shiny app to create and optimize `dichromat2`:

    * starts at gold
    * roughly evenly distributes warm/cool colors
    around two halves of the color wheel
    * roughly evenly distributes color-blindness sensitive colors
    around two halves of the color wheel, for "deutan", "protan", and "tritan".
    It isn't perfect, but should enhance the visual distinction in adjacent
    colors, while being fairly scalable to large `n`.

## new data

* `named_colors`

    * Superset of 4883 hexadecimal colors with corresponding color names:

        * 4447 colors from Github `"meodai/color-names"` repository;
        * 436 colors from `grDevices::colors()`, which were not already
        defined by Meodai colors.

    * Colors are intended to improve labeling `rainbowJam()` colors.
    * Color labels from `closest_named_color()` could become a QC step
    to confirm that `rainbowJam()` creates colors that can be assigned
    to different named colors.
    * In testing, `grDevices::colors()` did not provide sufficient detail,
    specific examples frequently included `"cornflowerblue"` and `"steelblue"`
    despite appearing for colors with visibly distinctive blue/purple
    color hues.

## new functions

* `launchColorjamShiny()`

    * simple R-shiny app to display categorical colors
    * selection of `'`, `preset`, `step`, `phase`, `subset`
    * plotly interactive plot of `h1` and `h2` values, which can be edited!
    Is it not perfect, need to override plotly "snap" behavior,
    which seems to be hard-coded.
    * optional `dichromat::dichromat()` adjustment to simulate color-blindness
    * `colorjamShinyServer()`,`colorjamShinyUI()` are internal functions
    to provide `server` and `ui` components to `shiny::runApp()`.

* `closest_named_color()`

    * simple wrapper to `closestRcolor()` except that it uses the 4883
    `named_colors` instead of R `grDevices::colors()`, although all
    colors are contained in `named_colors`.

* `validate_colorjam_preset()`

    * function to adjust raw `h1`,`h2` values, wrap within range `c(0, 360)`
    * breaks ties in a way that maintains proper sort order for `direction=1`
    and `direction=-1`
    * Todo: impose edits to `h1`,`h2` when out of order. E.g. is one point
    is adjusted past the next point, the next point should be shifted.

* `plot_colorjam_preset()`

    * visual plotting of `h1`,`h2` values, showing the resulting color
    along each respective axis.
    * option for base R plot, or plotly with editable control points
    (the editing is only functional inside an R-shiny app).

* `vibrant_color_by_hue()`

    * external convenience function that takes an HCL color hue
    and returns the most saturated HSL color.
    * internally it converts HCL to hex, hex to HSL - because HSL hue
    differs from HCL hue. Then using HSL hue it uses S=100, L=50
    to obtain the most saturated color.
    * it still shows color hue "drift" since HCL hue to HSL hue is not
    a linear relationship for different CL or SL values, but it's much
    better than using HCL and letting it randomly determine what hue
    is the closest match to the requested HCL values.

## changes to existing functions

* `rainbowJam()`

    * new argument option `nameStyle="closest_named_color"` which uses
    the 4883 `named_colors` for labeling.
    * `nameStyle="hcl"` now calculates actual HCL values and not the 
    input values to `hcl()`, because those values may change upon
    creating a color in gamut.
    * `h1`,`h2` are deprecated, instead use `preset`
    * `Cvals`,`Lvals` are deprecated, instead use `step`
    * `phase` can take one or more values, to select and order specific
    items in the given `step`

* `colorjam_presets()`,`add_colorjam_preset()`

    * Each `preset` now also has `direction` and `default_step`.

* `h2hw()`,`hw2h()`

    * now calls `colorjam_presets()`

* `approx_degrees()`

    * now accepts `preset` input
    * handles (and requires) `direction` instead of determining by `h1`,`h2`
    * calls `validate_colorjam_preset()` to handle tie-breaks

* `closestRcolor()`

    * names are assigned when `colorSet` has names, helpful with the
    reference colors are hex, and names are user-friendly labels.


# colorjam 0.0.24.900

This version of colorjam turned into a bit of a refactor:

* Color wheels are now stored as "presets":

    * presets are stored within the colorjam environment
    * preset names are given by `colorjam_presets()`
    * preset values are accessed by `colorjam_presets("dichromat")`
    * new presets are registered with `add_colorjam_preset()`
    * the preset can be defined with `options("colorjam.preset")`

* Rainbow colors are adjusted by Chroma/Luminance "steps":

    * "steps" are stored in the colorjam environment
    * Each step is a `list` with `numeric` vectors named `"C"` and `"L"`,
    each with values within the range: `c(0, 100)`.
    * steps are accessed with `colorjam_steps()`, `colorjam_steps("v24")`
    * new steps are registered with `add_colorjam_step()`
    * the steps can be defined with `options("colorjam.step")`
    * Background:
    
        * `rainbowJam()` used arguments `Cvals` and `Lvals` to apply
        a sequence of luminance/chroma values to a series of color hues.
        The variation in C and L values was intended to maximize
        visual distinctiveness of consecutive colors.
        In fact, this process was the inspiration for `colorjam`,
        to make rainbow catgorical colors more visually distinctive
        than comparable functions that notably intend to produce colors
        as similar as possible. Different motivations.
        * `colorspace::rainbow_hcl(n, c, l)` accepts only one value per
        `c` and `l` argument
        * `grDevices::rainbow()` uses the non-uniform `hsv` color space,
        and accepts multiple values for arguments `s` and `v`.
        (To be fair, the `hcl.colors()`

* Altogether `rainbowJam()` defines categorical colors with two core elements:

    * `"preset"`: color wheel with starting color, to define the color hues.
    * `"steps"`: series of Chroma/Luminance values to apply to color hues.

* Starting color:

    * The RGB color wheel classically "starts with" red.
    * It could be because the first color in the visible spectra
    ("ROY G. BIV": red, orange, yellow, green, blue, indigo, violet) is red.
    However, in a color "wheel" the colors are rolled back on themselves,
    so any color can be placed at the top. Red is traditional.
    * After very heavy usage of this package, it became clear that assigning
    categorical colors to statistical designs was not ideal.
    Specifically, we standardized experiment designs so that "the control"
    for each experiment factor is the first value in each column.
    
        * Some examples: `c("Control", "Dexamethasone", "Etoposide")`;
        `c("Wildtype", "ADAM19_Knockout")`;
        `c("Time0", "Time1", "Time2", "Time3")`
        * The first group was always colored bright red;
        subsequent groups were assigned orange, yellow, blue, purple.
        * Dr. Ayland Letsinger asked why we didn't start with yellow
        instead of red, so the control group would have a neutral color?
        Brilliant.

    * The new presets include:
    
        * `"dichromat2"` which starts with gold, proceeds in reverse to
        red, purple, blue. (There is no green in the dichromat color wheel.)
        * `"ybr"` derived by `"ryb"` except it starts with yellow,
        proceeds "forward" to green, blue, purple, then red.

* Added `cli` package dependency for improved messaging, particularly for
deprecated function arguments.

## Bug fixes

* Added missing package prefix with internal references to `jam_linear`
and `jam_divergent` data objects.

## changes to existing functions

* `rainbowJam()`

    * argument `preset` is the primary definition of color wheel
    * arguments `Cvals`,`Lvals`,`Crange`,`Lrange` are deprecated
    (frankly, who is affected though, only me)
    * arguments `h1`,`h2` are deprecated and ignored in favor of `preset`.
    To use custom `h1`,`h2` they must be added to new preset then referred
    by name.
    * new argument `step` defines the sequence of `C` and `L` values
    
        * steps from specific previous versions of colorjam are available:
        `"v19"`, `"v20"`, `"v23"`, and `"v24"`.
        * steps were adjusted to minimize out of bounds colors, which
        `colorspace` and `farver` handle by adjusting other color channels;
        sometimes substantially changing the output color hue.
        This change sometimes caused the color hue sequence to reverse itself
        to accommodate the requested `C` and `L` values, and was the driving
        use case for the refactored arguments `preset` and `steps`.

* `color_pie()`

    * text labels are rotated parallel to each pie wedge
    * text labels use `jamba::shadowText()` and `jamba::setTextContrastColor()`
    * default radius is now 1.1, to fit most plot devices.

* `closestRcolor()`

    * New argument `Cgrey` to recognize when input `x` colors are greyscale,
    in which case they are matched with colors from `colorSet` which are
    below `C_min` Chroma saturation. By default, color matching is much
    improved when using a mix of saturated and unsaturated colors.

* `scale_color_jam()`,`scale_fill_jam()`,`jam_pal()`

    * ggplot2-related functions dropped arguments `h1`,`h2` (before they
    could be used), and added argument `step`.
    * default `preset="dichromat2` consistent with `rainbowJam()`.

* `h2hw()`,`hw2h()`,`h2hwOptions()`

    * new argument default `preset=getOption("colorjam.preset", "custom")`
    behaves as previous `preset="custom"` except that it now honors
    when the option `"colorjam.preset"` is defined.
    * The only two exceptions to `preset` are with `blend_colors()` and
    `closestRcolor()` which both default to `"ryb"` because those specific
    use cases favor using the red-yellow-blue color wheel.

* `approx_degrees()`

    * refactored to improve handling of discontinuous angles,
    for example when the mapped color wheel wraps above 360 degrees,
    or below 0 degrees.

* `make_jam_divergent()`

    * New behavior: When `linear2` is not supplied, it calls
    `color_complement()` to determine a reasonable opposing color,
    so one can call `make_jam_divergent("red")` and it will provide
    a reasonable

## new functions

* `adjust_hue_warp()`

    * helper function to rotate or reverse a color wheel by way of
    the color hue warp. Maybe eventually the colors can start at gold/yellow
    and continue either to red or blue, so that the first assigned
    color will be more "neutral" (yellow) since the first group
    in a statistical comparison is typically the control.
    People have suggested the default color for a control group not
    be brightest red.

* `colorjam_preset()`,`add_colorjam_preset()`

    * manages recognized colorjam color hue presets, which allows for
    custom preset names without modifying the package code.
    * intended to register (or delete) colorjam named presets, which
    are custom color wheels.

* `colorjam_step()`,`add_colorjam_step()`

    * A "step" is a sequence of Luminance/Chroma values intended to
    maximize visual distinctiveness of consecutive colors.
    * The purpose of storing steps in the colorjam environment is to
    allow a convenient way to select a step sequence rather than by
    defining individual C and L values during the function call.
    * Specifically, the steps are driven by the change to start colors
    near yellow, rather than starting at red. For color sequences starting
    at red, early colors should generally be darker; for sequences starting
    at yellow or gold, the early colors should generally be lighter.

* `h2hwOptions()`

    * now calls `colorjam_presets()` instead of using internal values.


# colorjam 0.0.23.900

* bumped dependency `jamba (>= 0.0.83.900)` to include `hsl2col()`, `col2hsl()`

## changes to existing functions

* `subset_colors()` now calls `colors_to_df()` to create a `data.frame`,
any of whose columns can be used to sort colors.
* `theme_jam()` changes:

   * new argument `strip.text.size` with default `ggplot2::rel(0.8)`
   to help adjust facet panel strip text font size directly.
   * new argument `panel.border` to make adjustments convenient,
   by default panels now have thin dark gray outline.
   Set `panel.border=NULL` to hide the border.

* `scale_color_jam()`, `scale_fill_jam()`, `jam_pal()`:

   * new argument `darken` is `logical`, when `darken=TRUE` it automatically
   sets `darkFactor` and `sFactor` to reasonable values for subtle darkening.
   This option is convenient for slightly darker outlines around points.

## new functions

* `colors_to_df()` convert vector of colors to `data.frame` with column
values representing several common colorspace formats. Note that
HSL color dimensions have prefix `"hsl_"`, for example `"hsl_h"`,
`"hsl_s"`, and `"hsl_l"`.

   * For some reason, the H hue values in HCL
    do not match the hue values in HSL, despite both being angles with
    range `c(0, 360)`. Fun.
    * It is a non-linear conversion of hue which makes sense,
    it appears that HSV, HSL, and RGB share the same "color wheel",
    derived from the red-green-blue maxima used for computer monitors.
    HCL added an offset +12.2 degrees to every color, probably so color
    sorting would always have variations of red appearing first, near zero,
    avoiding some colors being -0.1 and sorting last with hue 359.9.
    * Surprising that the hue sort order is not shared between HCL and HSL,
    although it is probably caused by conversion of low-chroma/saturation
    colors, with underlying numeric rounding errors. It could be that
    HCL is based upon perception, giving slightly stronger weight to the
    perceptiveness of certain color components.

* `sort_colors()` applied sort criteria to data returned by `colors_to_df()`.

## HSL color update (preparation)

I anticipate the `rainbowJam()` function will shift from using HCL,
instead to using HSL for color selection.

Summary of thought process so far:

* HCL colors "out of gamut" are capped by each color channel, causing
the hue returned to differ from the hue requested in HCL color space.
* HCL has the benefit that in general, the C and L values can be used
to determine perceptive color difference between colors, allowing one
to set a minimum perceived color difference threshold. However,
every color hue has a different color gamut of allowable C and L values,
making it difficult to apply consistent rules to each color hue.
* HSL has the benefit that all "full saturation colors" for each hue has
the same coordinate (S=100, L=50), however there is no guarantee that
surrounding colors have consistent perceptual difference.
* `rainbowJam()` uses "tricked out" C and L sequences, that request
higher C chroma than possible for most hues, causing it to return
higher saturation than typical. Meanwhile the L values help create
6 distinct colors for every hue.
* The downside is the hue is often
different than requested, causing problems with the `n` is high,
sometimes a series of colors have nearly identical hue due to
the conversion issues described above.
* Also, practically speaking, colors like `"yellow"` are effectively
lost, since it is brighter with higher chroma than other color hues
No C,L sequence could permit using yellow and other color hues with
these C and L values. Large swaths of beautiful colors would never
be chosen for categorical color sets, and we just cannot have that.
Cue the "de Blob" video game cut scene.
* So the hope is to use HSL, and S,L value sequence to fill the gap.
* Surprisingly, HSL hues have different spacing than HCL hues, which
means even a red-yellow-blue color wheel will require new color warp
adjustments.



# colorjam 0.0.22.950

## bug fixes

* `approx_degrees()` was calling `tcount()` from the `jamba`
package, and because the `jamba` package is listed as "Depends"
its functions are loaded and available to `colorjam` once
it is loaded. However, for some curious reason in R internals,
when the function is called directly `colorjam::approx_degrees()`
it does not import `jamba` functions, therefore `jamba::tcount()`
is not available, and it throws an error. The issue was
reported via the `venndir` package, where default usage of
`venndir` causes an error for the most basic operations.
The workaround is to load colorjam or jamba first, however
the real fix is to include package prefix here.


# colorjam 0.0.22.900

## changes to existing functions

A new hue warp preset is available, a fully corrected "red-yellow-blue"
gradient which is intended to provide full color complement
capability. However, `rainbowJam()` is preparing for a larger
update that focuses on choosing colors within gamut for each
color hue. The `colorspace` and `farver` color conversion functions
are able to provide colors outside of gamut but altering some
components which ultimately change the hue. As a result, the
hue sequence from `rainbowJam()` is sometimes out of order
due to that correction. A new method that returns strictly
correct hue within gamut is being tested and will be applied
in an upcoming release.

New default `preset="custom"` is used in several functions,
which will re-use any defined `h1` and `h2` values
in `options("h2hw.h1")` and `options("h2hw.h2")`,
otherwise it will use `default_preset="dichromat"`.
For the default scenario, nothing will change. When
a custom `h1` and `h2` should be used, it will be easier
for it to be applied by default. Setting `preset` to any
named value will override any stored `h1` and `h2` values.

* `rainbowJam()` argument value changed `hue_pad_percent=0`,
subtle change that means there is less color buffer between
the first and last color in the hue sequence. This argument
was most useful as a partial workaround to the color hue
gamut conversion "bug" in colorspace and farver. Not technically
a bug, as it is documented behavior, but the output is
not what one would expect imo.
* `approx_degrees()` was modified to simplify the logic, and to
handle special cases like inverted angles, and rotated angles.
* `h2hwOptions()`, `h2hw()`, `hw2h()`, and `rainbowJam()` default
argument changed to `preset="custom"` which will preserve any
pre-existing `h1,h2` values, otherwise a named `preset` will
take priority and define `h1,h2` values directly.
* `scale_color_jam()`, `scale_fill_jam()`, `jam_pal()` were updated
to use `preset="custom"` by default, and to pass `h1` and `h2`
arguments.

New option:

* `options("colorjam.preset")` as a convenient way to maintain
the current preset.


# colorjam 0.0.21.900

## changes to existing functions

* `closestRcolor()` and `twostep_gradient()` were updated
to use proper package prefixing to the `jamba` package.
* `scale_color_jam()` and `scale_fill_jam()` no longer `require(ggplot2)`,
and use `jamba::check_pkg_installed()` to test whether ggplot2 is
available. Added examples for both.
* `theme_jam()` no longer `require(ggplot2)` and now properly
calls `ggplot2::theme_classic()` with argument `resetTheme=TRUE`.

# colorjam 0.0.20.900

## changes to existing functions

* `rainbowJam()` arguments `Lvals` and `Cvals` were updated,
making the categorical palette brighter overall,
with more color saturation, and visible distinction
in neighboring colors at higher `n` values. The
examples in `rainbowJam()` were updated to show a
before and after comparison.

## new experimental function

* `rainbowJamMulti()` is intended to extend `rainbowJam()`
specifically when a list of categorical colors should be
created at once, in a way that prevents duplicate colors.



# colorjam 0.0.19.900

Several new functions, and two new data objects with color
gradients; pkgdown site documentation was updated
to showcase several new visual examples in for
the new color functions.

## changes to existing functions

* `scale_color_jam()`, `scale_fill_jam()` and `jam_pal()` have a new
argument `preset` which is passed along to `rainbowJam()` to
define the color hue wheel preset. The default `preset="dichromat"`
uses color blind friendly color hue wheel (mainly by omitting green).
The full rainbow can be used with `preset="ryb"` which uses
the enhanced red-yellow-blue color hue wheel, where green
is a secondary color between yellow and blue. The default
R red-green-blue color hue wheel can be used with `preset="rgb"`
although this palette is optimal for computer monitor use of RGB,
and not at all optimal for human perception, even
among those who can see the full rainbow of color hues.


## new functions

* `subset_colors()` is a subset function for a vector of R colors,
which allows rapid operations on any RGB, HCL, or HSV attribute.
* `col_div_xf()` produces a color function that maps numeric
values to divergent color gradient. Its arguments define the
numeric range, and optional floor. The floor is a range below
which the absolute numeric value is assigned the middle color,
useful to represent visual whether a point meets a numeric
threshold. This function is motivated to be used with argument `"col"`
in `ComplexHeatmap::Heatmap()`, to define a numeric range
with zero as the fixed mid-point associated with divergent
colors.
The name of the function is derived as follows:

    * `col` matching the argument in `ComplexHeatmap::Heatmap()` `"col"`
    * `div` for divergent colors
    * `x` for the `x`-defined numeric range
    * `f` for ability to apply a numeric floor

* `col_linear_xf()` is the linear/sequential color equivalent
to `col_div_xf()`. It is useful for two features:

    * define a fixed numeric ceiling for color assignment
    * define optional numeric floor, below which numeric values are assigned
    the first color, which is usually the blank color in the color gradient.

* `make_jam_divergent()` creates a divergent color gradient with
lite (white) or dark (black) middle color as appropriate. It can
recognize one of the new Jam gradients from `jam_linear` or `jam_divergent
(see below).
* `twostep_gradient()` is a simple but fairly exciting new function
that produces a linear/sequential color gradient - which means it
proceeds from a baseline color to the saturated color - while also
applying two intermediate color gradients to improve visual
perception. Most gradients from `RColorBrewer` employ this technique
to expand the range of colors, and to improve visibility of
each color step by varying both the brightness, and the color hue.
See examples for visual illustrations.


## new color objects

Two new color gradient objects are added, and are still under some
development as they are being used to gain feedback. They are both
motivated by the use case of providing color gradients for
genome sequence coverage heatmaps, provided by `"platjam"` package
which extends Bioconductor `EnrichedHeatmap`, itself an extension
by author of `ComplexHeatmap`.

These colors aim to provide color-blind-friendly color gradients,
while providing some utility of categorical coloring for different
panels of a coverage heatmap. Essentially these colors avoid the
range of green hues, which vastly improves visual distinctiveness
for the three color blindness forms emulated by
`dichromat::dichromat()`.

* `jam_linear` is a new R `list` object that contains a set of
linear color gradients with white background color. These are
intended to be paired with `jam_divergent`. The names in `jam_linear`
are also used in `jam_divergent`.
* `jam_divergent` is a new R `list` object that contains a set of
divergent color gradients with black background color to distinguish
itself from `jam_linear`. The names in `jam_divergent` are also
used in `jam_linear`.


# colorjam 0.0.18.900

## bug fixes / enhancements

* `blend_colors()` was updated to handle blending entirely grey sets 
of colors. Previously they failed to blend because the lack of any
color saturation also have them zero weight.
* `blend_colors()` new argument `c_floor` defines the `C` chroma
color saturation floor, below which a color is considered to have
no color hue. Technically it is given hue weight 0.0001.

# colorjam 0.0.17.900

## bug fixes

* `mean_angle()` had bare reference to `jamba::deg2rad()` which
was fixed. This fixed an error in `blend_colors()` when the
`jamba` was not attached.

# colorjam 0.0.16.900

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

# colorjam 0.0.15.900

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

# colorjam 0.0.14.900

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


# colorjam 0.0.13.900

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

# colorjam 0.0.12.900

## changes

* `theme_jam()` was updated to make the major and minor grid
lines a lighter shade of gray, so they have less interference
with text labels on a plot panel.

# colorjam 0.0.11.900

## changes

* `scale_fill_jam()`, `scale_color_jam()`, and `jam_pal()` have
an argument `alpha` to control alpha transparency, on a scale of
0 (transparent) to 1 (non-transparent.)

# colorjam 0.0.10.900

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

# colorjam 0.0.9.900

## changes

* `vals2colorLevels()` was updated to handle edge cases where input
values had no numeric range.

# colorjam 0.0.8.900

## new functions

* `vals2colorLevels()` converts a numeric vector into a color gradient,
optionally divergent around a baseline, optionally applying the color
warp using `jamba::warpRamp()` and a `lens` adjustment value.

# colorjam 0.0.7.900

## changes

* Added more function prefixes "jamba::" to avoid handle the inconsistent
import implementation in R.

## new functions

* `matrix2heatColors()` to apply color gradient to each column in a matrix,
where each column has its own color scale.

# colorjam 0.0.5.900

## additions

* A README.Rmd file was created with a basic walkthrough for colorjam.
* DESCRIPTION was changed to move "jamba" to the "Depends:" field.
Guidance from R Package Development and R central docs is cryptic on
this point.

# colorjam 0.0.4.900

## new functions

* `theme_jam()` which provides a Jam-specific ggplot2 default.
* `scale_color_jam()`, `scale_fill_jam()`, and `jam_pal()` provide
categorical colors and fills, respectively, using `rainbowJam()`.

## bug fixes and enhancements

* Updated DESCRIPTION to include proper "Remotes" entry pointing
to the Github jamba package.
* Updated `closestRcolor()` to fix a small bug with name handling
of the output.

# colorjam 0.0.3.900

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

