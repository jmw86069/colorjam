# colorjam Todo

## 05dec2025

Prepare for CRAN to support: jamba, venndir, multienrichjam.

* Remove dependency on jamses for heatmaps.
* Consider new ggplot2 function using `col_div_xf()` to work the same way,
`scale_color_div_xf()`, `scale_fill_div_xf()`.
* Improve the default color wheel. Reduce hot pink, improve purple.
Revisit classic RYB color wheel, then remove 2 green wedges.

   * Consider defining C,L,S,L ranges per color hue to optimize aesthetics.

* Change examples to use `withr::with_par()`
* DONE. `theme_jam()`: define `rainbowJam()` default discrete colors.
* IN PROGRESS. Simplify exported functions, remove/hide all non-essential
* `color_pie()`

   * DONE. Consider init.angle=270 (top) especially for divergent colors.
   * DONE. Consider attribute 'divergent=TRUE' in `col_div_xf()`
   and `make_jam_divergent()`
   * DONE. Accept `function` input similar to `jamba::showColors()`
   * DEFER. Consider argument 'buffer' to add empty buffer zone. Nice-to-have.

* FIXED. `color_complement()` Note 'dichromat2' is not correct.
* `matrix2heatColors()` - used by multienrichjam

   * Consider converting to `col_div_xf()`, `col_linear_xf()`.

* Pie in the sky: Evaluate alternate k-means approach to color selection.

## 21apr2025

* Add `named_color` package maintenance functions.
* Evaluate color distance within color blindness types:

   * Consider `colorblindcheck::palette_dist()` for alternate color distance.
   * `colorblindr()` can convert grid graphics to different forms.

* Evaluate custom color wheel using `named_colors`
* `add_colors()`

   * DONE. Consider method to compare new colors to each other.
   
      * Test:
      `new_colors <- add_colors(rainbowJam(4), n=3, do_plot=TRUE, color_fn=colorspace::rainbow_hcl)`
      * DONE. Implement logic to subset `new_colors` by imposing `min_distance`
      before assigning `n` colors. For example, if `n=4` but
      12 new colors are found, the 12 could each be very different from
      `given_colors` but very similar to each other. The approach should
      select the `n` colors most distinct from each other.

* Add tests

   * Add tests for `slot_colors()`
   * Add tests for `color_distance()`
   * Add tests for `add_colors()`

## 11apr2025

* `add_colors()`

   * DONE. Iterate progressively lower color distance for dynamic use.
   * DEFER. Consider custom color distance for low-saturation bright colors,
   which look more similar than distance metrics show.
   * DENY. Consider defining dynamic color distance by overall available color
   palette, not by using the input colors.
   Denied bc the dynamic distance approach sidesteps the need.
   * DEFER. Consider finding **a reasonable color distance metric**, somehow
   the available metrics still fall short (of the intended effect here tbf).
   * Consider future approach to apply additional logic to color filtering,
   for example transform by color blindness before calculating distances,
   or add logic to reward colors with distinct luminance.

## 06apr2025

* Prepare for CRAN release.

   * Consider removing functions not absolutely necessary:
      `launchColorjamShiny()`,
      `matrix2heatColors()` (?),
      `rainbowJamMulti()` - may be superceded by `add_colors()`
      `rainbowJam_v1()`
      `remap_colorjam_preset()`
   * Consider transitioning all user-facing functions to use presets,
   thus hiding or removing all other functions.
   * Consider hiding (not exporting) some internal functions:
      `h2hw()`, `hw2h()`, `h2hwOptions()`
   * Consider renaming `color_to_df()` for clarity?
   Suggestions: `annotate_colors()` since it takes a vector of colors,
   and returns a `data.frame` of annotations.
   * Consider renaming functions from camelCase to snake_case for consistency?
   (Or both, shudder.)
   Mostly would mean adding `rainbow_jam()` and deprecating `rainbowJam()`.
   * Consider how best to include ggplot2 themes and scales.
   Confirm `scale_color_jam()` works with combination of assigned and
   missing colors.
   * Update function family and pkgdown categories.

* Consider changing `scale_color_jam()`, `scale_fill_jam()`

   * Add argument `colorSub` to assign colors by name, then use `add_colors()`.
   * Internally, see steps used by `ggplot2::manual_scale()`.

* Add functions to help "maintain" named_colors from "meodai/color_names"

   * Download latest
   * Re-create `named_colors`.
   Decide what to do with retired colors and backward compatibility.
   For now, they are lost, in part bc retired colors have tended to remove
   disrespectful or otherwise inappropriate labels.
   * Refresh the named color wheel, e.g. the "preset".
   Optional? It might be best to keep the wheel consistent over time.


## 05apr2025

* DONE. Prototype `add_colors()` function

   * `slot_colors()` - used to compare `x` colors to `y` colors, "assigning"
   colors in `x` to the corresponding best match in `y`, with default
   restrictions using color distance.
   * `color_distance()` - wrapper to `farver::compare::colour()`

* Experiment with, then add `named_colors` color wheel

   * Theory is to define the color wheel using "crowd sourcing"
   based upon people's interest and enthusiasm for colors across
   the RGB color wheel. It appears to be a natural method of pruning
   the blue-green/green colors, which appeat to have proportionally
   fewer names than yellow/gold/orange.

* Add `fibonacci_colors()`

* Transition `design2colors()` from `platjam::design2colors()`

   * Use `add_colors()` to add colors iteratively.

* Add testthis

   * `rainbowJam()`
   * `rainbowJam()` with alternative presets
   * `add_colors()`
   * `slot_colors()`

## 01apr2025

* `theme_jam()`

   * Add `axis.text.y.angle=0` to code and defaults.
   * Consider reducing strip margin around labels?

* `colorjam_presets()`

   * Consider hue wheel defined by `named_colors`.

* `named_colors`

   * Add maintenance functions to download latest `named_colors`, filter
   for approved colors, redefine the color wheel (see above) and update
   the colorjam package data object.
   * Color wheel: Sort colors by hue. Define steps using spline, using
   hue rank, scaled from 0 to 360. Consider some "rounding" of decimal values,
   then take the mean rank for ties, where the same hue appears consecutively.
   (Alternate is to reduce repeated values to single entry.)

## 19mar2025

* `col_linear_xf()`

   * Consider new arg `floor_colramp="Blues"` to assign colors below floor.
   * Consider optional argument with specific breakpoints, labels,
   to be used as default with heatmap legends.

* Consider function to create `ComplexHeatmap::Legend()` from
`col_div_xf()` or `col_linear_xf()`


## 21jan2025

* consider making a ggplot2 "theme" that automatically uses the colorjam
categorical color functions, as drop-in replacement for rainbow colors,
most notably used by Seurat/tSNE/UMAP plots to color each cluster.
* novel color wheel using "meodai/named_colors" as crowd-sourced evidence

   * filter named_colors for chroma, luminance, for usability by `rainbowJam()`

   

## 14jan2025

* `col_linear_xf()` and `col_div_xf()`

   * When `floor` is used, consider optional argument `floor_color`
   to apply a different color to indicate when a value is below the floor.
   For example, sometimes it's useful to use yellow to indicate a value
   is below the floor, rather than relying on discerning white versus
   slightly off-white, especially for `colramp="Reds"` where the
   baseline color is already slightly reddish-white.

## 11dec2024

* Improve `blend_color()` handling of alpha so it builds over time, use
formula: `a + (1-a) * b` where a,b are alpha values between 0 and 1.

## TODO 12jul2024

* `blend_colors()` - consider `preset=getOption("colorjam.blend.preset")`
so it can be easily customized with a fixed scheme.
* Fix package dependencies so it does not also load `jamba` upon load.
* `color_pie()`: make it more similar to `jamba::showColors()`

   * `color_pie(rainbowJam(12))` should show hex color as labels by default,
   same as `jamba::showColors()`
   * Consider argument `inner_radius` to make donut plots.
   * Consider replacing `label_radius` with combination of 

      * `label_just` for label justification relative to its position, where

         * `just=0` means the point is on the left(bottom), and
         * `just=1` means the point is on the right(top).

      * `label_pos` for label position within the inner/outer radius:
      
         * `label_pos=1` positions at the outer radius
         * `label_pos=0` positions at the inner radius

* Consider new color display function `color_spiral()`

   * Alternative to `color_pie()` to arrange colors around the color wheel
   with concentric rings. Consider using `circlize` for spiral display?
   * Goal is to show a lot of colors in an organized way to facilitate
   picking a color from a pre-defined set.

* Consider adding dependency on `farver>=2.1.2` to confirm fixed functions
* Improve the color wheels (angles for color breaks)

   * Revisit the color calculations given `farver>=2.1.2` which fixed
   bug related to color distance calculations, and nearest color calculations.
   * `"dichromat2"`
   
      * not enough actual red, too much brown/salmon/hot pink
      * some of this problem is with the C/L sequence not producing a good red
   
   * `"dichromat"`
   
      * too much salmon/orange not enough gold, transition from orange to gold
      seems too slow
      * using n=6 "mediumpurple1" (H=272), "purple_illusion" (H=279)
      are too similar. 
      * Maybe spread colors in four quadrants: gold, red, purple, blue?
      
   * `"ryb"`
   
      * too much fuschia/hot pink in the color wheel,
      with n=12 two sections are hot pink
      * with n=36 it looks nice, except too much pink
   
   * `"ryb2"`
   
      * red does not occur reliably after n=4, becomes brown/salmon/pink
      * even with n=36 WHERE IS THE RED? (Haha)


* Consider finally porting the Fibonacci color method.

   * Basic premise: Use magic number phi to represent a Fibonacci spiral
   around the color wheel.


## TODO 24apr2024

* `closest_named_color()` and `closestRcolor()`

   * Consider option to return the input/output values in a `data.frame`:
   
      * hex color value
      * H,C,L values
      * distance value calculated

   * Consider alternative color distance function.
   
      * Standard HCL color distance function.
      * Consider `farver::compare_colour()` with some adjustment for hue
      difference.


## TODO 14mar2024

* DONE. Bug: `blend_colors()` does not allow changing the `preset`

   * DONE. `h2hw()` and `hw2h()` prioritize `preset` over `h1`,`h2` which
   causes `blend_colors()` to use the global `preset`, since it does
   not pass `preset="custom"` to enable over-riding the default `h1`,`h2`
   values.
   


## TODO 08jan2024

* Consider new accessible color wheel.

   * Start with red-yellow-blue color wheel.
   * Remove wedges of green/yellow-green.
   * Remove corresponding complement with fuschia/pink.
   * Test remaining wheel for color-blindness friendly distinction.

## TODO 14nov2023

* Custom `showColors()` specifically for sorted colors

   * indicate along the x-axis where the numeric breaks occur
   * most often sorted by Hue, then indicate breaks along x-axis
   * determine numeric values of the first sorted vector, then range,
   then `pretty()`

## TODO 01oct2023

* `rainbowJam()`

   * consider `Hrange` argument to `rainbowJam()` which would restrict
   colors to this range of hues. Unclear it if means the virtual hue,
   or output hue, but probably the virtual hue.

* R-shiny app changes:

   * in h1,h2 scatterplot, editing a point should not change its rank
   
      * it should be restricted on the x/y axis by the neighboring points
      * help keep points properly aligned, for example preventing identical
      points from being re-ordered.

   * add `label` to plotly shapes, using just the row number
   * consider double-clicking to add a point to the h1,h2 plot.
   
      * Point should be fixed between neighboring points on the x-axis,
      thus constraining the y-axis value.
   
   Could use `plotly::showModal()` to pop-up a confirmation, but later.
   * consider right-clicking to remove a point on the h1,h2 plot.
   Again, it could use `showModal()` to confirm, to prevent accidentally
   removing a point. (There is no convenient "Undo"... but there could be I guess.)
   * Consider "Undo" button
   
      * When no previous values, the "Undo" button is hidden.
      * Reverts to the previous h1,h2 values, then hides the "Undo" button.
      * Upon editing any value, store previous values, display "Undo" button.

* `design2colors()` (when ported here)

   * consider changing how class colors are assigned, to ensure each class
   uses `phase` values starting at the same place, in the same order.
   Goal is to have first color in each class using the same phase step.
   * When `class` colors are assigned, consider assigning group colors
   using hue splitting instead of lightness splitting.
   Generate a range of hues so that they do not conflict with other class hues.
   (Then use new argument `Hrange`.)

* Consider retiring `h2hw()`,`hw2h()`,`h2hwOptions()`

   * simplify system to use "Virtual Hue" and "Actual Hue"?

## TODO 25sep2023

* consider adding `launchColorjamShiny()` to colorjam.shinyapps.io for testing.
* add unit testing with `testthat`, currently coverage is via `@examples`
* consider `shinytest2` to test the R-shiny app. Probably not urgent.

## TODO 20sep2023

* Consider secret test mode for `rainbowJam()` that uses HSL instead of HCL

   * likely to show less contrast than HCL, but with better saturation per hue
   * for testing, use C,L values rescaled to HSL range

* Move `platjam::design2colors()` here?
* Review `preset` and `step`

   * After real-world usage, decide whether default colors are visually
   distinct enough
   * R package `pals` provides functions to evaluate color palettes
   * Other packages: colorBlindness, colorblindr, shades, ggsci
   * Consider expanding `Lrange`, `Crange` to broaden visual range.

## TODO 19sep2023

* DONE. `sort_colors()` default should sort by hue, chroma, luminance.

* DONE. Consider HCL-to-HSL hue mapping
* DONE. `remap_colors()`

   * convert colors to use a different `preset`
   * upon changing the hue
   
      * convert HCL hue to HSL hue
      * convert color internally to HSL to determine the current S,L values
      * adjust to the new hue, then apply the same S,L values
      * upon testing, HSL does not work nearly as well as hoped; HCL was
      too dark; blending HSL and HCL conversions seemed most effective

## TODO 13sep2023

* DONE. Simple R-shiny app

   * visualize color palettes with adjustments
   * select the number of colors
   * edit the `step` values for Chroma and Luminance
   * edit the `phase` order of steps
   * choose the `preset` from known color wheels
   * bonus points: edit the color wheel `h1`, `h2` points:

      * click-and-drag to modify existing points
      * add a new point
      * delete an existing point

* DONE. enhance `"preset"`

   * DONE. associate with a `default_step`
   * DONE. associate with `direction`: `1` or `-1` for
   direction around the color wheel
   * `add_colorjam_preset()` should validate `h1`,`h2` and `direction`.

* DONE. Fix `approx_degrees()` when multiple `h1` or `h2` values are repeated.

   * The `approx()` function requires unique `x` values.
   * Data should be sorted by `x`,`y` then uniqueness enforced for `x`.
   * This bug mainly affects `h1`,`h2` when `h2` is used for `x`.

## TODO 07sep2023

* DONE. register a new color wheel

   * for example modifying an existing color wheel with `adjust_hue_warp()`
   then saving it as a named preset.
   * use `igraph:::.igraph.shapes` as a model, they store graph node shapes
   in that environment, then provide access `igraph::shapes()` and
   allow new shapes with `igraph::add_shape()`.

* DONE. port `color_complement()` into this package.
* `make_jam_divergent()`

   * When argument `linear2` is not supplied, get `color_complement(linear1)`
   * DONE. Error when using `jam_linear` and `jam_divergent` without
   package prefix when the colorjam package is not formally loaded.

## TODO 23sep2022

* Migrate `platjam::design2colors()` into this package

   * Consider `color_list` related functions, since `design2colors()`
   produces a list of colors and color functions.
   * `merge_color_lists()` would combine two color lists into one, taking
   either the union of assigned colors, or the first color function, where
   relevant.

## TODO 22may2022

* Migrate `platjam::design2colors()` here.

   * Intended to take `data.frame` with group, subgroup,
   and create a `list` of colors for each column, either
   with named `character` color vectors named by value,
   or color `functions` for `numeric` columns, as created
   by `circlize::colorRamp2()`.

* No longer necessary: Implement HSL color space for categorical colors,
slowly replacing HCL colorspace in `rainbowJam()`.

## TODO

These items are not likely to be on an active todo list:

* Add optional parameter to `group2colors()` which applies
a systematic re-ordering of colors assigned to labels. For
example, labels are typically assigned sequential color
hue, which may not be ideal for all cases. This method would
assign colors to every n-th color.
* Add `subgroup2colors()` which extends `group2colors()` by
splitting each color into subgroups as needed. In this way,
experiment factors can each be assigned colors, while the
factor levels are assigned values from a gradient for each
color. In future, this function will optionally use a
color family (or small range of color hues) to each factor,
then will vary the hue, as well as the chroma/luminance for
factor levels.
* `vals2colorLevels()` assigns colors to a numeric range,
keeping zero as a mid-point for divergent color scales.
Notably allows a "color lens" which applies a scalable adjustment
to the color ramp to enhance or reduce the color intensity across
the numeric range.

## color table viewer (ASCII)

* Perhaps the most frequently used JAM function is (was) a fixed-width
colorized table viewer for console output.

   * Similar in theory to the custom tibble print method, this function goes
   further by colorizing values in each column to help the visual summary.
   * Especially useful for checking experiment design, typos in
   labels, and numeric ranges. It can, for example, make a text
   heatmap for numeric values, which can be helpful spotting outliers.
   * It also trims column values so the table fits within one or
   more screen widths.
   * Similar R package `pillar` offers colorized formatted
   `data.frame`/`tibble` output.
   
      * Unclear whether it can provide equivalent capabilities.

