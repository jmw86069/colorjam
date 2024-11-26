# colorjam Todo

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

