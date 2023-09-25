## TODO 25sep2023

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

