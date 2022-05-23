
## TODO 22may2022

* Migrate `platjam::design2colors()` here.

   * Intended to take `data.frame` with group, subgroup,
   and create a `list` of colors for each column, either
   with named `character` color vectors named by value,
   or color `functions` for `numeric` columns, as created
   by `circlize::colorRamp2()`.

* Implement HSL color space for categorical colors, slowly replacing
HCL colorspace in `rainbowJam()`.

## TODO

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

## color table viewer

* Perhaps the most frequently used JAM function is a fixed-width
colorized table viewer for console output. Similar in theory to
the custom tibble print method, this function goes further by
colorizing values in each column to help the visual summary.
Especially useful for checking experiment design, typos in
labels, and numeric ranges. It can, for example, make a text
heatmap for numeric values, which can be helpful spotting outliers.
It also trims column values so the table fits within one or
more screen widths.

