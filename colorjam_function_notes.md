# Organize / Simplify overall functions

## Core user-facing functions

* col_div_xf
* col_linear_xf

* group2colors

* rainbowJam
* rainbowJamMulti - hide for now?

* scale_color_jam
* scale_fill_jam
* theme_jam

## Useful color utilities

* add_colors
* blend_colors


## Extra color utilities

* find_color_spread - (internal?)

* closestRcolor
* closest_named_color

* color_complement
* color_distance

* color_pie
* showDichromat

* colors_to_df
* sort_colors
* subset_colors
* combine_alphas - used by venndir (Todo: improve logic, allow darkening? allow color input?)

* show_color_distance - adds optional dependency on ComplexHeatmap

* vals2colorLevels

   * multienrichjam should migrate to `col_div_xf()`
   * used by legacy R/Rmd code


## Probably should be internal-only (for now)

### Hue warp / Preset / Step
* add_colorjam_preset
* add_colorjam_step
* adjust_hue_warp
* colorjam_presets
* colorjam_steps
* h2hw, hw2h, h2hwOptions
* hcl_to_hsl_hue, hsl_to_hcl_hue
* plot_colorjam_preset, plot_colorjam_steps
* remap_colorjam_preset
* validate_colorjam_preset
* vibrant_color_by_hue - used internally for presets?

### Others

* approx_degrees
* display_degrees
* mean_angle
* jam_pal - used by ggplot2-related functions
* slot_colors
* twostep_gradient - not used by any jam packages

### R-shiny app (for my own use mostly)

* colorjamShinyServer
* colorjamShinyUI
* launchColorjamShiny


## Remove altogether?

* rainbowJam_v1


# Comments

* Does `col_div_xf()` work with pheatmap?
