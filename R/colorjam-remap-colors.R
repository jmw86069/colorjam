
#' Remap colors to a new colorjam preset
#'
#' Remap colors to a new  colorjam preset, using existing preset
#' when available.
#'
#' This function is experimental, and is intended to convert a set
#' of categorical colors to a colorjam preset. When the existing
#' colors have attribute `"preset"` it is used to inform the starting
#' preset. The output of `rainbowJam()` includes the preset as
#' `attr(x, "preset")`.
#'
#' The HCL and HSL color conversions are blended together to improve
#' the imperfect result from either method alone. The end result
#' is imperfect, but better than the alternatives.
#'
#' @family colorjam hue warp
#'
#' @returns `character` vector of hexadecimal colors
#'
#' @examples
#' x <- rainbowJam(12)
#' x_new <- remap_colorjam_preset(x, preset="ryb2", do_plot=TRUE)
#'
#' x <- rainbowJam(12, preset="ryb2")
#' x_new <- remap_colorjam_preset(x, preset="dichromat2", do_plot=TRUE)
#'
#' x <- rainbowJam(12, preset="ryb")
#' x_new <- remap_colorjam_preset(x, preset="dichromat2", do_plot=TRUE)
#'
#' x <- rainbowJam(12, preset="dichromat2")
#' x_new <- remap_colorjam_preset(x, preset="ryb", do_plot=TRUE)
#'
#' x <- rainbowJam(12, preset="rgb")
#' x_new <- remap_colorjam_preset(x, preset="ryb", do_plot=TRUE)
#'
#' @export
remap_colorjam_preset <- function
(x,
 preset=NULL,
 preset_from=NULL,
 do_plot=FALSE,
 plot_debug=FALSE,
 ...)
{
   # determine HCL hue
   x_hcl <- jamba::col2hcl(x);
   x_hcl_hue <- x_hcl["H",];
   if (TRUE %in% plot_debug) {
      print("x_hcl_hue (input):");print(x_hcl_hue);# debug
      plot(x=seq_along(x), y=x_hcl_hue, ylim=c(0, 360), pch="1");
   }

   # accept preset encoded as attribute name
   if (length(preset_from) == 0 && "preset" %in% names(attributes(x))) {
      preset_from <- attr(x, "preset");
   }

   # optionally decode input preset
   if (length(preset_from) > 0) {
      x_hcl_hue <- round(h2hw(preset=preset_from,
         h=x_hcl_hue), digits=1) %% 360
      if (TRUE %in% plot_debug) {
         print("x_hcl_hue (preset_from):");print(x_hcl_hue);# debug
         points(x=seq_along(x), y=x_hcl_hue, pch="2");
      }
   }
   # adjust hue
   new_hcl_hue <- hw2h(preset=preset,
      h=x_hcl_hue)
   if (TRUE %in% plot_debug) {
      print("new_hcl_hue (output):");print(new_hcl_hue);# debug
      points(x=seq_along(x), y=new_hcl_hue, pch="3");
   }
   # determine HSL hue
   x_hsl_hue <- hcl_to_hsl_hue(new_hcl_hue);

   # determine HSL values
   x_hsl <- jamba::col2hsl(x)

   # create colors using HSL with new hue, existing S,L
   new_hsl_colors <- jamba::hsl2col(H=x_hsl_hue,
      S=x_hsl["S", ],
      L=x_hsl["L", ],
      alpha=x_hsl["alpha", ])

   # create colors using HCL with new hue, existing C,L
   new_hcl_colors <- jamba::hcl2col(H=new_hcl_hue,
      C=x_hcl["C", ],
      L=x_hcl["L", ],
      alpha=x_hsl["alpha", ])

   # blend them together
   new_hsl_colors <- sapply(seq_along(new_hsl_colors), function(i){
      blend_colors(x=c(new_hsl_colors[i],
         new_hcl_colors[i],
         new_hcl_colors[i]))
   })

   # optional plot
   if (TRUE %in% do_plot && !TRUE %in% plot_debug) {
      color_list <- list(
         input=x,
         output=new_hsl_colors);
      if (length(preset_from) > 0) {
         names(color_list)[1] <- preset_from;
      }
      names(color_list)[2] <- preset;
      jamba::showColors(color_list)
   }
   return(new_hsl_colors)
}

#' Convert HCL hue to HSL hue
#'
#' @family colorjam hue warp
#'
#' @returns `numeric` hue with values in range `c(0, 360)`
#'    intended for use with `jamba::hsl2col()`
#'
#' @param x `numeric` hue with values in range `c(0, 360)`
#'    intended for use with `jamba::hcl2col()`
#' @param ... additional arguments are ignored.
#'
#' @export
hcl_to_hsl_hue <- function
(x,
 ...)
{
   #
   approx_degrees(preset="hcl_to_hsl",
      h=x)
}

#' Convert HSL hue to HCL hue
#'
#' @family colorjam hue warp
#'
#' @returns `numeric` hue with values in range `c(0, 360)`
#'    intended for use with `jamba::hcl2col()`
#'
#' @param x `numeric` hue with values in range `c(0, 360)`
#'    intended for use with `jamba::hsl2col()`
#' @param ... additional arguments are ignored.
#'
#' @export
hsl_to_hcl_hue <- function
(x,
 ...)
{
   #
   hw2h(preset="hcl_to_hsl",
      h=x);
}

