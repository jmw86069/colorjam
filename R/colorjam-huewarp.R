
#' Get hue color warp options
#'
#' Get hue color warp options used to convert color wheels
#'
#' This function retrieves and/or defines, the `h1` and `h2`
#' hue vectors used to convert between `h1` "hue warp", and
#' `h2` "actual hue".
#'
#' The "actual" hue is used by standard R #' functions such as
#' `colorspace::polarLUV()`, `grDevices::hcl()`, and
#' `farver::convert_colour()`.
#'
#' The mapping from `h1` to `h2` allows customization of the spacing
#' and order of colors, which allows emulation of a red-yellow-blue
#' color wheel for example.
#'
#' The `h1` represents the color hue in terms of a degree angle ranging
#' from 0 to 360 - a full circle - for the observer. It is then
#' transformed to `h2` for use in generating actual R colors.
#'
#' * `colorjam_presets()` lists all recognized colorjam presets.
#' * `add_colorjam_preset()` will add or overwrite a colorjam preset by name.
#'
#' In general, most colorjam functions with argument `preset` will
#' follow this progression:
#' * Argument `preset=getOption("colorjam.preset", "custom")`
#' which uses `preset` when defined, otherwise `"custom"`.
#' * When this option matches a recognized preset name, the
#' corresponding `h1`,`h2` values are used.
#' * When `preset="custom"`, arguments `h1`,`h2` will also
#' poll `getOption("h2hw.h1")` and `getOption("h2hw.h2")` for
#' default values.
#' * When neither `h1`,`h2` is defined, the argument
#' `default_preset="dichromat"` is used to obtain `h1`,`h2` values.
#'
#' To disable the warped hue mechanic, set `preset="rgb"` which
#' usess the default R color wheel with no adjustment.
#'
#' ## Details
#'
#' The `h1`,`h2` values are passed to `approx_degrees()` to convert
#' hue degree angles. See `adjust_hue_warp()` for detailed examples
#' of manipulating color warp values.
#'
#' @family colorjam hue warp
#'
#' @param h1 `numeric` vector of color hue values which represent
#'    the "from" hue angles, also referred to as "hue warp" or "hw"
#'    values. One example of a typical operation: one may want to
#'    know the R hue for a particular red-yellow-blue hue.
#'    In this scenario the `h1` "hue warp" is the red-yellow-blue hue,
#'    and the "hue" is the R typical hue used in `colorspace::polarLUV()`
#'    `grDevices::hcl()`, and `farver::convert_colour()`. For
#'    evenly-spaced red-yellow-blue colors, one would define a sequence
#'    of "hue warp" values from 0 to 360, then convert them to
#'    the default hue used by other R functions.
#' @param h2 `numeric` vector of color hue values which represent
#'    the "to" hue angles, also referred to as "hue" or "h".
#' @param preset `character` string indicating whether to define
#'    `h1`, and `h2` values based upon named presets:
#'    * `"custom"` uses values defined in `options("h2hw.h1")`
#'    and `options("h2hw.h2")` if they exist, otherwise `default_preset`.
#'    * other `character` values obtained by `colorjam_presets()`, some
#'    examples include:
#'       * `"dichromat"` (default) color wheel intended to be color-blind
#'       friendly by omitting much of the green color region from the color
#'       wheel, and by reviewing output by `dichromat::dichromat()`
#'       * `"ryb"` basic red-yellow-blue color wheel
#'       * `"ryb1"`,`"ryb2"`,`"ryb3"` experimental red-yellow-blue
#'       alternative color wheels designed to emphasize various features
#'       in the red-orange-yellow-green range to varying degrees.
#'       * `"rgb"` for the default R red-green-blue color wheel
#' @param preset_names `character` vector of valid `preset` values,
#'    included only to make the values visible in the function arguments.
#' @param default_preset `character` string indicating which value
#'    in `preset` should be used as the default when `reset=FALSE` and
#'    `h1` and/or `h2` are not defined in `options()`.
#' @param reset logical whether to reset `h1` and `h2` values to the default
#'    values as defined in `h1default` and `h2default`. When `reset=TRUE`
#'    all other `preset` and `default_preset` arguments are ignored.
#' @param setOptions `character` or `logical` indicating whether to
#'    update `options()` for `"h2hw.h1"` and `"h2hw.h2"`. When `"ifnull"`
#'    the `options()` are only updated if they were previously `NULL`.
#' @param verbose logical whether to print verbose output
#'
#' @return list with names `h1` and `h2` containing numeric vectors
#'    of hues between 0 and 360.
#'
#' @examples
#' h2hwOptions()
#' h2hw(60)
#'
#' h2hwOptions(h1=c(0, 60,120,240,300,360),
#'    h2=c(0,120,180,240,280,360))
#' h2hw(300)
#'
#' @family hue warp functions
#'
#' @export
h2hwOptions <- function
(h1=getOption("h2hw.h1"),
 h2=getOption("h2hw.h2"),
 preset=getOption("colorjam.preset", "custom"),
 preset_names=c("custom",
    "none",
    "dichromat",
    "rgb",
    "ryb",
    "ryb1",
    "ryb2",
    "ryb3"),
 default_preset="dichromat",
 reset=FALSE,
 setOptions=c("FALSE",
    "TRUE",
    "ifnull"),
 verbose=FALSE,
 ...)
{
   ## Purpose is to define options("h2hw.h1") and options("h2hw.h2");
   ##
   ## Consider using different default values:
   ## h1 <- c(0,  60, 120, 240, 300, 340, 360);
   ## h2 <- c(0, 100, 160, 240, 330, 350, 360);
   ## These default values further expand colors from blue to purple
   preset_names <- colorjam_presets();
   preset <- match.arg(arg=preset,
      choices=c(preset_names, "custom"))
   default_preset <- match.arg(arg=default_preset,
      choices=setdiff(preset_names, "custom"));

   if (length(setOptions) > 0 && is.logical(setOptions)) {
      setOptions <- as.character(setOptions);
   }
   setOptions <- match.arg(setOptions);
   h1h2_undefined <- (length(h1) == 0 ||
         length(h2) == 0 ||
         !length(h1) == length(h2));
   if (verbose > 1) {
      jamba::printDebug("h2hwOptions(): ",
         "h1h2_undefined:", h1h2_undefined);
   }
   ## Order of operation:
   ## 1. reset=TRUE forces h1=h1default and h2=h2default
   ## 2. preset="something" forces h1, h2 to appropriate preset values
   ## 3. preset="none" and h1h2_undefined, uses default_preset
   ## 4. preset="none" and !h1h2_undefined uses h1,h2 as provided

   if (length(reset) > 0 && TRUE %in% reset) {
      preset <- default_preset;
   }

   if ("custom" %in% preset) {
      if (h1h2_undefined) {
         preset <- default_preset;
      }
   }
   if (!"custom" %in% preset) {
      ## use preset values
      h1h2_list <- colorjam_presets(preset);
      h1 <- h1h2_list$h1;
      h2 <- h1h2_list$h2;
   }

   if ("TRUE" %in% setOptions ||
         (TRUE %in% h1h2_undefined && "ifnull" %in% setOptions)) {
      if (verbose) {
         jamba::printDebug("h2hwOptions(): ",
            "Updated options()");
      }
      options("colorjam.preset"=preset);
      options("h2hw.h1"=h1);
      options("h2hw.h2"=h2);
   }
   list(
      h1=h1,
      h2=h2);
}

#' Convert standard hue to warped hue
#'
#' Convert standard hue to warped hue using the hue warp vectors
#'
#' This function is intended to convert from a vector of hue values to
#' the warped hues defined by the vectors returned by [h2hwOptions()].
#' The intent is to convert colors in RGB space into RYB space by default,
#' which substantially improves several other color manipulations, such
#' as selection of categorical colors, and color blending.
#'
#' Note the input hue is considered the "standard" color hue as defined
#' by the [colorspace::polarLUV()] function, ranging between 0 and 360.
#' By this standard, 0 is defined as red, 120 is defined as green, and
#' 240 is defined as blue.
#'
#' The default mappings convert RGB (red, green, blue) to RYB (red, yellow,
#' blue). The color yellow has hue=60 in RGB space, so the call to
#' `hw2h(60)` results in `120`, which is the hue in RYB space.
#'
#' @family colorjam hue warp
#'
#' @param h `numeric` vector of color hues between 0 and 360. These hues do
#'    not need to be in sequential order.
#' @param h1,h2 `numeric` vector of color hues, which by default are defined in
#'    [h2hwOptions()], but allowed here in cases where the global options
#'    should be overridden but not modified.
#'
#' @return `numeric` vector of hue values after applying the hue warp
#'    operation.
#'
#' @examples
#' ## Yellow when using an RGB color wheel is 60 degrees,
#' ## but on an RYB color wheel is 120 degrees.
#' h2hw(60, preset="ryb");
#'
#' # RGB colors are convenient, but are not ideal especially when blending
#' # colors. Note that blue and yellow have hues that differ by exactly 180
#' # degrees, meaning a hue average is as likely to be purple as green.
#' huesBY <- jamba::col2hcl(c("blue", "yellow"))["H",];
#' huesBY;
#'
#' warpedHuesBY <- h2hw(huesBY, preset="ryb");
#' warpedHuesBY;
#'
#' @family hue warp functions
#'
#' @export
h2hw <- function
(h,
 h1=NULL,
 h2=NULL,
 direction=1,
 preset=getOption("colorjam.preset", "custom"),
 ...)
{
   ## maps hue to a weighted hue, based upon the guidepoints
   ## given by h1 (reference hue) and h2 (weighted hue), on a scale
   ## of 1 to 360
   if (length(preset) == 0) {
      preset <- "custom";
   }
   if ("custom" %in% preset && (length(h1) == 0 || length(h2) == 0)) {
      cli::cli_abort(message=paste(
         "{.var preset} must not be \"{.field custom}\" when",
         "{.var h1} and {.var h2} are not provided."))
   }
   if (!"custom" %in% preset) {
      h1h2 <- colorjam_presets(preset=preset);
      h1 <- h1h2$h1;
      h2 <- h1h2$h2;
      direction <- h1h2$direction;
   } else {
      h1h2 <- jamba::rmNULL(validate_colorjam_preset(preset=preset,
         h1=h1,
         h2=h2,
         direction=direction,
         default_step=NULL))
   }
   h1 <- h1h2$h1;
   h2 <- h1h2$h2;
   direction <- h1h2$direction;

   hNew <- approx_degrees(
      h1=h1,
      h2=h2,
      h=h,
      direction=direction,
      preset="custom");

   return(hNew);
}

#' Convert warped hue to standard hue
#'
#' Convert warped hue to standard hue using the hue warp vectors
#'
#' This function is intended to convert from a vector of warped hue values to
#' the hues defined by the vectors returned by h2hwOptions()`.
#' The intent is to convert colors in RYB space into RGB space by default.
#'
#' One example of input would be to supply a uniformly spaced set of color
#' hues, and convert them to "standard" hues by HCL standards. It has the
#' effect of presenting categorical colors, using a non-linear set of
#' "standard" hue values.
#'
#' The default mappings convert RYB (red, yellow, blue) to RGB (red, green,
#' blue). The color yellow has hue=120 in RYB space, so the call to
#' `h2hw(120)` results in 60, which is the hue in RGB space.
#'
#' @family colorjam hue warp
#'
#' @returns `numeric` vector of color hues after applying the transformation
#'    from `h2` to `h1`.
#'
#' @param h `numeric` vector of color hues between 0 and 360. These hues do
#'    not need to be in sequential order.
#' @param h1,h2 `numeric` vector of color hues, which by default are defined in
#'    `h2hwOptions()`, but allowed here in cases where the global options
#'    should be overridden but not modified.
#'
#' @return `numeric` vector of hue values after applying the hue warp
#'    operation.
#'
#' @examples
#' # It can be useful to create a uniform sequence of angles in warped
#' # hues, which are visually more uniform than those using an RGB color wheel,
#' # then convert those hues to standard color hues.
#' warpedHues <- seq(from=0, to=330, length.out=12);
#' warpedHues;
#'
#' # rgb imposes no change
#' hues <- hw2h(warpedHues, preset="rgb");
#' hues;
#'
#' # ryb imposes changes
#' hues <- hw2h(warpedHues, preset="ryb");
#' hues;
#'
#' @family colorjam hue warp
#'
#' @export
hw2h <- function
(h,
 h1=NULL,
 h2=NULL,
 direction=1,
 preset=getOption("colorjam.preset", "custom"),
 ...)
{
   ## maps weighted hue to an unweighted hue, based upon the guidepoints
   ## given by h1 (reference hue) and h2 (weighted hue), on a scale
   ## of 1 to 360
   if (length(preset) == 0) {
      preset <- "custom";
   }
   if ("custom" %in% preset && (length(h1) == 0 || length(h2) == 0)) {
      cli::cli_abort(message=paste(
         "{.var preset} must not be \"{.field custom}\" when",
         "{.var h1} and {.var h2} are not provided."))
   }
   if (!"custom" %in% preset) {
      h1h2 <- colorjam_presets(preset=preset);
      h1 <- h1h2$h1;
      h2 <- h1h2$h2;
      direction <- h1h2$direction;
   } else {
      h1h2 <- jamba::rmNULL(validate_colorjam_preset(preset=preset,
         h1=h1,
         h2=h2,
         direction=direction,
         default_step=NULL))
   }
   h1 <- h1h2$h1;
   h2 <- h1h2$h2;
   direction <- h1h2$direction;

   hNew <- approx_degrees(
      h1=h2,
      h2=h1,
      h=h,
      direction=direction,
      preset="custom");

   return(hNew);
}

#' Adjust the color hue warp effect
#'
#' Adjust the color hue warp effect, experimental
#'
#' This function is currently being tested as an approach to adjust
#' the position and order of the warp color hues. For example,
#' the initial use case is to "rotate" the color wheel so the starting
#' color is not always red. Also, the color wheel can be reversed
#' so the color sequence is reversed.
#'
#' @family colorjam hue warp
#'
#' @returns `list` of color warp angles with elements `"h1"` and `"h2"`,
#'    suitable for use by `h2hw()` and `hw2h()`.
#'
#' @param h1,h2 `numeric` or `NULL`
#' @param preset `character` string used to define `h1` and `h2` when those
#'    values are not defined specifically.
#' @param h1_shift `numeric` angle in degrees to shift the `h1` hue.
#'    It is recommended to shift `h2` and not `h1`.
#' @param h2_shift `numeric` angle in degrees to shift the `h2` hue.
#'    It is recommended to shift `h2` and not `h1`.
#' @param reverse_h2 `logical` indicating whether to reverse the order
#'    of values in `h2`.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' new_h1h2 <- adjust_hue_warp(preset="dichromat", h2_shift=0, reverse_h2=FALSE)
#' add_colorjam_preset("temp", h1=new_h1h2$h1, h2=new_h1h2$h2)
#' rj_0 <- rainbowJam(n=24, preset="temp", step="v23")
#' color_pie(rj_0, radius=1,
#'    main="dichromat color wheel\nstep='v23'")
#'
#' rj_0 <- rainbowJam(n=24, preset="temp", step="v23", phase=c(1,2,4,3,6,5))
#' color_pie(rj_0, radius=1,
#'    main="dichromat color wheel\nstep='v23'\ncustom phase")
#'
#' n <- 24;
#' new_h1h2 <- adjust_hue_warp(preset="dichromat", h2_shift=-120, reverse_h2=FALSE)
#' add_colorjam_preset("temp", h1=new_h1h2$h1, h2=new_h1h2$h2)
#' rj_120 <- rainbowJam(n=n, preset="temp", step="v23",
#'    nameStyle="n")
#' color_pie(rj_120, radius=1,
#'    main="dichromat color wheel rotated -120 degrees\nstep='v23'")
#'
#' new_h1h2 <- adjust_hue_warp(preset="dichromat", h2_shift=0, reverse_h2=TRUE)
#' add_colorjam_preset("temp1", h1=new_h1h2$h1, h2=new_h1h2$h2)
#' rj_0rev <- rainbowJam(n=n, preset="temp1", step='v23')
#' names(rj_0rev) <- seq_len(n)
#' color_pie(rj_0rev, radius=1, main="dichromat color wheel (reversed)")
#'
#' new_h1h2 <- adjust_hue_warp(preset="dichromat", h2_shift=90, reverse_h2=FALSE)
#' add_colorjam_preset("temp2", h1=new_h1h2$h1, h2=new_h1h2$h2)
#' rj_90 <- rainbowJam(n=n, preset="temp2", step='v23')
#' color_pie(rj_90, radius=1,
#'    main="color wheel rotated 90 degrees\nstep='v23'")
#'
#' # RGB rotated to start at yellow, then red, then blue
#' new_h1h2 <- adjust_hue_warp(preset="rgb", h2_shift=-70, reverse_h2=TRUE)
#' add_colorjam_preset("temp3", h1=new_h1h2$h1, h2=new_h1h2$h2)
#' n <- 10
#' rgb_rev <- rainbowJam(n=n,
#'    preset="temp3", step='v24')
#' color_pie(rgb_rev,
#'    main="RGB color wheel rotated -30 degrees (reversed)\nstep='v24'")
#'
#' # same as above except using ryb3
#' ryb_h1h2 <- adjust_hue_warp(preset="ryb", h2_shift=-110, reverse_h2=TRUE)
#' add_colorjam_preset("temp4", h1=ryb_h1h2$h1, h2=ryb_h1h2$h2)
#' n <- 10
#' ryb_rev <- rainbowJam(n=n,
#'    #phase=c(1,4,5,2,6,3),
#'    preset="temp4", step='v24')
#' color_pie(ryb_rev,
#'    main="RYB color wheel rotated -110 degrees (reversed)\nstep='v24'")
#'
#' @export
adjust_hue_warp <- function
(h1=NULL,
 h2=NULL,
 preset=getOption("colorjam.preset", "custom"),
 h1_shift=0,
 h2_shift=0,
 reverse_h2=FALSE,
 ...)
{
   #
   if (length(preset) == 0) {
      preset <- "custom";
   }
   h1h2 <- h2hwOptions(preset=preset,
      h1=h1,
      h2=h2,
      setOptions="FALSE")
   h1h2_df <- jamba::mixedSortDF(data.frame(
      h1=h1h2$h1,
      h2=h1h2$h2));
   h1 <- h1h2_df$h1;
   h2 <- h1h2_df$h2;

   # shift each color vector
   if (length(h1_shift) > 0) {
      h1 <- h1 + h1_shift;
   }
   if (length(h2_shift) > 0) {
      h2 <- h2 + h2_shift;
   }

   # optionally flip h2
   if (TRUE %in% reverse_h2) {
      # h2 <- rev(h2)
      h2 <- 360 - h2
   }

   return(list(
      h1=h1,
      h2=h2))

   # new_h2
   h2_signs <- sign(diff(h2))
   h2_sign <- sign(mean(1e-9 + h2_signs[h2_signs != 0]))

   # new_h1
   h1_min_span <- floor(min(h1)/360) * 360
   h1_max_span <- ceiling(max(h1)/360) * 360
   h1_range_span <- ceiling(diff(range(h1))/360 + 1e-10) * 360;
   new_h1 <- h1;
   new_h2 <- h2;
   if (h1_min_span >= 0) {
      new_h1 <- c(h1 - h1_range_span, new_h1)
      if (h2_sign >= 0) {
         new_h2 <- c(h2 - h1_range_span, new_h2)
      } else {
         new_h2 <- c(h2 + h1_range_span, new_h2)
      }
   }
   # new_h2;diff(new_h2);
   if (h1_max_span <= 360) {
      new_h1 <- c(new_h1, h1 + h1_range_span)
      if (h2_sign >= 0) {
         new_h2 <- c(new_h2, h2 + h1_range_span)
      } else {
         new_h2 <- c(new_h2, h2 - h1_range_span)
      }
   }
   # new_h1
   # diff(new_h1)
   # new_h2
   # diff(new_h2)

   if (h2_sign >= 0) {
      new_h2 <- c(h2 - 360, h2, h2 + 360)
   } else {
      new_h2 <- c(h2 + 360, h2, h2 - 360)
   }
   return(list(
      h1=new_h1,
      h2=new_h2))
}

