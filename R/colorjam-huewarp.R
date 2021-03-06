
#' Get hue color warp options
#'
#' Get hue color warp options used to convert to and from warped hues
#'
#' This function simply retrieves the vector of hues in normal hue color
#' space, and warped color space. It is intended to convert from something
#' like a color wheel defined by RGB (red, green blue) where red=0, green=120,
#' and blue=240, to a color wheel like RYB (red, yellow, blue) where red=0,
#' yellow=120, and blue=240, (green becomes 180). This conversion is
#' non-linear, meaning it warps (bends) the colors.
#'
#' The purpose of using `options()` to store the color hue is to make it
#' easy to re-use the settings across multiple function calls. The related
#' reason for this function `h2hwOptions()` is to make it clear the
#' default values, and make it clear how to update and review the current
#' settings.
#'
#' Together the vector `h1` and `h2` are used by `stats::approx()` to
#' convert from the value ranges in `h1` to the corresponding value
#' ranges in `h2`. Using this mechanism, one could define ranges which
#' effectively remove entire slices of the color wheel.
#'
#' To disable the warped hue mechanism, set `h2` equal to `h1` for example
#' `h2hwOptions(h2=h2hwOptions()$h1)`. Doing so will cause `stats::approx()`
#' to interpret every hue as a 1:1 relationship with the warped hue.
#'
#' @family colorjam hue warp
#'
#' @param h1 NULL or numeric vector of color hue values, sequential between
#'    0 and 360.
#' @param h2 NULL or numeric vector of color hue values, sequential between
#'    0 and 360.
#' @param h1default,h2default numeric vector of color hue values to use when
#'    h1 or h2 are NULL, respectively. The default values are
#'    included here as a function parameter here for visibility.
#' @param preset `character` string indicating whether to define the
#'    `h1`, and `h2` values based upon named presets: `"none"` no change;
#'    `"ryb"` red-yellow-blue; `"dichromat"` color wheel that omits green,
#'    therefore provides relatively even spacing of color-blind-friendly
#'    colors based upon simulated output as produced by
#'    `dichromat::dichromat()` package; `"rgb"` red-green-blue which
#'    is the default color wheel used in R; `"ryb2"` alternate red-yellow-blue
#'    that slightly over-emphasizes yellow at the expense/benefit of
#'    even less green.
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
 h1default=c(0, 60,120,240,360),
 h2default=c(0,120,180,240,360),
 preset=c("none", "dichromat", "ryb", "rgb", "ryb2", "ryb3"),
 default_preset="dichromat",
 reset=FALSE,
 setOptions=c("ifnull", "TRUE", "FALSE"),
 verbose=FALSE,
 ...)
{
   ## Purpose is to define options("h2hw.h1") and options("h2hw.h2");
   ##
   ## Consider using different default values:
   ## h1 <- c(0,  60, 120, 240, 300, 340, 360);
   ## h2 <- c(0, 100, 160, 240, 330, 350, 360);
   ## These default values further expand colors from blue to purple
   preset <- match.arg(preset);
   if (length(setOptions) > 0 && is.logical(setOptions)) {
      setOptions <- as.character(setOptions);
   }
   setOptions <- match.arg(setOptions);
   h1h2_undefined <- (length(h1) == 0 || length(h2) == 0);
   if (verbose) {
      jamba::printDebug("h2hwOptions(): ",
         "h1h2_undefined:", h1h2_undefined);
   }
   ## Order of operation:
   ## 1. reset=TRUE forces h1=h1default and h2=h2default
   ## 2. preset="something" forces h1, h2 to appropriate preset values
   ## 3. preset="none" and h1h2_undefined, uses default_preset
   ## 4. preset="none" and !h1h2_undefined uses h1,h2 as provided

   get_presets <- function(preset, verbose=FALSE) {
      if ("ryb" %in% preset) {
         h1 <- eval(formals(h2hwOptions)$h1default);
         h2 <- eval(formals(h2hwOptions)$h2default);
         if (verbose) {
            jamba::printDebug("h2hwOptions(): ",
               c("preset '", "ryb", "', reset=TRUE"), sep="");
         }
      } else if ("dichromat" %in% preset) {
         h1 <- c(0, 8, 30,   65,    120,   200,   240,   260,   280,   330,   360);
         h2 <- c(0, 0, 79.1, 118.7, 118.7, 118.7, 126.6, 185.9, 304.6, 344.2, 360);
         if (verbose) {
            jamba::printDebug("h2hwOptions(): ",
               c("preset '", "dichromat", "'"), sep="");
         }
      } else if ("ryb2" %in% preset) {
         h1 <- c(0, 22,  60, 120, 240, 270, 360);
         h2 <- c(0, 80, 150, 180, 220, 290, 360);
      } else if ("ryb3" %in% preset) {
         h1 <- c(12.2, 27.3, 47.0, 66.5, 85.9, 106.3, 131.7,
            223.1, 263.2, 277.2, 307.7, 345.3, 372.2);
         h2 <- seq(from=0, to=360, length.out=13) + 12.2;
      } else {
      #} else if ("rgb" %in% preset) {
         h1 <- c(0, 360);
         h2 <- c(0, 360);
      }
      return(list(h1=h1, h2=h2));
   }

   if (length(reset) > 0 && reset) {
      h1 <- h1default;
      h2 <- h2default;
   } else if (!"none" %in% preset) {
      ## use preset values
      h1h2_presets <- get_presets(preset, verbose);
      h1 <- h1h2_presets$h1;
      h2 <- h1h2_presets$h2;
   } else if (h1h2_undefined) {
      ## use default_preset
      preset_avail <- setdiff(eval(formals(h2hwOptions)$preset), "none");
      if (length(default_preset) == 0) {
         default_preset <- head(preset_avail, 1);
      }
      if (!default_preset %in% eval(formals(h2hwOptions)$preset)) {
         stop(paste0("default_preset must be one value from preset: ",
            paste(paste0("'", preset_avail, "'"), collapse=", ")));
      }
      h1h2_presets <- get_presets(default_preset, verbose);
      h1 <- h1h2_presets$h1;
      h2 <- h1h2_presets$h2;
   } else {
      ## use getOption()
      ## h1,h2 as provided in function call
   }

   ## Integrity check of h1,h2 values
   if (1 == 2) {
      if (length(h1) != length(h2) || length(h1) < 2) {
         stop("h1,h2 values are not valid: must have length(h1) >= 2, and length(h1)==length(h2)");
      }
      if (max(round(h1)) < 360 || max(round(h2)) < 360) {
         stop("h1,h2 values are not valid: must have max(h1)==360 and max(h2)==360");
      }
   }

   if ("TRUE" %in% setOptions || (h1h2_undefined && "ifnull" %in% setOptions)) {
      if (verbose) {
         jamba::printDebug("h2hwOptions(): ",
            "Updated options()");
      }
      options("h2hw.h1"=h1);
      options("h2hw.h2"=h2);
   }
   list(h1=h1,
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
#' h2hw(60);
#'
#' # RGB colors are convenient, but are not ideal especially when blending
#' # colors. Note that blue and yellow have hues that differ by exactly 180
#' # degrees, meaning a hue average is as likely to be purple as green.
#' huesBY <- jamba::col2hcl(c("blue", "yellow"))["H",];
#' huesBY;
#' warpedHuesBY <- h2hw(huesBY);
#' warpedHuesBY;
#'
#' @family hue warp functions
#'
#' @export
h2hw <- function
(h,
 h1=h2hwOptions()$h1,
 h2=h2hwOptions()$h2,
 preset=NULL,
 ...)
{
   ## maps hue to a weighted hue, based upon the guidepoints
   ## given by h1 (reference hue) and h2 (weighted hue), on a scale
   ## of 1 to 360
   if (length(preset) > 0) {
      h1h2 <- h2hwOptions(preset=preset,
         setOptions="FALSE");
      h1 <- h1h2$h1;
      h2 <- h1h2$h2;
   }
   hNew <- approx_degrees(h1,
      h2,
      h)
   #hNew <- approx(x=h1,
   #   y=h2,
   #   ties="ordered",
   #   xout=(h %% 360))$y;
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
#' hues <- hw2h(warpedHues);
#' hues;
#'
#' @family hue warp functions
#'
#' @export
hw2h <- function
(h,
 h1=h2hwOptions()$h1,
 h2=h2hwOptions()$h2,
 preset=NULL,
 ...)
{
   ## maps weighted hue to an unweighted hue, based upon the guidepoints
   ## given by h1 (reference hue) and h2 (weighted hue), on a scale
   ## of 1 to 360
   if (length(preset) > 0) {
      h1h2 <- h2hwOptions(preset=preset,
         setOptions="FALSE");
      h1 <- h1h2$h1;
      h2 <- h1h2$h2;
   }
   hNew <- approx(x=h2,
      y=h1,
      ties="ordered",
      xout=(h %% 360))$y;
   return(hNew);
}
