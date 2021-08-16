
#' Get hue color warp options
#'
#' Get hue color warp options used to convert to and from warped hues
#'
#' This function retrieves, and/or defines, the `h1` and `h2`
#' hue vectors used to convert between `h1` "hue warp", and
#' `h2` "default hue". The "default" hue is used by standard R
#' functions such as `colorspace::polarLUV()`
#' `grDevices::hcl()`, and `farver::convert_colour()`,
#' based upon the red-green-blue color wheel in typical computer
#' video displays.
#'
#' The `h1` values may represent colors on red-yellow-blue color wheel
#' hue, while `h2` represents the corresponding hue in R "default hue"
#' space. The conversion is non-linear, therefore it warps (bends) the
#' color range accordingly.
#'
#' The output values from this function are used with `approx_degrees()`
#' which either performs the conversion, or returns a function to
#' perform the conversion.
#'
#' There are three `options()` used when available, intended to help
#' custom states to have persistence across other function calls.
#'
#' 1. `options("colorjam.preset")` which keeps track of the current
#' color warp preset.
#' 2. `options("h2hw.h1")` defines the `h1` value.
#' 3. `optinos("h2hw.h2")` defines the `h2` value.
#'
#' Note that `options("h2hw.h1")` and `options("h2hw.h2")` are only
#' used when `preset="custom"`, otherwise the named preset will
#' be used as first priority.
#'
#' To disable the warped hue mechanic, set `preset="none"` or define
#' `h1 = h2` so the conversion is always 1:1.
#'
#' When `preset="custom"` and `h1` and `h2` are not defined, the
#' preset is set to the value of `default_preset`.
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
#'    * `"ryb"` red-yellow-blue;
#'    `"dichromat"` color wheel intended to be color-blind friendly
#'    based upon simulated output as from the  `dichromat::dichromat()` package;
#'    * `"rgb"` or `"none"` both use the hue defined in R as red-green-blue;
#'    * `"ryb1"`,`"ryb2"`,`"ryb3"` experimental red-yellow-blue alternatives
#'    that continue to modify the effect of the huge blue-green range
#'    of hue angles.
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
#' @param h1default,h2default deprecated and ignored in this version
#'    of the function.
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
 setOptions=c("ifnull", "TRUE", "FALSE"),
 h1default=NULL,
 h2default=NULL,
 verbose=FALSE,
 ...)
{
   ## Purpose is to define options("h2hw.h1") and options("h2hw.h2");
   ##
   ## Consider using different default values:
   ## h1 <- c(0,  60, 120, 240, 300, 340, 360);
   ## h2 <- c(0, 100, 160, 240, 330, 350, 360);
   ## These default values further expand colors from blue to purple
   preset <- match.arg(arg=preset,
      choices=eval(formals(h2hwOptions)$preset_names));
   default_preset <- match.arg(arg=default_preset,
      choices=setdiff(eval(formals(h2hwOptions)$preset_names), "custom"));

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

   get_presets <- function(preset, verbose=FALSE) {
      if ("ryb" %in% preset) {
         h2 <- c(0, 30, 60,
            90, 120, 150,
            180, 210, 240,
            270, 300, 330);
         h1 <- c(12.2, 23, 40,
            60, 80.9, 107.3,
            127.7, 180, 245,
            265, 288, 315);
      } else if ("ryb1" %in% preset) {
         h1 <- c(0,  60, 120, 240, 360);
         h2 <- c(0, 120, 180, 240, 360);
      } else if ("dichromat" %in% preset) {
         nudge <- 1e-9;
         h1 <- c(8, 30,
            65, 120, 200,
            240, 260, 280,
            330);
         h2 <- c(0, 79.1,
            118.7+nudge, 118.7+nudge, 118.7+nudge,
            126.6, 185.9, 304.6,
            344.2);
      } else if ("ryb2" %in% preset) {
         h1 <- c(0, 22,  60, 120, 240, 270, 360);
         h2 <- c(0, 80, 150, 180, 220, 290, 360);
      } else if ("ryb3" %in% preset) {
         h1 <- c(12.2, 27.3, 47.0, 66.5, 85.9, 106.3, 131.7,
            223.1, 263.2, 277.2, 307.7, 345.3, 372.2);
         h2 <- seq(from=0, to=360, length.out=13) + 12.2;
         h1 <- c(12.2, 27.3, 47.0, 66.5, 85.9, 106.3, 131.7,
            223.1, 263.2, 277.2, 307.7, 345.3);
         h2 <- head(seq(from=0, to=360, length.out=13), 12) + 12.2;
      } else if (any(c("none", "rgb") %in% preset)) {
         h1 <- c(0, 360);
         h2 <- c(0, 360);
      } else {
         stop(paste0("preset not recognized: '", preset, "'"));
      }
      if (verbose) {
         jamba::printDebug("h2hwOptions(): ",
            c("preset '", preset, "'"), sep="");
      }
      return(list(h1=h1, h2=h2));
   }

   if (length(reset) > 0 && reset) {
      preset <- default_preset;
   }

   if ("custom" %in% preset) {
      if (h1h2_undefined) {
         preset <- default_preset;
      }
   }
   if (!"custom" %in% preset) {
      ## use preset values
      h1h2_presets <- get_presets(preset,
         verbose=verbose>1);
      h1 <- h1h2_presets$h1;
      h2 <- h1h2_presets$h2;
   }

   if ("TRUE" %in% setOptions || (h1h2_undefined && "ifnull" %in% setOptions)) {
      if (verbose) {
         jamba::printDebug("h2hwOptions(): ",
            "Updated options()");
      }
      options("colorjam.preset"=preset);
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
 h1=NULL,
 h2=NULL,
 #h1=h2hwOptions()$h1,
 #h2=h2hwOptions()$h2,
 preset="custom",
 ...)
{
   ## maps hue to a weighted hue, based upon the guidepoints
   ## given by h1 (reference hue) and h2 (weighted hue), on a scale
   ## of 1 to 360
   if (length(preset) == 0) {
      preset <- "custom";
   }
   preset <- match.arg(preset,
      choices=eval(formals(h2hwOptions)$preset_names));
   if (length(h1) > 0 && length(h2) > 0 && length(h1) == length(h2)) {
      h1h2 <- h2hwOptions(preset=preset,
         h1=h1,
         h2=h2,
         setOptions="FALSE");
   } else {
      h1h2 <- h2hwOptions(preset=preset,
         setOptions="FALSE");
   }
   h1 <- h1h2$h1;
   h2 <- h1h2$h2;

   hNew <- approx_degrees(h1,
      h2,
      h);
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
 h1=NULL,
 h2=NULL,
 #h1=h2hwOptions()$h1,
 #h2=h2hwOptions()$h2,
 preset="custom",
 ...)
{
   ## maps weighted hue to an unweighted hue, based upon the guidepoints
   ## given by h1 (reference hue) and h2 (weighted hue), on a scale
   ## of 1 to 360
   if (length(preset) == 0) {
      preset <- "custom";
   }
   preset <- match.arg(preset,
      choices=eval(formals(h2hwOptions)$preset_names));
   if (length(h1) > 0 && length(h2) > 0 && length(h1) == length(h2)) {
      h1h2 <- h2hwOptions(preset=preset,
         h1=h1,
         h2=h2,
         setOptions="FALSE");
   } else {
      h1h2 <- h2hwOptions(preset=preset,
         setOptions="FALSE");
   }

   h1 <- h1h2$h1;
   h2 <- h1h2$h2;

   hNew <- approx_degrees(h2,
      h1,
      h);
   #hNew <- approx(x=h2,
   #   y=h1,
   #   ties="ordered",
   #   xout=(h %% 360))$y;
   return(hNew);
}


