

#' rainbow categorical colors using varied luminance and chroma
#'
#' rainbow categorical colors using varied luminance and chroma
#'
#' This function customizes similar functions \code{grDevices::rainbow},
#' [colorspace::rainbow_hcl()], and [scales::hue_pal()] in two main
#' ways:
#'
#' 1. It uses the warped color wheel (see [h2hw()] which compresses the
#' green component of the standard HCL color hue wheel, extending the yellow.
#' 2. It uses a varying luminance and chroma vector which was selected to
#' optimize visual distinctiveness of adjacent colors. There is still a limit
#' to the maximum number of effectively different categorical colors, however
#' this function appears to improve other available methods.
#'
#' This function is also intended to enable use of a custom color wheel,
#' for example a set of color mappings could define color-blind friendly
#' ranges of colors when using the warped hue functions [h2hw()] and
#' [hw2h()]. When `warpHue=TRUE` the values for `h1` and
#' `h2` are used to define a mapping from warped hues to standard
#' hues recognized by [hcl()].
#'
#' @param n `integer` number of categorical colors to return
#' @param preset `character` string matching one entry in `colorjam_presets()`,
#'    which defines the color wheel to use.
#'    define the color wheel, only used when `warpHue=TRUE`.
#' @param step `character` string matching one entry in `colorjam_steps()`,
#'    which defines the sequence of Chroma and Luminance values across
#'    the range of color hues.
#' @param Hstart `numeric` hue to use for the first hue value in the
#'    color sequence. This value represents the first color in the color
#'    wheel defined by `preset`, and colors are arrayed across 360 degrees.
#' @param alpha `numeric` alpha transparency of colors, values ranging from
#'    0 to 1. If multiple values are supplied, they are applied in order to
#'    the categorical colors returned.
#' @param hues `numeric` optional vector with specific hues to use instead
#'    of using `Hstart` and filling the 360 degree color wheel with colors.
#' @param warpHue `logical` (deprecated) formerly to enable or disable the
#'    color warping for custom color wheel.
#'    To disable a custom color wheel use `preset="rgb"`.
#' @param h1,h2 `numeric` (deprecated) in favor of argument `preset` to
#'    define the `h1` and `h2` values.
#'    To use custom values for `h1` and `h2` use `add_colorjam_preset()`
#'    to define a new preset name, then use the name with `preset`.
#' @param Cvals,Lvals `numeric` (deprecated) in favor of argument `step`
#'    to define the sequence of Chroma and Luminance values.
#'    To define custom values, use `add_colorjam_step()` to define a new
#'    step name, then use the name with `step`.
#' @param Crange,Lrange `numeric` optional permitted ranges for
#'    Chroma and Luminance values.
#'    These adjustments may be useful to impose a darker or lighter
#'    set of categorical colors.
#'    * When any Chroma value is outside the given `Crange`, all color Chroma
#'    values are scaled to fit this range using `jamba::normScale()`.
#'    This process scales the lowest observed Chroma to the minimum Crange,
#'    and the highest observed Chroma to the maximum Crange, in order to
#'    preserve intermediate gradient values.
#'    * When any Luminance value is outside the given `Lrange`, all color
#'    Luminance values are scaled to fit this range using `jamba::normScale()`.
#' @param phase `integer` starting step value to use from the sequence
#'    of Chroma and Luminance values defined by `colorjam_steps()`.
#'    * Default `phase=1` begins with the first value; `phase=2` begins
#'    with the second value.
#'    * When `phase` is negative, the Chroma and Luminance values are
#'    each reversed, then the absolute value of `phase` is used.
#'    For example `phase=-1` reverses the sequence, then uses the first value.
#'    So it would begin with the last Chroma value, and the last Luminance
#'    value.
#' @param direction `character` value indicating the direction to travel
#'    around the color wheel, permitting the color wheel to be reversed.
#'    When using `direction="-1"` it may also be helpful to use a negative
#'    `phase=-1`.
#'    * `"1"` (default) travels forward, clockwise around the color wheel
#'    * `"-1"` travels in reverse, counter-clockwise around the color wheel
#' @param do_hue_pad `logical` indicating whether to apply padding to
#'    the end of the color hue sequence. This padding increases distinction
#'    between the first and last colors.
#' @param hue_pad_percent `numeric` value between 0 and 100, used when
#'    `do_hue_pad=TRUE` to apply a padding between the first and last color
#'    hues.
#' @param nameStyle `character` string for the style of name assigned:
#'    * `"none"` assigns no names
#'    * `"n"` assigns names in numerical order
#'    * `"closest_named_color"` assigns the closest matching color from
#'    `named_colors`, calling `closest_named_color()` using `...` for
#'    additional arguments.
#'    * `"closestRcolor"` assigns names from `closestRcolors()`, using `...`
#'    for additional arguments.
#'    * `"hcl"` assigns names using H, C, L values
#'    * `"color"` assigns names by hex color
#' @param min_requested_n `numeric` experimental value which defines the
#'    minimum internal color hues to use when `n` is low. Typically this
#'    argument restricts the first several color hues to prevent unusual
#'    colors.
#' @param doTest `logical` indicating whether to perform a visual test for
#'    `n` number of colors produced.
#' @param verbose `logical` whether to print verbose output
#' @param ... additional arguments are passed to
#'    * `closest_named_color()` when `nameStyle="closest_named_color"`
#'    * `closestRcolor()` when `nameStyle="closestRcolor"`
#'    * `jamba::makeNames()` when `nameStyle` is anything except `"none"`.
#'
#' @return `character` vector of categorical colors
#'
#' @family colorjam core
#'
#' @examples
#' rainbowJam(12);
#'
#' # show colors
#' jamba::showColors(rainbowJam(10));
#'
#' # show colors
#' color_pie(rainbowJam(10));
#'
#' # be fancy and label colors using the closest R named color
#' jamba::showColors(rainbowJam(6, nameStyle="closestRcolor"));
#' # or use the closest R color itself
#' jamba::showColors(names(rainbowJam(6, nameStyle="closestRcolor")));
#'
#' # be fancy and label colors using the closest named_color
#' jamba::showColors(rainbowJam(6, nameStyle="closest_named_color"));
#'
#' # be even fancier and use the nearest named color by its name
#' jamba::showColors(named_colors[names(rainbowJam(6, nameStyle="closest_named_color"))]);
#'
#' # comparison of version 0.0.19.900 and update with version 0.0.20.900
#' cat19 <- rainbowJam_v1(n=12)
#' cat20 <- rainbowJam(n=12)
#' jamba::showColors(list(version19=cat19, version20=cat20))
#'
#' @export
rainbowJam <- function
(n=NULL,
 preset=getOption("colorjam.preset", "dichromat2"),
 step=getOption("colorjam.step", "default"),
 Hstart=0,#12.2,
 alpha=1,
 hues=NULL,
 warpHue=NULL,
 h1=NULL,
 h2=NULL,
 Cvals=NULL,
 Lvals=NULL,
 Crange=NULL,
 Lrange=NULL,
 phase=1,
 direction=c("1", "-1"),
 do_hue_pad=FALSE,
 hue_pad_percent=0,
 nameStyle=c(
    "none",
    "n",
    "closest_named_color",
    "closestRcolor",
    "hcl",
    "color"),
 min_requested_n=3,
 doTest=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to provide categorical colors, using the HCL
   ## smooth colorspace (which offers uniform changes in lightness,
   ## chromaticity(saturation). But for here, we deliberately
   ## fluctuate the lightness and saturation as we rotate around
   ## the rainbow of colors, to help visually separate each color.
   ##
   ##
   nameStyle <- match.arg(nameStyle);
   direction <- match.arg(direction);

   # deprecated arguments
   warn_txt <- character(0);
   if (length(h1) > 0 || length(h2) > 0) {
      warn_txt <- c(warn_txt,
         "!"=paste("{.var h1} and {.var h2} are deprecated,",
            "use argument {.var preset}."))
   }
   if (length(Cvals) > 0 || length(Lvals) > 0) {
      warn_txt <- c(warn_txt,
         "!"=paste("{.var Cvals} and {.var Lvals} are deprecated,",
            "use argument {.var step}."))
   }
   if (length(warpHue) > 0) {
      warn_txt <- c(warn_txt,
         "!"=paste("{.var warpHue} is deprecated,",
            "disable warp hue with {.code preset='rgb'}."))
   }
   if (length(warn_txt) > 0) {
      cli::cli_warn(warn_txt);
   }


   # handle preset
   h1h2 <- colorjam_presets(preset)
   if (length(h1h2) == 0) {
      cli::cli_abort(
         "{.var preset} was not recognized in {.code colorjam_presets()}.")
   }
   h1 <- h1h2$h1;
   h2 <- h1h2$h2;
   if ("default" %in% step) {
      step <- h1h2$default_step;
   }
   direction <- h1h2$direction;
   if (verbose) {
      jamba::printDebug("h1: ", format(round(digits=4, h1)));
      jamba::printDebug("h2: ", format(round(digits=4, h2)));
   }

   if ("-1" %in% direction) {
      direction <- -1;
   } else {
      direction <- 1;
   }
   if (length(do_hue_pad) == 0) {
      do_hue_pad <- FALSE;
   }
   if (TRUE %in% doTest) {
      if (length(n) == 0 && length(hues) == 0) {
         n <- 8;
      }
   }

   ## Only when generating hues will we apply other categorical logic
   if (length(hues) == 0) {
      ## hue_pad is an adjustment that pads the total request number of hues
      ## - when requesting 7 colors, the 7th and 1st have the same Cval,Lval
      ##   so we "pad" the hue so the color hue is more than one hue step away
      ##   This logic is applied for any (multiple of 6) + 1, defined by
      ##   (n %% 6) == 1
      ## - same when the last color is 4
      if (TRUE %in% do_hue_pad) {
         hue_pad <- (
            ((n %% 6) == 1)*(floor(n/6)+0) +
            ((n %% 6) == 4)*(floor(n/6)+1) +
            ((n %% 6) == 5)*(floor(n/6)+1) +
            ((n %% 6) == 0)*(floor(n/6)+0)
         );
      } else {
         hue_pad <- 0;
      }

      ## requested_n is the number of hues to create
      ## - always at least n hues
      ## - always at least 4 hues
      ## - pad using hue_pad to reduce first,last color similarity
      # min_requested_n <- 3;
      requested_n <- max(c(min_requested_n, n));
      if (verbose && hue_pad != 0) {
         jamba::printDebug("rainbowJam(): ",
            "hue_pad:",
            hue_pad);
      }

      ## sequence of hues with optional weighted padding
      hue_wedge <- 360 / sum(c(rep(1, requested_n), hue_pad_percent/100));
      hues <- cumsum(c(
         Hstart,
         rep(hue_wedge, requested_n - 1))) %% 360;

      # alternate subsets when n < requested_n
      if (n < requested_n) {
         if (n == 1) {
            hues <- head(hues, 1);
         } else if (n == 2) {
            hues <- hues[c(1, 3)];
         } else if (n == 3) {
            hues <- hues[c(1, 3, 6)]
         } else if (n == 4) {
            hues <- hues[c(1, 3, 5, 6)];
         } else if (n == 5) {
            hues <- hues[c(1, 3, 4, 5, 6)];
         } else {
            hues <- hues[round(seq(from=1, to=length(hues), length.out=n))];
         }
      }

      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "Generated hues:",
            format(digits=3, hues));
      }
   } else {
      ## If not supplied n, then n=length(hues)
      if (length(n) == 0) {
         n <- length(hues);
      } else {
         ## if supplied n, expand hues to length=n
         hues <- rep(hues,
            length.out=n);
      }
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            " Supplied hues:",
            format(digits=2, hues));
      }
   }

   # apply warp hue logic
   if (verbose) {
      jamba::printDebug("rainbowJam(): ",
         "applying h2hw()");
   }
   if (2 %in% warpHue) {
      jamba::printDebug("rainbowJam(): ",
         "Using warpHue==2 secret defaults.");
      h1 <- c(0, 20,  60, 120, 240, 360);
      h2 <- c(0, 80, 120, 180, 240, 360);
   } else if (3 %in% warpHue) {
      jamba::printDebug("rainbowJam(): ",
         "Using warpHue==2 secret defaults.");
      h1 <- c(0,22,60,120,240,270,360);
      h2 <- c(0,80,150,180,220,290,360);
   }
   hues_in <- hues;
   hues <- hw2h(preset=preset,
      h=hues);
   # print(data.frame(hues_in, hues));# debug
   if (verbose) {
      jamba::printDebug("rainbowJam(): ",
         "   Warped hues:",
         format(digits=2, hues));
   }

   # Define Cvals
   step_list <- colorjam_steps(step);
   if (length(step_list) == 0) {
      cli::cli_abort(
         "{.var step} was not recognized in {.code colorjam_steps()}.")
   }
   # Cvals
   Cvals <- step_list[["C"]];
   if (verbose) {
      jamba::printDebug("rainbowJam(): ",
         " Input Cvals:",
         format(digits=2, Cvals));
   }
   # Optionally scale Cvals by Crange
   if (length(Crange) > 0) {
      Crange <- range(Crange);
      Cvals <- jamba::normScale(Cvals,
         from=Crange[1],
         to=Crange[2]);
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "Scaled Cvals:",
            format(digits=2, Cvals));
      }
   }
   # Lvals
   Lvals <- step_list[["L"]];
   if (verbose) {
      jamba::printDebug("rainbowJam(): ",
         " Input Lvals:",
         format(digits=2, Lvals));
   }
   # Optionally scale Lvals by Lrange
   if (length(Lrange) > 0) {
      Lrange <- range(Lrange);
      Lvals <- jamba::normScale(Lvals,
         from=Lrange[1],
         to=Lrange[2])
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "Scaled Lvals:",
            format(digits=2, Lvals));
      }
   }

   ## Optionally apply phase to offset the Cvals,Lvals
   if (length(phase) == 0) {
      phase <- 1;
   }
   if (length(phase) == 1) {
      if (phase <= 0) {
         Cvals <- rev(Cvals);
         Lvals <- rev(Lvals);
         phase <- jamba::noiseFloor(abs(phase), minimum=1)
      }
      Cphase <- seq(from=phase, by=1, length=length(Cvals))
      Cphase <- (Cphase - 1) %% length(Cvals) + 1;
      Lphase <- seq(from=phase, by=1, length=length(Lvals))
      Lphase <- (Lphase - 1) %% length(Lvals) + 1;
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "Extended phase starting at:",
            phase);
      }
   } else if (length(phase) > 1) {
      if (any(phase <= 0)) {
         Cvals <- rev(Cvals);
         Lvals <- rev(Lvals);
         phase <- jamba::noiseFloor(abs(phase), minimum=1)
      }
      Cphase <- (phase - 1) %% length(Cvals) + 1;
      Lphase <- (phase - 1) %% length(Lvals) + 1;
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "Recycled user-provided phase:",
            phase);
      }
   }
   Cvals <- Cvals[rep(Cphase, length.out=n)]
   Lvals <- Lvals[rep(Lphase, length.out=n)]
   if (length(alpha) == 0) {
      alpha <- 1;
   }
   alpha <- rep(alpha, length.out=n);

   # Generate colors using hcl2col()
   rainbow_set <- jamba::hcl2col(H=hues,
      C=Cvals,
      L=Lvals,
      model="hcl");
   if (any(alpha != 1)) {
      rainbow_set <- jamba::alpha2col(alpha=alpha,
         rainbow_set);
   }

   ## Define names
   if ("n" %in% nameStyle) {
      names(rainbow_set) <- seq_along(rainbow_set);
   } else if ("hcl" %in% nameStyle) {
      # 0.0.25.900 - change to actual HCL values and not input to hcl()
      # because hcl() may change values upon creating a color in gamut.
      rainbow_names <- jamba::pasteByRow(
         x=round(digits=0,
            t(jamba::col2hcl(rainbow_set)[c("H", "C", "L"), , drop=FALSE])),
         includeNames=TRUE,
         sep=" ",
         sepName="")
      names(rainbow_set) <- jamba::makeNames(rainbow_names,
         ...);
   } else if ("color" %in% nameStyle) {
      rainbow_names <- jamba::makeNames(rainbow_set,
         ...);
      names(rainbow_set) <- rainbow_names;
   } else if ("closestRcolor" %in% nameStyle) {
      rainbow_names <- jamba::makeNames(
         closestRcolor(rainbow_set,
            ...));
      names(rainbow_set) <- rainbow_names;
   } else if ("closest_named_color" %in% nameStyle) {
      rainbow_names <- jamba::makeNames(
         names(closest_named_color(rainbow_set,
            ...)));
      names(rainbow_set) <- rainbow_names;
   } else {
      rainbow_set <- unname(rainbow_set);
   }

   # doTest
   if (TRUE %in% doTest) {
      oPar <- par(no.readonly=TRUE);
      par(mfrow=c(2,1));
      rainbow_set <- rainbowJam(n);
      par("mar"=c(1, par("mar")[2:4]));
      print(par("mar"))
      jamba::showColors(rainbow_set,
         sub=paste("n =", n, " colors"),
         adjustMargins=FALSE,
         xaxt="n");
      box()
      title("The more colors, the more likely non-adjacent colors will look similar");
      sub_list <- list();
      if (n >= 4) {
         sub1 <- head(rep(rainbow_set[(seq_along(rainbow_set) %% 6) %in% c(1, 4)], 2),
            ceiling(n*0.45));
         sub_list <- c(sub_list, `dark`=list(sub1));
      }
      if (n >= 5) {
         sub2 <- head(rep(rainbow_set[(seq_along(rainbow_set) %% 6) %in% c(3, 5)], 2),
            ceiling(n*0.45));
         sub_list <- c(sub_list, `bright`=list(sub2));
      }
      if (n >= 6) {
         sub3 <- rep(rainbow_set[(seq_along(rainbow_set) %% 6) %in% c(2, 6)], 2);
         sub_list <- c(sub_list, `mid`=list(sub3));
      }
      firstlast_n <- unique(c(
         tail(seq_along(rainbow_set), 2),
         head(seq_along(rainbow_set), 2)));
      if (length(firstlast_n) > 2) {
         sub4 <- rep(rainbow_set[firstlast_n], 2);
         sub_list <- c(sub_list, `ends`=list(sub4));
      }
      jamba::showColors(sub_list,
         main="Similar subsets of colors",
         adjustMargins=FALSE,
         xaxt="n");
      par(oPar);
      return(invisible(rainbow_set));
   }

   # add optional attribute with preset name
   attr(rainbow_set, "preset") <- preset;
   return(rainbow_set);
}

