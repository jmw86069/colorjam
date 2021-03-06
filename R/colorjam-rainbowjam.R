

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
#' @param Hstart `numeric` hue to use for the first hue value in the
#'    color sequence. Standard red has a hue 12.2, which is the default.
#' @param alpha `numeric` alpha transparency of colors, values ranging from
#'    0 to 1. If multiple values are supplied, they are applied in order to
#'    the categorical colors returned.
#' @param hues `numeric` vector of hues to use, only useful when the
#'    exact hues should be used instead of taking slices along a hue color
#'    wheel. By default, `warpHue=FALSE` if not otherwise defined, when
#'    `hues` is supplied. However, then `warpHue=TRUE` then the `hues`
#'    will be adjusted using `h2hw()`.
#' @param warpHue `logical` indicating whether to apply a custom color wheel,
#'    which "warps" the hue using `h2hwOptions()`, by applying either
#'    `preset`, or specific `h1` and `h2` arguments.
#' @param preset `character` string passed to `h2hwOptions()` to
#'    define the color wheel, only used when `warpHue=TRUE`.
#' @param h1,h2 `numeric` vectors as used by `h2hw()` and `hw2h()` to
#'    convert from warped hues to standard hues. The default values define
#'    red-yellow-blue (additive) color space, which is converted to
#'    red-green-blue color space to produce the actual R color.
#' @param Cvals,Lvals `numeric` vector of chroma (C) and luminance (L) values
#'    to be cycled when creating colors along the vector of color hues. These
#'    values are intended to maximize visual distinctiveness of adjacent and
#'    nearly-adjacent colors. For example, varying from bright to dark may
#'    provide additional distinction between two similar color hues.
#' @param Crange,Lrange vector of two numeric values which define the allowable
#'    chroma (C) and luminance (L) ranges for \code{Cvals} and \code{Lvals}
#'    parameter values. If supplied, the numeric vector Cvals will be scaled
#'    so the lowest Cvals value maps to the first value in Crange, and the
#'    highest Cvals value maps to the last value in Crange. Varying the
#'    Crange and Lrange values can help produce categorical colors on a
#'    dark or light background, by changing the range of values being used.
#' @param phase `integer` value which indicates the first value to use
#'    from the sequence of `Cvals`,`Lvals` values, where `phase=1` starts
#'    with the first value for each, `phase=2` starts with the second
#'    value from each, etc. When `phase` is negative, the order of
#'    `Cvals` and `Lvals` is reversed.
#' @param direction `character` value indicating the direction to travel
#'    around the color wheel, where `"1"` travels forward (red-yellow-blue),
#'    and `"-1"` travels in reverse (red-blue-yellow).
#' @param do_hue_pad `logical` indicating whether to apply padding to
#'    the color hue sequence, intended mainly so the last color hue
#'    in a sequence is less similar to the first color hue in the
#'    sequence.
#' @param hue_pad_percent `numeric` value between 0 and 100, used when
#'    `do_hue_pad` to apply a padding between the first and last color
#'    hues.
#' @param nameStyle `character` value indicating how to name the output
#'    colors: "none" returns colors with no names; "hcl" assigns names with
#'    the color number prefix, followed by H, C, L values; "colors" names the
#'    vector by the hex color code.
#' @param doTest `logical` indicating whether to perform a visual test for
#'    the \code{n} number of colors produced, which helps judge the
#'    visual distinctiveness of different combinations of dark and light
#'    colors.
#' @param verbose `logical` whether to print verbose output
#' @param ... additional arguments are ignored.
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
#' jamba::showColors(names(rainbowJam(6, nameStyle="closestRcolor")));
#'
#' # comparison of version 0.0.19.900 and update with version 0.0.20.900
#' cat19 <- rainbowJam(n=12,
#'    Cvals=c(140, 150, 160, 130, 200, 100),
#'    Lvals=c( 47,  85,  62,  42,  77,  54))
#' cat20 <- rainbowJam(n=12,
#'    Cvals=c(200, 120, 160,  90, 180, 150),
#'    Lvals=c( 44,  88,  74,  58,  80,  65))
#' jamba::showColors(list(version19=cat19, version20=cat20))
#'
#' @export
rainbowJam <- function
(n=NULL,
 Hstart=12.2,
 alpha=1,
 hues=NULL,
 warpHue=NULL,
 preset=c("dichromat", "ryb", "ryb2", "ryb3", "rgb", "none"),
 h1=NULL,
 h2=NULL,
 #Cvals=c(140, 150, 160, 130, 200, 100),
 Cvals=c(200, 120, 160,  90, 180, 150),
 #Lvals=c( 47,  85,  62,  42,  77,  54),
 Lvals=c( 44,  88,  74,  58,  80,  65),
 Crange=NULL,
 Lrange=NULL,
 phase=1,
 direction=c("1", "-1"),
 do_hue_pad=FALSE,
 hue_pad_percent=50,
 nameStyle=c(
    "none",
    "n",
    "hcl",
    "color",
    "closestRcolor"),
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
   if (length(warpHue) == 0) {
      if (length(hues) > 0) {
         warpHue <- FALSE;
      } else {
         warpHue <- TRUE;
      }
   }
   if (length(h1) == 0 || length(h2) == 0) {
      preset <- match.arg(preset);
      h1h2 <- h2hwOptions(preset=preset,
         setOptions="FALSE",
         verbose=verbose);
      h1 <- h1h2$h1;
      h2 <- h1h2$h2;
   }
   if ("-1" %in% direction) {
      direction <- -1;
   } else {
      direction <- 1;
   }
   if (length(do_hue_pad) == 0) {
      do_hue_pad <- FALSE;
   }
   if (doTest) {
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
      if (do_hue_pad) {
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
      requested_n <- max(c(4, n));
      if (verbose && hue_pad != 0) {
         jamba::printDebug("rainbowJam(): ",
            "hue_pad:",
            hue_pad);
      }

      ## sequence of hues with optional weighted padding
      hue_wedge <- 360 / sum(c(rep(1, requested_n), hue_pad_percent/100));
      hues <- cumsum(c(Hstart, rep(hue_wedge * direction, n - 1))) %% 360;

      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "Generated hues:",
            format(digits=2, hues));
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
            "Using supplied hues:",
            format(digits=2, hues));
      }
   }

   ## Optionally apply warp hue logic
   if (warpHue) {
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "applying h2hw()");
      }
      if (warpHue == 2) {
         jamba::printDebug("rainbowJam(): ",
            "Using warpHue==2 secret defaults.");
         h1 <- c(0, 20,  60, 120, 240, 360);
         h2 <- c(0, 80, 120, 180, 240, 360);
      } else if (warpHue == 3) {
         jamba::printDebug("rainbowJam(): ",
            "Using warpHue==2 secret defaults.");
         h1 <- c(0,22,60,120,240,270,360);
         h2 <- c(0,80,150,180,220,290,360);
      }
      hues_in <- hues;
      hues <- hw2h(hues,
         h1=h1,
         h2=h2);
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "warped hues:");
         print(data.frame(hues_in=hues_in,
            hues_out=hues));
      }
   }

   ## Define Cvals
   if (length(Cvals) == 0) {
      ## If empty, use function default
      Cvals <- eval(formals(colorjam::rainbowJam)$Cvals);
   }
   if (verbose) {
      jamba::printDebug("rainbowJam(): ",
         "Cvals:",
         format(digits=2, Cvals));
   }
   ## Optionally force Cvals to fit range Crange
   if (length(Crange) > 0) {
      Crange <- range(Crange);
      Cvals <- jamba::normScale(Cvals,
         from=Crange[1],
         to=Crange[2]);
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "Rescaled Cvals:",
            format(digits=2, Cvals));
      }
   }

   ## Define Lvals
   if (length(Lvals) == 0) {
      ## If empty, use function default
      Lvals <- eval(formals(colorjam::rainbowJam)$Lvals);
   }
   if (verbose) {
      jamba::printDebug("rainbowJam(): ",
         "Lvals:",
         format(digits=2, Lvals));
   }
   ## Optionally force Lvals to fit range Lrange
   if (length(Lrange) > 0) {
      Lrange <- range(Lrange);
      Lvals <- jamba::normScale(Lvals,
         from=Lrange[1],
         to=Lrange[2]);
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "Rescaled Lvals:",
            format(digits=2, Lvals));
      }
   }

   ## Optionally apply phase to offset the Cvals,Lvals
   if (phase <= 0) {
      Cvals <- rev(Cvals);
      Lvals <- rev(Lvals);
      phase <- abs(phase) + 5;
   }
   Cphase <- ((phase - 1) %% length(Cvals)) + 1;
   if (Cphase != 1) {
      Cvals <- head(tail(rep(Cvals, 2), 1 - Cphase), length(Cvals))
   }
   Lphase <- ((phase - 1) %% length(Lvals)) + 1;
   if (Lphase != 1) {
      Lvals <- head(tail(rep(Lvals, 2), 1 - Lphase), length(Lvals))
   }

   ## Expand Cvals,Lvals,alpha to length=n
   Cvals <- rep(Cvals, length.out=n);
   Lvals <- rep(Lvals, length.out=n);
   alpha <- rep(alpha, length.out=n);

   ## Generate colors using HCL definitions
   ## Note we always use model="hcl" which uses R package farver
   ## equivalent to colorspace fixup=TRUE
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
   } else if (nameStyle %in% "hcl") {
      rainbow_names <- jamba::makeNames(paste(seq_len(n),
         paste0("h", signif(hues, digits=2)),
         paste0("c", signif(Cvals, digits=2)),
         paste0("l", signif(Lvals, digits=2)),
         sep=" "));
      names(rainbow_set) <- rainbow_names;
   } else if (nameStyle %in% "color") {
      rainbow_names <- jamba::makeNames(rainbow_set);
      names(rainbow_set) <- rainbow_names;
   } else if (jamba::igrepHas("close", nameStyle)) {
      rainbow_names <- jamba::makeNames(closestRcolor(rainbow_set));
      names(rainbow_set) <- rainbow_names;
   } else {
      rainbow_set <- unname(rainbow_set);
   }
   ## Test with doTest=TRUE
   if (doTest) {
      oPar <- par(no.readonly=TRUE);
      par(mfrow=c(2,1));
      rainbow_set <- rainbowJam_dev(n);
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

   return(rainbow_set);
}

#' Show colors spread around a pie chart
#'
#' Show colors spread around a pie chart
#'
#' This function simply displays colors in a pie chart
#' format.
#'
#' If the input is a `list`, each list is used to produce
#' layers of a pie chart, in order to help compare colors
#' from each vector in the list.
#'
#' @family colorjam display
#'
#' @param colors `vector` of R colors.
#' @param border `vector` of R colors used to draw a border around
#'    each pie wedge. By default it uses input `colors`.
#' @param lwd numeric value used to define the line width of the
#'    pie wedge borders.
#' @param radius numeric value representing the radius of the
#'    overall pie chart, where `radius=1` represents the default
#'    radius used by `graphics::pie()`. The default is `radius=1.5`
#'    in order to use more of the output plot size.
#' @param label_radius numeric value indicating the radius used
#'    for labels, intended to allow labels to appear inside each
#'    pie wedge.
#' @param add logical indicating whether to draw the pie chart
#'    onto the existing plot device, without creating a new plot.
#' @param ... additional arguments are passed to `graphics::pie()`.
#'
#' @examples
#' color_pie(rainbowJam(20, nameStyle="none"),
#'    sub="rainbowJam(20)")
#'
#' n <- 8;
#' color_pie(rainbowJam(n),
#'    sub="rainbowJam(8)")
#'
#' color_pie(rainbow(n),
#'    sub="rainbow(8)")
#'
#' color_pie(colorspace::rainbow_hcl(n),
#'    sub="colorspace::rainbow_hcl(8)")
#'
#' color_pie(colorspace::rainbow_hcl(n, c=120),
#'    sub="colorspace::rainbow_hcl(8, c=120)")
#'
#' color_list <- list(rainbowJam=rainbowJam(n),
#'    rainbow_hcl=colorspace::rainbow_hcl(n, c=120));
#' color_pie(color_list,
#'    sub="inside ring:rainbow_hcl()\nouter ring: rainbowJam()")
#'
#' rainbow_list <- lapply(4*c(5,4,2,1), function(n){
#'    rainbowJam(n, nameStyle="none");
#' });
#' color_pie(rainbow_list,
#'    sub="rainbowJam()\nn=4, 8, 16, 20")
#'
#' @export
color_pie <- function
(colors,
 border=colors,
 lwd=2,
 radius=1.5,
 label_radius=radius*0.75,
 add=FALSE,
 ...)
{
   ##
   if (is.list(colors)) {
      radius_seq <- head(
         seq(from=radius,
            to=0.3,
            length.out=length(colors)+1),
         length(colors));
      if (!is.list(border)) {
         border <- as.list(border);
      }
      radius_diff <- head(c(diff(radius_seq)/2, -radius*0.25), 1);
      border <- rep(border,
         length.out=length(colors));
      l <- lapply(seq_along(colors), function(i){
         if (i == 1) {
            color_pie(colors=colors[[i]],
               border=border[[i]],
               lwd=lwd,
               add=(i > 1),
               radius=radius_seq[i],
               label_radius=radius_seq[i]*0.92 + radius_diff,
               ...);
         } else {
            color_pie(colors=colors[[i]],
               border=border[[i]],
               lwd=lwd,
               add=(i > 1),
               radius=radius_seq[i],
               label_radius=radius_seq[i]*0.92 + radius_diff);
         }
      });
      return(invisible(l));
   }
   op <- par(no.readonly=TRUE);
   on.exit(par(op));
   par("xpd"=TRUE);
   par("lwd"=lwd);
   if (length(colors) == 1) {
      par("lwd"=0.001);
   }
   if (add) {
      par("new"=TRUE);
   }
   pie(x=rep(1, length.out=length(colors)),
      col=colors,
      border=border,
      labels="",
      lwd=lwd,
      radius=radius,
      ...);
   if (length(names(colors)) > 0) {
      par("new"=TRUE);
      par("lwd"=0.001);
      pie(x=rep(1, length.out=length(colors)),
         col="transparent",
         border=FALSE,
         radius=label_radius);
   }
   invisible(colors);
}
