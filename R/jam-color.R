
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
#' reason for this function [h2hwOptions()] is to make it clear the
#' default values, and make it clear how to update and review the current
#' settings.
#'
#' Together the vector `h1` and `h2` are used by [stats::approx()] to
#' convert from the value ranges in `h1` to the corresponding value
#' ranges in `h2`. Using this mechanism, one could define ranges which
#' effectively remove entire slices of the color wheel.
#'
#' To disable the warped hue mechanism, set `h2` equal to `h1` for example
#' `h2hwOptions(h2=h2hwOptions()$h1)`. Doing so will cause [stats::approx()]
#' to interpret every hue as a 1:1 relationship with the warped hue.
#'
#' @param h1 NULL or numeric vector of color hue values, sequential between
#'    0 and 360.
#' @param h2 NULL or numeric vector of color hue values, sequential between
#'    0 and 360.
#' @param h1default,h2default numeric vector of color hue values to use when
#'    h1 or h2 are NULL, respectively. The default values are
#'    included here as a function parameter here for visibility.
#' @param reset logical whether to reset `h1` and `h2` values to the defaults,
#'    as defined in `h1defaults` and `h2defaults`, respectively.
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
(h1,
 h2,
 h1default=c(0, 60,120,240,360),
 h2default=c(0,120,180,240,360),
 reset=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to define options("h2hw.h1") and options("h2hw.h2");
   ##
   ## Consider using different default values:
   ## h1 <- c(0,  60, 120, 240, 300, 340, 360);
   ## h2 <- c(0, 100, 160, 240, 330, 350, 360);
   ## These default values further expand colors from blue to purple
   if (length(reset) && reset) {
      h1 <- formals(h2hwOptions)$h1defaults;
      h2 <- formals(h2hwOptions)$h2defaults;
   }
   if (!missing(h1)) {
      if (length(h1) == 0 || !igrepHas("numeric|integer", class(h1))) {
         h1 <- h1default;
      }
      if (verbose) {
         printDebug("h2hwOptions(): ",
            "Defining h2hw.h1=", h1);
      }
      options("h2hw.h1"=h1);
   } else {
      h1 <- getOption("h2hw.h1", default=h1default);
      if (length(h1) == 0 || !igrepHas("numeric|integer", class(h1))) {
         h1 <- h1default;
      }
      options("h2hw.h1"=h1);
   }
   if (!missing(h2)) {
      if (length(h2) == 0 || !igrepHas("numeric|integer", class(h2))) {
         h2 <- h2default;
      }
      if (verbose) {
         printDebug("h2hwOptions(): ",
            "Defining h2hw.h2=", h2);
      }
      options("h2hw.h2"=h2);
   } else {
      h2 <- getOption("h2hw.h2", default=h2default);
      if (length(h2) == 0 || !igrepHas("numeric|integer", class(h2))) {
         h2 <- h2default;
      }
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
#' @param h numeric vector of color hues between 0 and 360. These hues do
#'    not need to be in sequential order.
#' @param h1,h2 vector of color hues, which by default are defined in
#'    [h2hwOptions()], but allowed here in cases where the global options
#'    should be overridden but not modified.
#'
#' @return numeric vector of hue values after applying the hue warp
#'    operation.
#'
#' @return numeric vector of warped color hues.
#'
#' @examples
#' ## Yellow when using an RGB color wheel is 60 degrees,
#' ## but on an RYB color wheel is 120 degrees.
#' h2hw(60);
#'
#' # RGB colors are convenient, but are not ideal especially when blending
#' # colors. Note that blue and yellow have hues that differ by exactly 180
#' # degrees, meaning a hue average is as likely to be purple as green.
#' huesBY <- col2hcl(c("blue", "yellow"))["H",];
#' huesBY;
#' warpedHuesBY <- h2hw(huesBY);
#' warpedHuesBY;
#' warpedHues <- h2hw(hues);
#'
#' @family hue warp functions
#'
#' @export
h2hw <- function
(h,
 h1=h2hwOptions()$h1,
 h2=h2hwOptions()$h2,
 ...)
{
   ## maps hue to a weighted hue, based upon the guidepoints
   ## given by h1 (reference hue) and h2 (weighted hue), on a scale
   ## of 1 to 360
   hNew <- approx(x=h1,
      y=h2,
      xout=(h %% 360))$y;
   return(hNew);
}

#' Convert warped hue to standard hue
#'
#' Convert warped hue to standard hue using the hue warp vectors
#'
#' This function is intended to convert from a vector of warped hue values to
#' the hues defined by the vectors returned by \code{h2hwOptions}.
#' The intent is to convert colors in RYB space into RGB space by default.
#'
#' One example of input would be to supply a uniformly spaced set of color
#' hues, and convert them to "standard" hues by HCL standards. It has the
#' effect of presenting categorical colors, using a non-linear set of
#' "standard" hue values.
#'
#' The default mappings convert RYB (red, yellow, blue) to RGB (red, green,
#' blue). The color yellow has hue=120 in RYB space, so the call to
#' \code{h2hw(120)} results in 60, which is the hue in RGB space.
#'
#' @param h numeric vector of color hues between 0 and 360. These hues do
#'    not need to be in sequential order.
#' @param h1,h2 vector of color hues, which by default are defined in
#'    [h2hwOptions()], but allowed here in cases where the global options
#'    should be overridden but not modified.
#'
#' @return numeric vector of color hues.
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
 ...)
{
   ## maps weighted hue to an unweighted hue, based upon the guidepoints
   ## given by h1 (reference hue) and h2 (weighted hue), on a scale
   ## of 1 to 360
   hNew <- approx(x=h2,
      y=h1,
      xout=(h %% 360))$y;
   return(hNew);
}

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
#' @param n integer number of categorical colors to return
#' @param alpha numeric alpha transparency of colors, values ranging from
#'    0 to 1. If multiple values are supplied, they are applied in order to
#'    the categorical colors returned.
#' @param nfloor the minimum number of effective color slices taken from the
#'    hue color wheel, primarily used as an aesthetic choice so the first two
#'    colors will be reasonably consistent when choosing 1, 2, 3, 4, or 5
#'    categorical colors.
#' @param hues optional numeric vector of hues to use, only useful when the
#'    exact hues should be used instead of taking slices along a hue color
#'    wheel. Note that to use hue values with no modification, one should
#'    also set \code{warpHue=FALSE}, otherwise the given hues are assumed to
#'    warped hue values.
#' @param Cvals,Lvals vector of chroma (C) and luminance (L) values to be
#'    cycled when creating colors along the vector of color hues. These
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
#' @param Hstart the hue to use for the first hue value in the color sequence.
#'    Standard red has a hue 12.2, which is the default for this function.
#' @param doTest boolen indicating whether to perform a visual test for
#'    the \code{n} number of colors produced, which helps judge the
#'    visual distinctiveness of different combinations of dark and light
#'    colors.
#' @param sFactor,darkFactor parameters sent to \code{jamba::makeColorDarker}
#'    if either is not equal to 1. Setting \code{darkFactor=2} is a quick
#'    way of generating categorical border colors, for example drawing a
#'    colored border around categorical colors. Alternatively, setting
#'    \code{sFactor=-2, darkFactor=-2} can be used to desaturate and lighten
#'    colors used for the background area of a rectangle. The \code{alpha}
#'    transparency parameter can also be helpful, however not all graphics
#'    devices support transparency, in which case it is more robust to define
#'    the exact color.
#' @param nameStyle character value indicating how to name the output
#'    colors: "none" returns colors with no names; "hcl" assigns names with
#'    the color number prefix, followed by H, C, L values; "colors" names the
#'    vector by the hex color code.
#' @param h1,h2 numeric vectors as used by [h2hw()] and [hw2h()] to
#'    convert from warped hues to standard hues. The default values define
#'    red-yellow-blue (additive) color space, which is converted to
#'    red-green-blue color space to produce the actual R color.
#' @param verbose logical whether to print verbose output
#'
#' @return vector of colors
#'
#' @examples
#' rainbowJam(12);
#'
#' # show colors
#' showColors(rainbowJam(12));
#'
#' # be fancy and label colors using the closest R named color
#' showColors(rainbowJam(12, nameStyle="colors"));
#'
#' @export
rainbowJam <- function
(n=NULL,
 alpha=1,
 nfloor=4,
 hues=NULL,
 Cvals=c(75,95,80, 65,72,80),
 Lvals=c(62,83,69,58,65,74),
 # Scaling of the ranges above
 Crange=NULL,
 Lrange=NULL,
 Hstart=12.2,
 warpHue=TRUE,
 doTest=FALSE,
 sFactor=1,
 darkFactor=1,
 nameStyle=c("none","hcl","colors"),
 h1=h2hwOptions()$h1,
 h2=h2hwOptions()$h2,
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
   ## Test with doTest=TRUE:
   if (doTest) {
      oPar <- par();
      par(mfrow=c(5,1));
      colSet <- nameVector(rainbowCat(n), 1:n);
      showColors(colSet,
         main=paste("n =", n, " colors"),
         xaxt="n");
      title("The more colors, the more likely non-adjacent colors will look similar");
      showColors(colSet[seq(from=1, to=n, by=3)],
         main="offset by 3 (Dark colors)",
         xaxt="n");
      if (n > 1) {
         fullHiSet <- seq(from=2, to=n, by=3);
         whichHiSet <- fullHiSet[-seq(from=2, to=length(fullHiSet), by=2)];
         showColors(colSet[whichHiSet],
            main="Light colors",
            xaxt="n");
         showColors(colSet[-whichHiSet],
            main="Non-light colors",
            xaxt="n");
         if (n > 2) {
            showColors(colSet[seq(from=3, to=n, by=2)],
               main="offset by 2 + 3 (Medium colors)",
               xaxt="n");
         }
      }
      par(oPar);
      return(NULL);
   }
   if (length(n) == 0) {
      n <- 1;
   }

   if (length(Cvals) == 0) {
      stop("rainbowJam() requires Cvals and Lvals to be defined.");
   }
   if (length(Lvals) == 0) {
      stop("rainbowJam() requires Cvals and Lvals to be defined.");
   }

   ## Generate a color hue sequence
   nHues <- max(c(nfloor, n));
   if (length(hues) == 0) {
      ## hue starts from 0 to 360,
      ## add the Hstart to rotate the color hues,
      ## trim to values between 0 and 360
      hues <- head(
         (seq(from=0,
            to=360,
            length.out=(nHues+1)) + Hstart) %% 360,
         n);
   }

   ## adjust hues to warped hues
   if (warpHue) {
      if (verbose) {
         printDebug("rainbowJam(): ",
            "warping hues");
         ch(data.frame(h1=h1, h2=h2), Inf);
      }
      hues <- hw2h(hues,
         h1=h1,
         h2=h2);
   }

   if (verbose) {
      printDebug("rainbowJam(): ",
         "hues:",
         format(digits=2, hues));
   }

   ## Scale the lVals and cVals ranges to given lRange and cRange if needed
   ## (note we have to do the scaling before taking a subset of values!)
   if (length(Lrange) == 2 && !all(Lrange %in% range(Lvals))) {
      Lvals <- normScale(Lvals,
         from=Lrange[1],
         to=Lrange[2]);
   }
   if (length(Crange) == 2 && !all(Crange %in% range(Cvals))) {
      Cvals <- normScale(Cvals,
         from=Crange[1],
         to=Crange[2]);
   }

   Cvals <- rep(Cvals, length.out=n);
   Lvals <- rep(Lvals, length.out=n);
   alpha <- rep(alpha, length.out=n);

   if (verbose) {
      hclDF <- data.frame(H=hues,
         C=Cvals,
         L=Lvals,
         alpha=alpha);
      printDebug("H: ", format(digits=2, hues));
      printDebug("C:", format(digits=2, Cvals));
      printDebug("L:", format(digits=2, Lvals));
      printDebug("alpha:", alpha);
   }

   ## Generate colors using HCL definitions
   hclSet <- hcl2col(H=hues,
      C=Cvals,
      L=Lvals,
      alpha=alpha);
   if (any(abs(sFactor) != 1) || any(abs(darkFactor) != 1)) {
      if (verbose) {
         printDebug("rainbowJam(): ",
            "applying darkFactor, sFactor with makeColorDarker().")
      }
      hclSet <- makeColorDarker(hclSet,
         darkFactor=postDarkFactor,
         sFactor=postSfactor,
         ...);
   }
   if (nameStyle %in% "hcl") {
      hclNames <- makeNames(paste(seq_len(n),
         paste0("H", signif(hues, digits=3)),
         paste0("C", signif(Cvals, digits=3)),
         paste0("L", signif(Lvals, digits=3)),
         sep="_"));
      names(hclSet) <- hclNames;
   } else if (nameStyle %in% "colors") {
      hclNames <- makeNames(closestRcolor(hclSet));
      names(hclSet) <- hclNames;
   } else {
      hclSet <- unname(hclSet);
   }
   return(hclSet);
}

#' Find the closest R color
#'
#' Find the closest R color for a vector of colors
#'
#' This function is intended as a relatively efficient method to compare
#' a set of colors to the named R colors provided by \code{colors()}.
#' It has some distinction from similar methods, in that it uses HCL
#' color space, unlike similar methods which may use RGB color space.
#' It also prioritizes the color hue, then the luminance (visual brightness),
#' then chroma (saturation). These weights can be adjusted if necessary
#' although the default values appear to work fairly well.
#'
#' @param x character vector of colors, either in hex format or any
#'    valid color in R.
#' @param colorSet vector of colors, by default includes the R colors
#'    provided by \code{colors()} minus the "grey##" greyscale colors,
#'    however any vector of colors can be used, whether named or not.
#' @param showPalette boolean indicating whether to display the input
#'    colors and resulting closest matching colors by using
#'    \code{jamba::showColors}.
#' @param colorModel character color model to use, currently only using
#'    "hcl", though "rgb" and "ryb" were both tested and judged to perform
#'    less effectively.
#' @param Hwt,Cwt,Lwt relative weights for each dimension of HCL colors,
#'    respectively.
#' @param warpHue boolean indicating whether to perform the hue warp
#'    operation using \code{h2hw()} which improves the ability to match
#'    colors between orange and green.
#' @param returnType character type of data to return: "color" will return
#'    the actual closest color; "name" will return the name of the closest
#'    color, or if the \code{colorSet} vector has no names, its values will
#'    be used; "match" will return an integer vector as an index to colors
#'    in \code{colorSet}.
#' @param verbose logical whether to print verbose output
#'
#' @examples
#' closestRcolor(rainbowJam(12), showPalette=TRUE);
#'
#' @export
closestRcolor <- function
(x,
 colorSet=unvigrep("^gr[ae]y($|[0-9]+$)", colors()),
 showPalette=FALSE,
 colorModel=c("hcl"),
 Hwt=2,
 Cwt=1,
 Lwt=5,
 warpHue=TRUE,
 returnType=c("color","name","match"),
 verbose=FALSE,
 ...)
{
   ## Purpose is simply to name a color by its nearest colors from R colors()
   ##
   ## returnType == "color" will return the closest color from colorSet
   ## returnType == "name" will return the name of the closest color from colorSet
   ## returnType == "which" will return to closest match as an index integer
   colorModel <- match.arg(colorModel);
   returnType <- match.arg(returnType);
   classX <- class(x);
   if (classX %in% "data.frame") {
      origXdf <- x;
      origX <- as.vector(as.matrix(origXdf));
   } else if (classX %in% "matrix") {
      origXdf <- x;
      origX <- as.vector(origXdf);
   } else {
      origX <- x;
   }
   if (length(names(origX)) == 0) {
      names(origX) <- makeNames(origX);
   }
   if (returnType %in% "name" && length(names(colorSet)) == 0) {
      names(colorSet) <- makeNames(colorSet);
   }
   x <- nameVector(unique(origX));
   if (colorModel %in% "hcl") {

      # hcl
      # Simple angular distance
      angDist <- function(a, b, ...){
         x1 <- rep(a, length(b));
         y1 <- rep(b, each=length(a));
         diff1 <- abs(x1-y1);
         diff1[diff1 > 180] <- 360 - diff1[diff1 > 180];
         diff1;
         matrix(diff1, ncol=length(b), nrow=length(a),
            dimnames=list(names(a), names(b)));
      }
      xHCL <- col2hcl(x);
      colorSetHCL <- col2hcl(colorSet);

      ## Adjust H to RYB
      if (warpHue) {
         xHCL["H",] <- h2hw(xHCL["H",]);
         colorSetHCL["H",] <- h2hw(colorSetHCL["H",]);
      }

      Hdist <- angDist(a=xHCL["H",], b=colorSetHCL["H",])/180*100;

      CLm <- rbind(t(xHCL), t(colorSetHCL))[,c("L","C"),drop=FALSE];
      CLm[,"C"] <- CLm[,"C"]*Cwt;
      CLm[,"L"] <- CLm[,"L"]*Lwt;
      CLdist <- as.matrix(dist(CLm))[colnames(xHCL),colnames(colorSetHCL),drop=FALSE];
      HCLdist <- Hdist * Hwt + CLdist;
      iClosestColorWhich <- apply(HCLdist, 1, which.min);

      ## Define the proper return value
      if (returnType %in% "match") {
         newX <- iClosestColorWhich;
      } else if (returnType %in% "name") {
         newX <- nameVector(colnames(HCLdist)[iClosestColorWhich], rownames(HCLdist));
      } else {
         newX <- nameVector(colorSet[iClosestColorWhich], rownames(HCLdist));
      }
   }

   retX <- newX[origX];
   if (length(names(origX)) > 0) {
      names(retX) <- names(origX);
   }

   ## Optionally display the palette before and after
   if (showPalette) {
      showColors(list(original=nameVector(origX),
         returned=nameVector(retX)),
         ...);
   }
   ## Return to data.frame or matrix form if needed
   if (classX %in% c("data.frame", "matrix")) {
      retX <- matrix(ncol=ncol(origXdf), retX, dimnames=dimnames(origXdf));
      if (classX %in% c("data.frame")) {
         retX <- as.data.frame(retX);
      }
   }
   return(retX);
}

#' Assign colors to vector of group labels
#'
#' Assign colors to vector of group labels
#'
#' This function takes a character or factor vector as input, then
#' assigns categorical colors to each label using `colorFunc`, by
#' default `rainbowJam()`.
#'
#' If a previous set of colors has already been defined, the parameter
#' `colorSub` is intended to maintain that same set of colors. However,
#' all input values in `x` must be present in the `names(colorSub)`
#' otherwise all colors are reassigned.
#'
#' In future, this function will maintain a partial set of colors,
#' while assigning colors with maximum visible differences from the
#' existing colors.
#'
#' @param x character or factor vector representing group membership.
#' @param alpha numerical value indicating the alpha transparency to
#'    apply to the output colors, scaled from 0 (fully transparent) to
#'    1 (no transparency).
#' @param colorFunc function whose first parameter is the number of
#'    colors to return, and where `...` is passed for additional
#'    parameters as needed. By default it uses `colorjam::rainbowJam()`.
#' @param colorSub optional named vector of colors, whose names must
#'    match all entries in `x`. This vector is used to re-apply
#'    colors which have already been assigned to the labels in `x`.
#' @param useGradient logical indicating whether to apply a light-to-dark
#'    gradient to repeated colors, for example to distinguish multiple
#'    replicates of a group.
#' @param sortFunc function to use when sorting character or numeric
#'    input in `x`, by default `jamba::mixedSort()`. When input `x` is
#'    a factor, the factor levels are maintained in the same order.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional parameters are passed to `colorFunc`
#'
#' @examples
#' abcde <- group2colors(letters[1:5]);
#' aabbccddee <- group2colors(rep(letters[1:5], each=2));
#' aaabbcccccdeeee <- group2colors(rep(letters[1:5], c(3,2,5,1,4)));
#' aaabbcccccdeeee2 <- group2colors(rep(letters[1:5], c(3,2,5,1,4)), useGradient=TRUE);
#'
#' showColors(list(abcde=abcde,
#'    aabbccddee=aabbccddee,
#'    aaabbcccccdeeee=aaabbcccccdeeee,
#'    aaabbcccccdeeee2=aaabbcccccdeeee2));
#'
#' @export
group2colors <- function
(x,
 alpha=1,
 colorFunc=rainbowJam,
 colorSub=NULL,
 sortFunc=jamba::mixedSort,
 useGradient=FALSE,
 verbose=FALSE,
   ...)
{
   ## Purpose is to take a character vector input, and assign colors
   ## to each unique value.
   ## By default, it uses colorjam::rainbowJam(), however any function
   ## which return n number of colors will suffice, for example
   ##
   if (igrepHas("factor", class(x))) {
      xLabels <- levels(x);
   } else {
      xLabels <- mixedSort(unique(x));
   }
   if (all(xLabels %in% names(colorSub))) {
      xColors <- colorSub;
   } else {
      xColors <- nameVector(
         colorFunc(length(xLabels),
            ...),
         xLabels);
   }

   ## Apply colors to the input data
   xColorsNew <- xColors[as.character(x)];
   if (useGradient) {
      xColorsNew <- color2gradient(xColorsNew,
         ...);
   }
   if (length(names(x)) > 0) {
      names(xColorsNew) <- names(x);
   }
   xColorsNew;
}

#' Jam default theme for ggplot2
#'
#' Jam default theme for ggplot2
#'
#' This function applies some default theme settings to ggplot2, mainly
#' taking away the default grey newspaper background color, also rotates the
#' x-axis label text to 60 degrees, to accomodate longer labels without
#' overlaps.
#'
#' @param theme_default function representing a ggplot2 theme.
#' @param base_size default font point size, used for scaling the
#'    overall text sizes larger or smaller.
#' @param grid.major.size,grid.minor.size the line width for the major
#'    and minor grid lines, respectively. Set to 0 to suppress either.
#' @param strip.background.colour,strip.background.fill color for the
#'    border and strip background itself when ggplot2 is using a
#'    faceted layout.
#' @param panel.grid.major.colour,panel.grid.minor.colour colors for
#'    the major and minor grid lines, respectively.
#' @param axis.text.x.angle numeric degrees to rotate the x-axis
#'    labels, apparently starts at 0 (horizontal) and goes
#'    counter-clockwise (to the left.)
#' @param blankGrid,blankXgrid,blankYgrid logical indicating whether
#'    to have a blank grid for everything, major, or minor axis lines,
#'    respectively. Intended to make it fast and easy to remove all
#'    gridlines.
#' @param resetTheme logical whether to call the function `theme_default`
#'    which essentially resets (replaces) all previous settings with
#'    those defined in the theme function. If `FALSE` then only the
#'    specific settings defined in this function will be applied.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to `ggplot2::theme()` in
#'    order to allow custom settings beyond what this function provides.
#'
#' @export
theme_jam <- function
(theme_default=theme_bw,
 base_size=18,
 grid.major.size=0.5,
 grid.minor.size=0.25,
 strip.background.colour="grey30",
 strip.background.fill="lightgoldenrod1",
 panel.grid.major.colour="grey60",
 panel.grid.minor.colour="grey80",
 axis.text.x.angle=60,
 blankGrid=FALSE,
 blankXgrid=FALSE,
 blankYgrid=FALSE,
 resetTheme=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to provide simple theme for ggplot2 plots
   ##
   ## if blankXgrid=TRUE it substitutes panel.grid.major.x=element_blank()
   ## and panel.grid.minor.x=element_blank()
   ##
   ## Anything in '...' is passed to theme(...) in order to customize other
   ## theme options.
   if (resetTheme) {
      tNew <- theme_default(base_size=base_size);
   } else {
      tNew <- theme_get();
   }
   tNew <- tNew +
      theme(
         axis.text.x=element_text(angle=axis.text.x.angle,
            hjust=1),
         strip.text=element_text(colour=setTextContrastColor(strip.background.fill),
         ),
         strip.background=element_rect(
            colour=strip.background.colour,
            fill=strip.background.fill),
         panel.grid.major=element_line(
            colour=panel.grid.major.colour,
            size=grid.major.size),
         panel.grid.minor=element_line(
            colour=panel.grid.minor.colour,
            size=grid.minor.size));
   if (blankGrid || blankXgrid) {
      if (verbose) {
         printDebug("theme_jam(): ",
            "blankXgrid");
      }
      tNew <- tNew + theme(panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank());
   }
   if (blankGrid || blankYgrid) {
      if (verbose) {
         printDebug("theme_jam(): ",
            "blankYgrid");
      }
      tNew <- tNew + theme(panel.grid.major.y=element_blank(),
         panel.grid.minor.y=element_blank());
   }
   if (length(list(...)) > 0) {
      tNew <- tNew + theme(...);
   }
   invisible(tNew);
}

#' Apply rainbowJam categorical colors to a ggplot2 object
#'
#' Apply rainbowJam categorical colors to a ggplot2 object
#'
#' This function provides a function in the format `scale_color_*`
#' to be applied to ggplot2 objects. It can provide a more visibly
#' distinct set of categorical colors than `ggplot2::scale_color_hue()`.
#'
#' @param ... additional arguments are passed to `ggplot2::discrete_scale()`.
#' @param type character string indicating the colors are sequential
#'    `"seq"`, and is passed to `colorjam::jam_pal()`.
#' @param palette integer value indicating the categorical palette
#'    to use, intended to provide variety in the color assignment.
#'    (Not yet implemented.)
#' @param direction integer indicating whether to reverse the color
#'    assignment, either `1` for the default forward assignment, or
#'    `-1` for reverse assignment. Any negative value will reverse
#'    the colors.
#' @param invert logical indicating whether to return corresponding
#'    contrasting colors, for example for text labels, typically either
#'    `"white"` or `"black"` as defined by `jamba::setTextContrastColor()`.
#' @param useGrey integer value between 0 and 100 indicating the grey
#'    value, as sent to `jamba::setTextContrastColor()`, used only when
#'    `invert=TRUE`.
#'
#' @examples
#' if (suppressPackageStartupMessages(require(ggplot2))) {
#'    dsamp <- diamonds[sample(nrow(diamonds), 1000),];
#'    (d <- ggplot(dsamp, aes(carat, price)) + geom_point(aes(colour=clarity)));
#'
#'    d + scale_color_hue() + ggtitle("scale_color_hue()");
#'    d + scale_color_jam() + ggtitle("scale_color_jam()");
#' }
#' @export
scale_color_jam <- function
(...,
 type="seq",
 palette=1,
 direction=1,
 invert=FALSE,
 useGrey=20)
{
   ## Purpose is to provide rainbowJam() in ggplot2 context
   if (suppressPackageStartupMessages(!require(ggplot2))) {
      stop("scale_color_jam() requires the ggplot2 package.");
   }
   discrete_scale("colour",
      "jam",
      jam_pal(type=type,
         palette=palette,
         direction=direction,
         invert=invert,
         useGrey=useGrey),
      ...);
}

#' Apply rainbowJam categorical color fill to a ggplot2 object
#'
#' Apply rainbowJam categorical color fill to a ggplot2 object
#'
#' This function provides a function in the format `scale_fill_*`
#' to be applied to ggplot2 objects. It can provide a more visibly
#' distinct set of categorical colors than `ggplot2::scale_fill_hue()`.
#'
#' @param ... additional arguments are passed to `ggplot2::discrete_scale()`.
#' @param type character string indicating the colors are sequential
#'    `"seq"`, and is passed to `colorjam::jam_pal()`.
#' @param palette integer value indicating the categorical palette
#'    to use, intended to provide variety in the color assignment.
#'    (Not yet implemented.)
#' @param direction integer indicating whether to reverse the color
#'    assignment, either `1` for the default forward assignment, or
#'    `-1` for reverse assignment. Any negative value will reverse
#'    the colors.
#' @param invert logical indicating whether to return corresponding
#'    contrasting colors, for example for text labels, typically either
#'    `"white"` or `"black"` as defined by `jamba::setTextContrastColor()`.
#' @param useGrey integer value between 0 and 100 indicating the grey
#'    value, as sent to `jamba::setTextContrastColor()`, used only when
#'    `invert=TRUE`.
#'
#' @export
scale_fill_jam <- function
(...,
 type="seq",
 palette=1,
 direction=1,
 invert=FALSE,
 useGrey=20)
{
   ## Purpose is to provide rainbowJam() in ggplot2 context
   if (suppressPackageStartupMessages(!require(ggplot2))) {
      stop("scale_fill_jam() requires the ggplot2 package.");
   }
   discrete_scale("fill",
      "jam",
      jam_pal(type=type,
         palette=palette,
         direction=direction,
         invert=invert,
         useGrey=useGrey),
      ...);
}

#' Jam color palette for ggplot2
#'
#' Jam color palette for ggplot2
#'
#' @param type character string indicating the colors are sequential
#'    `"seq"`.
#' @param palette integer value indicating the categorical palette
#'    to use, intended to provide variety in the color assignment.
#'    (Not yet implemented.)
#' @param direction integer indicating whether to reverse the color
#'    assignment, either `1` for the default forward assignment, or
#'    `-1` for reverse assignment. Any negative value will reverse
#'    the colors.
#' @param invert logical indicating whether to return corresponding
#'    contrasting colors, for example for text labels, typically either
#'    `"white"` or `"black"` as defined by `jamba::setTextContrastColor()`.
#' @param useGrey integer value between 0 and 100 indicating the grey
#'    value, as sent to `jamba::setTextContrastColor()`, used only when
#'    `invert=TRUE`.
#'
#' @export
jam_pal <- function
(type="seq",
 palette=1,
 direction=1,
 invert=FALSE,
 useGrey=20)
{
   ## Note this function does not specifically require ggplot2
   if (invert) {
      function(n) {
         pal <- setTextContrastColor(rainbowJam(n),
            useGrey=useGrey);
         names(pal) <- NULL;
         printDebug(pal);
         pal <- pal[seq_len(n)];
         if (direction < 0) {
            pal <- rev(pal);
         }
         pal;
      }
   } else {
      function(n) {
         pal <- rainbowJam(n);
         names(pal) <- NULL;
         printDebug(pal);
         pal <- pal[seq_len(n)];
         if (direction < 0) {
            pal <- rev(pal);
         }
         pal;
      }
   }
}
