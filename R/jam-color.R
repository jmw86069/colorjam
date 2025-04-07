
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
#' @family colorjam deprecated
#'
#' @examples
#' rainbowJam_v1(12);
#'
#' # show colors
#' jamba::showColors(rainbowJam_v1(12));
#'
#' # be fancy and label colors using the closest R named color
#' jamba::showColors(rainbowJam_v1(12, nameStyle="colors"));
#'
#' @export
rainbowJam_v1 <- function
(n=NULL,
 alpha=1,
 nfloor=4,
 hues=NULL,
 Cvals=c(75,95,80, 65,72,80),
 Lvals=c(62,83,69,58,65,74),
 # Scaling of the ranges above
 #Crange=getOption("jam.Crange"),
 #Lrange=getOption("jam.Lrange"),
 #Cgrey=getOption("jam.Cgrey"),
 Crange=NULL,
 Lrange=NULL,
 Cgrey=NULL,
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
      colSet <- jamba::nameVector(rainbowJam_v1(n), 1:n);
      jamba::showColors(colSet,
         main=paste("n =", n, " colors"),
         xaxt="n");
      title("The more colors, the more likely non-adjacent colors will look similar");
      jamba::showColors(colSet[seq(from=1, to=n, by=3)],
         main="offset by 3 (Dark colors)",
         xaxt="n");
      if (n > 1) {
         fullHiSet <- seq(from=2, to=n, by=3);
         whichHiSet <- fullHiSet[-seq(from=2, to=length(fullHiSet), by=2)];
         jamba::showColors(colSet[whichHiSet],
            main="Light colors",
            xaxt="n");
         jamba::showColors(colSet[-whichHiSet],
            main="Non-light colors",
            xaxt="n");
         if (n > 2) {
            jamba::showColors(colSet[seq(from=3, to=n, by=2)],
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
         jamba::printDebug("rainbowJam(): ",
            "warping hues");
         print(data.frame(h1=h1, h2=h2));
      }
      hues <- hw2h(hues,
         h1=h1,
         h2=h2);
   }

   if (verbose) {
      jamba::printDebug("rainbowJam(): ",
         "hues:",
         format(digits=2, hues));
   }

   ## Scale the lVals and cVals ranges to given lRange and cRange if needed
   ## (note we have to do the scaling before taking a subset of values!)
   if (length(Lrange) == 2 && !all(Lrange %in% range(Lvals))) {
      #Lvals <- jamba::normScale(Lvals,
      #   from=Lrange[1],
      #   to=Lrange[2]);
      ## 19nov2018 changed to restrict the extremes, not stretch the
      ## interior values to the minimum/maximum
      Lvals <- jamba::normScale(Lvals,
         low=min(c(Lrange[1], min(Lvals))),
         high=max(c(Lrange[2], max(Lvals))),
         from=Lrange[1],
         to=Lrange[2]);
   }
   if (length(Crange) == 2 && !all(Crange %in% range(Cvals))) {
      #Cvals <- jamba::normScale(Cvals,
      #   from=Crange[1],
      #   to=Crange[2]);
      Cvals <- jamba::normScale(Cvals,
         low=min(c(Crange[1], min(Cvals))),
         high=max(c(Crange[2], max(Cvals))),
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
      jamba::printDebug("H: ", format(digits=2, hues));
      jamba::printDebug("C:", format(digits=2, Cvals));
      jamba::printDebug("L:", format(digits=2, Lvals));
      jamba::printDebug("alpha:", alpha);
   }

   ## Generate colors using HCL definitions
   hclSet <- jamba::hcl2col(H=hues,
      C=Cvals,
      L=Lvals,
      alpha=alpha);
   if (any(abs(sFactor) != 1) || any(abs(darkFactor) != 1)) {
      if (verbose) {
         jamba::printDebug("rainbowJam(): ",
            "applying darkFactor, sFactor with jamba::makeColorDarker().")
      }
      hclSet <- jamba::makeColorDarker(hclSet,
         darkFactor=postDarkFactor,
         sFactor=postSfactor,
         ...);
   }
   if (nameStyle %in% "hcl") {
      hclNames <- jamba::makeNames(paste(seq_len(n),
         paste0("H", signif(hues, digits=3)),
         paste0("C", signif(Cvals, digits=3)),
         paste0("L", signif(Lvals, digits=3)),
         sep="_"));
      names(hclSet) <- hclNames;
   } else if (nameStyle %in% "colors") {
      hclNames <- jamba::makeNames(closestRcolor(hclSet));
      names(hclSet) <- hclNames;
   } else {
      hclSet <- unname(hclSet);
   }
   return(hclSet);
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
#' @family colorjam core
#' @family colorjam assignment
#'
#' @examples
#' abcde <- group2colors(letters[1:5]);
#' aabbccddee <- group2colors(rep(letters[1:5], each=2));
#' aaabbcccccdeeee <- group2colors(rep(letters[1:5], c(3,2,5,1,4)));
#' aaabbcccccdeeee2 <- group2colors(rep(letters[1:5], c(3,2,5,1,4)), useGradient=TRUE);
#'
#' jamba::showColors(list(abcde=abcde,
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
   if (length(sortFunc) == 0 || !is.function(sortFunc)) {
      sortFunc <- c;
   }
   if (jamba::igrepHas("factor", class(x))) {
      xLabels <- levels(x);
   } else {
      xLabels <- sortFunc(unique(x));
   }
   if (all(xLabels %in% names(colorSub))) {
      xColors <- colorSub;
   } else {
      xColors <- jamba::nameVector(
         colorFunc(length(xLabels),
            ...),
         xLabels);
   }

   ## Apply colors to the input data
   xColorsNew <- xColors[match(as.character(x), names(xColors))];
   if (useGradient) {
      xColorsNew <- jamba::color2gradient(xColorsNew,
         ...);
   }
   if (length(names(x)) > 0) {
      names(xColorsNew) <- names(x);
   }
   xColorsNew;
}


#' Convert numeric matrix to heatmap colors
#'
#' Convert numeric matrix to heatmap colors
#'
#' This function is intended as a rapid way of applying a color
#' gradient to columns of numeric values, where each column
#' has its own base color. It calls `jamba::getColorRamp()`
#' for each column, and when supplied with one color, it
#' creates a color gradient from `"grey95"` to the output
#' of `jamba::color2gradient()`.
#'
#' When `lens` is non-zero, the color gradient is warped in order
#' to intensify the color saturation across the numeric range.
#'
#' @param x `numeric` matrix. If there are no `colnames(x)` they will
#'    be created using `jamba::makeNames(rep("x", ncol(x)))`.
#' @param colorV `character` vector of R colors, named by `colnames(x)`,
#'    and recycled to `ncol(x)` if needed. If `colorV` is supplied as
#'    a list, the list elements are mapped to `colnames(x)` in order.
#' @param defaultBaseColor `character` vector of R colors used as the default
#'    base color, when `colorV` is supplied as a vector.
#' @param transformFunc `function` applied to numeric values before
#'    the color gradient is mapped to numeric values. For example,
#'    `transformFunc=function(i)-log10(i)` would map colors to P-value
#'    using a `-log10(p)` transformation.
#' @param lens `numeric` value passed to `jamba::warpRamp()` to adjust the
#'    distribution of colors along the numeric range.
#' @param shareLimit `logical` indicating whether one numeric limit `numLimit`
#'    should be used to define the numeric range for color mapping.
#' @param numLimitFactor `numeric` when `numLimit` is NULL,
#'    this factor is applied to
#'    the maximum numeric value to determine the `numLimit`.
#' @param numLimit `numeric` value to define the maximum numeric value
#'    above which all numeric values are mapped to the maximum color.
#'    When set to `NULL` the `numLimitFactor` is used to define
#'    the `numLimit`.
#' @param baseline `numeric` value to define the numeric baseline, used
#'    when `divergent=FALSE`. Values are recycled to `ncol(x)` to be
#'    applied to each column individually.
#' @param color_below_baseline `character` color used when numeric value is
#'    below the `baseline`. Values are recycled to `ncol(x)` to be
#'    applied to each column individually. When `color_below_baseline`
#'    is `NULL`, the first color in the color ramp is used for all
#'    values below the baseline.
#' @param divergent `logical` indicating whether to apply colors to the numeric
#'    range symmetric around zero.
#' @param rampN `integer` value to define the number of color breaks for
#'    each color gradient.
#' @param trimRamp `numeric` vector with two values, used by
#'    `jamba::getColorRamp()` to trim the intermediate color gradient before
#'    creating the final color ramp with length `rampN`. For example,
#'    by default `jamba::getColorRamp()` creates a color gradient with
#'    15 colorr, defined by argument `gradientN=15`, so the argument
#'    `trimRamp=c(4,2)` will trim the first 4 colors and the last 2 colors
#'    from the 15-color gradient, before generating the final color
#'    gradient with length `rampN`. The `trimRamp` argument is especially
#'    useful to remove the leading white color, or to trim the first
#'    few colors to ensure the first color in the gradient is visibly
#'    different from the background color defined by `defaultBaseColor`.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to `jamba::getColorRamp()`
#'    for additional customization. These arguments are handled across
#'    all columns, and not a column-by-column basis.
#'
#' @family colorjam assignment
#'
#' @examples
#' set.seed(123);
#' # generate a random numeric matrix
#' m1 <- matrix(ncol=12, rnorm(120));
#' m1n <- m1;
#' m1n[] <- format(round(abs(m1), digits=2), trim=TRUE);
#' jamba::imageByColors(
#'    matrix2heatColors(abs(m1),
#'       transformFunc=c,
#'       divergent=FALSE,
#'       lens=-5,
#'       shareNumLimit=TRUE,
#'       baseline=0,
#'       numLimit=4),
#'    cellnote=m1n);
#'
#' @export
matrix2heatColors <- function
(x,
 colorV=group2colors(colnames(x)),
 defaultBaseColor="#FFFFFF",
 transformFunc=c,
 lens=0,
 shareLimit=TRUE,
 numLimitFactor=0.95,
 numLimit=NULL,
 baseline=0,
 color_below_baseline="#FFFFFF",
 divergent=FALSE,
 rampN=15,
 trimRamp=c(0, 0),
 verbose=FALSE,
...)
{
   ## Purpose is to create a color gradient from a numeric matrix
   ## intended for when each column should have its own distinct color
   ## usually a gradient from white to the specified color.
   ##
   ## This function calls vals2colorLevels() for each column.
   ##
   ## Make sure x has colnames
   if (length(colnames(x)) == 0) {
      colnames(x) <- jamba::makeNames(rep("x", ncol(x)));
   }
   xNames <- colnames(x);

   ## Define farbeLim if not provided
   if (length(numLimit) == 0) {
      if (shareLimit) {
         ## Shared max color value
         numLimit <- max(jamba::rmNA(abs(transformFunc(x)))) * numLimitFactor;
         if (verbose) {
            jamba::printDebug("matrix2heatColors():",
               "defined shared farbeLim value:",
               format(digits=2, numLimit));
         }
         numLimit <- rep(numLimit, length.out=ncol(x));
      } else {
         numLimit <- sapply(1:ncol(x), function(i){
            numLimit <- max(jamba::rmNA(abs(transformFunc(x)))) * numLimitFactor;
         });
         if (verbose) {
            jamba::printDebug("matrix2heatColors():",
               "defined individual numLimit values:",
               format(digits=2, numLimit));
         }
      }
      names(numLimit) <- xNames;
   } else {
      numLimit <- rep(numLimit, length.out=ncol(x));
      if (verbose) {
         jamba::printDebug("matrix2heatColors():",
            "numLimit values as provided:",
            format(digits=2, numLimit));
      }
      if (length(names(numLimit)) > 0 &&
            all(names(numLimit) %in% xNames)) {
         numLimit <- numLimit[xNames];
      } else {
         names(numLimit) <- xNames;
      }
   }
   lens <- jamba::nameVector(rep(lens,
      length.out=ncol(x)),
      xNames);
   defaultBaseColor <- jamba::nameVector(rep(defaultBaseColor,
      length.out=ncol(x)),
      xNames);
   baseline <- jamba::nameVector(rep(baseline,
      length.out=ncol(x)),
      xNames);
   color_below_baseline <- jamba::nameVector(rep(color_below_baseline,
      length.out=ncol(x)),
      xNames);
   numLimit <- jamba::nameVector(rep(numLimit,
      length.out=ncol(x)),
      xNames);
   rampN <- jamba::nameVector(rep(rampN,
      length.out=ncol(x)),
      xNames);
   divergent <- jamba::nameVector(rep(divergent,
      length.out=ncol(x)),
      xNames);
   colorV <- jamba::nameVector(rep(colorV,
      length.out=ncol(x)),
      xNames);

   xColors <- do.call(cbind, lapply(jamba::nameVector(colnames(x)), function(i){
      k <- (transformFunc(x[,i]));
      if (verbose) {
         jamba::printDebug("matrix2heatColors():", i,
            ", lens:", format(lens[i], digits=2),
            ", defaultBaseColor:", defaultBaseColor[i],
            ", baseline:", format(baseline[i], digits=2),
            ", numLimit:", format(numLimit[i], digits=2),
            ", colorRamp:", colorV[[i]],
            fgText=list("orange", "lightblue",
               "orange", "lightblue",
               "orange", defaultBaseColor[i],
               "orange", "lightblue",
               "orange", "lightblue",
               "orange", colorV[[i]]));
      }
      if (divergent[i]) {
         ## divergent color ramp
         k <- jamba::noiseFloor(k,
            minimum=-numLimit[i],
            ceiling=numLimit[i]);
         kRamp <- jamba::getColorRamp(colorV[[i]],
            defaultBaseColor=defaultBaseColor[i],
            lens=lens[i],
            divergent=TRUE,
            n=rampN[i],
            trimRamp=trimRamp,
            ...);
         if (verbose) {
            jamba::printDebug("matrix2heatColors(): ",
               "divergent:", divergent,
               ",\nkRamp:", kRamp,
               fgText=list("orange", "dodgerblue", "orange",
                  "dodgerblue", kRamp));
         }
         kCut <- cut(k,
            include.lowest=TRUE,
            breaks=seq(from=-numLimit[i],
               to=numLimit[i],
               length.out=rampN[i]+1));
         kColor <- kRamp[kCut];
      } else {
         ## one-directional color ramp
         k <- jamba::noiseFloor(k,
            minimum=baseline[i],
            newValue=baseline[i]-1,
            ceiling=numLimit[i]);
         kRamp <- jamba::getColorRamp(colorV[[i]],
            defaultBaseColor=defaultBaseColor[i],
            lens=lens[i],
            divergent=FALSE,
            n=rampN[i],
            trimRamp=trimRamp,
            ...);
         if (length(color_below_baseline[i]) == 0) {
            kRamp <- c(kRamp[1], kRamp);
         } else {
            kRamp <- c(color_below_baseline[i], kRamp);
         }
         if (verbose) {
            jamba::printDebug("matrix2heatColors(): ",
               "divergent:", divergent[i],
               ",\nkRamp:", kRamp,
               fgText=list("orange", "dodgerblue", "orange",
                  "dodgerblue", kRamp));
         }
         kCut <- cut(k,
            include.lowest=TRUE,
            breaks=c(baseline[i]-1,
               seq(from=baseline[i],
                  to=numLimit[i],
                  length.out=rampN[i]+1)));
         kColor <- kRamp[kCut];
      }
      kColor;
   }));
   rownames(xColors) <- rownames(x);
   return(xColors);
}

#' Apply color gradient to numeric values
#'
#' Apply color gradient to numeric values
#'
#' This function is similar to several other existing R functions
#' that take a vector of numeric values, and apply a color gradient
#' (color ramp) to the numeric values. This function provides the ability
#' to warp the color ramp, for example using `jamba::warpRamp()` in order
#' to adjust the color gradient relative to the numeric range of the
#' data.
#'
#' Note that the function `col_div_xf()` and `col_linear_xf()` may
#' be preferable to this function. Those functions assign colors
#' to specific numeric values, instead of assigning colors between
#' numeric break points.
#'
#' @family colorjam assignment
#'
#' @param x numeric vector
#' @param divergent logical indicating whether the numeric values
#'    are divergent, by default baseline=0 will center the color
#'    ramp at zero.
#' @param col color value compatible with the `col` argument of
#'    `jamba::getColorRamp()`. Example include: single color; multiple
#'    colors; single color ramp name; or a custom color function.
#' @param defaultBaseColor character color used as a base color when
#'    a single color is supplied in `col`.
#' @param lens numeric value sent to `jamba::warpRamp()`, to define the
#'    level of color warping to apply to the color gradient, where `lens=0`
#'    applies no adjustment.
#' @param numLimit numeric value indicating the maximum numeric value,
#'    where values in `x` greater than this value are assigned to the
#'    maximum color. When not defined, and `divergent=TRUE` it uses
#'    `max(abs(x), na.rm=TRUE)`, or `divergent=FALSE` it uses
#'    `max(x, na.rm=TRUE)`.
#' @param baseline numeric value indicating the minimum numeric value,
#'    where values in `x` less than this value are assigned to the
#'    minimum color. When not defined, and `divergent=TRUE` it sets
#'    `baseline=0`; when `divergent=FALSE` it uses `min(x, na.rm=TRUE)`.
#' @param rampN integer number of colors to define for the color
#'    gradient. Higher values define a smooth color gradient.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to `jamba::getColorRamp()`.
#'
#' @family jam color functions
#'
#' @examples
#' # Start with an example numeric vector
#' x <- jamba::nameVector(-5:10);
#' jamba::showColors(vals2colorLevels(x));
#'
#' # decrease the number of gradient colors
#' jamba::showColors(vals2colorLevels(x, rampN=15))
#'
#' # change the baseline
#' jamba::showColors(vals2colorLevels(x, baseline=-2));
#'
#' # adjust the gradient using lens
#' par("mar"=c(5,5,4,2));
#' jamba::imageByColors(jamba::rbindList(lapply(jamba::nameVector(c(-5,-2,0,2,5)), function(lens){
#'    vals2colorLevels(x, rampN=25, lens=lens);
#' })));
#' title(ylab="color lens factor", xlab="numeric value",
#'    main="Effects of warping the color gradient");
#'
#' @export
vals2colorLevels <- function
(x,
 divergent=TRUE,
 col="RdBu_r",
 defaultBaseColor="#FFFFFF",
 lens=0,
 numLimit=NULL,
 baseline=NULL,
 rampN=25,
 verbose=FALSE,
 ...)
{
   ## Purpose is to convert a numeric vector into a color gradient
   if (length(divergent) == 0) {
      divergent <- FALSE;
   }
   if (length(defaultBaseColor) == 0) {
      defaultBaseColor <- "#FFFFFF";
   }
   if (length(lens) == 0) {
      lens <- 0;
   }
   if (length(rampN) == 0) {
      rampN <- 15;
   }
   if (length(col) == 0) {
      if (divergent) {
         col <- "RdBu_r";
      } else {
         col <- "Reds";
      }
   }
   ## If no baseline is provided, infer an appropriate one
   if (length(baseline) == 0) {
      if (divergent || min(x, na.rm=TRUE) == max(x, na.rm=TRUE)) {
         baseline <- 0;
      } else {
         baseline <- min(x, na.rm=TRUE);
      }
   }
   if (length(numLimit) == 0) {
      if (divergent) {
         numLimit <- max(abs(x-baseline)+baseline, na.rm=TRUE);
      } else {
         numLimit <- max(x, na.rm=TRUE);
      }
      ## Correct issue when numLimit == baseline
      if (numLimit == baseline) {
         numLimit <- baseline + 1;
      }
   }

   ## Get the color ramp values
   colorV <- jamba::getColorRamp(col=col,
      n=rampN,
      defaultBaseColor=defaultBaseColor,
      verbose=verbose,
      ...);

   ## Optionally print verbose output
   if (verbose) {
      if (divergent) {
         colorVnames <- rep(seq(from=1, to=floor(length(colorV)/2)), each=2)*c(-1,1);
         if (length(colorV) %% 2) {
            colorVnames <- sort(c(0, colorVnames));
         }
      } else {
         colorVnames <- seq_along(colorV);
      }
      jamba::printDebug("vals2colorLevels(): ",
         "colorV:\n",
         format(colorVnames),
         sep=", ",
         Lrange=c(10,95), Crange=c(40,95),
         fgText=list("orange", "dodgerblue", colorV));
   }
   ## Optionally warp the color ramp
   if (lens != 0) {
      colorV <- jamba::warpRamp(colorV,
         divergent=TRUE,
         lens=lens,
         ...);
      if (verbose) {
         jamba::printDebug("vals2colorLevels(): ",
            "colorV (lens):\n",
            format(colorVnames),
            sep=", ",
            Lrange=c(10,95), Crange=c(40,95),
            fgText=list("orange", "dodgerblue", colorV));
      }
   }

   ## Apply the color gradient
   if (divergent) {
      ## divergent color ramp
      k <- jamba::noiseFloor(x,
         minimum=baseline-numLimit,
         ceiling=numLimit);
      kBreaks <- seq(from=2*baseline-numLimit,
         to=numLimit,
         length.out=rampN+1);
   } else {
      ## one-directional color ramp
      k <- jamba::noiseFloor(x,
         minimum=baseline,
         ceiling=numLimit);
      kBreaks <- seq(from=baseline,
         to=numLimit,
         length.out=rampN+1);
   }
   if (verbose) {
      colorVbreaks <- c(head(colorV, ceiling(length(colorV)/2)),
         tail(colorV, ceiling(length(colorV)/2)));
      jamba::printDebug("vals2colorLevels(): ",
         "kBreaks:\n",
         format(round(digits=1, kBreaks)),
         sep=", ",
         Lrange=c(10,95), Crange=c(40,95),
         fgText=list("orange", "dodgerblue", colorVbreaks));
   }
   kCut <- cut(k,
      include.lowest=TRUE,
      breaks=kBreaks);
   kColor <- unname(colorV[as.numeric(kCut)]);
   if (length(names(x)) > 0) {
      names(kColor) <- names(x);
   }
   kColor;
}

#' Closest colorjam named_colors
#'
#' Closest colorjam named_colors for a vector of colors
#'
#' @family colorjam core
#'
#' @inheritParams closestRcolor
#'
#' @param colorSet `character` vector of colors, by default `named_colors`
#'    with provides 4,447 total hex colors, each with human-assigned
#'    color name. These colors also include hex colors from R `colors()`
#'    which were not already included in the reference colors.
#'
#' @export
closest_named_color <- function
(x,
 colorSet=colorjam::named_colors,
 C_min=Cgrey,
 Cgrey=getOption("jam.Cgrey", 5),
 showPalette=FALSE,
 colorModel=c("hcl", "LUV"),
 Hwt=2.5,
 Cwt=1,
 Lwt=4,
 warpHue=TRUE,
 preset="ryb",
 method="maximum",
 returnType=c("color",
    "name",
    "match"),
 verbose=FALSE,
 ...)
{
   #
   closestRcolor(x=x,
      colorSet=colorSet,
      C_min=C_min,
      Cgrey=Cgrey,
      showPalette=showPalette,
      colorModel=colorModel,
      Hwt=Hwt,
      Cwt=Cwt,
      Lwt=Lwt,
      warpHue=warpHue,
      preset=preset,
      method=method,
      returnType=returnType,
      verbose=verbose,
      ...);
}
