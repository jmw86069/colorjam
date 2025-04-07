
#' Find the closest R color
#'
#' Find the closest R color for a vector of colors
#'
#' This function is intended as a relatively efficient method to compare
#' a set of colors to the named R colors provided by `grDevices::colors()`.
#'
#' Color matching provides substantial improvements over similar functions
#' from other R packages. Notably, colors are matched using either
#' HCL or LUB color model by default, both of which provide vast
#' improvement over RGB color matching, due to better spacing of
#' colors, and increased resolution of color hue.
#'
#' For `colorModel="HCL"` the coordinates are weighted to prioritize
#' matching color Hue above Chroma and Luminance. The distance method
#' by default uses `method="maximum"` which also emphasizes the lowest
#' distance in any of the three dimensions.
#'
#' @returns `character` vector of colors, optionally customized
#'    by argument `returnType`.
#'
#' @param x character vector of colors, either in hex format or any
#'    valid color in R.
#' @param colorSet `character` vector of colors, by default includes
#'    the R colors `grDevices::colors()`.
#' @param Cgrey,C_min `numeric` default 5, using `getOption("jamba.Cgrey", 5)`,
#'    the Chroma at which colors are considered "grey" ("gray").
#'    * `Cgrey` is applied to `colorSet`.
#'    * `C_min` is applied to `x`.
#'
#'    The purpose is for saturated colors to match saturated colors,
#'    and non-saturated colors to match non-saturated colors.
#'    Rules:
#'    * All non-grey colors `x` are compared with non-grey `colorSet`.
#'    * Grey colors `x` are compared with grey `colorSet`,
#'    therefore color Hue is not used.
#'    * Note `Cgrey` is an argument in `jamba::make_styles()`, and
#'    `jamba::applyCLrange()` for similar use cases.
#' @param showPalette `logical` indicating whether to display the input
#'    colors and resulting closest matching colors by using
#'    `jamba::showColors()`.
#' @param colorModel `character` color model to use:
#'    * `"hcl"`: default, uses HCL provided by `jamba::col2hcl()` which
#'    uses the equivalent of `colorspace::polarLUV()` and considers
#'    color hues in terms of 360 degree angles along a color wheel.
#'    * `"LUV"`: uses CIELUV color space, provided by `colorspace::LUV()`
#'    which encodes the angular color hue in 3-D Cartesian space,
#'    allowing comparisons using Euclidean distance.
#' @param Hwt,Cwt,Lwt `numeric` relative weights for each dimension of
#'    HCL colors, for the H, C, and L channels, respectively.
#' @param warpHue `logical` indicating whether to perform the hue warp
#'    operation using `h2hw()` which improves the ability to match
#'    colors between orange and green.
#' @param preset `character` string to define the color wheel used
#'    when matching input colors `x` to colors in `colorSet`.
#'    This preset is used with `h2hw()` and `hw2h()`.
#'    The default `preset="ryb"` allows greatest distinction in colors
#'    without imposing additional restrictions such as by `preset="dichromat"`
#'    which would only match color-safe colors. The purpose here is
#'    to identify and label colors based upon a reference set of colors.
#' @param method `character` string passed to `stats::dist()`. The default
#'    `method="maximum"` works well for `colorModel="hcl"`, and
#'    assigns distance using the largest distance across
#'    the three color coordinates H, C, and L. It requires the best
#'    overall match across all three coordinates rather than any weighted
#'    combination of coordinate distances. Other methods in testing allowed
#'    matches of different color hues when luminance and chroma values
#'    were very similar.
#'    With  `colorModel="LUV"` we recommend using `method="euclidean"`,
#'    which seems to work well with projected color coordinates
#'    L, U, and V. The U, and V coordinates are roughly the angular
#'    color hue projected into a flat plane, the L describing Luminance.
#' @param returnType `character` type of data to return:
#'    * `"color"` returns the color values in `colorSet`, which by default
#'    are color names from `grDevices::colors()`
#'    * `"name"` returns `names(colorSet)` if they exist, otherwise
#'    values from `colorSet`
#'    * `"match"` returns an integer vector as an index to `colorSet`
#' @param verbose `logical` whether to print verbose output.
#'
#' @family colorjam core
#'
#' @examples
#' closestRcolor(rainbowJam(12), showPalette=TRUE);
#'
#' @export
closestRcolor <- function
(x,
 colorSet=colors(),
 Cgrey=getOption("jam.Cgrey", 5),
 C_min=Cgrey,
 showPalette=FALSE,
 colorModel=c("hcl",
    "LUV"),
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
      names(origX) <- jamba::makeNames(origX);
   }

   if (length(C_min) == 0) {
      C_min <- 0;
   } else {
      C_min <- head(C_min, 1);
   }
   colorSet_lo <- NULL;
   if (C_min > 0) {
      colorSet_hcl <- jamba::col2hcl(colorSet);
      colorSet_hcl["C",] <- round(colorSet_hcl["C",],
         digits=3)
      colorSet_lo <- colorSet[colorSet_hcl["C",] < C_min];
      colorSet <- colorSet[colorSet_hcl["C",] >= C_min];
   }

   if (returnType %in% "name" && length(names(colorSet)) == 0) {
      names(colorSet) <- jamba::makeNames(colorSet);
   }
   x <- jamba::nameVector(unique(origX));
   xHCL <- NULL;
   newX <- NULL;
   if (Cgrey > 0 && C_min > 0 && length(colorSet_lo) > 0) {
      if (verbose) {
         jamba::printDebug("closestRcolor(): ",
            "processing low chroma colors.")
      }
      # convert to HCL
      xHCL <- jamba::col2hcl(x);
      is_lo <- (xHCL["C",] < Cgrey);
      if (any(is_lo)) {
         # process unsaturated colors
         newX_lo <- closestRcolor(x=x[is_lo],
            colorSet=colorSet_lo,
            C_min=0,
            Cgrey=0,
            colorModel=colorModel,
            Hwt=Hwt,
            Cwt=Cwt,
            Lwt=Lwt,
            warpHue=warpHue,
            preset=preset,
            method=method,
            returnType="color",
            verbose=verbose,
            ...)
         newX <- rep("", length(x));
         newX[is_lo] <- newX_lo;
         # names(newX)[is_lo] <- x[is_lo];
         # process saturated colors
         if (any(!is_lo)) {
            newX_hi <- closestRcolor(
               x=x[!is_lo],
               colorSet=colorSet,
               C_min=0,
               Cgrey=0,
               colorModel=colorModel,
               Hwt=Hwt,
               Cwt=Cwt,
               Lwt=Lwt,
               warpHue=warpHue,
               preset=preset,
               method=method,
               returnType="color",
               verbose=verbose,
               ...)
            newX[!is_lo] <- newX_hi;
            # names(newX)[!is_lo] <- names(newX_hi);
            # names(newX)[!is_lo] <- x[!is_lo];
         } else {
            newX_hi <- NULL;
         }
         names(newX) <- x;
      }
   }
   if (length(newX) == 0) {
      if ("hcl" %in% colorModel) {
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
         if (length(xHCL) == 0) {
            xHCL <- jamba::col2hcl(x);
         }
         colorSetHCL <- jamba::col2hcl(jamba::nameVector(colorSet));

         ## Adjust H to RYB
         if (warpHue) {
            xHCL["H",] <- h2hw(xHCL["H",],
               preset=preset);
            colorSetHCL["H",] <- h2hw(colorSetHCL["H",],
               preset=preset);
         }

         Hdist <- angDist(a=xHCL["H",],
            b=colorSetHCL["H",])/180*100;

         CLm <- rbind(t(xHCL), t(colorSetHCL))[,c("L","C"),drop=FALSE];
         CLm[,"C"] <- CLm[,"C"] * Cwt;
         CLm[,"L"] <- CLm[,"L"] * Lwt;
         CLdist <- as.matrix(dist(CLm,
            method=method))[colnames(xHCL), colnames(colorSetHCL), drop=FALSE];
         if (verbose) {
            jamba::printDebug("dim(Hdist):", dim(Hdist));
            jamba::printDebug("dim(CLdist):", dim(CLdist));
         }
         HCLdist <- Hdist * Hwt + CLdist;
         iClosestColorWhich <- apply(HCLdist, 1, which.min);

         newX <- jamba::nameVector(colorSet[iClosestColorWhich],
            colnames(xHCL));
      } else if ("LUV" %in% colorModel) {
         ## Use LUV
         col2LUV <- function(a) {
            if (length(names(a)) == 0) {
               names(a) <- jamba::makeNames(a);
            }
            # convert color
            colorspace::coords(as(colorspace::hex2RGB(
               jamba::rgb2col(grDevices::col2rgb(a))), "LUV"));
         }
         xLUV <- col2LUV(x);
         colorSetLUV <- col2LUV(colorSet);
         LUVdist <- as.matrix(dist(rbind(xLUV,
            colorSetLUV),
            method=method))[rownames(xLUV), rownames(colorSetLUV), drop=FALSE];
         iClosestColorWhich <- apply(LUVdist, 1, which.min);
         newX <- jamba::nameVector(colorSet[iClosestColorWhich],
            rownames(xLUV));
      }
   }

   # 0.0.25.900 - names are not assigned from input
   # instead are assigned from `colorSet`
   retX <- newX[origX];
   if (length(colorSet_lo) > 0) {
      colorSet <- c(colorSet, colorSet_lo);
   }
   imatch <- match(retX, colorSet);
   # print("head(imatch, 20):");print(head(imatch, 20));# debug
   if (length(names(colorSet)) > 0) {
      names(retX) <- jamba::makeNames(names(colorSet)[imatch]);
   } else {
      names(retX) <- NULL;
   }
   if ("match" %in% returnType) {
      retX[] <- imatch;
   } else if ("name" %in% returnType && length(names(colorSet)) > 0) {
      retX[] <- names(colorSet)[imatch];
   }
   # if (length(names(origX)) > 0) {
   #    names(retX) <- names(origX);
   # }

   ## Optionally display the palette before and after
   if (showPalette) {
      use_origX <- origX;
      if (length(names(use_origX)) == 0) {
         names(use_origX) <- origX;
      }
      use_retX <- retX;
      if (length(names(use_retX)) == 0) {
         names(use_retX) <- retX;
      }
      jamba::showColors(list(
         original=use_origX,
         returned=use_retX),
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
