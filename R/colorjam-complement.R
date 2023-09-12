
#' Create color complement by rotating the color hue
#'
#' Create color complement by rotating the color hue
#'
#' This function rotates the color hue to create a complementary
#' color for each `color` input. It differs from standard methods
#' by using warped color hue by default (`useWarpHue=TRUE`), which
#' uses a red-yellow-blue color wheel instead of R default
#' red-green-blue. It also imposes a minimum chroma, which
#' ensures the output color is reasonably high in color
#' saturation.
#'
#' @family jam utility functions
#'
#' @param color `character` vector of R compatible colors.
#' @param Hflip `numeric` value in degrees (from 0 to 360) added
#'    to the color hue to produce the final color hue. Typically
#'    180 degrees will select the color opposite the input color
#'    on a virtual color wheel. Note that `warpHue=TRUE` also
#'    enables a customized color wheel.
#' @param Cfloor `numeric` deprecated in favor of `Crange`, however
#'    when `Cfloor` is provided, it is given priority over `Crange`.
#'    value used to limit output chroma `C`
#'    values to this minimum value, to ensure a minimum color saturation.
#' @param Crange `numeric` vector with the permitted range of output
#'    color chroma `C` values. When supplied, output values are
#'    forced to this range with no other scaling of intermediate values.
#'    Note that input colors at or below chroma `Cgrey` are
#'    considered greyscale and are not complemented.
#' @param Lrange `numeric` vector with the permitted range of output
#'    luminance `L` values. When supplied, output values are
#'    simply forced to this range with no other scaling of intermediate
#'    values.
#' @param Cgrey `numeric` color chroma, at or below which a color is
#'    considered greyscale, therefore the color hue is not relevant,
#'    and the `Crange` is not applied.
#' @param useWarpHue `logical` indicating whether to use the warp
#'    hue functions `colorjam::h2hw()` and `colorjam::hw2h()` which
#'    effectively change the color wheel from red-green-blue to
#'    red-yellow-blue.
#' @param ... additional arguments are ignored.
#'
#' @family colorjam core
#'
#' @returns `character` vector of complementary colors.
#'
#' @examples
#' n <- 5;
#' rc <- colorjam::rainbowJam(n);
#' rc_comp <- color_complement(rc, preset="dichromat");
#' rc_comp2 <- color_complement(rc, preset="dichromat", useWarpHue=FALSE);
#' rc_comp3 <- color_complement(rc, preset="ryb");
#' jamba::showColors(list(rainbowJam=rc,
#'    `complement\n(preset="dichromat")`=rc_comp,
#'    `complement\n(useWarpHue=FALSE)`=rc_comp2,
#'    `complement\n(preset="ryb")`=rc_comp3));
#'
#' rc <- colorjam::rainbowJam(n, preset="ryb");
#' rc_comp <- color_complement(rc, preset="ryb");
#' jamba::showColors(list(`rainbowJam\n(preset="ryb")`=rc,
#'    `complement\n(preset="ryb")`=rc_comp));
#'
#' ## divergent color gradients through white
#' ## hint: use higher lens value to make middle colors more intense
#' rc <- colorjam::rainbowJam(n);
#' rc_comp <- color_complement(rc);
#' rc_ramps <- lapply(jamba::nameVector(seq_along(rc)), function(i){
#'    j <- jamba::getColorRamp(c(rc[i], "white", rc_comp[i]),
#'       n=25,
#'       lens=0,
#'       divergent=TRUE);
#'    names(j) <- "";
#'    names(j)[1] <- "original colors";
#'    names(j)[25] <- "color complements";
#'    j;
#' });
#' jamba::showColors(rc_ramps, groupCellnotes=TRUE, groupByColors=FALSE);
#'
#' ## divergent color gradients through black
#' ## hint: use higher lens value to make middle colors more intense
#' rc_ramps2 <- lapply(jamba::nameVector(seq_along(rc)), function(i){
#'    j <- jamba::getColorRamp(c(rc[i], "black", rc_comp[i]),
#'       n=25,
#'       lens=1,
#'       divergent=TRUE);
#'    names(j) <- "";
#'    names(j)[1] <- "original colors";
#'    names(j)[25] <- "color complements";
#'    j;
#' });
#' jamba::showColors(rc_ramps2, groupCellnotes=TRUE, groupByColors=FALSE);
#'
#' # test divergent color gradient
#' RdBu_r <- jamba::getColorRamp("RdBu_r");
#' rc_divergent <- color_complement(RdBu_r)
#' jamba::showColors(list(RdBu_r=RdBu_r, complement=rc_divergent));
#'
#' @export
color_complement <- function
(color,
 Hflip=180,
 Cfloor=NULL,
 Crange=c(5, 100),
 Lrange=c(10, 95),
 Cgrey=getOption("jam.Cgrey", 5),
 preset=getOption("colorjam.preset", "dichromat"),
 useWarpHue=TRUE,
 use_hsl=FALSE,
 verbose=FALSE,
 ...)
{
   if (length(jamba::rmNA(Crange)) == 0) {
      Crange <- c(60, 100);
   } else {
      Crange <- range(jamba::rmNA(Crange));
   }
   if (length(Cfloor) == 0) {
      Cfloor <- min(Crange, na.rm=TRUE);
   }
   if (length(jamba::rmNA(Lrange)) == 0) {
      Lrange <- c(0, 100);
   } else {
      Lrange <- range(jamba::rmNA(Lrange));
   }

   # convert input to HCL
   if (TRUE %in% use_hsl) {
      hcl <- jamba::col2hsl(color);
      rownames(hcl)[2] <- "C";
   } else {
      hcl <- jamba::col2hcl(color);
   }
   H <- hcl["H",];

   # print verbose output
   if (TRUE %in% verbose) {
      jamba::printDebug("color_complement(): ",
         "hcl (before):");
      print(round(digits=3, t(hcl)));
   }

   # rotate the color hue
   newH <- (Hflip + H) %% 360;

   # optionally adjust color wheel
   if (TRUE %in% useWarpHue) {
      H <- colorjam::h2hw(h=H,
         preset=preset);
   }

   # optionally revert the adjusted color wheel
   if (TRUE %in% useWarpHue) {
      newH <- colorjam::hw2h(h=newH,
         preset=preset);
   }
   hcl["H",] <- newH;

   # apply Crange
   non_grey <- (hcl["C",] > Cgrey);
   C_oob <- (non_grey &
         (hcl["C",] < Cfloor |
               hcl["C",] > max(Crange, na.rm=TRUE)))
   if (any(C_oob)) {
      hcl["C", non_grey] <- jamba::normScale(
         x=hcl["C", non_grey],
         from=Cfloor,
         to=max(Crange),
         low=Cgrey,
         high=100);
      # hcl["C", C_oob] <- jamba::noiseFloor(
      #    hcl["C", C_oob],
      #    ceiling=max(Crange, na.rm=TRUE),
      #    minimum=Cfloor);
   }
   # apply Lrange
   L_oob <- (non_grey &
         (hcl["L",] < min(Lrange) |
               hcl["L",] > max(Lrange)))
   if (any(L_oob)) {
      hcl["L", non_grey] <- jamba::normScale(
         x=hcl["L", non_grey],
         from=min(Lrange),
         to=max(Lrange),
         low=5,
         high=100)
      # hcl["L", L_oob] <- jamba::noiseFloor(
      #    hcl["L", L_oob],
      #    minimum=min(Lrange),
      #    ceiling=max(Lrange));
   }

   # print verbose output
   if (TRUE %in% verbose) {
      jamba::printDebug("color_complement(): ",
         "hcl (after):");
      print(round(digits=3, t(hcl)));
   }

   # convert back to hex
   if (TRUE %in% use_hsl) {
      rownames(hcl)[2] <- "S";
      color2 <- jamba::hsl2col(hcl);
   } else {
      color2 <- jamba::hcl2col(hcl);
   }
   return(color2);
}
