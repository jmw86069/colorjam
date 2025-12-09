
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
#' @family colorjam assignment
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
#' @param useWarpHue `logical` default TRUE, whether to use the preset.
#'    When FALSE it effectively uses preset='rgb' which performs no
#'    particular hue adjustments. Note the 'rgb' color wheel is
#'    not particularly intuitive for complementary colors.
#' @param use_hsl `logical` default TRUE, use HSL to determine equivalent
#'    complementary values for Saturation (S) and Lightness (L), rather
#'    than HCL where Chroma (C) and Luminance (L) is not at all consistent
#'    across the color wheel.
#' @param ... additional arguments are ignored.
#'
#' @family colorjam core
#'
#' @returns `character` vector of complementary colors.
#'
#' @examples
#' n <- 5;
#' rc <- colorjam::rainbowJam(n);
#' rc_comp <- color_complement(rc, preset="dichromat2");
#' rc_comp2 <- color_complement(rc, preset="dichromat2", useWarpHue=FALSE);
#' rc_comp2rgb <- color_complement(rc, preset="rgb");
#' rc_comp3 <- color_complement(rc, preset="ryb");
#' jamba::showColors(list(rainbowJam=rc,
#'    `color_complement\n(preset="dichromat")`=rc_comp,
#'    `color_complement\n(useWarpHue=FALSE)`=rc_comp2,
#'    `color_complement\n(preset="rgb")`=rc_comp2rgb,
#'    `color_complement\n(preset="ryb")`=rc_comp3));
#'
#' n <- 8
#' rc <- colorjam::rainbowJam(n, preset="ryb");
#' rc_comp <- color_complement(rc, preset="ryb");
#' jamba::showColors(list(
#'    `rainbowJam\n(preset="ryb")`=rc,
#'    `color_complement\n(preset="ryb")`=rc_comp));
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
 preset=getOption("colorjam.preset", "dichromat2"),
 useWarpHue=TRUE,
 use_hsl=TRUE,
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
      # convert HSL hue to HCL hue for consistency
      hcl["H",] <- hsl_to_hcl_hue(hcl["H",]);
   } else {
      hcl <- jamba::col2hcl(color);
   }
   H <- hcl["H",];

   # print verbose output
   if (TRUE %in% verbose) {
      jamba::printDebug("color_complement(): ",
         "preset: ", preset);
      # jamba::printDebug("color_complement(): ",
      #    "hcl (before):");
      # print(round(digits=3, t(hcl)));
      jamba::printDebug("color_complement(): ",
         "H original:        ", jamba::padInteger(round(H)));
   }

   ## rotate the color hue
   # newH1 <- (Hflip + H) %% 360;

   # optionally adjust color wheel
   if (TRUE %in% useWarpHue) {
      H <- colorjam::h2hw(h=H, preset=preset);
      if (TRUE %in% verbose) {
         jamba::printDebug("color_complement(): ",
            "H warped:          ", jamba::padInteger(round(H)));
      }
   }

   # 0.0.34.900: rotate the color hue after warp
   newH <- (Hflip + H) %% 360;
   if (TRUE %in% verbose) {
      jamba::printDebug("color_complement(): ",
         "newH from warped H:", jamba::padInteger(round(newH)));
   }

   # revert the warped hue if needed
   if (TRUE %in% useWarpHue) {
      newH <- colorjam::hw2h(h=newH,
         preset=preset);
      if (TRUE %in% verbose) {
         jamba::printDebug("color_complement(): ",
            "un-warped newH:    ", jamba::padInteger(round(newH)));
      }
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
      # convert HCL hue back to HSL hue
      hcl["H",] <- hcl_to_hsl_hue(hcl["H",]);
      # rename dimensions to match H,S,L
      rownames(hcl)[2] <- "S";
      color2 <- jamba::hsl2col(hcl);
   } else {
      color2 <- jamba::hcl2col(hcl);
   }
   return(color2);
}
