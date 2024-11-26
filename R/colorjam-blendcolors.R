
#' Blend multiple colors
#'
#' Blend multiple colors
#'
#' This function is intended to blend two or more colors,
#' by default using "paint mixing" style, similar to subtractive
#' color mixing. It accomplishes this goal by using a red-yellow-blue
#' color wheel (very similar to cyan-yellow-magenta), then determines
#' the average color hue with appropriate loss of color saturation.
#'
#' This function also utilized color transparency, applied internally
#' as relative color weights, during the color mixing process.
#'
#' This function blends multiple colors, including several useful
#' features:
#'
#' * color wheel red-yellow-blue, subtractive color mixing
#' * can blend more than two colors at once
#' * accounts for transparency of individual colors
#'
#' The basic design guide was to meet these expectations:
#'
#' * red + yellow = orange
#' * blue + yellow = green
#' * red + blue = purple
#' * blue + red + yellow = some brown/gray substance
#'
#' The input `x` can be a vector of colors, or a `list`. When
#' `x` is a `list` then the unique vectors are blended, returning
#' a vector with length `length(x)`.
#'
#' The default additive color mixing, with red-green-blue colors
#' used in electronic monitors, does not meet these criteria.
#' (In no logical paint mixing exercise would someone expect that
#' mixing red and green would make yellow; or that
#' blue and yellow would make grey.)
#'
#' In general the function performs well, with some exceptions
#' where the color hue angle is not well-normalized opposite
#' its complementary color, and therefore does not make the
#' expected "brownish/grey" output. Examples include
#' `blend_colors(c("yellow", "purple"))` which is closer
#' to blue + yellow = green, because purple is also composed
#' of blue with some red. Indeed, the R color hue for purple
#' is 283; the hue for blue is 266; the hue for red is 12 (372);
#' which means purple is substantially closer to blue than red.
#' A suitable workaround in this case is to use
#' `blend_colors(c("yellow", "deeppink4"))`.
#'
#' @family colorjam core
#'
#' @return `character` vector with blended color; when input `x`
#'    is a `list` the returned vector will have length `length(x)`.
#'
#' @param x `character` vector of R colors in hex format, or `list`
#'    of color vectors, where each vector will be independently
#'    blended.
#' @param preset `character` value indicating the color wheel preset,
#'    default `"ryb"` for red-yellow-blue paint-like color blending.
#'    It is passed to `colorjam::h2hwOptions()`, and permits any value
#'    returned by `colorjam_presets()`.
#' @param lens `numeric` value used to influence the color saturation
#'    after averaging color wheel angles.
#' @param do_plot `logical` indicating whether to depict the color
#'    blend operation using `jamba::showColors()`.
#' @param c_weight `numeric` value used to weight the average color
#'    chroma (saturation) using the mean chroma values of the input
#'    colors. When `c_weight=0` the chroma uses the radius returned
#'    by the mean color wheel angle.
#' @param c_floor `numeric` value indicating the `C` chroma HCL value
#'    below which a color is considered to be "grey" and unsaturated.
#'    When this happens, the hue contribution is set to 0.001 relative
#'    to other colors being blended. This correction is done because
#'    every color is assigned one `H` hue value in HCL, even when
#'    the `C` chroma (saturation) is zero, therefore these colors
#'    effectively have no `H` hue.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' blend_colors(c("red", "yellow"), do_plot=TRUE)
#'
#' blend_colors(c("blue", "gold"), do_plot=TRUE)
#'
#' blend_colors(c("blue", "red3"), do_plot=TRUE)
#'
#' blend_colors(c("dodgerblue", "springgreen3"), do_plot=TRUE)
#'
#' blend_colors(c("green", "dodgerblue"), do_plot=TRUE)
#'
#' blend_colors(c("red", "gold", "blue"), do_plot=TRUE)
#'
#' blend_colors(c("green4", "red"), do_plot=TRUE)
#'
#' blend_colors(c("deeppink4", "gold"), do_plot=TRUE)
#'
#' blend_colors(c("blue4", "darkorange1"), do_plot=TRUE)
#'
#' @export
blend_colors <- function
(x,
 preset=c("ryb",
    "none",
    "dichromat",
    "rgb",
    colorjam_presets()),
 h1=NULL,
 h2=NULL,
 do_plot=FALSE,
 lens=0,
 c_weight=0.2,
 c_floor=12,
 ...)
{
   ## 1. Convert colors to ryb
   ## 2. Implement color subtraction
   ##     new_r <- 255 - sqrt( (255 - r_1)^2 + (255 - r_2)^2 )
   ## 3. convert to rgb
   #x <- jamba::nameVector(c("red", "yellow", "blue"));
   preset <- match.arg(preset);
   if (length(c_floor) == 0) {
      c_floor <- 0;
   }
   c_floor <- head(c_floor, 1);

   ## handle list input
   if (is.list(x)) {
      x_unique <- unique(x);
      x_match <- match(x, x_unique);
      x_blends <- sapply(x_unique, function(x1){
         blend_colors(x=x1,
            preset=preset,
            lens=lens,
            h1=h1,
            h2=h2,
            do_plot=FALSE,
            c_weight=c_weight);
      });
      x_blend <- x_blends[x_match];
      names(x_blend) <- names(x);
      return(x_blend);
   }

   ## weights are defined by transparency
   x_w <- jamba::col2alpha(x);

   x_HCL <- jamba::col2hcl(x);
   x_w_use <- ifelse(x_HCL["C",] <= c_floor,
      x_w - x_w*0.999,
      ifelse(x_HCL["C",] < 20,
         x_w - x_w * 0.8,
         x_w));

   ## adjust hue using color wheel preset
   if (length(h1) == 0 || length(h2) == 0) {
      h1h2 <- colorjam::h2hwOptions(preset=preset,
         setOptions="FALSE");
   } else {
      h1h2 <- list(h1=h1, h2=h2);
   }
   h_rgb <- x_HCL["H",];
   # 0.0.28.900: changed to pass preset="custom" so it uses h1,h2 values
   h_ryb <- colorjam::h2hw(h=h_rgb,
      h1=h1h2$h1,
      h2=h1h2$h2,
      preset="custom");

   ## mean hue angle
   if (all(x_w_use == 0)) {
      x_w_use <- rep(0.001, length(x_w_use));
   }
   h_ryb_mean_v <- mean_angle(h_ryb,
      lens=lens,
      w=x_w_use);
   h_ryb_mean <- h_ryb_mean_v["deg"];

   # 0.0.28.900: changed to pass preset="custom" so it uses h1,h2 values
   h_rgb_mean <- colorjam::hw2h(h=h_ryb_mean,
      h1=h1h2$h1,
      h2=h1h2$h2,
      preset="custom");

   mean_radius <- weighted.mean(c(1, h_ryb_mean_v["radius2"]),
      w=c(c_weight, 1));
   x_mean_C <- weighted.mean(x_HCL["C",], w=x_w) * mean_radius;
   x_mean_L <- weighted.mean(x_HCL["L",], w=x_w);
   new_HCL <- as.matrix(c(H=h_rgb_mean,
      C=unname(x_mean_C),
      L=x_mean_L,
      alpha=1));
   new_col <- jamba::hcl2col(new_HCL);
   if (do_plot) {
      jamba::showColors(list(x=x,
         blended=rep(new_col, length(x))));
   }
   return(new_col);

}

#' Calculate the mean angle
#'
#' Calculate the mean angle
#'
#' This function takes a vector of angles in degrees (0 to 360 degrees)
#' and returns the mean angle based upon the average of unit vectors.
#'
#' The function also optionally accomodates weighted mean values,
#' if a vector of weights is supplied as `w`.
#'
#' Part of the intent of this function is to be used for color blending
#' methods, for example taking the average color hue from a vector of
#' colors. For this purpose, some colors may have varying color saturation
#' and transparency, which are mapped here as weight `w`. Colors which are
#' fully transparent should therefore have weight `w=0` so they do not
#' contribute to the resulting average color hue. Also during color blending
#' operations, the resulting color saturation is adjusted using the `lens`
#' argument, the default `lens=-5` has a tendency to increase intermediate
#' color saturation.
#'
#' @family colorjam hue warp
#'
#' @return `numeric` vector that contains
#'    * `degree` the mean angle in degrees
#'    * `radius` the actual radius based upon mean unit vectors
#'    * `radius2` the adjusted radius using `jamba::warpAroundZero()`
#'
#' @param x `numeric` vector of angles in degrees
#' @param w `numeric` vector representing weights
#' @param do_plot `logical` indicating whether to create a visual summary plot
#' @param lens `numeric` value passed to `jamba::warpAroundZero()` to adjust
#'    the radius
#' @param ... additional arguments are ignored
#'
#' @examples
#' mean_angle(c(120, 45), do_plot=TRUE);
#'
#' @export
mean_angle <- function
(x,
 w=NULL,
 do_plot=FALSE,
 lens=-5,
 ...)
{
   xy <- data.frame(x=sin(jamba::deg2rad(x)),
      y=cos(jamba::deg2rad(x)));
   if (length(w) == 0) {
      w <- 1;
   }
   w <- rep(w,
      length.out=length(x));

   # if (jamba::check_pkg_installed("matrixStats")) {
   #    xy_mean <- matrixStats::colWeightedMeans(
   #       x=as.matrix(xy),
   #       w=w);
   # } else {
      xy_mean <- apply(as.matrix(xy), 2, function(i){
         weighted.mean(x=i, w=w)
      })
   # }
   xy_m <- matrix(ncol=2, byrow=TRUE,
      c(0, 0, xy_mean));
   x_radius <- dist(xy_m);

   x_radius2 <- jamba::warpAroundZero(x_radius,
      xCeiling=1,
      lens=lens);

   x_deg <- jamba::rad2deg(atan2(x=xy_mean["y"], y=xy_mean["x"])) %% 360;

   if (do_plot) {
      jamba::nullPlot(xlim=c(-1,1),
         ylim=c(-1,1),
         asp=1,
         doBoxes=FALSE);
      aseq <- seq(from=0, to=360, by=2);
      lines(x=sin(jamba::deg2rad(aseq)),
         y=cos(jamba::deg2rad(aseq)),
         type="l",
         lty="dotted");
      arrows(x0=0,
         y0=0,
         x1=xy$x * w,
         y1=xy$y * w,
         lwd=2,
         angle=30);
      arrows(x0=0,
         y0=0,
         x1=xy$x,
         y1=xy$y,
         lty="dotted",
         lwd=2,
         angle=90);
      arrows(x0=0,
         y0=0,
         x1=sin(jamba::deg2rad(x_deg)) * x_radius,
         y1=cos(jamba::deg2rad(x_deg)) * x_radius,
         lty="solid",
         lwd=2,
         angle=90,
         col="dodgerblue");
      arrows(x0=0,
         y0=0,
         x1=sin(jamba::deg2rad(x_deg)) * x_radius2,
         y1=cos(jamba::deg2rad(x_deg)) * x_radius2,
         lwd=4,
         angle=30,
         col="darkorange1");
   }
   c(deg=unname(x_deg),
      radius=x_radius,
      radius2=x_radius2);
}

