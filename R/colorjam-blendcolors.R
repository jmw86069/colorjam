
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
#' The process creates a unit vector for each color, whose length
#' is scaled relative to the saturation and alpha transparency,
#' with maximum length=1.
#' The average angle of these unit vectors is used as the final color
#' hue, and the distance from origin is used to derive the new color
#' chroma (similar to saturation). The end goal is for blue-yellow
#' to make green, blue-red to make purple, and red-yellow to make orange.
#' Current RGB color blending fails at least one of these criteria.
#'
#' This approach enables blending more than
#' two colors, which is fairly unique for color functions in R.
#' Note that the approach, when used to blend multiple very different
#' colors, tends to "muddy" the output color, similar to using finger
#' paints. Eventually if you add enough colors, it turns "bleh".
#'
#' ## New transparency arguments in 0.0.30.900
#'
#' * `apply_alpha=TRUE` will
#' return a color with appropriate alpha transparency based upon the
#' input colors. For example, blending red with red should always
#' produce red. However, blending 50% transparent red with 50% transparent
#' red should produce 75% transparent red. In effect, the redness
#' should build with more layers of transparent red.
#' * `flatten_alpha=TRUE` (default is FALSE) will flatten a transparent
#' blended color to the background, which is useful for situations
#' where the alpha transparency would be ignored. In other words,
#' 34% transparent red would be flattened to `"#FFAAAAFF"` and should
#' appear nearly identical to `"#FF000057"` in R plots.
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
#' @param apply_alpha `logical` default TRUE, whether to apply alpha
#'    transparency to the output color. In other words, if the input
#'    colors are transparent, the output will also contain transparency
#'    when `apply_alpha=TRUE`.
#' @param flatten_alpha `logical` default FALSE, whether to "flatten" the
#'    color transparency by blending with the current background color,
#'    defined by `bg`.
#'    This argument is only used when `apply_alpha=TRUE`.
#' @param bg `character` default NULL, used to define the default
#'    background color, used only when `flatten_alpha=TRUE`.
#'    When NULL, it checks for an open graphics device with `dev.list()`
#'    and if open it open it uses `par("bg")`. However if no
#'    graphics device is open, it does not call `par("bg")` because
#'    that would open a new graphics device. Therefore when no graphics
#'    device is open, NULL is converted to "white" background.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' blend_colors(c("red", "yellow"), do_plot=TRUE)
#'
#' blend_colors(c("blue", "gold"), do_plot=TRUE)
#'
#' blend_colors(c("dodgerblue", "firebrick2"), do_plot=TRUE)
#'
#' blend_colors(c("green", "dodgerblue"), do_plot=TRUE)
#'
#' blend_colors(c("red", "gold", "blue"), do_plot=TRUE)
#'
#' blend_colors(c("deeppink2", "yellow"), do_plot=TRUE)
#'
#' blend_colors(c("blue4", "darkorange1"), do_plot=TRUE)
#'
#' blend_colors(c("#FF000040", "#FF000080"), do_plot=TRUE)
#' title(main=paste0("blend identical transparent colors\n",
#'    "returning transparent colors\n"))
#'
#' blend_colors(c("#FF000040", "#FF000080"), do_plot=TRUE, flatten_alpha=TRUE)
#' title(main=paste0("blend identical transparent colors\n",
#'    "then flatten alpha transparency\n(same visual result)"))
#'
#' blend_colors(list(c("red", "yellow"), c("blue", "gold")), do_plot=FALSE)
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
 apply_alpha=FALSE,
 flatten_alpha=FALSE,
 bg=NULL,
 ...)
{
   ## 1. Convert colors to ryb
   ## 2. Implement color subtraction
   ##     new_r <- 255 - sqrt( (255 - r_1)^2 + (255 - r_2)^2 )
   ## 3. convert to rgb
   #x <- jamba::nameVector(c("red", "yellow", "blue"));
   preset <- match.arg(preset);
   if (length(apply_alpha) == 0) {
      apply_alpha <- FALSE
   }
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
            apply_alpha=apply_alpha,
            flatten_alpha=flatten_alpha,
            bg=bg,
            c_weight=c_weight);
      });
      if (TRUE %in% do_plot) {
         jamba::showColors(list(
            x=unlist(x),
            blended=setNames(rep(x_blends, lengths(x)),
               rep(seq_along(x_blends), lengths(x)))
         ));
      }
      x_blend <- x_blends[x_match];
      names(x_blend) <- names(x);
      return(x_blend);
   }

   ## weights are defined by transparency
   x_w <- jamba::col2alpha(x);

   ## apply alpha
   if (TRUE %in% apply_alpha) {
      new_alpha <- combine_alphas(x_w);
   } else {
      new_alpha <- 1;
   }

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
   if (new_alpha < 1) {
      new_col <- jamba::alpha2col(new_col,
         alpha=new_alpha);
      # Todo: Figure out how to flatten versus the back color
      if (TRUE %in% flatten_alpha) {
         # optionally flatten to the background color
         if (length(bg) == 0) {
            if (length(dev.list()) > 0) {
               bg <- gsub("^transparent$", "#FFFFFF", par("bg"))
            } else {
               bg <- "#FFFFFF";
            }
         }
         bg <- jamba::alpha2col(bg, alpha=(1 - jamba::col2alpha(new_col))^1.8);
         new_col <- blend_colors(x=c(rep(new_col, 1), bg), apply_alpha=FALSE)
      }
   }
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

#' Combine alpha transparency values with additive logic
#'
#' Combine alpha transparency values with additive logic
#'
#' Alpha transparency is defined as 0 for fully transparent, and 1
#' (or `max_alpha`) for fully opaque (not transparent).
#' The purpose is to permit combining multiple colors, where the
#' alpha transparency builds over time. Each color should contribute
#' some proportional fraction to the overall opacity of the final
#' color.
#'
#' The basic formula:
#'
#' `new_alpha <- alpha1 + (1 - alpha1) * alpha2`
#'
#' Or when `max_alpha` is defined:
#'
#' `new_alpha <- alpha1 + (max_alpha - alpha1) * alpha2`
#'
#' * Any `NA` values are considered equivalent to `0` and are
#' therefore not applied.
#' * All input alpha values are restricted to values between
#' `0` and `max_alpha`.
#' * For more than two values, each value is applied in series,
#' which works out to the same result if applied in any order.
#'
#' @family colorjam display
#'
#' @returns `numeric` value after combining alpha values.
#'
#' @param x `numeric` alpha value, typically limited between 0 and 1.
#'    The max value can be set using `max_alpha`.
#' @param max_alpha `numeric` default 1, the maximum permitted alpha value.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' # it progressively fills 50% of remaining transparency
#' combine_alphas(c(0.5, 0.5))
#' combine_alphas(c(0.5, 0.5, 0.5))
#' combine_alphas(c(0.5, 0.5, 0.5, 0.5))
#'
#' base_alpha <- 0.5;
#' new_alphas <- sapply(1:5, function(i){
#'    combine_alphas(rep(base_alpha, i))
#' })
#' names(new_alphas) <- seq_along(new_alphas);
#' bp <- barplot(new_alphas, ylim=c(0, 1.1), col="navy",
#'    xlab=paste0("Number of ",
#'    base_alpha,
#'    " alpha values combined"))
#' abline(h=1, lty=2);
#' jamba::shadowText(x=bp[, 1], y=new_alphas,
#'    col="white", cex=1.5,
#'    pos=1,
#'    xpd=TRUE,
#'    label=round(new_alphas, digits=3))
#' @export
combine_alphas <- function
(x,
 max_alpha=1,
 ...)
{
   #
   if (length(x) == 0) {
      return(numeric(0))
   }
   if (any(is.na(x))) {
      x <- x[!x %in% NA]
   }
   if (!inherits(x, c("numeric", "integer"))) {
      stop("x input must be numeric.")
   }
   if (any(x < 0)) {
      x[x < 0] <- 0;
   }
   if (length(max_alpha) == 0) {
      max_alpha <- 1;
   }
   max_alpha <- head(max_alpha, 1);
   if (max_alpha <= 0) {
      stop("max_alpha must be greater than 0.")
   }
   if (any(x > max_alpha)) {
      x[x > max_alpha] <- max_alpha;
   }
   if (length(x) <= 1) {
      return(x)
   }
   # apply alpha in series
   new_alpha <- head(x, 1) / max_alpha;
   for (i1 in seq(2, length(x))) {
      new_alpha <- new_alpha + (1 - new_alpha) * x[i1] / max_alpha;
   }
   new_alpha <- new_alpha * max_alpha;
   return(new_alpha);
}
