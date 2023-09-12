
#' Interpolation for degree angles
#'
#' Interpolation for degree angles
#'
#' This function is analogous to `stats::approx()` except for
#' the special case of degree angles. In this case, degrees
#' are confined to the range `[0, 360]`, and angle are smoothly
#' interpolated around the degrees of a circle.
#'
#' This function should work properly even when the degree angles
#' in `h2` are reversed, or offset. The only implicit requirement
#' is that angles in "from" should be mapped to one and only one
#' angle in "to".
#'
#' @param h1 `numeric` vector of degree angles, which should
#'    represent the "degree angles from".
#' @param h2 `numeric` vector of degree angles, which should
#'    represent the "degree angles to".
#' @param h `numeric` or `NULL`, where a `numeric` vector is a
#'    vector of degree angles "from" that should be converted to
#'    the corresponding interpolated angle "to", When `h is NULL`
#'    then the object returned is a `function` to be called
#'    to convert a numeric vector "from" to degree angles "to".
#' @param digits `integer` value indicating the number of digits
#'    of precision to use for the input `h1` and `h2` degree angles,
#'    used when confining to 360 degrees with `h1 %% 360`, and
#'    this step sometimes produces slight variations for equivalent
#'    values. For example `((12.2 %% 360) == (372.2 %% 360))` is not
#'    `TRUE` without rounding to `13` or fewer digits.
#' @param ... additional arguments are ignored.
#'
#' @family colorjam hue warp
#'
#' @examples
#' h_colors <- jamba::getColorRamp(c("white", "firebrick"), n=35, trimRamp=c(1, 0));
#'
#' h1 <- c(12.2, 27.3, 47.0, 66.5, 85.9, 106.3, 131.7,
#'    223.1, 263.2, 277.2, 307.7, 345.3, 372.2)
#' h2 <- seq(from=0, to=360, length.out=13)
#' h_from <- seq(from=0, to=360, length.out=36)[-36]
#' h_to <- approx_degrees(h1, h2, h_from)
#' par("mfrow"=c(2, 2))
#' display_degrees(h_from, col=h_colors)
#' display_degrees(h_to, col=h_colors)
#' plot(h_from, h_to, pch=20, col=h_colors)
#' par("mfrow"=c(1, 1))
#'
#' h2 <- c(12.2, 27.3, 47.0, 66.5, 85.9, 106.3, 131.7,
#'    223.1, 263.2, 277.2, 307.7, 345.3, 372.2)
#' h1 <- seq(from=0, to=360, length.out=13)
#' h_from <- seq(from=0, to=360, length.out=36)[-36]
#' h_to2 <- approx_degrees(h2, h1, h_from)
#' par("mfrow"=c(2, 2))
#' display_degrees(h_from, col=h_colors)
#' display_degrees(h_to2, col=h_colors)
#' plot(h_from, h_to2, pch=20, col=h_colors)
#' par("mfrow"=c(1, 1))
#'
#' h1 <- c(12.2, 27.3, 47.0, 66.5, 85.9, 106.3, 131.7,
#'    223.1, 263.2, 277.2, 307.7, 345.3, 372.2)
#' h2 <- rev((seq(from=0, to=360, length.out=13))[c(9:12,1:9)])
#' h_from <- seq(from=0, to=360, length.out=36)[-36]
#' h_to <- approx_degrees(h1, h2, h_from)
#' par("mfrow"=c(2, 2))
#' display_degrees(h_from, col=h_colors)
#' display_degrees(h_to, col=h_colors)
#' plot(h_from, h_to, pch=20, col=h_colors)
#' par("mfrow"=c(1, 1))
#'
#' # apply no transform
#' approx_degrees(h1=0, h2=0, h=c(0, 90, 180, 270))
#'
#' # apply 180 degree transform
#' approx_degrees(h1=0, h2=180, h=c(0, 90, 180, 270))
#' approx_degrees(h1=180, h2=0, h=c(0, 90, 180, 270))
#'
#' # flip the direction
#' approx_degrees(h1=c(1, 360), h2=c(359, 0), h=c(0, 90, 180, 270))
#' approx_degrees(h1=c(1, 360), h2=c(359, 0)+90, h=c(0, 90, 180, 270))
#' approx_degrees(h1=c(1, 360)+90, h2=c(359, 0), h=c(0, 90, 180, 270))
#'
#' # verify reverse h2 with break across 0-360
#' seq1 <- seq(from=0, to=330, by=30)
#' seq2 <- (rev(seq1) + 120) %% 360
#' seq_out <- seq(from=0, to=350, by=10);
#' approx_out <- approx_degrees(h1=seq1, h2=seq2, h=seq_out, verbose=TRUE)
#' plot(seq1, seq2, pch=20, col="blue", asp=1, ylim=c(0, 360))
#' points(seq_out, approx_out, col="red", add=TRUE, cex=2)
#'
#' # verify forward h2 with break across 0-360
#' seq1 <- seq(from=0, to=330, by=30)
#' seq2 <- (seq1 + 120) %% 360
#' seq_out <- seq(from=0, to=350, by=10);
#' approx_out <- approx_degrees(h1=seq1, h2=seq2, h=seq_out, verbose=TRUE)
#' plot(seq1, seq2, pch=20, col="blue", asp=1, ylim=c(0, 360))
#' points(seq_out, approx_out, col="red", add=TRUE, cex=2)
#'
#' new_h1h2 <- adjust_hue_warp(preset="dichromat", h2_shift=15, reverse_h2=TRUE)
#' hseq <- seq(from=0, to=350, by=15);
#' approx_degrees(h2=new_h1h2$h1, h1=new_h1h2$h2, h=hseq, verbose=FALSE)
#'
#' @export
approx_degrees <- function
(h1,
 h2,
 h=NULL,
 preset="custom",
 digits=10,
 verbose=FALSE,
 ...)
{
   # define h1,h2 from input h1,h2,preset
   h1h2 <- h2hwOptions(preset=preset,
      h1=h1,
      h2=h2,
      setOptions="FALSE")
   h1 <- h1h2$h1;
   h2 <- h1h2$h2;

   # apply rounding for convenience
   if (length(digits) > 0) {
      h1 <- round(digits=digits, h1)
      h2 <- round(digits=digits, h2)
   }

   # enforce h1 is increasing order
   h1h2_df <- jamba::mixedSortDF(data.frame(
      h1=h1,
      h2=h2));
   h1h2_df$h2_diff <- diff(c(tail(h1h2_df$h2, 1), h1h2_df$h2))

   if (verbose) {
      jamba::printDebug("approx_degrees(): ",
         "h1h2_df (input):");
      print(h1h2_df);
   }
   h1 <- h1h2_df$h1;
   h2 <- h1h2_df$h2;
   h2diff <- diff(h2);
   h2_sign <- 1;
   if (length(h2diff) > 0) {
      h2_signs <- sign(h2diff)
      h2_sign <- sign(mean(1e-9 + h2_signs[h2_signs != 0]))
   }

   # check for duplicated numeric values
   if (any(duplicated(h2))) {
      h2_dupes <- unique(h2[duplicated(h2)]);
      for (h2_dupe in h2_dupes) {
         i_dupe <- which(h2 %in% h2_dupe)
         i_dupe_seq <- seq(from=0, by=h2_sign * 1e-9, length.out=length(i_dupe))
         h2[i_dupe] <- h2[i_dupe] + i_dupe_seq;
      }
      if (verbose) {
         jamba::printDebug("approx_degrees(): ",
            "adjusted duplicate h2 values");
      }
   }
   if (any(duplicated(h1))) {
      h1_dupes <- unique(h1[duplicated(h1)]);
      for (h1_dupe in h1_dupes) {
         i_dupe <- which(h1 %in% h1_dupe)
         i_dupe_seq <- seq(from=0, by=1 * 1e-9, length.out=length(i_dupe))
         h1[i_dupe] <- h1[i_dupe] + i_dupe_seq;
      }
      if (verbose) {
         jamba::printDebug("approx_degrees(): ",
            "adjusted duplicate h1 values");
      }
   }

   # detect breaks in sequence (across range 0-360)
   if (h2_sign < 0 && any(h1h2_df$h2_diff > 180)) {
      which_flips <- setdiff(which(h1h2_df$h2_diff > 180), 1);
      for (which_flip in which_flips) {
         flip_seq <- seq(from=which_flip, to=nrow(h1h2_df))
         h1h2_df$h2[flip_seq] <- h1h2_df$h2[flip_seq] - 360;
      }
      # update h2
      h2 <- h1h2_df$h2;
      h1h2_df$h2_diff <- diff(c(tail(h1h2_df$h2, 1), h1h2_df$h2))
      if (verbose) {
         jamba::printDebug("approx_degrees(): ",
            "correcting discontinuity in h2 angles crossing below 0");
         jamba::printDebug("approx_degrees(): ",
            "h1h2_df:");
         print(h1h2_df);
      }
   } else if (h2_sign >= 0 && any(h1h2_df$h2_diff < 180)) {
      which_flips <- setdiff(which(h1h2_df$h2_diff < -180), 1);
      for (which_flip in which_flips) {
         flip_seq <- seq(from=which_flip, to=nrow(h1h2_df))
         h1h2_df$h2[flip_seq] <- h1h2_df$h2[flip_seq] + 360;
      }
      # update h2
      h2 <- h1h2_df$h2;
      if (verbose) {
         jamba::printDebug("approx_degrees(): ",
            "correcting discontinuity in h2 angles crossing above 360");
         jamba::printDebug("approx_degrees(): ",
            "h1h2_df:");
         print(h1h2_df);
      }
   }

   # new_h1
   h1_min_span <- floor(min(h1)/360) * 360
   h1_max_span <- ceiling(max(h1)/360) * 360
   h1_range_span <- ceiling(diff(range(h1))/360 + 1e-10) * 360;
   new_h1 <- h1;
   new_h2 <- h2;
   if (h1_min_span >= 0) {
      new_h1 <- c(h1 - h1_range_span, new_h1)
      if (h2_sign >= 0) {
         new_h2 <- c(h2 - h1_range_span, new_h2)
      } else {
         new_h2 <- c(h2 + h1_range_span, new_h2)
      }
   }
   if (h1_max_span <= 360) {
      new_h1 <- c(new_h1, h1 + h1_range_span)
      if (h2_sign >= 0) {
         new_h2 <- c(new_h2, h2 + h1_range_span)
      } else {
         new_h2 <- c(new_h2, h2 - h1_range_span)
      }
   }
   # data.frame(new_h1, new_h2)
   # data.frame(diff(new_h1), diff(new_h2))

   if (verbose) {
      jamba::printDebug("approx_degrees(): ",
         "Expanded table:");
      print(data.frame(h1=new_h1,
         h1_diff=c(NA, diff(new_h1)),
         h2=new_h2,
         h2_diff=c(NA, diff(new_h2))));
   }

   # define approxfun
   h_fun <- function(h) {
      h_new <- approx(
         x=new_h1,
         y=new_h2,
         ties="ordered",
         xout=(h %% 360))$y %% 360;
      return(h_new);
   }

   # return the appropriate value or function
   if (length(h) > 0) {
      return(h_fun(h))
   }
   return(h_fun);

   if (head(h1h2df1$h1, 1) > 0) {
      h1 <- c(tail(h1h2df1$h1, 1) - 360, h1);
      if (h2dir > 0) {
         if (tail(h1h2df1$h2, 1) > head(h1h2df1$h2, 1)) {
            h2 <- c(tail(h1h2df1$h2, 1) - 360, h2);
         } else {
            h2 <- c(tail(h1h2df1$h2, 1), h2);
         }
      } else {
         if (tail(h1h2df1$h2, 1) < head(h1h2df1$h2, 1)) {
            h2 <- c(tail(h1h2df1$h2, 1) + 360, h2);
         } else {
            h2 <- c(tail(h1h2df1$h2, 1), h2);
         }
      }
   }
   if (tail(h1h2df1$h1, 1) < 360) {
      h1 <- c(h1, head(h1h2df1$h1, 1) + 360);
      if (h2dir > 0) {
         if (head(h1h2df1$h2, 1) < tail(h1h2df1$h2, 1)) {
            h2 <- c(h2, head(h1h2df1$h2, 1) + 360);
         } else {
            h2 <- c(h2, head(h1h2df1$h2, 1));
         }
      } else {
         if (head(h1h2df1$h2, 1) > tail(h1h2df1$h2, 1)) {
            h2 <- c(h2, head(h1h2df1$h2, 1) - 360);
         } else {
            h2 <- c(h2, head(h1h2df1$h2, 1));
         }
      }
   }
   h1h2df2 <- data.frame(h1=h1, h2=h2)
   if (verbose) {
      jamba::printDebug("approx_degrees(): ",
         "h1h2df2:");
      print(h1h2df2);
   }

   # define approx() function that includes %% 360
   # to limit output angles between 0 and 360
   h_fun <- function(h) {
      h_new <- approx(
         x=h1,
         y=h2,
         ties="ordered",
         xout=(h %% 360))$y %% 360;
      return(h_new);
   }
   if (length(h) > 0) {
      return(h_fun(h))
   }
   return(h_fun);
}

#' Display degree angles around a unit circle
#'
#' Display degree angles around a unit circle
#'
#' @family colorjam hue warp
#'
#' @param x `numeric` angles in degrees
#' @param x2 `numeric` angles in degrees, optionally used to show
#'    when an angle changes from `x` to `x2`.
#' @param add `logical` indicating whether to add to an existing open
#'    plot device.
#' @param col `character` vector of colors recycled to `length(x)`.
#' @param lwd `numeric` line width.
#' @param top_degree `numeric` the angle in degrees for the top
#'    (12 o'clock) position of the graph.
#' @param clockwise `logical` indicating whether angles proceed from
#'    the `top_degree` in clockwise (top, top-right, right, bottom-right,
#'    bottom, bottom-left, left, top-left, top) or counter-clockwise
#'    orientation.
#' @param r1,r2 `numeric` radius values for the start and end position
#'    of each arrow vector drawn.
#' @param r0 `numeric` radius used for the axis with labeled angles.
#' @param arrow.length `numeric` passed to `arrows()` to define the
#'    arrow head length.
#' @param xlim,ylim `numeric` x-axis and y-axis plot limits, respectively.
#'    When not supplied, they automatically use 1.1 times the higher
#'    value from `c(r1, r2, r0, 1)` so that the minimum radius is at
#'    at least `1` unless specified otherwise.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' display_degrees(c(0, 45, 180, 289),
#'    lwd=4,
#'    col=c("red", "blue2", "purple3", "gold"))
#'
#' display_degrees(c(90, 270), col=c("purple", "gold"), r1=0, r2=0.5)
#' display_degrees(c(0, 45), col=c("red", "blue"), r1=0.5, r2=1, add=TRUE)
#'
#' display_degrees(c(90, 270), col=c("purple", "gold"), r1=0, r2=0.5,
#'    arrow.length=0)
#' display_degrees(c(90, 270), x2=c(100, 280), col=c("purple", "gold"),
#'    r1=0.5, r2=1, add=TRUE)
#' display_degrees(c(90, 270), x2=c(80, 260), col=c("purple", "gold"),
#'    r1=0.5, r2=1, add=TRUE)
#'
#' @export
display_degrees <- function
(x,
 x2=x,
 add=FALSE,
 col="darkorange",
 lwd=2,
 top_degree=0,
 clockwise=TRUE,
 r1=0,
 r2=1,
 r0=1,
 arrow.length=0.2,
 xlim=NULL,
 ylim=NULL,
 asp=1,
 ...)
{
   if (TRUE %in% clockwise) {
      step_degrees <- (rev(seq(from=0, to=360, by=5)) + 90 + top_degree) %% 360;
      degree_trans <- function(n){(-n + 90 + top_degree) %% 360}
      degree_trans(c(0, 45))
   } else {
      step_degrees <- (seq(from=0, to=360, by=5) + 90 - top_degree) %% 360;
      degree_trans <- function(n){(n + 90 - top_degree) %% 360}
      degree_trans(c(0, 45))
   }
   if (!add) {
      label_degrees <- (seq(from=0, to=360, by=5) + 0 + top_degree*0) %% 360;
      label_degrees[(label_degrees %% 45) != 0] <- "";
      ldf <- data.frame(step_degrees, label_degrees);
      head(ldf, 10)
      ldf$x1 <- cos(jamba::deg2rad(step_degrees)) * r0;
      ldf$y1 <- sin(jamba::deg2rad(step_degrees)) * r0;
      ldf1 <- subset(ldf, nchar(as.character(label_degrees)) > 0)
      opar <- par(lend="square",
         ljoin="mitre",
         mar=c(1,1,1,1));
      if (length(xlim) == 0) {
         xlim <- max(c(r1, r2, r0, 1)) * c(-1.1, 1.1);
      }
      if (length(ylim) == 0) {
         ylim <- max(c(r1, r2, r0, 1)) * c(-1.1, 1.1);
      }
      on.exit(par(opar));
      plot(x=ldf$x1,
         y=ldf$y1,
         col="grey",
         bty="n",
         xaxt="n",
         yaxt="n",
         type="l",
         asp=asp,
         xlim=xlim,
         ylim=ylim);
      points(x=ldf1$x1,
         y=ldf1$y1,
         pch=20)
      text(x=ldf1$x1 * 1.1,
         y=ldf1$y1 * 1.05,
         labels=ldf1$label_degrees)
   }
   # draw grey ring for each radius
   for (r in setdiff(c(r1, r2), 0)) {
      lines(
         x=cos(jamba::deg2rad(
            degree_trans(step_degrees))) * r,
         y=sin(jamba::deg2rad(
            degree_trans(step_degrees))) * r,
         col="grey")
   }
   # draw arrow for each angle in x
   col <- rep(col, length.out=length(x));
   lwd <- rep(lwd, length.out=length(x));
   if (length(x2) == 0) {
      x2 <- x;
   }
   if (length(x2) != length(x)) {
      x2 <- rep(x2, length.out=length(x))
   }
   for (i in seq_along(x)) {
      x0 <- cos(jamba::deg2rad(degree_trans(x[i]))) * r1;
      y0 <- sin(jamba::deg2rad(degree_trans(x[i]))) * r1;
      x1 <- cos(jamba::deg2rad(degree_trans(x2[i]))) * r2;
      y1 <- sin(jamba::deg2rad(degree_trans(x2[i]))) * r2;
      arrows(x0=x0,
         y0=y0,
         x1=x1,
         y1=y1,
         col=col[i],
         length=arrow.length,
         lwd=lwd[i]);
      if (1 == 2 && length(names(x)) > 0) {
         text(x=x1,
            y=y1,
            labels=names(x)[i],
            adj=unlist(degrees_to_adj(x[i], expand=c(1,1))[1,c("adjx","adjy")]))
      }

   }
}
