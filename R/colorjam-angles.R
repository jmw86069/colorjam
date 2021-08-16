
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
#' @export
approx_degrees <- function
(h1,
 h2,
 h=NULL,
 digits=5,
 verbose=FALSE,
 ...)
{
   #h2 <- h2[c(7:12, 1:7)]
   if (length(digits) > 0) {
      h1 <- round(digits=digits, h1)
      h2 <- round(digits=digits, h2)
   }
   #h1dir <- unique(sign(diff(h1)));

   # force h1 to increasing order
   h1h2df1 <- jamba::mixedSortDF(byCols="h1",
      data.frame(h1=h1,
         h2=h2))
   if (verbose) {
      jamba::printDebug("approx_degrees() :",
         "h1h2df1:");
      print(h1h2df1);
   }
   h2diff <- diff(h1h2df1$h2);
   if (length(h2diff) > 0) {
      h2dir <- as.numeric(names(head(
         tcount(sign(diff(h1h2df1$h2))), 1)));
   } else {
      h2dir <- 1;
   }

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
      jamba::printDebug("approx_degrees() :",
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
#' @examples
#' display_degrees(c(0, 45), col=c("red", "blue"))
#'
#' @export
display_degrees <- function
(x,
 add=FALSE,
 col="darkorange",
 lwd=2,
 top_degree=0,
 clockwise=TRUE,
 r1=0,
 r2=1,
 ...)
{
   top_degree <- 0;clockwise <- TRUE;
   if (clockwise) {
      step_degrees <- (rev(seq(from=0, to=360, by=5)) + 90) %% 360;
      degree_trans <- function(n){(-n + 90) %% 360}
      degree_trans(c(0, 45))
   } else {
      step_degrees <- (seq(from=0, to=360, by=5) + 90) %% 360;
      degree_trans <- function(n){(n + 90) %% 360}
      degree_trans(c(0, 45))
   }
   if (!add) {
      label_degrees <- (seq(from=0, to=360, by=5) + top_degree) %% 360;
      label_degrees[(label_degrees %% 45) != 0] <- "";
      ldf <- data.frame(step_degrees, label_degrees);
      ldf$x1 <- cos(jamba::deg2rad(step_degrees)) * r2;
      ldf$y1 <- sin(jamba::deg2rad(step_degrees)) * r2;
      ldf1 <- subset(ldf, nchar(as.character(label_degrees)) > 0)
      opar <- par(lend="square",
         ljoin="mitre",
         mar=c(1,1,1,1));
      on.exit(par(opar));
      plot(x=ldf$x1,
         y=ldf$y1,
         bty="n",
         xaxt="n",
         yaxt="n",
         type="l",
         asp=1,
         xlim=r2 * c(-1.1, 1.1),
         ylim=r2 * c(-1.1, 1.1));
      points(x=ldf1$x1,
         y=ldf1$y1,
         pch=20)
      text(x=ldf1$x1 * 1.2,
         y=ldf1$y1 * 1.1,
         labels=ldf1$label_degrees)
   }
   # draw arrow for each angle in x
   col <- rep(col, length.out=length(x));
   lwd <- rep(lwd, length.out=length(x));
   for (i in seq_along(x)) {
      x0 <- cos(jamba::deg2rad(degree_trans(x[i]))) * r1;
      y0 <- sin(jamba::deg2rad(degree_trans(x[i]))) * r1;
      x1 <- cos(jamba::deg2rad(degree_trans(x[i]))) * r2;
      y1 <- sin(jamba::deg2rad(degree_trans(x[i]))) * r2;
      arrows(x0=x0,
         y0=y0,
         x1=x1,
         y1=y1,
         col=col[i],
         length=0.2,
         lwd=lwd[i]);
      if (1 == 2 && length(names(x)) > 0) {
         text(x=x1,
            y=y1,
            labels=names(x)[i],
            adj=unlist(degrees_to_adj(x[i], expand=c(1,1))[1,c("adjx","adjy")]))
      }

   }
}
