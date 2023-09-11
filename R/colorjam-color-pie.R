
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
 radius=1.1,
 label_radius=radius*0.65,
 add=FALSE,
 init.angle=NULL,
 clockwise=TRUE,
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
               init.angle=init.angle,
               clockwise=clockwise,
               radius=radius_seq[i],
               label_radius=radius_seq[i]*0.92 + radius_diff,
               ...);
         } else {
            color_pie(colors=colors[[i]],
               border=border[[i]],
               lwd=lwd,
               add=(i > 1),
               init.angle=init.angle,
               clockwise=clockwise,
               radius=radius_seq[i],
               label_radius=radius_seq[i]*0.92 + radius_diff);
         }
      });
      return(invisible(l));
   }
   if (length(init.angle) == 0) {
      if (clockwise) {
         init.angle <- 90 + 360 / length(colors) / 2;
      } else {
         init.angle <- 90 - 360 / length(colors) / 2;
      }
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
      init.angle=init.angle,
      clockwise=clockwise,
      ...);
   if (length(names(colors)) > 0) {
      par("new"=TRUE);
      par("lwd"=0.001);
      jamba::printDebug("init.angle:", round(init.angle));
      pie(x=rep(1, length.out=length(colors)),
         col="transparent",
         labels=names(colors),
         border=FALSE,
         init.angle=init.angle,
         clockwise=clockwise,
         radius=label_radius,
         ...);
   }
   invisible(colors);
}
