
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
#' color_pie(rainbowJam(15, nameStyle="none"),
#'    sub="rainbowJam(15)")
#'
#' n <- 12;
#' color_pie(list(
#'    rainbowJam(n),
#'    rainbow(n)),
#'    main="rainbowJam(12) [outer]\n       rainbow(12) [inner]")
#'
#' n <- 15
#' color_pie(list(
#'    rainbowJam(n),
#'    rainbow_hcl(n, c=85)),
#'    main="rainbowJam(15) [outer]\nrainbow_hcl(15) [inner]")
#'
#' rainbow_list <- lapply(4*c(5,4,2,1), function(n){
#'    rainbowJam(n, preset="ryb", step='v23', nameStyle="n");
#' });
#' color_pie(rainbow_list,
#'    main="preset='ryb'\nstep='v23",
#'    sub="rainbowJam()\nn=4, 8, 16, 20")
#'
#' rainbow_list2 <- lapply(4*c(5,4,2,1), function(n){
#'    rainbowJam(n, nameStyle="n");
#' });
#' color_pie(rainbow_list2,
#'    main="default settings",
#'    sub="rainbowJam()\nn=4, 8, 16, 20")
#'
#' rainbow_list3 <- lapply(4*c(5,4,2,1), function(n){
#'    rainbowJam(n, preset="dichromat", step="v23", nameStyle="n");
#' });
#' color_pie(rainbow_list3,
#'    main="preset='dichromat'\nstep='v23'",
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

   # op <- par(no.readonly=TRUE);
   if (length(colors) == 1) {
      lwd <- 0.001;
   }
   op <- par("xpd"=TRUE, "lwd"=lwd);
   on.exit(par(op));
   if (TRUE %in% add) {
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
      label_angles <- round(head(seq(from=init.angle - 180 / length(colors),
         to=(init.angle - 360 - 180 / length(colors)),
         length.out=length(colors) + 1), -1)) %% 360;
      label_angles1 <- ((label_angles + 89) %% 180 - 89) %% 360;
      angle_switch <- (label_angles != label_angles1) * 1;
      lx <- cos(jamba::deg2rad(label_angles)) * label_radius;
      ly <- sin(jamba::deg2rad(label_angles)) * label_radius;
      for (k in split(seq_along(lx), paste0(label_angles, "_", angle_switch))) {
         jamba::shadowText(x=lx[k],
            y=ly[k],
            adj=c(head(angle_switch[k], 1), 0.5),
            col=jamba::setTextContrastColor(colors[k], useGrey=15),
            labels=names(colors)[k],
            srt=head(label_angles1[k], 1))
      }
   }
   invisible(colors);
}
