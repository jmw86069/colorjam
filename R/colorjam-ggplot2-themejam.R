
#' Jam default theme for ggplot2
#'
#' Jam default theme for ggplot2
#'
#' This function applies some default theme settings to ggplot2, mainly
#' taking away the default grey newspaper background color, also rotates the
#' x-axis label text to 60 degrees, to accomodate longer labels without
#' overlaps.
#'
#' @param theme_default `function` representing a ggplot2 theme.
#' @param base_size `numeric` default font point size, used for scaling the
#'    overall text sizes larger or smaller.
#' @param grid.major.size,grid.minor.size `numeric` the line width for the major
#'    and minor grid lines, respectively. Set to 0 to suppress either.
#' @param strip.background.colour,strip.background.fill `character`
#'    color for the border and strip background itself when ggplot2 is
#'    using a faceted layout.
#' @param strip.text.size `numeric` or relative class `ggplot2::rel()` to
#'    define direct or relative text font size, respectively.
#' @param panel.grid.major.colour,panel.grid.minor.colour `character`
#'    colors for the major and minor grid lines, respectively.
#' @param panel.background,panel.border `element_rect` or `NULL`
#'    indicating the type of background or border
#'    to draw around each plot panel. When set to `NULL` it is
#'    set to `ggplot2::element_blank()` which displays nothing.
#' @param axis.text.x.angle `numeric` degrees to rotate the x-axis
#'    labels, apparently starts at 0 (horizontal) and goes
#'    counter-clockwise (to the left.)
#' @param blankGrid,blankXgrid,blankYgrid `logical` indicating whether
#'    to have a blank grid for everything, major, or minor axis lines,
#'    respectively. Intended to make it fast and easy to remove all
#'    gridlines.
#' @param resetTheme `logical` whether to call the function `theme_default`
#'    which essentially resets (replaces) all previous settings with
#'    those defined in the theme function. If `FALSE` then only the
#'    specific settings defined in this function will be applied.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to `ggplot2::theme()` in
#'    order to allow custom settings beyond what this function provides.
#'
#' @family colorjam ggplot2
#'
#' @examples
#' if (jamba::check_pkg_installed("ggplot2")) {
#'    dsamp <- ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 1000),];
#'    d <- ggplot2::ggplot(dsamp,
#'       ggplot2::aes(carat, price)) +
#'       ggplot2::geom_point(
#'          ggplot2::aes(colour=cut),
#'          size=2);
#'
#'    print(d + scale_color_jam() + ggplot2::ggtitle("scale_color_jam()"));
#'    print(d + scale_color_jam() + theme_jam() + ggplot2::ggtitle("scale_color_jam() + theme_jam()"));
#' }
#'
#' @export
theme_jam <- function
(theme_default=ggplot2::theme_bw,
 base_size=18,
 grid.major.size=0.5,
 grid.minor.size=0.25,
 strip.background.colour="grey30",
 strip.background.fill="lightgoldenrod1",
 strip.text.size=ggplot2::rel(0.8),
 panel.grid.major.colour="grey80",
 panel.grid.minor.colour="grey90",
 panel.background=ggplot2::element_rect(
    fill="white",
    colour=NA),
 panel.border=ggplot2::element_rect(
    fill=NA,
    colour="grey15"),
 axis.text.x.angle=60,
 blankGrid=FALSE,
 blankXgrid=FALSE,
 blankYgrid=FALSE,
 resetTheme=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to provide simple theme for ggplot2 plots
   ##
   ## if blankXgrid=TRUE it substitutes panel.grid.major.x=element_blank()
   ## and panel.grid.minor.x=element_blank()
   ##
   ## Anything in '...' is passed to theme(...) in order to customize other
   ## theme options.
   if (!jamba::check_pkg_installed("ggplot2")) {
      stop("theme_jam() requires the ggplot2 package.");
   }
   if (TRUE %in% resetTheme) {
      tNew <- theme_default(base_size=base_size);
   } else {
      tNew <- ggplot2::theme_get();
   }
   if (length(panel.border) == 0) {
      panel.border <- ggplot2::element_blank();
   }
   if (length(panel.background) == 0) {
      panel.background <- ggplot2::element_blank();
   }
   tNew <- tNew +
      ggplot2::theme(
         axis.text.x=ggplot2::element_text(angle=axis.text.x.angle,
            hjust=1),
         strip.text=ggplot2::element_text(
            size=strip.text.size,
            colour=jamba::setTextContrastColor(strip.background.fill),
         ),
         strip.background=ggplot2::element_rect(
            colour=strip.background.colour,
            fill=strip.background.fill),
         panel.background=panel.background,
         panel.border=panel.border,
         panel.grid.major=ggplot2::element_line(
            colour=panel.grid.major.colour,
            size=grid.major.size),
         panel.grid.minor=ggplot2::element_line(
            colour=panel.grid.minor.colour,
            size=grid.minor.size));
   if (blankGrid || blankXgrid) {
      if (verbose) {
         jamba::printDebug("theme_jam(): ",
            "blankXgrid");
      }
      tNew <- tNew + ggplot2::theme(
         panel.grid.major.x=ggplot2::element_blank(),
         panel.grid.minor.x=ggplot2::element_blank());
   }
   if (blankGrid || blankYgrid) {
      if (verbose) {
         jamba::printDebug("theme_jam(): ",
            "blankYgrid");
      }
      tNew <- tNew + ggplot2::theme(
         panel.grid.major.y=ggplot2::element_blank(),
         panel.grid.minor.y=ggplot2::element_blank());
   }
   if (length(list(...)) > 0) {
      tNew <- tNew + ggplot2::theme(...);
   }
   invisible(tNew);
}

#' Apply rainbowJam categorical colors to a ggplot2 object
#'
#' Apply rainbowJam categorical colors to a ggplot2 object
#'
#' This function provides a function in the format `scale_color_*`
#' to be applied to ggplot2 objects. It can provide a more visibly
#' distinct set of categorical colors than `ggplot2::scale_color_hue()`.
#'
#' @param ... additional arguments are passed to `ggplot2::discrete_scale()`.
#' @param type `character` string indicating the colors are sequential
#'    `"seq"`, and is passed to `colorjam::jam_pal()`.
#' @param palette `integer` value indicating the categorical palette
#'    to use, intended to provide variety in the color assignment.
#'    (Not yet implemented.)
#' @param direction `integer` indicating whether to reverse the color
#'    assignment, either `1` for the default forward assignment, or
#'    `-1` for reverse assignment. Any negative value will reverse
#'    the colors.
#' @param invert `logical` indicating whether to return corresponding
#'    contrasting colors, for example for text labels, typically either
#'    `"white"` or `"black"` as defined by `jamba::setTextContrastColor()`.
#' @param darkFactor,sFactor `numeric` passed to `jamba::makeColorDarker()`
#'    for optional adjustment of the color, by darkness and saturation,
#'    respectively. Intended when using `scale_color_jam()` and
#'    `scale_fill_jam()` where you want the color value to be lighter
#'    or darker than the fill color, a useful effect for outlines.
#' @param darken `logical` indicating whether to apply default values
#'    for `darkFactor` and `sFactor` to darken the resulting colors.
#' @param alpha `numeric` value indicating the alpha transparency, on a
#'    scale of 0 (transparent) to 1 (non-transparent).
#' @param useGrey `integer` value between 0 and 100 indicating the grey
#'    value, as sent to `jamba::setTextContrastColor()`, used only when
#'    `invert=TRUE`.
#'
#' @family colorjam ggplot2
#'
#' @examplesIf (requireNamespace("ggplot2", quietly=TRUE))
#' dsamp <- ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 1000),];
#' d <- ggplot2::ggplot(
#'    dsamp, ggplot2::aes(carat, price)) +
#'    ggplot2::geom_point(ggplot2::aes(colour=cut),
#'       size=4)
#'
#' print(d + ggplot2::scale_color_hue() + ggplot2::ggtitle("scale_color_hue()"));
#' print(d + scale_color_jam() + ggplot2::ggtitle("scale_color_jam()"));
#' print(d + scale_color_jam(preset="ryb") + ggplot2::ggtitle("scale_color_jam(preset='ryb')"));
#'
#' print(d + scale_color_jam() + ggplot2::ggtitle("scale_color_jam()"));
#'
#' @export
scale_color_jam <- function
(...,
 type="seq",
 palette=1,
 direction=1,
 invert=FALSE,
 darkFactor=1,
 sFactor=1,
 darken=FALSE,
 alpha=1,
 useGrey=20,
 preset=getOption("colorjam.preset", "dichromat2"),
 step=getOption("colorjam.step", "default"))
{
   ## Purpose is to provide rainbowJam() in ggplot2 context
   if (!jamba::check_pkg_installed("ggplot2")) {
      stop("scale_color_jam() requires the ggplot2 package.");
   }
   ggplot2::discrete_scale("colour",
      "jam",
      jam_pal(type=type,
         palette=palette,
         direction=direction,
         invert=invert,
         darkFactor=darkFactor,
         sFactor=sFactor,
         darken=darken,
         alpha=alpha,
         useGrey=useGrey,
         preset=preset,
         step=step),
      ...);
}

#' Apply rainbowJam categorical color fill to a ggplot2 object
#'
#' Apply rainbowJam categorical color fill to a ggplot2 object
#'
#' This function provides a function in the format `scale_fill_*`
#' to be applied to ggplot2 objects. It can provide a more visibly
#' distinct set of categorical colors than `ggplot2::scale_fill_hue()`.
#'
#' @param ... additional arguments are passed to `ggplot2::discrete_scale()`.
#' @param type `character` string indicating the colors are sequential
#'    `"seq"`, and is passed to `colorjam::jam_pal()`.
#' @param palette `integer` value indicating the categorical palette
#'    to use, intended to provide variety in the color assignment.
#'    (Not yet implemented.)
#' @param direction `integer` indicating whether to reverse the color
#'    assignment, either `1` for the default forward assignment, or
#'    `-1` for reverse assignment. Any negative value will reverse
#'    the colors.
#' @param invert `logical` indicating whether to return corresponding
#'    contrasting colors, for example for text labels, typically either
#'    `"white"` or `"black"` as defined by `jamba::setTextContrastColor()`.
#' @param darkFactor,sFactor `numeric` passed to `jamba::makeColorDarker()`
#'    for optional adjustment of the color, by darkness and saturation,
#'    respectively. Intended when using `scale_color_jam()` and
#'    `scale_fill_jam()` where you want the color value to be lighter
#'    or darker than the fill color, a useful effect for outlines.
#' @param darken `logical` indicating whether to apply default values
#'    for `darkFactor` and `sFactor` to darken the resulting colors.
#' @param alpha `numeric` value indicating the alpha transparency, on a
#'    scale of 0 (transparent) to 1 (non-transparent).
#' @param useGrey `integer` value between 0 and 100 indicating the grey
#'    value, as sent to `jamba::setTextContrastColor()`, used only when
#'    `invert=TRUE`.
#'
#' @family colorjam ggplot2
#'
#' @examples
#' if (jamba::check_pkg_installed("ggplot2")) {
#'    dsamp <- ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 1000),];
#'    d <- ggplot2::ggplot(dsamp,
#'       ggplot2::aes(carat, price)) +
#'       ggplot2::geom_point(
#'          ggplot2::aes(colour=cut, bg=cut),
#'          pch=21,
#'          size=4);
#'
#'    print(d +
#'       scale_color_jam(darkFactor=1.5) +
#'       scale_fill_jam() +
#'       ggplot2::ggtitle("scale_color_jam(darkFactor=1.5) + scale_fill_jam()"));
#'
#'    print(d +
#'       scale_color_jam(darken=TRUE) +
#'       scale_fill_jam() +
#'       ggplot2::ggtitle("scale_color_jam(darkFactor=1.5) + scale_fill_jam()"));
#' }
#'
#' @export
scale_fill_jam <- function
(...,
 type="seq",
 palette=1,
 direction=1,
 invert=FALSE,
 darkFactor=1,
 sFactor=1,
 darken=FALSE,
 alpha=1,
 useGrey=20,
 preset=getOption("colorjam.preset", "dichromat2"),
 step=getOption("colorjam.step", "default"))
{
   ## Purpose is to provide rainbowJam() in ggplot2 context
   if (!jamba::check_pkg_installed("ggplot2")) {
      stop("scale_fill_jam() requires the ggplot2 package.");
   }
   ggplot2::discrete_scale("fill",
      "jam",
      jam_pal(type=type,
         palette=palette,
         direction=direction,
         invert=invert,
         darkFactor=darkFactor,
         sFactor=sFactor,
         darken=darken,
         alpha=alpha,
         useGrey=useGrey,
         preset=preset,
         step=step),
      ...);
}

#' Jam color palette for ggplot2
#'
#' Jam color palette for ggplot2
#'
#' @param type `character` string indicating the colors are sequential
#'    `"seq"`.
#' @param palette `integer` value indicating the categorical palette
#'    to use, intended to provide variety in the color assignment.
#'    (Not yet implemented.)
#' @param direction `integer` indicating whether to reverse the color
#'    assignment, either `1` for the default forward assignment, or
#'    `-1` for reverse assignment. Any negative value will reverse
#'    the colors.
#' @param invert `logical` indicating whether to return corresponding
#'    contrasting colors, for example for text labels, typically either
#'    `"white"` or `"black"` as defined by `jamba::setTextContrastColor()`.
#' @param darkFactor,sFactor `numeric` passed to `jamba::makeColorDarker()`
#'    for optional adjustment of the color, by darkness and saturation,
#'    respectively. Intended when using `scale_color_jam()` and
#'    `scale_fill_jam()` where you want the color value to be lighter
#'    or darker than the fill color, a useful effect for outlines.
#' @param darken `logical` indicating whether to apply default values
#'    for `darkFactor` and `sFactor` to darken the resulting colors.
#' @param alpha `numeric` value indicating the alpha transparency, on a
#'    scale of 0 (transparent) to 1 (non-transparent).
#' @param useGrey `integer` value between 0 and 100 indicating the grey
#'    value, as sent to `jamba::setTextContrastColor()`, used only when
#'    `invert=TRUE`.
#' @param ... additional arguments are passed to `rainbowJam()`.
#'
#' @family colorjam ggplot2
#'
#' @export
jam_pal <- function
(type="seq",
 palette=1,
 direction=1,
 invert=FALSE,
 darkFactor=1,
 sFactor=1,
 darken=FALSE,
 alpha=1,
 useGrey=20,
 preset=getOption("colorjam.preset", "dichromat2"),
 step=getOption("colorjam.step", "default"),
 ...)
{
   ## Note this function does not specifically require ggplot2
   if (length(darken) > 0 && any(darken %in% TRUE)) {
      darkFactor <- 1.5;
      sFactor <- 1.5;
   }
   if (invert) {
      function(n) {
         pal <- jamba::setTextContrastColor(
            rainbowJam(n,
               alpha=alpha,
               preset=preset,
               step=step,
               ...),
            useGrey=useGrey);
         names(pal) <- NULL;
         pal <- pal[seq_len(n)];
         if (direction < 0) {
            pal <- rev(pal);
         }
         pal;
      }
   } else {
      function(n) {
         pal <- rainbowJam(n,
            alpha=alpha,
            preset=preset,
            step=step,
            ...);
         if (darkFactor != 1 || sFactor != 1) {
            pal <- jamba::makeColorDarker(pal,
               darkFactor=darkFactor,
               sFactor=sFactor);
         }
         names(pal) <- NULL;
         pal <- pal[seq_len(n)];
         if (direction < 0) {
            pal <- rev(pal);
         }
         pal;
      }
   }
}
