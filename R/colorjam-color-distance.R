#' Calculate color distance between two or more colors
#'
#' Calculate color distance between two or more colors
#'
#' Color distance is calculated using `farver::compare_colour()`,
#' with some defaults intended in future to assist with color blindness
#' calculations.
#'
#' @family colorjam internal
#'
#' @returns `numeric` color distance with `length(x)` entries when both
#'    `x` and `y` are supplied, or a `numeric` matrix with color distances
#'    between all entries in `x`.
#'    * An attribute 'method' is added with the color distance used,
#'    mainly so the method can be used by `show_color_distance()`,
#'    but also by other methods as relevant.
#'
#' @param x `character` color, required input colors.
#'    * When `y` is supplied, values in `y` are recycled to `length(x)`,
#'    and each entry in `x` is directly compared to `y`.
#'    * When `y` is not supplied, `x` is compared to itself,
#'    returning a `matrix`.
#' @param y `character` color, default NULL
#' @param method `character`, default 'cmc', the color distance method,
#'    passed to `farver::compare_colour()`.
#'    * Note that 'cmc' uses arguments `lightness` and `chroma` which
#'    have custom values 2/3 and 1, respectively. The defaults are 2 and 1
#'    for acceptability, and 1 and 1 for perceptability.
#'    In our testing, 2/3 and 1 were more useful, since lightness on
#'    computer screen and in print is more easily perceived than
#'    on a painted surface.
#'    * In future, this argument
#'    may permit additional distance methods, and/or specific color function,
#'    in order to impose other criteria and adjustments.
#' @param use_white `character` default "F5" representing the
#'    white reference, any value recognized by `farver::as_white_ref()`.
#'    * The default 'F5' represents 'daylight fluorescent' and in
#'    qualitative testing was most effective when defining color
#'    distances.
#'    * The typical default 'D65' is 'daylight 6500K' and
#'    is typically used for neutral daylight without blue (cool) or
#'    yellow (warm) shifted background lighting.
#' @param do_plot `logical` default FALSE, whether to plot results using
#'    `show_color_distance()`.
#' @param ... additional arguments are passed to `farver::compare_colour()`
#'    and to `show_color_distance()`.
#'
#' @examples
#' color_distance("red", "firebrick", use_white="D65")
#'
#' palette15 <- sort_colors(grDevices::palette.colors(15, "Polychrome 36"))
#' color_distance(palette15, do_plot=TRUE)
#'
#' pc <- rainbowJam(8)
#' cd <- color_distance(pc);
#' show_color_distance(cd, pc=pc);
#'
#' @export
color_distance <- function
(x,
 y=NULL,
 method=c(
    "cmc",
    "cie2000",
    "cie94",
    "cie1976",
    "euclidean"),
 use_white="F5",
 lightness=2/3,
 chroma=1,
 do_plot=FALSE,
 ...)
{
   #
   if (length(x) == 0) {
      return(NULL)
   }
   method <- match.arg(method);
   if (length(names(x)) == 0) {
      names(x) <- jamba::makeNames(x,
         renameFirst=FALSE)
   }
   if (length(y) == 0) {
      y <- x;
   }
   x <- farver::decode_colour(colour=x,
      white=use_white);
   if (length(y) > 0) {
      if (length(names(y)) == 0) {
         names(y) <- jamba::makeNames(y,
            renameFirst=FALSE)
      }
      y <- jamba::call_fn_ellipsis(farver::decode_colour,
         colour=y,
         white=use_white,
         ...)
   }
   cd <- jamba::call_fn_ellipsis(farver::compare_colour,
      from=x,
      to=y,
      from_space="rgb",
      white_from=use_white,
      to_space="rgb",
      white_to=use_white,
      method=method,
      lightness=lightness,
      chroma=chroma,
      ...)
   attr(cd, "method") <- method;

   if (isTRUE(do_plot)) {
      show_color_distance(cd,
         ...)
   }

   return(cd)
}

