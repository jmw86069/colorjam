
#' Subset a vector of colors using one or more color attributes
#'
#' Subset a vector of colors using one or more color attributes
#'
#' The input colors in `x` are converted internally to a `data.frame`
#' with colnames added by relevant helper functions.
#'
#' * `"num"` contains the integer index of the input vector `x`.
#' * `"hex"` contains `character` values with hexadecimal colors
#' including alpha. If the input `x` is `"red"` then the hex value
#' will be converted to `"#FF0000FF"`.
#'
#' # Added by `jamba::col2hcl()`
#'
#' * `"H"` contains color hue as values from `0` to `360`.
#' * `"C"` contains color chroma (aka saturation) ranging from `0` up to `200`,
#' where typical "full saturation" is represented as values above 100.
#' * `"L"` contains color luminance (brightness/lightness) ranging from
#' `0` to `100`.
#' * `"alpha"` the alpha transparency, ranging from `0` (fully transparent)
#' to `1` (fully opaque, not transparent).
#'
#' # Added by `grDevices::col2rgb()`
#'
#' * `"red"` contains the red color channel, values range from `0` to `255`.
#' * `"green"` contains the green color channel, values range from `0` to `255`.
#' * `"blue"` contains the blue color channel, values range from `0` to `255`.
#'
#' # Added by `jamba::col2hsv()`
#'
#' * `"h"` contains color hue as values from `0` to `1`. Note these values
#' may not map directly to color hue obtained from `jamba::col2hcl()`.
#' * `"s"` contains color saturation with values from `0` to `1`.
#' * `"v"` contains color vibrance (brightness/lightness) with values
#' from `0` to `1`.
#'
#' @return `character` vector of colors that meet the filter criteria.
#'    When `return_type="df"` the returned object is a `data.frame` with
#'    the subset columns included for review.
#'
#' @family colorjam sort
#'
#' @examples
#' # subset for blue colors
#' jamba::showColors(subset_colors(colors(), H > 200 & H < 265 & C > 80))
#'
#' # subset for saturated colors
#' jamba::showColors(subset_colors(colors(), C > 120))
#'
#' # subset for saturated colors then sort by hue
#' jamba::showColors(subset_colors(colors(), C > 120, byCols=c("H", "-C", "-L")))
#'
#' # review the data.frame itself
#' subset_colors(colors(), C > 135, return_type="df")
#'
#' # for curiosity, compare H to h
#' colors_df <- subset_colors(colors(),
#'    C > 20,
#'    byCols=c("C"),
#'    return_type="df");
#' plot(colors_df$h, colors_df$H,
#'    xlab="hsv hue h",
#'    ylab="HCL hue H",
#'    pch=20,
#'    cex=colors_df$s * 1 + 1,
#'    col=colors_df$hex);
#' title("Comparison of HCL hue H\nwith hsv hue h")
#'
#' plot(colors_df$s, colors_df$C,
#'    xlab="hsv saturation s",
#'    ylab="HCL chroma C",
#'    pch=20,
#'    cex=colors_df$s * 1 + 1,
#'    col=colors_df$hex);
#' title("Comparison of HCL C\nwith hsv s")
#'
#' plot(colors_df$v, colors_df$L,
#'    xlab="hsv vibrance v",
#'    ylab="HCL luminance L",
#'    pch=20,
#'    cex=colors_df$s * 1 + 1,
#'    col=colors_df$hex);
#' title("Comparison of HCL L\nwith hsv v")
#'
#' @param x `character` vector of R colors
#' @param ... any valid criteria to subset the color `data.frame`.
#' @param alpha `logical` indicating whether to retain `alpha`
#'    transparency in intermediate operations. When `alpha=FALSE` then
#'    all color transparency is ignored, and colors will be returned
#'    with no transparency.
#' @param byCols `character` vector of colnames to sort after the subset
#'    operation.
#' @param return_type `character` string to define the return object,
#'    where `return_type="colors"` returns a `character` vector of colors,
#'    and `return_type="df"` returns a `data.frame` with the additional
#'    subset colnames included for review.
#'
#' @family colorjam core
#'
#' @export
subset_colors <- function
(x,
 ...,
 alpha=TRUE,
 byCols=NULL,
 return_type=c("colors", "df"))
{
   ## Purpose is to emulate subset.data.frame() by converting colors
   ## to a data.frame including RGB, HCL, and hsv values
   return_type <- match.arg(return_type);
   x_df <- colors_to_df(x,
      ...,
      alpha=alpha,
      byCols=byCols);

   if ("df" %in% return_type) {
      return(x_df);
   }
   if (length(x_df) == 0) {
      return(character(0))
   }
   return(x[x_df$num]);
}


#' Sort a vector of colors using one or more color attributes
#'
#' Sort a vector of colors using one or more color attributes
#'
#' This function is an extension of `subset_colors()` except that
#' it applies a sort order to the results.
#'
#' The input colors in `x` are converted internally to a `data.frame`
#' with colnames added by relevant helper functions.
#'
#' * `"num"` contains the integer index of the input vector `x`.
#' * `"hex"` contains `character` values with hexadecimal colors
#' including alpha. If the input `x` is `"red"` then the hex value
#' will be converted to `"#FF0000FF"`.
#'
#' # Added by `jamba::col2hcl()`
#'
#' * `"H"` contains color hue as values from `0` to `360`.
#' * `"C"` contains color chroma (aka saturation) ranging from `0` up to `200`,
#' where typical "full saturation" is represented as values above 100.
#' * `"L"` contains color luminance (brightness/lightness) ranging from
#' `0` to `100`.
#' * `"alpha"` the alpha transparency, ranging from `0` (fully transparent)
#' to `1` (fully opaque, not transparent).
#'
#' # Added by `grDevices::col2rgb()`
#'
#' * `"red"` contains the red color channel, values range from `0` to `255`.
#' * `"green"` contains the green color channel, values range from `0` to `255`.
#' * `"blue"` contains the blue color channel, values range from `0` to `255`.
#'
#' # Added by `jamba::col2hsv()`
#'
#' * `"h"` contains color hue as values from `0` to `1`. Note these values
#' may not map directly to color hue obtained from `jamba::col2hcl()`.
#' * `"s"` contains color saturation with values from `0` to `1`.
#' * `"v"` contains color vibrance (brightness/lightness) with values
#' from `0` to `1`.
#'
#' @return `character` vector of colors that meet the filter criteria.
#'    When `return_type="df"` the returned object is a `data.frame` with
#'    the subset columns included for review.
#'
#' @family colorjam sort
#' @family colorjam core
#'
#' @examples
#' # subset for blue colors
#' jamba::showColors(subset_colors(colors(), H > 200 & H < 265 & C > 80))
#'
#' # subset for saturated colors
#' jamba::showColors(subset_colors(colors(), C > 120))
#'
#' # subset for saturated colors then sort by hue
#' jamba::showColors(subset_colors(colors(), C > 120, byCols=c("H", "-C", "-L")))
#'
#' # review the data.frame itself
#' subset_colors(colors(), C > 135, return_type="df")
#'
#' # for curiosity, compare H to h
#' colors_df <- subset_colors(colors(),
#'    C > 20,
#'    byCols=c("C"),
#'    return_type="df");
#' plot(colors_df$h, colors_df$H,
#'    xlab="hsv hue h",
#'    ylab="HCL hue H",
#'    pch=20,
#'    cex=colors_df$s * 1 + 1,
#'    col=colors_df$hex);
#' title("Comparison of HCL hue H\nwith hsv hue h")
#'
#' plot(colors_df$s, colors_df$C,
#'    xlab="hsv saturation s",
#'    ylab="HCL chroma C",
#'    pch=20,
#'    cex=colors_df$s * 1 + 1,
#'    col=colors_df$hex);
#' title("Comparison of HCL C\nwith hsv s")
#'
#' plot(colors_df$v, colors_df$L,
#'    xlab="hsv vibrance v",
#'    ylab="HCL luminance L",
#'    pch=20,
#'    cex=colors_df$s * 1 + 1,
#'    col=colors_df$hex);
#' title("Comparison of HCL L\nwith hsv v")
#'
#' @inheritParams subset_colors
#'
#' @export
sort_colors <- function
(x,
 ...,
 alpha=TRUE,
 byCols=c("H", "C", "L"),
 return_type=c("colors", "df"))
{
   ## Purpose is to emulate subset.data.frame() by converting colors
   ## to a data.frame including RGB, HCL, and hsv values
   return_type <- match.arg(return_type);
   x_df <- colors_to_df(x,
      ...,
      alpha=alpha,
      byCols=byCols);

   if ("df" %in% return_type) {
      return(x_df);
   }
   if (length(x_df) == 0) {
      return(character(0))
   }
   return(x[x_df$num]);
}


#' Convert colors to data.frame of color attributes
#'
#' Convert colors to data.frame of color attributes
#'
#' This function takes a vector of colors and returns a `data.frame`
#' with relevant color attributes:
#'
#' * `"num"` contains the integer index of the input vector `x`.
#' * `"hex"` contains `character` values with hexadecimal colors
#' including alpha. If the input `x` is `"red"` then the hex value
#' will be converted to `"#FF0000FF"`.
#' * `"name"` if `names(x)` is not empty
#'
#' # Added by `jamba::col2hcl()`
#'
#' * `"H"` contains color hue as values from `0` to `360`.
#' * `"C"` contains color chroma (aka saturation) ranging from `0` up to `200`,
#' where typical "full saturation" is represented as values above 100.
#' * `"L"` contains color luminance (brightness/lightness) ranging from
#' `0` to `100`.
#' * `"alpha"` the alpha transparency, ranging from `0` (fully transparent)
#' to `1` (fully opaque, not transparent).
#'
#' # Added by `grDevices::col2rgb()`
#'
#' * `"red"` contains the red color channel, values range from `0` to `255`.
#' * `"green"` contains the green color channel, values range from `0` to `255`.
#' * `"blue"` contains the blue color channel, values range from `0` to `255`.
#'
#' # Added by `jamba::col2hsv()`
#'
#' * `"h"` contains color hue as values from `0` to `1`. Note these values
#' may not map directly to color hue obtained from `jamba::col2hcl()`.
#' * `"s"` contains color saturation with values from `0` to `1`.
#' * `"v"` contains color vibrance (brightness/lightness) with values
#' from `0` to `1`.
#'
#' # Added by `jamba::col2hsl()`
#'
#' * `"hsl_h"` contains color hue as values from `0` to `1`. Note these values
#' may not map directly to color hue obtained from `jamba::col2hcl()`.
#' * `"hsl_s"` contains color saturation with values from `0` to `100`.
#' * `"hsl_l"` contains color luminance (brightness/lightness) with values
#' from `0` to `100`.
#'
#' @family colorjam sort
#' @family colorjam core
#'
#' @inheritParams subset_colors
#'
#' @return `data.frame` with color attributes as columns.
#'    When `byCols` is defined, the `data.frame` is sorted using
#'    `jamba::mixedSortDF()`.
#'    When `...` is supplied, `subset()` is applied to subset colors.
#'
#' @export
colors_to_df <- function
(x,
 ...,
 alpha=TRUE,
 byCols=NULL)
{
   ## Purpose is to emulate subset.data.frame() by converting colors
   ## to a data.frame including RGB, HCL, and hsv values
   color_colnames <- c("hex",
      "num",
      "H", "C", "L",
      "alpha", "red", "green", "blue",
      "h", "s", "v");
   x_df <- NULL;
   if (any("data.frame" %in% class(x))) {
      if (all(color_colnames %in% colnames(x))) {
         x_df <- x;
      } else if (ncol(x) == 1) {
         x <- unlist(x[,1]);
      } else if (nrow(x) == 1) {
         x <- unlist(x[1,]);
      } else {
         stop(paste0(
            "data.frame input requires colnames: ",
            jamba::cPaste(color_colnames)));
      }
   }
   if (length(x_df) == 0) {
      x_df <- data.frame(
         stringsAsFactors=FALSE,
         check.names=FALSE,
         num=seq_along(x),
         hex=rgb2col(col2rgb(x))
      );
      if (length(names(x)) > 0) {
         x_df$name <- names(x);
      }
      x_df <- data.frame(
         stringsAsFactors=FALSE,
         check.names=FALSE,
         x_df,
         t(jamba::col2hcl(x)),
         t(grDevices::col2rgb(x, alpha=FALSE)),
         t(jamba::col2hsv(x, alpha=FALSE))[,c("h","s","v")]);
      # add hsl columns
      hsl_df <- jamba::renameColumn(data.frame(check.names=FALSE,
         t(jamba::col2hsl(x))[,c("H", "S", "L")]),
         from=c("H", "S", "L"),
         to=c("hsl_h", "hsl_s", "hsl_l"));
      x_df[,colnames(hsl_df)] <- hsl_df;
   }

   # optional subset
   x_df <- subset(x_df, ...);

   if (length(byCols) > 0 &&
         (is.numeric(byCols) ||
            any(gsub("^-", "", byCols) %in% colnames(x_df)))) {
      x_df <- jamba::mixedSortDF(x_df,
         byCols=byCols);
   }
   return(x_df);
}

