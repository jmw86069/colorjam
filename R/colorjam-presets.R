
# model this behavior after igraph::shapes() and .igraph.shapes:
.colorjam_presets <- new.env();
.colorjam_presets[["ryb"]] <- list(
   h1=c(12,  60, 120, 240, 360),
   h2=c(0, 120, 180, 240, 360));
.colorjam_presets[["ryb1"]] <- list(
   h2=c(0, 30, 60,
      90, 120, 150,
      180, 210, 240,
      270, 300, 330),
   h1=c(12.2, 23, 40,
      60, 80.9, 107.3,
      127.7, 180, 245,
      265, 288, 315));
# reversed ryb starting at yellow
.colorjam_presets[["ryb2"]] <- list(
   h1=c(12.2, 23, 40,
      60, 81, 107,
      128, 180, 245,
      265, 288, 315),
   h2=c(470, 440, 410,
      380, 350, 320,
      290, 260, 230,
      200, 170, 140));
# legacy compatibility
.colorjam_presets[["ryb3"]] <- .colorjam_presets[["ryb"]]
.colorjam_presets[["rgb"]] <- list(
   h1=c(0, 360),
   h2=c(0, 360));
# reversed rgb starting at yellow
.colorjam_presets[["rgb2"]] <- list(
   h1=c(0, 360),
   h2=c(430, 70));
.colorjam_presets[["none"]] <- list(
   h1=c(0, 360),
   h2=c(0, 360));
.colorjam_presets[["dichromat"]] <- list(
   h1=c(8, 30,
      65, 120, 200,
      240, 260, 280,
      330),
   h2=c(0, 79.1,
      118.7 - 1e-8, 118.7, 118.7 + 1e-8,
      118.7 + 2e-8,
      185.9, 304.6,
      344.2))
# reversed dichromat starting at yellow
.colorjam_presets[["dichromat2"]] <- list(
   h1=c(8, 30, 65,
      120, 200, 240,
      260, 280, 330),
   h2=c(115, 35.9, -3.7+1e-9,
      -3.7, -3.7-1e-9, -3.7-2e-9,
      -70.9, -189.6, -229.2))

#' Colorjam hue warp presets
#'
#' Colorjam hue warp presets
#'
#' `colorjam_presets()`: list the names of available colorjam presets,
#' or when a preset name is provided, it returns hue warp data as a
#' `list` with names `"h1"` and `"h2"` suitable for use by
#' `h2hw()` and `hw2h()`.
#'
#' @returns `character` vector of recognized colorjam presets, or when
#'    `preset` is provided, it returns a `list` with elements
#'    `"h1"` and `"h2"` suitable for use with `h2hw()` or `hw2h()`.
#'
#' @family colorjam hue warp
#'
#' @param preset `NULL` to return a `character` vector of all recognized
#'    preset names, or `character` string to return specific data associated
#'    with a recognized preset name.
#' @param ... additional arguments are ignored.
#'
#' @export
colorjam_presets <- function
(preset=NULL,
 ...)
{
   #
   if (length(preset) == 0) {
      ls(.colorjam_presets)
   } else {
      .colorjam_presets[[preset]]
   }
}


#' Add colorjam hue warp preset
#'
#' Add colorjam hue warp preset
#'
#' @returns `TRUE`, invisibly.
#'
#' @family colorjam hue warp
#'
#' @param preset `character` string with the preset name.
#' @param h1,h2 `numeric` vectors of equal length, or `NULL` to
#'    remove an existing preset.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' h1 <- c(8, 30, 65,
#'    120, 200, 240,
#'    260, 280, 330)
#' h2 <- c(115, 35.9, -3.7,
#'    -3.7, -3.7, -3.7,
#'    -70.9, -189.6, -229.2)
#' add_colorjam_preset("dichromat2", h1=h1, h2=h2)
#' color_pie(rainbowJam(n=10,
#'    preset="dichromat2",
#'    h1=h1, h2=h2,
#'    phase=c(2,1,3,4,5,6)))
#'
#' add_colorjam_preset("dichromat2", h1=NULL, h2=NULL)
#' colorjam_presets()
#'
#' @export
add_colorjam_preset <- function
(preset,
 h1,
 h2,
 verbose=TRUE,
 ...)
{
   # validate input
   if (length(h1) != length(h2)) {
      stop("length(h1) must equal length(h2)");
   }
   if (length(h1) == 0) {
      if (preset %in% ls(.colorjam_presets)) {
         # remove
         rm(preset,
            envir=.colorjam_presets)
         if (TRUE %in% verbose) {
            jamba::printDebug("add_colorjam_preset(): ",
               "removed preset='", preset, "'");
         }
      }
   } else {
      assign(x=preset,
         value=list(
            h1=h1,
            h2=h2),
         envir=.colorjam_presets)
      if (TRUE %in% verbose) {
         jamba::printDebug("add_colorjam_preset(): ",
            "added preset='", preset, "'");
      }
   }
   invisible(TRUE)
}
