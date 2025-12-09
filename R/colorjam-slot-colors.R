
#' Slot a set of colors into a reference set of colors
#'
#' Slot a set of colors into a reference set of colors
#'
#' The primary purpose is for `add_colors()` to determine whether
#' a given set of colors `given_colors` are already represented,
#' within a given color tolerance or distance,
#' within a reference set of colors `ref_colors`.
#'
#' Colors are "slotted" into the corresponding reference colors,
#' and un-slotted colors can be considered "not used" in the reference
#' colors, and therefore they are available to be new colors
#' to add to the set.
#'
#' `add_colors()` is recommended for most users, since it applies the
#' logic by calling `slot_colors()` itself. However, for debugging,
#' in order to understand why certain colors are "slotted" and other
#' colors are not, `slot_colors()` may be useful to call directly,
#' especially with `do_plot=TRUE` for visual review.
#'
#' Color distance metrics are imperfect, and do not represent
#' color blindness conditions effectively. Further, color distance
#' metrics are designed with different goals in mind. For example
#' the typical metrics are designed to ensure tolerance to a
#' standard color. They are not primarily designed to quantify
#' the magnitude of perceptible difference between two colors,
#' and this latter scenario is the primary motivation of
#' `slot_colors()` and `add_colors()`.
#'
#' @family colorjam internal
#'
#' @returns `list` with length(given_colors) with `integer` vectors
#'    referencing one or more elements in `ref_colors`, or `NULL`.
#'
#' @param given_colors `character` vector of given colors to test versus
#'    `ref_colors`. Colors in `given_colors` which are within the distance
#'    threshold of one or more colors in `ref_colors` are assigned (slotted)
#'    into those colors.
#' @param ref_colors `character` vector of reference colors.
#' @param dist_threshold `numeric` default NULL, with pre-defined color
#'    distance threshold.
#' @param min_distance `numeric` default 11.5 with a minimum distance to use,
#'    when the dynamic distance threshold is below this value, and
#'    when `dist_threshold` is not provided.
#' @param return_type `character` string, default 'list' with return type:
#'    * 'list': returns `list` with length(given_colors) and `integer` values
#'    of assigned colors in `ref_colors`, or `NA` when no values in
#'    `ref_colors` are within the distance threshold.
#'    * `vector`: returns vector equivalent of the `list` output,
#'    flattened using `jamba::cPaste()`. Values are `NA` or 'NA' for
#'    unslotted colors.
#' @param use_white `character` default "F5" representing the
#'    white reference, any value recognized by `farver::as_white_ref()`.
#'    * The default 'F5' represents 'daylight fluorescent' and in
#'    qualitative testing was most effective when defining color
#'    distances.
#'    * The typical default 'D65' is 'daylight 6500K' and
#'    is typically used for neutral daylight without blue (cool) or
#'    yellow (warm) shifted background lighting.
#' @param method `character` default 'cie2000' passed to `color_distance()`
#' @param do_plot `logical` default FALSE, whether to plot a visual
#'    of the slot colors assignments.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to internal functions
#'    `color_distance()`, or when `do_plot==TRUE`, `imageByColors()`.
#'
#' @examples
#' given_colors <- rainbowJam(5)
#' ref_colors <- colorspace::rainbow_hcl(6)
#' slot_colors(given_colors, ref_colors, do_plot=TRUE)
#'
#' given_colors <- rainbowJam(5)
#' ref_colors <- colorspace::rainbow_hcl(16)
#' slot_colors(given_colors, ref_colors, verbose=TRUE, do_plot=TRUE)
#'
#' @export
slot_colors <- function
(given_colors,
 ref_colors,
 dist_threshold=NULL,
 min_distance=11.5,
 return_type=c("list",
    "vector"),
 use_white="F5",
 method="cie2000",
 do_plot=FALSE,
 verbose=FALSE,
 ...)
{
   #
   return_type <- match.arg(return_type);
   if (length(names(given_colors)) == 0) {
      names(given_colors) <- jamba::makeNames(given_colors,
         renameFirst=FALSE)
   }
   if (length(names(ref_colors)) == 0) {
      names(ref_colors) <- jamba::makeNames(ref_colors,
         renameFirst=FALSE)
   }
   cd <- color_distance(x=given_colors,
      y=ref_colors,
      use_white=use_white,
      method=method,
      ...)

   if (length(min_distance) != 1) {
      min_distance <- 0;
   }

   # determine distance threshold (if not specifically defined)
   if (length(dist_threshold) != 1) {
      if (verbose) {
         jamba::printDebug("slot_colors(): ",
            "Determining color distance threshold.")
      }
      # given
      cd_given <- color_distance(x=given_colors,
         use_white=use_white,
         ...)
      # show_color_distance(cd_given)
      diag(cd_given) <- NA;
      given_min_dists <- apply(cd_given, 2, min, na.rm=TRUE)
      given_mean <- mean(given_min_dists)
      given_median <- median(given_min_dists)
      # ref
      cd_ref <- color_distance(x=ref_colors,
         use_white=use_white,
         ...)
      # show_color_distance(cd_ref)
      diag(cd_ref) <- NA;
      ref_min_dists <- apply(cd_ref, 2, min, na.rm=TRUE)
      ref_mean <- mean(ref_min_dists)
      ref_median <- median(ref_min_dists)

      ## Todo: decide between mean and median, for now use median
      # dist_threshold <- ref_mean * (2 / 5);
      use_metric <- max(c(
         given_mean,
         given_median));
      use_metric <- max(c(
         ref_mean,
         ref_median));

      calc_threshold <- use_metric * (2 / 5);
      dist_threshold <- max(c(min_distance, calc_threshold))
      if (verbose) {
         jamba::printDebug("",
            indent=5,
            "given_mean:    ", given_mean);
         jamba::printDebug("",
            indent=5,
            "given_median:  ", given_median);
         jamba::printDebug("",
            indent=5,
            "ref_mean:      ", ref_mean);
         jamba::printDebug("",
            indent=5,
            "ref_median:    ", ref_median);
         jamba::printDebug("",
            indent=5,
            "use_metric:    ", use_metric);
         if (!calc_threshold == dist_threshold) {
            jamba::printDebug(
               indent=5,
               "calc_threshold:", calc_threshold);
         }
         jamba::printDebug("",
            indent=5,
            "dist_threshold:", dist_threshold);
      }
   }
   if (verbose) {
      jamba::printDebug("slot_colors(): ",
         "dist_threshold:", dist_threshold);
   }

   # # or use default
   # d_threshold <- 10;

   # find color matches within this distance
   slotted_colors <- lapply(rownames(cd), function(icol){
      ival <- which(cd[icol, ] < dist_threshold)
      idist <- cd[icol, ][ival]
      attr(ival, "dist") <- idist;
      if (length(ival) == 0) {
         return(NA)
      }
      ival
   })

   # optional plot
   if (TRUE %in% do_plot) {
      given_seq <- rep(seq_along(given_colors), lengths(slotted_colors))
      given_dist <- lapply(slotted_colors, function(i){
         jamba::rmNULL(nullValue=NA, attr(i, "dist"))
      })
      gdf <- data.frame(given=given_seq,
         ref=unlist(slotted_colors),
         dist=format(digits=2, trim=TRUE, unlist(given_dist)))
      gdf1 <- data.frame(given=seq_along(given_colors))
      gdf2 <- data.frame(ref=seq_along(ref_colors))
      gdf12 <- jamba::mixedSortDF(na.last=FALSE,
         jamba::mergeAllXY(list(gdf, gdf1, gdf2))[, c("ref", "given", "dist")])
      is_slotted <- (!is.na(gdf12$ref) & !is.na(gdf12$given))
      gdf12$slotted <- ifelse(is_slotted,
         gdf12$given,
         NA)
      gdf12$unslotted <- ifelse(is_slotted,
         NA,
         gdf12$given)
      gdflist <- list(
         unslotted=jamba::nameVector(
            given_colors[gdf12$unslotted],
            ifelse(is.na(gdf12$unslotted), "",
               gdf12$unslotted),
            makeNamesFunc=c),
         slotted=jamba::nameVector(
            given_colors[gdf12$slotted],
            ifelse(is.na(gdf12$slotted), "",
               paste0(gdf12$slotted, " (dist ", gdf12$dist, ")")),
            makeNamesFunc=c),
         ref=jamba::nameVector(
            ref_colors[gdf12$ref],
            jamba::rmNA(gdf12$ref, naValue=""),
            makeNamesFunc=c))
      gdflist <- jamba::rmNULL(lapply(gdflist, function(i){
         if (all(is.na(i))) {
            NULL
         } else {
            i
         }
      }))
      gdfm <- do.call(cbind, lapply(gdflist, rev))
      gdfmn <- do.call(cbind, lapply(gdflist, function(i){rev(names(i))}))
      # custom labels
      white_refs <- c(
         D50="daylight 5000K",
         D55="daylight 5500K",
         D65="daylight 6500K",
         D75="overcast 7500K",
         A="tungsten lamp 2856K",
         B="direct daylight",
         C="average daylight",
         E="equal power radiator",
         F1="daylight fluorescent",
         F2="cool white fluorescent",
         F4="warm white fluorescent",
         F5="daylight fluorescent",
         F7="daylight D65 fluorescent",
         F11="narrow tri-band fluorescent")

      #
      jamba::imageByColors(gdfm,
         cellnote=gdfmn,
         las=1,
         main=paste0("dist_threshold: ",
            format(dist_threshold, digits=2), "\n",
            use_white, ": ", white_refs[use_white]),
         ...)
   }

   if ("vector" %in% return_type) {
      if (all(lengths(slotted_colors) <= 1)) {
         retvals <- unlist(slotted_colors);
      } else {
         retvals <- jamba::cPaste(slotted_colors)
      }
   } else {
      retvals <- slotted_colors;
   }
   return(invisible(retvals))
}
