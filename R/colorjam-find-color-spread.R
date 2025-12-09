
#' Find a spread of colors with minimum distance between them
#'
#' Find a spread of colors with minimum distance between them
#'
#' Intended to be called internally by `add_colors()`, this function
#' takes a vector of colors `x`, and finds a subset of
#' at least `n` colors that each have color distance `min_distance`
#' using the `method` and `use_white` white reference.
#'
#' It is intended to solve the problem when M colors are available,
#' very close to a neighboring color, but a subset N colors are
#' requested which each have at least `min_distance` from each other.
#'
#' This function is currently not very optimized, although it does
#' avoid repeating combinations of color tests.
#'
#' It currently iterates each color, then each secondary color with
#' at least `min_distance`, and so on, until at least `n` colors
#' in a set have at least `min_distance` distance between them.
#' It then runs all combinations and sorts for the set with the
#' highest minimum distance, thereby the "most distinctive subset".
#'
#' @returns `character` vector of colors with minimum distances to each
#'    other of at least `min_distance`, or lower when `step_distance` is
#'    negative. Note that it may return more than `n` colors, when
#'    there are multiple colors to meet this criteria.
#'
#' @param x `character` vector of colors
#' @param n `integer` minimum number of colors
#' @param min_distance `numeric` minimum distance required between colors.
#' @param step_distance `numeric` default -1, when non-zero the `min_distance`
#'    is iterated until the `min_distance` criteria are met for at least
#'    `n` colors.
#'    * Use `step_distance=0` to prevent returning colors when there are
#'    not `n` colors with distance `min_distance`.
#' @param method `character` distance method, default 'cie2000'.
#' @param use_white `character` white reference, default 'F5'.
#' @param byCols `character` with optional column sorting, used to prioritize
#'    results when `first_only=FALSE`.
#'    The columns are sorted as follows:
#'    * `'-met_n'` - decreasing filter of whether the `n` threshold was met
#'    * `'-d'` - decreasing minimum distance in each color set
#'    * `'-found_n'` - decreasing number of colors that met the criteria
#' @param first_only `logical` default TRUE, whether to return only the
#'    first successful color combination meeting the criteria for
#'    `min_distance` and `n`.
#'    * When `first_only=FALSE` it will exhaustively determine all possible
#'    combinations of colors, which is time consuming for larger `n` values,
#'    even when `n=6`. However, this approach is able to find the most
#'    different combination of colors from `x`, based upon `byCols` column
#'    sorting.
#' @param ... additional arguments are passed to `color_distance()`.
#'
#' @family colorjam internal
#'
#' @examples
#' x12 <- colorspace::rainbow_hcl(12, c=95)
#' new5 <- sort_colors(find_color_spread(x12, n=5, min_distance=40))
#' color_pie(list(x12, new5))
#'
#' x12 <- rainbow(50)
#' new7 <- sort_colors(find_color_spread(x12, n=7, min_distance=30))
#' color_pie(list(x12, new7))
#'
#' x3a <- rainbow(30)
#' x3b <- sort_colors(find_color_spread(x3a, n=12, min_distance=20))
#' color_pie(list(x3a, x3b))
#'
#' @export
find_color_spread <- function
(x,
 n=2,
 min_distance=11.5,
 step_distance=-1,
 method="cie2000",
 use_white="F5",
 byCols=c("-met_n", "-d", "-found_n"),
 first_only=TRUE,
 verbose=FALSE,
 ...)
{
   #
   # if (n == 1) {
   #    return(given_colors)
   # }
   names_x <- names(x);
   names(x) <- jamba::colNum2excelName(seq_along(x));
   cd <- color_distance(x,
      method=method,
      use_white=use_white,
      ...)
   diag(cd) <- NA;
   cd_min <- apply(cd, 1, min, na.rm=TRUE)
   if (all(cd_min >= min_distance)) {
      return(x)
   }
   cd_num_valid <- apply(cd, 1, function(i){
      sum(!is.na(i) & i >= min_distance)
   })
   if (!any(cd_num_valid >= n)) {
      warning("No color met the min_distance criteria.")
      return(NULL)
   }

   # cd
   # - iterate each column K
   #    - given row j
   #    - find subset columns > min_distance
   #    - iterate each column j
   # internal function
   snake_colors <- function
   (cd,
    keep_lineage=NULL,
    min_distance,
    n,
    first_only=FALSE,
    cache_env,
    verbose=FALSE)
   {
      # iterate
      if (verbose && length(keep_lineage) == 0) {
         seq_vals <- head(jamba::nameVector(colnames(cd)), 4)
      } else {
         use_n <- ceiling(length(x) / n)
         seq_vals <- head(jamba::nameVector(colnames(cd)), use_n)
      }
      out <- lapply(seq_vals, function(startx) {
         if (TRUE %in% first_only) {
            have_found <- get("have_found", envir=cache_env)
            if (length(have_found) > 0) {
               return(NULL)
            }
         }
         keep <- names(which(unlist(cd[startx, ]) >= min_distance))
         if (length(c(keep_lineage, startx, keep)) < n) {
            return(NULL)
         }
         if (length(keep) <= 1) {
            if (TRUE %in% first_only) {
               new_found <- c(keep_lineage, startx, keep);
               assign("have_found",
                  envir=cache_env,
                  value=new_found);
            }
            return(c(keep_lineage, startx, keep))
         }
         keep_v <- jamba::cPasteS(keep)
         # skip previously calculated values
         if (verbose && length(keep_lineage) == 0) {
            jamba::printDebug("startx:", startx);# debug
         }
         if (verbose && length(keep_lineage) == 1) {
            jamba::printDebug("startx:", startx, indent=5);# debug
         }
         if (verbose && length(keep_lineage) == 2) {
            jamba::printDebug("startx:", startx, indent=10);# debug
         }
         have_iterated <- get("have_iterated", envir=cache_env);
         if (keep_v %in% have_iterated) {
            # jamba::printDebug("Skipped keep_v:", keep_v,
            #    ", length(have_iterated):", length(have_iterated),
            #    indent=5);# debug
            return(NULL)
         }
         new_iterated <- c(keep_v, have_iterated);
         assign("have_iterated",
            envir=cache_env,
            value=new_iterated);
         # jamba::printDebug("", "Calculating keep_v:", keep_v, indent=5);# debug
         snake_colors(cd=cd[keep, keep, drop=FALSE],
            min_distance=min_distance,
            keep_lineage=c(keep_lineage, startx),
            cache_env=cache_env,
            first_only=first_only,
            verbose=verbose,
            n=n)
      })
      if (TRUE %in% first_only) {
         have_found <- get("have_found", envir=cache_env)
         out <- list(list(have_found))
      }
      if (length(out) == 0) {
         return(NULL)
      }
      if (length(keep_lineage) == 0) {
         out_list <- jamba::unnestList(out,
            stopClasses=c("character"));
         out_sublist_v <- unique(jamba::cPasteSU(out_list))
         out_sublist <- strsplit(out_sublist_v, ",")
         # return(out_sublist)
         out_sublist_d <- sapply(out_sublist, function(k){
            k1 <- cd[k, k, drop=FALSE];
            if (length(k1) == 0) {
               return(0)
            }
            min(cd[k, k, drop=FALSE], na.rm=TRUE)
         })
         out_sublist_n <- lengths(out_sublist);
         out_df <- data.frame(d=out_sublist_d,
            found_n=out_sublist_n,
            num=seq_along(out_sublist_d),
            met_n=(out_sublist_n >= n) * 1,
            v=out_sublist_v);
         out_df_sorted <- jamba::mixedSortDF(out_df,
            byCols=byCols)
         # print(head(out_df_sorted));# debug
         out_df_sorted2 <- subset(out_df_sorted, found_n >= n);
         keep_n <- head(out_df_sorted, 1)$n;
         return(out_sublist[[keep_n]])
      }
      return(out)
   }
   if (step_distance == 0) {
      seq_distance <- min_distance;
   } else {
      seq_distance <- seq(from=min_distance,
         to=1e-5,
         by=abs(head(step_distance, 1)) * -1);
   }
   for (use_distance in seq_distance) {
      cache_env <- new.env();
      assign("have_iterated", envir=cache_env, value=character(0))
      assign("have_found", envir=cache_env, value=character(0))
      k <- snake_colors(cd,
         keep_lineage=NULL,
         min_distance=use_distance,
         first_only=first_only,
         cache_env=cache_env,
         verbose=verbose,
         n=n);
      if (length(k) >= n) {
         break;
      }
   }

   return_x <- x[k];
   names(return_x) <- names_x[k];
   return(return_x)

}
