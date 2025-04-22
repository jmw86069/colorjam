
#' Add categorical colors to an existing color set
#'
#' Add categorical colors to an existing color set
#'
#' @param given_colors `character` vector of colors, default NULL.
#'    * When `given_colors` is NULL, `n` new colors will be returned.
#' @param n `integer` number of colors to add to `given_colors`
#' @param return_type `character`, default "new", what colors to return:
#'    * `"new"` - return only the newly assigned colors
#'    * `"full"` - return input colors and assigned colors together,
#'    in order: given, then new colors.
#' @param color_fn `function`, default `rainbowJam()`.
#'    * The first argument `n`  is expected to be the integer number
#'    of colors to return, and the function should return `n` color values.
#'    Other arguments in `...` are passed to this function for custom options.
#'    * Alternatively, `character` input is expanded using
#'    `jamba::color2gradient()`, although this process is not well-tested.
#' @param check_internal `logical` default FALSE whether to check the
#'    `color_fn` output for internal color distances. This step improves
#'    color output, however is currently time-consuming.
#'    * For best results, `color_fn` should already provide colors
#'    are as distinct from one another as possible, generally true
#'    for example `rainbowJam()`.
#'    * However when using a function that provides relatively uniform
#'    colors, such as  `colorspace::rainbow_hcl()`, the colors which
#'    are most distinct from `given_colors` are often also very similar
#'    to each other. The `check_internal=TRUE` also requires colors
#'    from `color_fn` to meet the `min_distance` threshold, which
#'    requires a recursive, nested algorithm in `find_color_spread()`.
#' @param max_iterations `integer` default 50, maximum iterations to
#'    attempt. The algorithm begins at `n` and increases the attempted
#'    colors by 1 each iteration until it defines at least `n` new colors.
#'    * When `step_distance` is non-zero, the `min_distance` is reduced
#'    by `abs(step_distance)` then the iterations are repeated.
#'    * When `step_distance` is zero, if no solution is found it
#'    returns 'NULL'.
#' @param min_distance `numeric` default 30, minimum distance
#'    to require for new colors compared to `given_colors`.
#'    * When at least `n` colors are defined with at least `min_distance`
#'    distance from `given_colors`, the `n` colors with the greatest
#'    distance are returned.
#'    * When `n` colors do not meet these critera, and `step_distance`
#'    is non-zero, the `min_distance` is reduced by `abs(step_distance)`
#'    and the process is repeated.
#'    * Finally, if `n` colors cannot be defined, it returns 'NULL'.
#' @param step_distance `numeric` default 1, the default step size when
#'    iterating progressively smaller `min_distance` values.
#'    * When 'NULL' or '0', the `min_distance` is not decreased after
#'    `max_iterations` iterations.
#' @param use_white `character` default "F5" representing the
#'    white reference, any value recognized by `farver::as_white_ref()`.
#'    * The default 'F5' represents 'daylight fluorescent' and in
#'    qualitative testing was most effective when defining color
#'    distances.
#'    * The typical default 'D65' is 'daylight 6500K' and
#'    is typically used for neutral daylight without blue (cool) or
#'    yellow (warm) shifted background lighting.
#' @param method `character`, default 'cie2000', passed to `slot_colors()`,
#'    then `color_distance()` to define the color distance method.
#' @param do_plot `logical` default FALSE, whether to plot the given_colors
#'    and new colors.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to internal functions
#'    `color_fn`, `slot_colors()`, and optionally `jamba::color2gradient()`.
#'
#' @returns `character` vector of colors with length `n`.
#'
#' @examples
#' n1 <- 6;
#' n <- 2;
#' given <- jamba::nameVector(rainbowJam(n1));
#' new_colors <- add_colors(given, n=n, do_plot=TRUE, method="cmc")
#' names(new_colors) <- seq_along(new_colors);
#' show_color_distance(c(given, new_colors))
#' show_color_distance(c(given, new_colors), cluster_data=TRUE)
#' show_color_distance(sort_colors(c(given, new_colors)))
#'
#' given2 <- c(given, new_colors);
#' color_pie(given2)
#' new_colors2 <- add_colors(unname(given2), n=n, do_plot=TRUE)
#' new_colors2 <- add_colors(unname(given2), n=n, do_plot=TRUE, dist_threshold=15)
#' new_colors2 <- add_colors(unname(given2), n=n, do_plot=TRUE, dist_threshold=20)
#' names(new_colors2) <- seq_along(new_colors2) + 2;
#' show_color_distance(sort_colors(c(given2, new_colors2)), cluster_data=TRUE)
#'
#' jamba::showColors(list(
#'    given=sort_colors(given),
#'    `add 2`=sort_colors(c(given, new_colors)),
#'    `add 2 more`=sort_colors(c(given2, new_colors2))))
#'
#' new_colors4 <- add_colors(given, n=4, do_plot=TRUE, dist_threshold=20)
#' names(new_colors4) <- seq_along(new_colors4);
#' show_color_distance(sort_colors(c(given, new_colors4)))
#'
#' jamba::showColors(list(given=given,
#'    `add 2`=sort_colors(c(given, new_colors)),
#'    `add 2 more`=sort_colors(c(given2, new_colors2)),
#'    `add 4 upfront`=sort_colors(c(given, new_colors4))))
#'
#' # Todo: consider ensuring desaturated colors are somewhat different also
#' show_color_distance(color_distance(given, new_colors), pc=c(given, new_colors))
#' hm1 <- show_color_distance(sort_colors(c(given, new_colors)), pc=unname(c(given, new_colors)))
#' hm1
#' hm2 <- show_color_distance(colorspace::desaturate(amount=0.7, sort_colors(c(given, new_colors))))
#' hm3 <- show_color_distance(colorspace::desaturate(amount=1, sort_colors(c(given, new_colors))))
#' hm1 + hm2 + hm3
#'
#' # test commoon themes
#' given <- c(DM="dodgerblue3", CTL="gold")
#' add_colors(unname(given), n=3, do_plot=TRUE)
#'
#' @export
add_colors <- function
(given_colors=NULL,
 n=1,
 return_type=c("new",
    "full",
    "list"),
 color_fn=rainbowJam,
 check_internal=FALSE,
 max_iterations=50,
 min_distance=30,
 step_distance=-1,
 use_white="F5",
 method="cie2000",
 do_plot=FALSE,
 verbose=FALSE,
 seed=123,
 ...)
{
   #
   return_type <- match.arg(return_type);
   set.seed(seed);
   if (length(min_distance) == 0) {
      min_distance <- 30;
   }
   if (length(step_distance) == 0) {
      step_distance <- 0;
   } else {
      step_distance <- abs(head(step_distance, 1)) * -1;
   }
   if (TRUE %in% do_plot &&
         !requireNamespace("jamses", quietly=TRUE)) {
      do_plot <- FALSE
   }
   if (step_distance < 0) {
      orig_min_distance <- min_distance;
      # define sequence of distances to attempt
      seq_min_distance <- seq(from=min_distance, to=1e-5, by=step_distance);
      #
      if (verbose) {
         jamba::printDebug("add_colors(): ", "",
            "seq_min_distance:", seq_min_distance);
      }
      for (use_min_distance in seq_min_distance) {
         if (verbose) {
            jamba::printDebug("add_colors(): ", "",
               "use_min_distance:", use_min_distance);
         }
         ret_colors <- suppressMessages(
            add_colors(given_colors=given_colors,
               n=n,
               return_type="list",
               color_fn=color_fn,
               check_internal=check_internal,
               max_iterations=max_iterations,
               min_distance=use_min_distance,
               step_distance=0,
               use_white=use_white,
               method=method,
               do_plot=FALSE,
               verbose=FALSE,
               seed=seed,
               ...))
         if (length(ret_colors) > 0) {
            break;
         }
      }
      if (length(ret_colors) == 0) {
         warn_txt <- paste("No dynamic solution found after",
            "{max_iterations} {.field max_iterations}",
            "at {min_distance} {.field min_distance},",
            "returning {.emph NULL}")
         cli::cli_abort(warn_txt)
         return(NULL)
      }
      new_colors <- ret_colors$new_colors;
      given_colors <- ret_colors$given_colors;
      #
   } else {
      min_distance <- head(min_distance, 1)
      #
      effective_n <- n;
      new_n <- 0;
      iteration_n <- 0;
      new_colors <- character(0);

      if (length(given_colors) > 0 &&
            length(names(given_colors)) > 0 &&
            any(names(given_colors) %in% c(NA, ""))) {
         jb <- (names(given_colors) %in% c(NA, ""));
         names(given_colors)[jb] <- paste0("GIVEN", seq_along(jb))[jb];
      }

      while(new_n < n) {
         iteration_n <- iteration_n + 1;
         if (verbose > 1) {
            jamba::printDebug("add_colors(): ",
               "iteration:", iteration_n,
               ", effective_n:", effective_n);
         }
         if (iteration_n > max_iterations) {
            warn_txt <- paste("No solution found after",
               "{max_iterations} {.field max_iterations},",
               "at {min_distance} {.field min_distance},",
               "returning {.emph NULL}")
            cli::cli_alert_warning(warn_txt)
            return(NULL)
         }
         # Begin iterations
         if (inherits(color_fn, "function")) {
            ref_colors <- jamba::call_fn_ellipsis(color_fn,
               n=effective_n,
               ...)
         } else if (inherits(color_fn, "character")) {
            # expand fixed set of colors
            # split repeated colors with color2gradient()
            ref_colors <- jamba::color2gradient(
               rep(color_fn, length.out=effective_n),
               ...)
         }

         # Slot colors
         if (length(given_colors) == 0) {
            slotted_colors <- NULL
         } else {
            slotted_colors <- slot_colors(given_colors,
               ref_colors,
               min_distance=min_distance,
               method=method,
               verbose=FALSE,
               use_white=use_white,
               ...)
         }

         # Remaining colors
         new_colors <- setdiff(ref_colors,
            ref_colors[unlist(slotted_colors)]);

         # Optionally apply secondary distance threshold
         if (length(min_distance) == 1 && min_distance > 1) {
            # apply distance here
         }

         # filter colors versus themselves
         if (TRUE %in% check_internal &&
               length(new_colors) >= n) {
            # jamba::printDebug("find_color_spread(new_colors): ");print(new_colors);# debug
            new_colors <- find_color_spread(x=new_colors,
               n=n,
               min_distance=min_distance,
               step_distance=0,
               # min_distance=min_distance * 2,
               # step_distance=-1,
               method=method,
               use_white=use_white,
               first_only=TRUE,
               ...)
         }

         new_n <- length(new_colors)

         # increment by 1
         effective_n <- effective_n + 1;
      }

      # optionally prioritize colors when there are more than necessary
      if (length(new_colors) > n) {
         new_dist <- color_distance(new_colors,
            ref_colors,
            use_white=use_white,
            method=method,
            ...);
         new_min <- apply(new_dist, 1, min, na.rm=TRUE);
         new_colors <- intersect(new_colors,
            head(new_colors[order(-new_min)], n))
      }
      if (length(names(new_colors)) == 0) {
         names(new_colors) <- jamba::makeNames(new_colors,
            ...);
      }
   }
   if (any(do_plot)) {
      k <- jamba::nameVector(new_colors,
         paste0("new", seq_along(new_colors)));
      if (length(given_colors) == 0) {
         j <- NULL;
      } else {
         j <- given_colors;
         if (length(names(j)) == 0) {
            names(j) <- paste0("GIVEN", seq_along(j));
         } else if (any(names(j) %in% c(NA, ""))) {
            jb <- (names(j) %in% c(NA, ""));
            names(j)[jb] <- paste0("GIVEN", seq_along(jb))[jb];
         }
      }
      if (TRUE %in% do_plot &&
            requireNamespace("jamses", quietly=TRUE)) {
         jk <- sort_colors(c(j, k))
         # print(color_distance(jk));# debug
         jkcd <- color_distance(jk,
            method=method,
            use_white=use_white,
            ...)
         hm <- show_color_distance(jkcd,
            pc=c(jk),
            ...);
         ComplexHeatmap::draw(hm, merge_legends=TRUE);
      } else if (2 %in% do_plot &&
            requireNamespace("jamses", quietly=TRUE)) {
         jkcd <- color_distance(j,
            k,
            method=method,
            use_white=use_white,
            ...)
         hm <- show_color_distance(jkcd, pc=c(j, k));
         ComplexHeatmap::draw(hm, merge_legends=TRUE);
      } else {
         jamba::showColors(list(
            given_colors=given_colors,
            new_colors=k,
            all_colors=sort_colors(c(given_colors, k))))
      }
   }

   # return the first n colors
   use_colors <- head(new_colors, n);

   # Todo: If there are more than 'n' return the most different?
   if ("list" %in% return_type) {
      return(list(
         new_colors=new_colors,
         given_colors=given_colors))
   }
   if ("full" %in% return_type) {
      return(c(given_colors, use_colors));
   }
   return(use_colors);
}


