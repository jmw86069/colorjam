
#' Add categorical colors to an existing color set
#'
#' Add categorical colors to an existing color set
#'
#' @param given_colors `character` vector of colors.
#'    * When `given_colors` is NULL, `n` new colors will be returned.
#' @param n `integer` number of colors to add to `given_colors`
#' @param color_fn `function`, default `rainbowJam()`. The first argument
#'    is expected to be the integer number of colors to return.
#'    Other arguments in `...` are passed to this function for custom options.
#' @param max_iterations `integer` default 100, maximum iterations to
#'    attempt.
#' @param do_plot `logical` default FALSE, whether to plot the input and
#'    added colors.
#' @param ... additional arguments are passed to internal functions.
#'
#' @returns `character` vector of colors with length `n`.
#'
#' @examples
#' n1 <- 6;
#' n <- 2;
#' given <- jamba::nameVector(rainbowJam(n1));
#' new_colors <- add_colors(given, n=n, do_plot=TRUE, dist_threshold=20)
#' names(new_colors) <- seq_along(new_colors);
#' show_color_distance(c(given, new_colors))
#' show_color_distance(sort_colors(c(given, new_colors)))
#'
#' given2 <- c(given, new_colors);
#' new_colors2 <- add_colors(unname(given2), n=n, do_plot=TRUE, dist_threshold=20)
#' names(new_colors2) <- seq_along(new_colors2) + 2;
#' show_color_distance(sort_colors(c(given2, new_colors2)))
#'
#' jamba::showColors(list(given=given,
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
    "full"),
 color_fn=rainbowJam,
 max_iterations=100,
 do_plot=FALSE,
 verbose=FALSE,
 ...)
{
   #
   return_type <- match.arg(return_type);

   effective_n <- n;
   new_n <- 0;
   iteration_n <- 0;
   new_colors <- character(0);

   if (length(given_colors) > 0 &&
         length(names(given_colors)) > 0 &&
         any(names(given_colors) %in% c(NA, ""))) {
      jb <- (names(given_colors) %in% c(NA, ""));
      names(given_colors)[jb] <- paste0("given", seq_along(jb))[jb];
   }

   while(new_n < n) {
      iteration_n <- iteration_n + 1;
      if (verbose) {
         jamba::printDebug("add_colors(): ",
            "iteration:", iteration_n,
            ", effective_n:", effective_n);
      }
      if (iteration_n > max_iterations) {
         stop(paste0("No solution found after ",
            max_iterations, " iterations."));
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
            verbose=verbose,
            ...)
      }

      # Remaining colors
      new_colors <- setdiff(ref_colors,
         ref_colors[unlist(slotted_colors)]);
      new_n <- length(new_colors)

      # increment by 1
      effective_n <- effective_n + 1;
   }

   if (any(do_plot)) {
      k <- jamba::nameVector(new_colors,
         paste0("new", seq_along(new_colors)));
      if (length(given_colors) == 0) {
         j <- NULL;
      } else {
         j <- given_colors;
         if (length(names(j)) == 0) {
            names(j) <- paste0("given", seq_along(j));
         } else if (any(names(j) %in% c(NA, ""))) {
            jb <- (names(j) %in% c(NA, ""));
            names(j)[jb] <- paste0("given", seq_along(jb))[jb];
         }
      }
      if (do_plot %in% c(TRUE, 1)) {
         jk <- sort_colors(c(j, k))
         # print(color_distance(jk));# debug
         hm <- show_color_distance(color_distance(jk),
            pc=c(jk),
            ...);
         ComplexHeatmap::draw(hm, merge_legends=TRUE);
      } else if (do_plot %in% c(2)) {
         hm <- show_color_distance(color_distance(j, k), pc=c(j, k));
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
   if ("full" %in% return_type) {
      return(c(given_colors, use_colors));
   }
   return(use_colors);
}


#' Slot a set of colors into a reference set of colors
#'
#' Slot a set of colors into a reference set of colors
#'
#' @return `list` with length(given_colors) with `integer` vectors
#'    referencing one or more elements in `ref_colors`, or `NULL`.
#'
#' @examples
#' given_colors <- rainbowJam(5)
#' ref_colors <- colorspace::rainbow_hcl(6)
#' slot_colors(given_colors, ref_colors)
#'
#' @export
slot_colors <- function
(given_colors,
 ref_colors,
 dist_threshold=NULL,
 verbose=FALSE,
 ...)
{
   #
   if (length(names(given_colors)) == 0) {
      names(given_colors) <- jamba::makeNames(given_colors,
         renameFirst=FALSE)
   }
   if (length(names(ref_colors)) == 0) {
      names(ref_colors) <- jamba::makeNames(ref_colors,
         renameFirst=FALSE)
   }
   cd <- color_distance(x=given_colors,
      y=ref_colors)

   # determine distance threshold (if not specifically defined)
   if (length(dist_threshold) != 1) {
      # given
      cd_given <- color_distance(x=given_colors)
      # show_color_distance(cd_given)
      diag(cd_given) <- NA;
      given_min_dists <- apply(cd_given, 2, min, na.rm=TRUE)
      given_mean <- mean(given_min_dists)
      given_median <- median(given_min_dists)
      # ref
      cd_ref <- color_distance(x=ref_colors)
      # show_color_distance(cd_ref)
      diag(cd_ref) <- NA;
      ref_min_dists <- apply(cd_ref, 2, min, na.rm=TRUE)
      ref_mean <- mean(ref_min_dists)
      ref_median <- median(ref_min_dists)
      ## Todo: decide between mean and median, for now use median
      # dist_threshold <- ref_mean * (2 / 5);
      given_metric <- max(c(given_mean, given_median));
      if (verbose) {
         jamba::printDebug("slot_colors(): ",
            indent=5,
            "given_mean:", given_mean);
         jamba::printDebug("slot_colors(): ",
            indent=5,
            "given_median:", given_median);
         jamba::printDebug("slot_colors(): ",
            indent=5,
            "given_metric:", given_metric);
      }
      dist_threshold <- given_metric * (2 / 5);
   }
   if (verbose) {
      jamba::printDebug("slot_colors(): ",
         indent=5,
         "dist_threshold:", dist_threshold);
   }

   # # or use default
   # d_threshold <- 10;

   # find color matches within this distance
   slotted_colors <- lapply(rownames(cd), function(icol){
      ival <- which(cd[icol, ] < dist_threshold)
      if (length(ival) == 0) {
         return(NA)
      }
      ival
   })
   return(slotted_colors)
}

#' Calculate color distance between two or more colors
#'
#' Calculate color distance between two or more colors
#'
#' Color distance is calculated using `farver::compare_colour()`,
#' with some defaults intended in future to assist with color blindness
#' calculations.
#'
#' @returns `numeric` color distance with `length(x)` entries when both
#'    `x` and `y` are supplied, or a `numeric` matrix with color distances
#'    between all entries in `x`.
#'
#' @param x `character` color, required input colors.
#'    * When `y` is supplied, values in `y` are recycled to `length(x)`,
#'    and each entry in `x` is directly compared to `y`.
#'    * When `y` is not supplied, `x` is compared to itself,
#'    returning a `matrix`.
#' @param y `character` color, default NULL
#' @param ... additional arguments are passed to `farver::compare_colour()`
#'
#' @examples
#' color_distance("red", "firebrick")
#' color_distance("red", "red2")
#'
#' color_distance(grDevices::palette.colors(15))
#'
#' pc <- grDevices::palette.colors(15);
#' pc <- rainbowJam(5)
#' cd <- color_distance(pc);
#' cd[lower.tri(cd)] <- NA;
#' cse <- SummarizedExperiment::SummarizedExperiment(
#'    assays=list(distance=cd),
#'    rowData=data.frame(color=colnames(cd)),
#'    colData=data.frame(color=colnames(cd)))
#' jamses::heatmap_se(cse, use_raster=FALSE,
#'    top_colnames="color", rowData_colnames="color",
#'    sample_color_list=list(color=pc),
#'    cluster_columns=FALSE, cluster_rows=FALSE, color_max=360,
#'    centerby_colnames=FALSE,
#'    legend_at=seq(0, 300, by=100), legend_labels=seq(0, 300, by=100))
#'
#' @export
color_distance <- function
(x,
 y=NULL,
 method=c("cie2000",
    "cie94",
    "cie1976",
    "cmc",
    "euclidean"),
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
   x <- farver::decode_colour(x);
   if (length(y) > 0) {
      if (length(names(y)) == 0) {
         names(y) <- jamba::makeNames(y,
            renameFirst=FALSE)
      }
      y <- farver::decode_colour(y,
         ...)
   }
   cd <- farver::compare_colour(from=x,
      to=y,
      from_space="rgb",
      to_space="rgb",
      method=method,
      ...)
   attr(cd, "method") <- method;

   return(cd)
}

#' Show color distance as a heatmap
#'
#' Show color distance as a heatmap
#'
#' @examples
#' pc <- grDevices::palette.colors(25);
#' cd <- color_distance(pc, method="cmc");
#' cd <- color_distance(pc, method="euclidean");
#' cd <- color_distance(pc, method="cie2000");
#' show_color_distance(cd, pc, cluster_data=TRUE, row_split=4)
#'
#' show_color_distance(cd, pc, cluster_data=TRUE, row_split=5,
#' clustering_distance_rows="euclidean", clustering_distance_columns="euclidean")
#' cd2 <- 360 - cd;
#' diag(cd2) <- 360;
#' show_color_distance(cd2, pc, cluster_data=TRUE, row_split=4,
#' clustering_distance_rows="euclidean", clustering_distance_columns="euclidean")
#'
#' show_color_distance(cd, pc)
#'
#' pc <- rainbowJam(5)
#' @export
show_color_distance <- function
(cd=NULL,
 pc=NULL,
 show_labels=TRUE,
 cluster_data=FALSE,
 row_split=0,
 ...)
{
   #
   if (!requireNamespace("jamses", quietly=TRUE)) {
      stop("This function requires jamses, SummarizedExperiment, ComplexHeatmap")
   }
   if (inherits(cd, "character") && is.atomic(cd)) {
      pc <- cd;
      cd <- NULL;
   }
   if (length(cd) == 0 && length(pc) > 1) {
      cd <- color_distance(pc,
         ...);

   }
   if (length(pc) == 0) {
      pc <- unique(c(colnames(cd),
         rownames(cd)))
   }

   if (length(names(pc)) == 0) {
      names(pc) <- jamba::makeNames(pc,
         renameFirst=FALSE);
   }
   legend_at <- pretty(c(cd, 150))
   legend_labels <- legend_at;
   color_max <- max(legend_at);
   if (length(row_split) == 0 || any(row_split %in% c(0, 1))) {
      row_split <- NULL;
   }

   # optional cell labels
   use_cell_fn <- NULL;
   if (TRUE %in% show_labels) {
      use_cell_fn <- jamba::cell_fun_label(
         m=list(cd,
            round(cd)),
         col_hm=colorjam::col_div_xf(color_max),
         show=2);
   }

   # make SE for convenience
   cse <- SummarizedExperiment::SummarizedExperiment(
      assays=list(distance=cd),
      rowData=data.frame(color=rownames(cd)),
      colData=data.frame(color=colnames(cd)))

   data_type <- "distance";
   if ("method" %in% names(attributes(cd))) {
      data_type <- attr(cd, "method")
   }

   # make heatmap
   jamses::heatmap_se(cse,
      use_raster=FALSE,
      data_type=data_type,
      cell_fun=use_cell_fn,
      top_colnames="color",
      rowData_colnames="color",
      sample_color_list=list(color=pc),
      cluster_columns=cluster_data,
      cluster_rows=cluster_data,
      show_left_legend=FALSE,
      show_top_legend=FALSE,
      color_max=color_max,
      centerby_colnames=FALSE,
      legend_at=legend_at,
      row_names_side="left",
      row_dend_side="right",
      column_names_side="top",
      column_names_rot=60,
      row_split=row_split,
      column_split=row_split,
      column_dend_side="bottom",
      legend_labels=legend_labels,
      ...)

}
