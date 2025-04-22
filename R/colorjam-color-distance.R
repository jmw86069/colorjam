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
#' @param method `character`, default 'cie2000', the color distance method,
#'    passed to `farver::compare_colour()`. In future, this argument
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
#' @param ... additional arguments are passed to `farver::compare_colour()`
#'
#' @examples
#' color_distance("red", "firebrick", use_white="D65")
#'
#' color_distance(grDevices::palette.colors(15))
#'
#' pc <- rainbowJam(5)
#' cd <- color_distance(pc);
#' show_color_distance(cd, pc=pc, column_title="rainbowJam()");
#'
#' pc <- grDevices::palette.colors(15);
#' cd <- color_distance(pc);
#' show_color_distance(cd, pc=pc, column_title="palette.colors()");
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
 use_white="F5",
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
      ...)
   attr(cd, "method") <- method;

   return(cd)
}

#' Show color distance as a heatmap
#'
#' Show color distance as a heatmap
#'
#' This function uses `ComplexHeatmap::Heatmap()`, which is not
#' required for colorjam as a whole. It creates a matrix visual
#' summary of color distances, and displays the actual colors as
#' row and column annotations outside the color distance
#' heatmap.
#'
#' @returns `ComplexHeatmap::Heatmap` object, when printed it will
#'    draw a heatmap.
#'
#' @param cd `numeric` matrix, default NULL, with pre-calculated values.
#' @param pc `character` vector of colors used in `cd`, useful when
#'    there are `names(pc)` to use as heatmap column or row labels.
#'    When only `pc` is provided and not `cd`, the `cd` is calculated
#'    by calling `color_distance(pc)`.
#' @param show_labels `logical` default TRUE, used when `pc` is provided.
#' @param cluster_data `logical` default FALSE, whether to enable row
#'    and column hierarchical clustering in the heatmap.
#' @param row_split `numeric` default 0, used when `cluster_data=TRUE`
#'    to subdivide the dendrogram into this many separate subclusters.
#' @param ... additional arguments are passed to `color_distance()`,
#'    then to `jamses::heatmap_se()`. Many arguments in `jamses::heatmap_se()`
#'    are also passed through to `ComplexHeatmap::Heatmap()`.
#'
#' @examples
#' pc <- grDevices::palette.colors(25);
#' cd <- color_distance(pc, method="cie2000");
#' show_color_distance(cd, pc)
#'
#' # with clustering
#' show_color_distance(cd, pc, cluster_data=TRUE,
#'    column_title_gp=grid::gpar(fontsize=20),
#'    column_title="palette.colors()")
#'
#' # compare two color vectors
#' pc1 <- rainbowJam(10, preset="ryb2")
#' pc2 <- colorspace::rainbow_hcl(10)
#' cd <- color_distance(pc1, pc2)
#' show_color_distance(cd, cluster_data=TRUE,
#'    row_title_rot=90, row_title="rainbowJam()",
#'    row_title_gp=grid::gpar(fontsize=20),
#'    column_title_gp=grid::gpar(fontsize=20),
#'    column_title_rot=0, column_title="rainbow_hcl()")
#'
#' # evaluate the small step size between HCL rainbow colors
#' show_color_distance(pc2,
#'    column_title_gp=grid::gpar(fontsize=20),
#'    column_title_rot=0, column_title="rainbow_hcl()")
#'
#' # evaluate the larger step sizes between colorjam rainbow colors
#' show_color_distance(pc1, cluster_data=FALSE,
#'    column_title_gp=grid::gpar(fontsize=20),
#'    column_title_rot=0, column_title="rainbowJam()")
#'
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
