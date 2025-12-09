# colorjam-show-distance.R

#' Show color distance as a heatmap
#'
#' Show color distance as a heatmap
#'
#' This function formerly used `ComplexHeatmap::Heatmap()`, now uses
#' `jamba::imageByColors()` to remove the dependency on another
#' R package.
#'
#' It creates a matrix visual
#' summary of color distances, and displays the actual colors as
#' row and column annotations outside the color distance
#' heatmap.
#'
#' @family colorjam internal
#'
#' @returns `list` with two elements, suitable to use with
#'    `jamba::imageByColors()`.
#'    * `cd`: the color distance matrix
#'    * `cdc`: the heatmap colorized matrix
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
#' @examplesIf (requireNamespace("ComplexHeatmap", quietly=TRUE))
#' pc <- grDevices::palette.colors(25, palette="Polychrome 36");
#' cd <- color_distance(pc, method="cie2000");
#' show_color_distance(cd, pc)
#'
#' # with clustering
#' show_color_distance(cd, pc, cluster_data=TRUE)
#'
#' # compare two color vectors
#' pc1 <- rainbowJam(10, preset="ryb2")
#' pc2 <- colorspace::rainbow_hcl(10)
#' cd <- color_distance(pc1, pc2)
#' show_color_distance(cd, cluster_data=FALSE)
#'
#' # evaluate the small step size between HCL rainbow colors
#' show_color_distance(pc2)
#'
#' # evaluate the larger step sizes between colorjam rainbow colors
#' show_color_distance(pc1, cluster_data=FALSE)
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
   if (inherits(cd, "character") && is.atomic(cd)) {
      pc <- cd;
      cd <- NULL;
   }
   if (length(cd) == 0 && length(pc) > 1) {
      cd <- color_distance(pc,
         ...);
   }

   ## Optionally cluster
   if (isTRUE(cluster_data)) {
      cd <- tryCatch({
         hcr <- hclust(dist(-cd));
         hcc <- hclust(dist(t(-cd)));
         cdro <- labels(as.dendrogram(hcr));
         cdco <- labels(as.dendrogram(hcc));
         if (length(pc) == ncol(cd)) {
            pc <- pc[match(cdco, colnames(cd))]
         } else if (length(pc) == nrow(cd)) {
            pc <- pc[match(cdro, rownames(cd))]
         }
         cd[cdro, cdco, drop=FALSE];
      }, error=function(e){
         cd
      })
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

   # make color matrix
   cdc <- matrix(ncol=ncol(cd), nrow=nrow(cd),
      dimnames=dimnames(cd),
      data=col_div_xf(color_max)(cd));

   withr::with_par(list(mar=c(5, 7, 1, 1)), {
      cexCellnote <- 1;
      if (ncol(cd) > 12) {
         cexCellnote <- 3 / sqrt(ncol(cd))
      }

      # draw the matrix heatmap
      jamba::imageByColors(cdc,
         cexCellnote=cexCellnote,
         xaxt="n",
         yaxt="n",
         cellnote=signif(cd, digits=3));

      labelCex <- 0.8;
      if (ncol(cd) > 12) {
         labelCex <- 2.4 / sqrt(ncol(cd))
      }
      labelCexY <- 0.8;
      if (nrow(cd) > 12) {
         labelCexY <- 2.4 / sqrt(nrow(cd))
      }
      # add colorized axis labels
      axisLabels <- paste0(" \n", colnames(cd), "\n ");
      jamba::drawLabels(x=seq_along(colnames(cd)),
         y=0.45,
         adjPreset="bottom",
         labelCex=labelCex,
         txt=axisLabels,
         boxColor=pc[colnames(cd)])

      axisLabelsY <- ifelse(jamba::isColor(rownames(cd)),
         rownames(cd),
         rownames(cd))
      jamba::drawLabels(y=seq_along(rownames(cd)),
         x=0.45,
         adjPreset="left",
         labelCex=labelCexY * 1.5,
         txt=axisLabelsY,
         boxColor=pc[rownames(cd)])
   })
   return(invisible(list(cd=cd, cdc=cdc, pc=pc)));
}
