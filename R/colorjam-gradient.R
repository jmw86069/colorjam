
#' Divergent color interpolation function with adjustable range and floor
#'
#' Divergent color interpolation function with adjustable range and optional
#' color floor
#'
#' This function is intended to extend the very useful function
#' `circlize::colorRamp2()` which takes a `numeric` `vector` of
#' breaks, and a `character` `vector` of R colors, and returns
#' a function that maps `numeric` values to R colors using
#' interpolated color gradient. This function is intended for
#' specific cases using a divergent color gradient, where this
#' function assumes colors should be mapped to positive and
#' negative numeric values centered at zero.
#'
#' A driving use case is with `ComplexHeatmap::Heatmap()`, with
#' argument `col` that contains a color function produced by
#' `circlize::colorRamp2()` or a color vector. However, when
#' supplying a divergent color vector, the colors are not applied
#' symmetrically above and below zero.
#'
#' @family colorjam gradients
#' @family colorjam assignment
#'
#' @return `function` that maps a vector of `numeric` values
#'    to R colors using the divergent color gradient and numeric
#'    thresholds defined.
#'
#' @param x `numeric` value used as a threshold, where numeric
#'    values at or above this value `x` are assigned the last
#'    color in the color gradient. Negative values at or below
#'    this negative value `-x` are assigned the first color
#'    in the color gradient.
#' @param floor `numeric` optional value where numeric values
#'    between `-x` and `x` are assigned the middle color in the
#'    color gradient. Note that values at exactly `x` or `-x`
#'    are assigned the next respective color away from the middle
#'    color. When `floor=0` or `floor=NULL` no floor is applied,
#'    and colors are assigned using a continuous range of
#'    numeric values from `-x` to `x` with length `n`.
#' @param lens `numeric` value indicating a color lens applied
#'    to the color gradient, passed to `jamba::getColorRamp()`.
#'    Lens values `lens > 0` will condense the color gradient,
#'    making smaller changes more visually distinct; `lens < 0`
#'    expands the color gradient, making smaller changes less
#'    visually distinct.
#' @param n `integer` number of colors used for the initial
#'    color gradient. This value is forced to be an odd number,
#'    so the "middle color" will always be represented as one
#'    strict color. Note that when using a `floor`, the first
#'    non-middle color is used for the `floor` assignment
#'    which means a smaller `n` value will assign a more visibly
#'    distinct color than using a larger `n`. See examples.
#' @param colramp `character` passed to `jamba::getColorRamp()`
#'    which recognizes one of several forms of input:
#'    * `character` string matching the name of a color ramp
#'    from `RColorBrewer` (see divergent palettes with
#'    `RColorBrewer::display.brewer.all(type="div")`).
#'    Note that adding `"_r"` will reverse the color gradient,
#'    so the default `"BuRd_r"` will create a color gradient
#'    with "blue-white-red" - with red for high values
#'    consistent with "heat" in "heatmaps" - where heat is red.
#'    * `character` vector of R colors, which define a specific
#'    color ramp. This vector will be expanded to `n` length.
#' @param open_floor `logical` indicating whether colors below
#'    the assigned `floor` will still receive non-middle color.
#'    Setting `open_floor=TRUE` is the best method to compare
#'    the effect of assigning the strict middle-color to values
#'    below the `floor`, versus using gradient colors below the
#'    `floor`, while all remaining numeric-color assignments
#'    are held constant.
#' @param debug `logical` indicating whether to produce a plot
#'    that shows the resulting color gradient.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' col_fn1 <- col_div_xf(x=3, floor=0, n=21)
#' col_fn2 <- col_div_xf(x=3, floor=1, n=13)
#' col_fn3 <- col_div_xf(x=3, floor=1, n=9)
#' col_fn4 <- col_div_xf(x=3, floor=1, n=5)
#'
#' col_fn2o <- col_div_xf(x=3, floor=1, n=13, open_floor=TRUE)
#' col_fn3o <- col_div_xf(x=3, floor=1, n=9, open_floor=TRUE)
#' col_fn4o <- col_div_xf(x=3, floor=1, n=5, open_floor=TRUE)
#'
#' test_seq <- seq(from=-3, to=3, by=0.05);
#' names(test_seq) <- round(test_seq, digits=2);
#'
#' opar <- par("mfrow"=c(1, 1));
#' bp0 <- barplot(abs(test_seq),
#'    las=2, yaxt="n",
#'    main="floor=0",
#'    col=col_fn1(test_seq),
#'    border="#22222222")
#' abline(v=bp0[abs(test_seq) == 1,], lty="dashed")
#' bp1 <- barplot(abs(test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1",
#'    col=col_fn2(test_seq),
#'    border="#22222222")
#' abline(v=bp1[abs(test_seq) == 1,], lty="dashed")
#' bp2 <- barplot(abs(test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1\nopen_floor=TRUE",
#'    col=col_fn2o(test_seq),
#'    border="#22222222")
#' abline(v=bp2[abs(test_seq) == 1,], lty="dashed")
#' par(opar)
#'
#' test_seq <- seq(from=-3, to=3, by=0.5);
#' names(test_seq) <- round(test_seq, digits=2);
#' test_seq <- c(test_seq,
#'    `-0.999`=-0.999,
#'    `0.999`=0.999);
#' test_seq <- test_seq[order(test_seq)]
#'
#' opar <- par("mfrow"=c(1, 2));
#' bp1 <- barplot((test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1\nn=19",
#'    col=col_fn2(test_seq),
#'    border="#22222244")
#' abline(v=bp1[abs(test_seq) == 1,], lty="dashed")
#' bp2 <- barplot((test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1\nn=19\nopen_floor=TRUE",
#'    col=col_fn2o(test_seq),
#'    border="#22222244")
#' abline(v=bp2[abs(test_seq) == 1,], lty="dashed")
#' bp3 <- barplot((test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1\nn=9",
#'    col=col_fn3(test_seq),
#'    border="#22222244")
#' abline(v=bp3[abs(test_seq) == 1,], lty="dashed")
#' bp3 <- barplot((test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1\nn=9\nopen_floor=TRUE",
#'    col=col_fn3o(test_seq),
#'    border="#22222244")
#' abline(v=bp3[abs(test_seq) == 1,], lty="dashed")
#' bp4 <- barplot((test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1\nn=5",
#'    col=col_fn4(test_seq),
#'    border="#22222244")
#' abline(v=bp4[abs(test_seq) == 1,], lty="dashed")
#' bp4 <- barplot((test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1\nn=5\nopen_floor=TRUE",
#'    col=col_fn4o(test_seq),
#'    border="#22222244")
#' abline(v=bp4[abs(test_seq) == 1,], lty="dashed")
#' par(opar)
#'
#' @export
col_div_xf <- function
(x=1,
 floor=0,
 lens=0,
 n=15,
 colramp="RdBu_r",
 open_floor=FALSE,
 debug=FALSE,
 ...)
{
   if (!(n %% 2) == 1) {
      stop("n must be an odd number");
   }
   x <- abs(x);
   if (length(x) != 1 || x == 0) {
      x <- 1;
   }

   # internal debug plot function
   debug_colors <- function(x, col_fn) {
      test_seq <- seq(from=-x, to=x, length.out=n);
      seq_colors <- col_fn(test_seq);
      names(seq_colors) <- test_seq;
      if (!exists("cbdf")) {
         cbdf <- data.frame(colors=seq_colors,
            breaks=test_seq);
      }
      exp_factor <- ceiling(100 / nrow(cbdf));
      cbdf_v <- rep(jamba::nameVector(cbdf), each=exp_factor);
      names(cbdf_v) <- jamba::breaksByVector(names(cbdf_v))$newLabels;
      jamba::showColors(list(color_breaks=jamba::nameVector(cbdf)), labelCells=TRUE);
   }

   if (length(floor) == 0 || floor <= 0) {
      x_seq <- seq(from=-x, to=x, length.out=n);
      col_fn <- circlize::colorRamp2(
         breaks=x_seq,
         colors=jamba::getColorRamp(colramp,
            n=n,
            lens=lens,
            ...))
      if (debug) {
         debug_colors(x=x, col_fn=col_fn);
      }
      return(invisible(col_fn))
   }
   color_v <- jamba::getColorRamp(colramp, n=n, lens=lens, ...);
   color_1 <- head(color_v, floor(n/2));
   color_2 <- tail(color_v, floor(n/2));
   mid_color <- color_v[ceiling(n/2)];
   colors_v <- c(color_1, rep(mid_color, 3), color_2);

   floor_buffer <- weighted.mean(c(floor, 0), w=c(1e10, 1))
   break_2 <- c(floor_buffer,
      seq(from=floor,
         to=x,
         length.out=floor(n/2)));
   break_1 <- rev(-1 * break_2);
   breaks_v <- c(break_1, 0, break_2);

   # assemble into data.frame to keep values aligned
   cbdf <- data.frame(colors=colors_v,
      breaks=breaks_v);

   # open_floor=TRUE
   # allows colors below the floor to be continuous
   if (open_floor) {
      remove_rows <- c(length(color_1) + 1,
         length(color_1) + 3);
      keep_rows <- setdiff(
         seq_len(nrow(cbdf)),
         remove_rows);
      cbdf <- cbdf[keep_rows,,drop=FALSE];
   }

   # color function used by ComplexHeatmap::Heatmap()
   col_fn <- circlize::colorRamp2(
      breaks=cbdf$breaks,
      colors=cbdf$colors);

   # optional debug=TRUE displays the result for review
   if (debug) {
      print(cbdf);
      debug_colors(x=x, col_fn=col_fn);
   }

   return(invisible(col_fn));
}


#' Linear color interpolation function with adjustable range, baseline, and floor
#'
#' Linear color interpolation function with adjustable range, baseline, and floor
#'
#' This function is the linear equivalent of `col_div_xf()`, in that
#' it takes linear/sequential color gradient instead of a divergent
#' color gradient.
#'
#' @family colorjam gradients
#' @family colorjam assignment
#'
#' @inheritParams col_div_xf
#' @param baseline `numeric` value to define the baseline value, used
#'    when zero is not the initial value. Note that `baseline` can be
#'    either higher or lower than `x`, and colors from `colramp` will
#'    be applied starting at `baseline` through `x`.
#'
#' @examples
#' col_fn1 <- col_linear_xf(x=3, baseline=0, floor=0)
#' col_fn2 <- col_linear_xf(x=3, baseline=0, floor=1)
#'
#' col_fn2o <- col_linear_xf(x=3, baseline=0, floor=1, open_floor=TRUE)
#'
#' test_seq <- seq(from=0, to=3, by=0.05);
#' names(test_seq) <- round(test_seq, digits=2);
#'
#' opar <- par("mfrow"=c(1, 1));
#' bp0 <- barplot(abs(test_seq),
#'    las=2, yaxt="n",
#'    main="floor=0",
#'    col=col_fn1(test_seq),
#'    border="#22222222")
#' abline(v=bp0[abs(test_seq) == 1,], lty="dashed")
#' bp1 <- barplot(abs(test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1",
#'    col=col_fn2(test_seq),
#'    border="#22222222")
#' abline(v=bp1[abs(test_seq) == 1,], lty="dashed")
#' bp2 <- barplot(abs(test_seq),
#'    las=2, yaxt="n",
#'    main="floor=1\nopen_floor=TRUE",
#'    col=col_fn2o(test_seq),
#'    border="#22222222")
#' abline(v=bp2[abs(test_seq) == 1,], lty="dashed")
#' par(opar)
#'
#' col_fn3 <- col_linear_xf(x=3, baseline=6, floor=5)
#' test_seq <- seq(from=0, to=7, by=0.1);
#' names(test_seq) <- round(test_seq, digits=2);
#' bp3 <- barplot(abs(test_seq),
#'    las=2, yaxt="n",
#'    main="baseline=6, x=3, floor=5",
#'    col=col_fn3(test_seq),
#'    border="#22222222")
#' abline(v=bp3[abs(test_seq) == 5,], lty="dashed")
#' abline(v=bp3[abs(test_seq) == 3,], lty="dashed")
#'
#' @export
col_linear_xf <- function
(x=1,
 floor=0,
 baseline=0,
 lens=0,
 n=6,
 colramp="Purples",
 open_floor=FALSE,
 debug=FALSE,
 ...)
{
   if (length(baseline) == 0) {
      stop("baseline must be defined.")
   }
   #x <- abs(x);
   if (length(x) != 1 || x == baseline) {
      x <- baseline + 1;
   }

   # internal debug plot function
   debug_colors_linear <- function(x, col_fn, baseline) {
      test_seq <- seq(from=baseline, to=x, length.out=n);
      seq_colors <- col_fn(test_seq);
      names(seq_colors) <- test_seq;
      if (!exists("cbdf")) {
         cbdf <- data.frame(colors=seq_colors,
            breaks=test_seq);
      }
      exp_factor <- ceiling(100 / nrow(cbdf));
      cbdf_v <- rep(jamba::nameVector(cbdf), each=exp_factor);
      names(cbdf_v) <- jamba::breaksByVector(names(cbdf_v))$newLabels;
      jamba::showColors(list(color_breaks=jamba::nameVector(cbdf)), labelCells=TRUE);
   }

   x_range <- range(c(baseline, x));
   if (length(floor) == 0) {
      floor <- baseline;
   }
   floor_in_range <- (floor > min(x_range) && floor < max(x_range));
   color_v <- jamba::getColorRamp(colramp, n=n, lens=lens, ...);
   if (!floor_in_range) {
      x_seq <- seq(from=baseline, to=x, length.out=n);
      col_fn <- circlize::colorRamp2(
         breaks=x_seq,
         colors=color_v)
      if (debug) {
         debug_colors_linear(x=x, col_fn=col_fn, baseline);
      }
      return(invisible(col_fn))
   }

   color_2 <- tail(color_v, n - 1);
   mid_color <- head(color_v, 1);
   colors_v <- c(rep(mid_color, 2), color_2);

   floor_buffer <- weighted.mean(c(floor, baseline),
      w=c(1e10, 1));
   break_2 <- c(floor_buffer,
      seq(from=floor,
         to=x,
         length.out=n - 1));
   breaks_v <- c(baseline, break_2);

   # assemble into data.frame to keep values aligned
   cbdf <- data.frame(colors=colors_v,
      breaks=breaks_v);

   # open_floor=TRUE
   # allows colors below the floor to be continuous
   if (open_floor) {
      remove_rows <- 2;
      keep_rows <- setdiff(
         seq_len(nrow(cbdf)),
         remove_rows);
      cbdf <- cbdf[keep_rows,,drop=FALSE];
   }

   # color function used by ComplexHeatmap::Heatmap()
   col_fn <- circlize::colorRamp2(
      breaks=cbdf$breaks,
      colors=cbdf$colors);

   # optional debug=TRUE displays the result for review
   if (debug) {
      print(cbdf);
      debug_colors_linear(x=x, col_fn=col_fn, baseline=baseline);
   }

   return(invisible(col_fn));
}


#' Make divergent color gradient
#'
#' Make divergent color gradients that may also use jam_linear and jam_divergent
#'
#' This function is intended for a broad capability to create divergent
#' color gradients. It can take several types of input for each "side"
#' of a divergent gradient, and will apply light (white) or dark (black)
#' middle color as defined.
#'
#' The types of input recognized:
#'
#' * `character` string indicating a single R color, which is passed to
#' `jamba::getColorRamp()` in order to create one linear color gradient
#' with the relevant light or dark baseline color.
#' * `character` vector indicating a specific sequence of R colors, also
#' passed to `jamba::getColorRamp()` to return a single linear color gradient.
#' In this case, the color vector should already include the baseline light (white)
#' or dark (black) color. The order of colors is expected to be from
#' blank color to maximum color.
#' * `character` string indicating the name of a recognized color gradient,
#' which can be from `RColorBrewer`, `viridis`, or one of the names
#' in `jam_linear`.
#'
#' When a color from `jam_linear` is provided, the appropriate gradient
#' is used for the corresponding lite or dark baseline color,
#' where `lite=TRUE` uses `jam_linear`, and `lite=FALSE` uses the
#' appropriate half gradient from `jam_divergent`.
#'
#' Note that this function does not apply the color gradient to a range
#' of numeric values. For that capability, use `col_div_xf()` with the
#' color gradient produced by this function.
#'
#' @family colorjam gradients
#'
#' @examples
#' jamba::showColors(jam_linear)
#'
#' jg1 <- make_jam_divergent("royalblue", "orangered")
#' jamba::showColors(jg1)
#' showDichromat(jg1)
#'
#' jg2 <- make_jam_divergent("slateblue", "firebrick", n=21)
#' jamba::showColors(jg2)
#' showDichromat(jg2)
#'
#' jg3 <- make_jam_divergent("slateblue", "firebrick", lite=FALSE, n=21)
#' jamba::showColors(jg3)
#' showDichromat(jg3)
#'
#' jg4 <- make_jam_divergent("Blues", "Reds", lite=TRUE, n=21)
#' jamba::showColors(c(jg4,
#'    list(BuRd=jamba::getColorRamp("RdBu_r", n=21))))
#'
#' jg5 <- make_jam_divergent("inferno", "dodgerblue1", lite=FALSE, n=21, gradientWtFactor=1)
#' jamba::showColors(jg5)
#'
#' xseq <- seq(from=-1, to=1, by=0.1);
#' mseq <- matrix(xseq, ncol=1);
#' m <- mseq %*% t(mseq);
#' rownames(m) <- seq_len(nrow(m));
#' colnames(m) <- seq_len(ncol(m));
#' hm1 <- ComplexHeatmap::Heatmap(m[,1:10],
#'    cluster_columns=FALSE,
#'    cluster_rows=FALSE,
#'    row_names_side="left",
#'    border=TRUE,
#'    heatmap_legend_param=list(
#'       border="grey10",
#'       at=seq(from=-1, to=1, by=0.25),
#'       color_bar="discrete"),
#'    col=jg3[[1]])
#'
#' hm2 <- ComplexHeatmap::Heatmap(m[21:1,12:21],
#'    cluster_columns=FALSE,
#'    cluster_rows=FALSE,
#'    border=TRUE,
#'    heatmap_legend_param=list(
#'       border=TRUE,
#'       at=seq(from=-1, to=1, by=0.25),
#'       color_bar="discrete"),
#'    col=jg2[[1]])
#' hm1 + hm2
#'
#' # same as above but showing where to use lens
#' hm3 <- ComplexHeatmap::Heatmap(m[,1:10],
#'    cluster_columns=FALSE,
#'    cluster_rows=FALSE,
#'    row_names_side="left",
#'    border=TRUE,
#'    heatmap_legend_param=list(
#'       border=TRUE,
#'       at=seq(from=-1, to=1, by=0.25),
#'       color_bar="discrete"),
#'    col=jamba::getColorRamp(jg3[[1]], divergent=TRUE, lens=2))
#'
#' hm4 <- ComplexHeatmap::Heatmap(m[21:1,12:21],
#'    cluster_columns=FALSE,
#'    cluster_rows=FALSE,
#'    border=TRUE,
#'    heatmap_legend_param=list(
#'       border=TRUE,
#'       at=seq(from=-1, to=1, by=0.25),
#'       color_bar="discrete"),
#'    col=jamba::getColorRamp(jg2[[1]], divergent=TRUE, lens=2))
#' hm3 + hm4
#'
#' @param linear1 `character` input consisting of one of:
#'    a single R color; a single color gradient name; or a vector
#'    of R colors. When supplying a vector of colors, the order
#'    is expected to be from blank to maximum color.
#' @param linear2 `character` input consisting of one of:
#'    a single R color; a single color gradient name; or a vector
#'    of R colors. When supplying a vector of colors, the order
#'    is expected to be from blank to maximum color.
#' @param lite `logical` indicating whether the middle color
#'    should be lite (white), or when `lite=FALSE` the middle
#'    color will be dark (black).
#' @param n `integer` number of final colors to produce. Note that
#'    `n` must be an odd number, in order to preserve the middle color.
#' @param ... additional arguments are passed to functions called
#'    as needed.
#'
#' @export
make_jam_divergent <- function
(linear1,
 linear2,
 lite=TRUE,
 n=21,
 ...)
{
   # determine the number of entries being requested
   # by using the max length of these arguments
   n_out <- max(c(
      length(linear1),
      length(linear2),
      length(lite),
      length(n)));
   linear1 <- rep(linear1, length.out=n_out);
   linear2 <- rep(linear2, length.out=n_out);
   lite <- rep(lite, length.out=n_out);
   n <- rep(n, length.out=n_out);

   get_jam_gradient <- function(x, lite=TRUE, n=11, ...) {
      if (length(x) == 1 && x %in% names(jam_linear)) {
         if (lite) {
            xcolors <- jam_linear[[x]];
         } else {
            xwhich <- match(x, gsub("_.+", "", names(jam_divergent)));
            xcolors <- jam_divergent[[xwhich]];
            xcolors <- rev(head(xcolors, ceiling(length(xcolors)/2)));
         }
         jamba::getColorRamp(xcolors,
            n=n,
            ...)
      } else {
         if (lite) {
            defaultBaseColor <- "white";
         } else {
            defaultBaseColor <- "black";
         }
         xcolors <- jamba::getColorRamp(x,
            n=n,
            defaultBaseColor=defaultBaseColor,
            ...)
      }
   }

   if (length(names(linear1)) == 0) {
      if (is.atomic(linear1)) {
         names(linear1) <- linear1;
      } else {
         names(linear1) <- jamba::makeNames(rep("linear1", length(linear1)));
      }
   }
   if (length(names(linear2)) == 0) {
      if (is.atomic(linear2)) {
         names(linear2) <- linear2;
      } else {
         names(linear2) <- jamba::makeNames(rep("linear2", length(linear2)));
      }
   }
   gradient_names <- paste0(
      names(linear1),
      "_",
      names(linear2));
   gradient_names <- ifelse(lite,
      gradient_names,
      paste0(gradient_names, "_dark"));
   gradient_list <- lapply(seq_along(gradient_names), function(k){
      nk <- ceiling(n[k] / 2);
      if (nk == 0) {
         nk <- 11;
      }
      gr1 <- head(rev(
         get_jam_gradient(linear1[[k]],
            lite=lite[k],
            n=nk,
            ...)), -1);
      gr2 <- get_jam_gradient(linear2[[k]],
         lite=lite[k],
         n=nk,
         ...);
      if (1 == 2) {
         if (lite[k]) {
            gr1 <- head(rev(jam_linear[[linear1[k]]]), -1)
            gr2 <- jam_linear[[linear2[k]]];
         } else {
            n1 <- jamba::vigrep(paste0("^", linear1[k], "_"), names(jam_divergent));
            n2 <- jamba::vigrep(paste0("_", linear2[k], "$"), names(jam_divergent));
            gr1 <- head(jam_divergent[[n1]], 10)
            gr2 <- tail(jam_divergent[[n2]], 11)
         }
      }
      gr12 <- c(gr1, gr2);
      if (n == 0) {
         gr12 <- jamba::getColorRamp(gr12,
            n=NULL,
            divergent=TRUE)
      }
      gr12;
   })
   names(gradient_list) <- gradient_names;
   return(gradient_list);
}


#' Show colors using dichromat color blindness adjustment
#'
#' Show colors using dichromat color blindness adjustment
#'
#' This function is a very simple wrapper around `jamba::showColors()`
#' which also applies one of the color blindness emulations from
#' `dichromat::dichromat()`.
#'
#' @family colorjam display
#'
#' @param x `list` or `character` vector with R compatible colors.
#' @param type `character` passed to `dichromat::dichromat()` for one
#'    or more types of color blindness to simulate.
#' @param sep `character` used as a delimited to label each resulting
#'    color vector.
#' @param spacer `logical` indicating whether to include a blank spacer
#'    between sets of colors. This spacer is mainly useful for display.
#' @param original `logical` indicating whether to include original colors
#'    and adjusted colors.
#' @param do_plot `logical` indicating whether to plot the results
#'    using `jamba::showColors()`.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' showDichromat(jam_linear["firebrick"])
#'
#' showDichromat(jam_linear[1:2])
#'
#' showDichromat(jam_linear[7:9])
#'
#' showDichromat(jam_linear, type="tritan", spacer=FALSE)
#'
#' showDichromat(jam_linear, type="tritan", spacer=FALSE, original=FALSE)
#'
#' @export
showDichromat <- function
(x,
   type=c("deutan", "protan", "tritan"),
   sep="\n",
   spacer=TRUE,
   original=TRUE,
   do_plot=TRUE,
   ...)
{
   if (!require(dichromat)) {
      stop("The dichromat package is required.");
   }
   if (is.atomic(x)) {
      x <- list(x);
   }
   if (length(names(x)) == 0) {
      names(x) <- seq_along(x);
   }
   type <- match.arg(type, several.ok=TRUE);
   names(type) <- type;
   x_new <- lapply(seq_along(x), function(i){
      x_di <- lapply(type, function(k){
         dichromat::dichromat(x[[i]], type=k)
      })
      names(x_di) <- paste0(names(x)[i], sep, type);
      if (original) {
         c(x[i], x_di)
      } else {
         x_di
      }
   })
   x_set <- x_new[[1]];
   if (spacer) {
      blank <- list(` `="transparent")
   } else {
      blank <- NULL
   }
   for (i in tail(seq_along(x_new), -1)) {
      x_set <- c(x_set,
         blank,
         x_new[[i]]);
   }
   if (do_plot) {
      jamba::showColors(x_set,
         ...)
   }
   return(invisible(x_set));
}


#' Create two-step linear gradient
#'
#' Create two-step linear gradient by gradually blending
#' two linear color gradients
#'
#' This function is intended to produce a two-step linear gradient
#' effect, similar to the strategy used by `RColorBrewer`, but
#' without specific color constraints. See examples.
#'
#' This function takes two color gradients and blends them
#' using a weighting scheme that begins with 100% `color1`, and
#' gradually becomes 100% `color2`.
#'
#' The input `color1` and `color2` can be any input recognized
#' by `jamba::getColorRamp()`. For example a single color can
#' be used to create a gradient, or the name of a known color
#' gradient can be used, for example `"Reds"` will refer
#' to `RColorBrewer` palette `"Reds"`. See the examples.
#'
#' In general most gradients can be blended using this function
#' to produce a new color gradient where both the visual intensity
#' and color hue vary along the gradient, making each color step
#' more visibly distinct than when only the visual intensity
#' changes.
#'
#' When supplying a single color as input to `color1` or `color2`
#' it sometimes works best to alter the brightness of one or both
#' colors so the intermediate gradients have similar intensities.
#' Experimenting with `debug=TRUE` is recommended.
#'
#' @family colorjam gradients
#'
#' @examples
#' ts <- twostep_gradient("yellow", debug=TRUE)
#'
#' ts1 <- twostep_gradient("orange2", "firebrick", n=11, debug=TRUE)
#' ts2 <- twostep_gradient("aquamarine", "dodgerblue", n=11, debug=TRUE)
#'
#' # stitch them together with make_jam_divergent()
#' ts1ts2 <- make_jam_divergent(list(ts2=ts2), list(ts1=ts1), n=21)
#' jamba::showColors(ts1ts2)
#' ts1ts2flat <- make_jam_divergent("dodgerblue", "firebrick", n=21)
#' jamba::showColors(list(
#'    twostep=ts1ts2[[1]],
#'    flat=ts1ts2flat[[1]]))
#'
#' ts3 <- twostep_gradient("Greens", "Blues", n=11, debug=TRUE)
#'
#' ts4 <- twostep_gradient("slateblue2", "firebrick", n=11, debug=TRUE)
#'
#' ts5 <- twostep_gradient("cividis", "inferno", n=11, debug=TRUE, adjust=-1.2)
#'
#' gr1 <- twostep_gradient("slateblue", "purple", debug=TRUE)
#' gr2 <- twostep_gradient("gold", "darkorange", debug=TRUE)
#' div12 <- make_jam_divergent(list(gr1=gr1), list(gr2=gr2))
#' jamba::showColors(div12)
#' div12flat <- make_jam_divergent("purple", "gold")
#' jamba::showColors(list(
#'    twostep=div12[[1]],
#'    flat=div12flat[[1]]))
#'
#' gr1d <- twostep_gradient("slateblue1", "purple", debug=TRUE, lite=FALSE)
#' gr2d <- twostep_gradient("darkorange", "gold", debug=TRUE, lite=FALSE)
#' div12d <- make_jam_divergent(list(gr1d=gr1d), list(gr2d=gr2d))
#' jamba::showColors(div12d)
#' div12dflat <- make_jam_divergent("purple", "gold", lite=FALSE)
#' jamba::showColors(list(
#'    twostep=div12d[[1]],
#'    flat=div12dflat[[1]]))
#'
#' @param color1 `character` color or name of a recognized color gradient.
#' @param color2 `character` color or name of a recognized color gradient;
#'    or when `color2=NULL` then the hue of `color1` is shifted to
#'    emulate the effect of having a similar neighboring color hue.
#'    In this case the input `color1` is used as `color2` to become
#'    the primary output color.
#' @param n `integer` number of gradient colors to return. When `n=0`
#'    or `n=NULL` the output is a color function.
#' @param lite `logical` indicating whether the background color
#'    should be white, or when `lite=FALSE` the background color
#'    is black.
#' @param defaultBaseColor `character` used to define a specific
#'    background color, and therefore overrides `lite`.
#' @param adjust `numeric` value used to adjust the relative
#'    weight between `color1` and `color2`, where values higher
#'    than 1 favor `color2` and negative values, or values less
#'    than 1 favor `color1`.
#' @param do_fixYellow `logical` indicating whether to call
#'    `jamba::fixYellow()` which fixes the greenish hue that
#'    sometimes results from what is intended to be pure yellow.
#' @param debug `logical` indicating whether to create a plot
#'    to show the color blending steps.
#' @param ... additional arguments are passed to `jamba::getColorRamp()`.
#'
#' @export
twostep_gradient <- function
(color1=NULL,
 color2=NULL,
 n=11,
 lite=TRUE,
 defaultBaseColor=NULL,
 adjust=1.5,
 do_fixYellow=TRUE,
 debug=FALSE,
 ...)
{
   nk <- jamba::noiseFloor(n, minimum=5);

   # special case where color2 is NULL
   if (length(color2) == 0) {
      color2 <- color1;
      color1hcl <- jamba::col2hcl(color2);
      H_add <- ifelse(color1hcl["H",] < 40, 50,
         ifelse(color1hcl["H",] > 200, -20,
            ifelse(color1hcl["H",] < 120, -60, 70)));
      color1hcl["H",] <- colorjam::hw2h(preset="ryb",
         colorjam::h2hw(color1hcl["H",], preset="ryb") + H_add);
      color1hcl["L",] <- jamba::noiseFloor(color1hcl["L",],
         minimum=45);
      color1hcl["C",] <- jamba::noiseFloor(color1hcl["C",],
         minimum=160);
      color1 <- jamba::hcl2col(color1hcl)
      showColors(c(color1, color2))
   }
   if (length(defaultBaseColor) == 0) {
      if (lite) {
         defaultBaseColor <- "white"
      } else {
         defaultBaseColor <- "black"
      }
   }
   g1 <- jamba::getColorRamp(color1,
      n=nk,
      defaultBaseColor=defaultBaseColor,
      ...);
   g2 <- jamba::getColorRamp(color2,
      n=nk,
      defaultBaseColor=defaultBaseColor,
      ...);

   # gradient weight
   wseq <- seq(from=1, to=0, length.out=nk - 1);
   if (adjust > 1) {
      wseq <- wseq ^ adjust
   } else if (adjust > 0 && adjust < 1) {
      wseq <- wseq ^ adjust
   } else if (adjust < 0) {
      wseq <- wseq ^ (1/-adjust)
   }
   w1 <- c(1, wseq);
   w2 <- 1 - w1;
   wdf <- data.frame(w1=w1, w2=w2);
   print(wdf);

   g12 <- sapply(seq_len(n), function(i){
      colorjam::blend_colors(c(
         jamba::alpha2col(g1[i], alpha=w1[i]),
         jamba::alpha2col(g2[i], alpha=w2[i])
      ))})

   # optionally "fix" yellow hues
   if (do_fixYellow) {
      g12 <- jamba::fixYellow(g12);
   }

   if (debug) {
      jamba::showColors(list(g1=g1, g2=g2, g12=g12))
      lines(x=seq_len(nk),
         y=-1 * wdf$w1 + 2,
         type="b", lwd=2)
      lines(x=seq_len(nk),
         y=-1 * wdf$w2 + 2,
         type="b", lwd=2)
   }
   if (!n == nk) {
      if (n == 0) {
         n <- NULL
      }
      g12 <- jamba::getColorRamp(g12,
         n=n,
         divergent=TRUE);
   }
   return(g12);
}
