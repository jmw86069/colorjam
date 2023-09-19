
# model this behavior after igraph::shapes() and .igraph.shapes:
.colorjam_presets <- new.env();
.colorjam_presets[["ryb"]] <- list(
   h1=c(12,  60, 120, 240),
   h2=c(0, 120, 180, 240),
   direction=1,
   default_step="v23");
attr(.colorjam_presets[["ryb"]], "description") <- "Red-Yellow-Blue";

# .colorjam_presets[["ryb1"]] <- list(
#    h2=c(0, 30, 60,
#       90, 120, 150,
#       180, 210, 240,
#       270, 300, 330),
#    h1=c(12.2, 23, 40,
#       60, 80.9, 107.3,
#       127.7, 180, 245,
#       265, 288, 315),
#    direction=1,
#    default_step="v23");

# reversed ryb starting at yellow
.colorjam_presets[["ryb2"]] <- list(
   h1=c(12.2, 23, 40,
      60, 81, 107,
      128, 180, 245,
      265, 288, 315),
   h2=c(470, 440, 410,
      380, 350, 320,
      290, 260, 230,
      200, 170, 140),
   direction=-1,
   default_step="v24");
attr(.colorjam_presets[["ryb2"]], "description") <- (
   "Yellow-Blue-Red rotated to begin at yellow, and reversed relative to ryb");

# legacy compatibility
# .colorjam_presets[["ryb3"]] <- .colorjam_presets[["ryb"]]

# Red-Green_blue
.colorjam_presets[["rgb"]] <- list(
   h1=c(0, 360),
   h2=c(0, 360),
   direction=1,
   default_step="v23");
attr(.colorjam_presets[["rgb"]], "description") <- "Red-Green-Blue";

# reversed rgb starting at yellow
.colorjam_presets[["rgb2"]] <- list(
   h1=c(0, 360),
   h2=c(430, 70),
   direction=-1,
   default_step="v24");
attr(.colorjam_presets[["rgb2"]], "description") <- (
   "Red-Green-Blue rotated to begin at yellow, and reversed relative to rgb");

.colorjam_presets[["none"]] <- list(
   h1=c(0, 360),
   h2=c(0, 360),
   direction=1,
   default_step="v23");
attr(.colorjam_presets[["none"]], "description") <- "Red-Green-Blue";

# original dichromat color wheel
.colorjam_presets[["dichromat"]] <- list(
   h1=c(8, 30,
      65, 120, 200,
      240, 260, 280,
      330),
   h2=c(0, 79.1,
      118.7 - 1e-8, 118.7, 118.7 + 1e-8,
      118.7 + 2e-8,
      185.9, 304.6,
      344.2),
   direction=1,
   default_step="v23")
attr(.colorjam_presets[["dichromat"]], "description") <- (
   "dichromat-adjusted color wheel");

# modified dichromat starting at yellow
.colorjam_presets[["dichromat2"]] <- list(
   h1=c(45,   8, 340, 306, 280, 240, 180,    120,     70),
   h2=c(45, 115, 185, 192, 200, 330, 359.99, 359.99, 359.99),
   direction=-1,
   default_step="v24")
attr(.colorjam_presets[["dichromat2"]], "description") <- paste(
   "dichromat-adjusted color wheel rotated to begin at yellow,",
   "and reversed relative to dichromat");

   # h1=c(8, 30, 65,
   #    120, 200, 240,
   #    260, 280, 330),
   # h2=c(115, 35.9, -3.7+1e-9,
   #    -3.7, -3.7-1e-9, -3.7-2e-9,
   #    -70.9, -189.6, -229.2),
   # direction=-1,
   # default_step="v24")

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
#'    `preset` is provided, it returns a `list` with elements:
#'    * `h1`: HCL color hue indicating the actual hue to be used
#'    when generating a color.
#'    * `h2`: HCL color hue indicating the virtual hue assigned to each
#'    actual hue in `h1`.
#'    * `direction`: with `1` indicating forward (increasing, clockwise)
#'    progression around the color wheel, and `-1` indicating reverse
#'    (decreasing, counter-clockwise) progression around the color
#'    wheel.
#'    * `default_step`: `character` string with name matching a
#'    value returned by `colorjam_steps()`
#'    * optional attribute `attr(x, "description")` which may contain
#'    a text description for each color preset
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
#' The colorjam presets represent customized color wheels, used
#' for various color operations.
#'
#' * `h1` contains actual color hue values, with range `c(0, 360)`.
#' Its values are intended to be used in HCL color hue operations,
#' such as `jamba::hcl2col()`, or `colorspace::polarLUV()`. These
#' are not equal to `HSV` or `HLS` (also known as HSL) color hues,
#' although colorjam preset could be used to convert from HCL to HSV.
#' * `h2` contains virtual color hue values, with range `c(0, 360)`.
#' These values are intended to represent the full virtual color
#' wheel, mapping the virtual hue `h2` to actual hue `h1`.
#' Not all values in `h1` need be represented, in fact `"dichromat"`
#' presets explicitly remove a sizable chunk of green color hues.
#' * `direction` the direction of `h1` relative to `h2` encoded as:
#'
#'    * `1` for forward direction, and
#'    * `-1` for reverse direction.
#'
#' * `default_step` used primarily by `rainbowJam()` to define the
#' appropriate Chroma/Luminance stepping pattern when choosing
#' actual HCL colors around the color wheel.
#' * optional attribute `"description"` as `attr(x, "description")`.
#'
#' @returns `TRUE`, invisibly.
#'
#' @family colorjam hue warp
#'
#' @param preset `character` string with the preset name.
#' @param h1,h2 `numeric` vectors of equal length, or `NULL` to
#'    remove an existing preset.
#' @param direction `numeric` with one of two accepted values:
#'    * `1`: (default) indicating forward (increasing, clockwise)
#'    progression around the color wheel
#'    * `-1`: indicating reverse
#'    (decreasing, counter-clockwise) progression around the color
#'    wheel.
#' @param default_step `character` string indicating the recommended
#'    `step` from `colorjam_steps()`.
#' @param description `character` with optional text description of
#'    the preset. The description will be added as an attribute:
#'    `attr(x, "description")`
#' @param ... additional arguments are ignored.
#'
#' @examples
#' h1 <- c(8, 30, 65,
#'    120, 200, 240,
#'    260, 280, 330)
#' h2 <- c(115, 35.9, -3.7,
#'    -3.7, -3.7, -3.7,
#'    -70.9, -189.6, -229.2)
#' add_colorjam_preset("custom_dichromat",
#'    h1=h1, h2=h2, direction=-1, default_step="v24",
#'    description="Custom dichromat example")
#'
#' # behold the new preset name appears
#' colorjam_presets()
#'
#' # the preset data is available
#' colorjam_preset("custom_dichromat")
#'
#' # the preset is used to create rainbow categorical colors
#' color_pie(rainbowJam(n=10,
#'    preset="custom_dichromat"))
#'
#' # remove a preset by defining `h1=NULL`
#' add_colorjam_preset("custom_dichromat", h1=NULL)
#' colorjam_presets()
#'
#' @export
add_colorjam_preset <- function
(preset,
 h1,
 h2=NULL,
 direction=1,
 default_step=NULL,
 description=NULL,
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
            cli::cli_alert_info("removed preset '{.field {preset}}'.")
         }
      }
   } else {
      # validate input
      h1h2 <- validate_colorjam_preset(h1=h1,
         h2=h2,
         direction=direction,
         default_step=default_step,
         description=description,
         ...)

      # update colorjam preset data
      new_preset <- list(
         h1=h1h2$h1,
         h2=h1h2$h2,
         direction=h1h2$direction,
         default_step=h1h2$default_step);
      if (length(description) > 0 && is.character(description)) {
         attr(new_preset, "description") <- description;
      }
      assign(x=preset,
         value=new_preset,
         envir=.colorjam_presets)
      if (TRUE %in% verbose) {
         cli::cli_alert_info("added preset '{.field {preset}}'.")
      }
   }
   invisible(TRUE)
}

#' Validate h1,h2 color hue warp data
#'
#' Validate h1,h2 color hue warp data, internal function
#'
#' Validate the `h1`,`h2` color hue values alongside the `direction`
#' the angles should proceed. This function handles cases where
#' `h2` contains duplicate values, which is used to remove a
#' section of `h1` from the output `h2` color wheel. Duplicated
#' values are not handled well by `approx()`, so this function
#' rounds values to 4 digits, then sorts data based upon `direction`,
#' then finally adjusts any duplicated `h2` values by adding
#' `1e-8`.
#'
#' ## Processing steps:
#'
#' * `h1`,`h2` angles are adjusted within range `c(0, 360)` using
#' `x %% 360`.
#' * `h1`,`h2` values are rounded using `round(x, digits=digits)`
#' * `h1`,`h2` values are sorted by `h2` increasing, and `h1`
#' in order of `direction`
#' * Duplicated `h2` values are adjusted by adding `c(0, 1, 2, ...) + 1e-8`
#' to each duplicated value, per set of duplicated values.
#' The first duplicated value in each set is unchanged, and
#' subsequent values in the set are increased by `1e-8`.
#'
#' ## TODO:
#'
#' * Enforce sensible ordering of values, mainly so the R-shiny app
#' h1,h2 edited values will permit pushing identical values.
#' For example, two identical `h2` values cause the corresponding
#' `h1` values to be skipped, thus removing that range of hues.
#' When one `h2` value is increased, it should also increase subsequent
#' values.
#'
#' @family colorjam hue warp
#'
#' @returns `list` with components:
#'    * `h1`: HCL color hue indicating the actual hue to be used
#'    when generating a color.
#'    * `h2`: HCL color hue indicating the virtual hue assigned to each
#'    actual hue in `h1`.
#'    * `direction`: with `1` indicating forward (increasing, clockwise)
#'    progression around the color wheel, and `-1` indicating reverse
#'    (decreasing, counter-clockwise) progression around the color
#'    wheel.
#'    * `default_step`: `character` string with name matching a
#'    value returned by `colorjam_steps()`
#'
#' @param h1,h2 `numeric` color hue in degrees
#' @param direction `numeric` with one of two accepted values:
#'    * `1`: (default) indicating forward (increasing, clockwise)
#'    progression around the color wheel
#'    * `-1`: indicating reverse
#'    (decreasing, counter-clockwise) progression around the color
#'    wheel.
#' @param digits `numeric` passed to `round()` before comparing
#'    `h2` values for duplicated values.
#' @param ... additional arguments are ignored.
#'
#' @export
validate_colorjam_preset <- function
(h1=NULL,
 h2=NULL,
 direction=NULL,
 default_step=NULL,
 preset=NULL,
 digits=4,
 ...)
{
   # check if h1 is list output from colorjam_presets()
   if (is.list(h1) && all(c("h1", "h2", "direction") %in% names(h1))) {
      h2 <- h1$h2;
      direction <- h1$direction;
      if ("default_step" %in% names(h1)) {
         default_step <- h1$default_step;
      }
      h1 <- h1$h1;
   }
   # optional preset
   if (length(preset) > 0 && !"custom" %in% preset) {
      h1h2 <- colorjam_presets(preset);
      h1 <- h1h2$h1;
      h2 <- h1h2$h2;
      direction <- head(h1h2$direction, 1);
      default_step <- head(h1h2$default_step, 1);
   }
   # create data.frame by rounding input h1,h2 angles
   h1h2 <- data.frame(
      h1=round(digits=digits, h1) %% 360,
      h2=round(digits=digits, h2) %% 360);

   # sort by increasing h2
   # also take unique rows although they should not originate from colorjam
   h1h2_sorted <- unique(jamba::mixedSortDF(h1h2,
      byCols=c(2, 1 * direction)))

   # adjust duplicated h2
   if (any(duplicated(h1h2_sorted$h2))) {
      h2_duped <- unique(h1h2_sorted$h2[duplicated(h1h2_sorted$h2)])
      for (h2_dupe in h2_duped) {
         h2_which <- which(h1h2_sorted$h2 %in% h2_dupe)
         # the first value is unchanged
         # which means the first value is preferred
         vseq <- seq(from=0, length.out=length(h2_which)) * 1e-8;
         h1h2_sorted[h2_which, "h2"] <- h1h2_sorted[h2_which, "h2"] + vseq;
      }
   }
   # adjust duplicated h1
   if (any(duplicated(h1h2_sorted$h1))) {
      h1_duped <- unique(h1h2_sorted$h1[duplicated(h1h2_sorted$h1)])
      for (h1_dupe in h1_duped) {
         h1_which <- which(h1h2_sorted$h1 %in% h1_dupe)
         # the first value is unchanged
         # which means the first value is preferred
         vseq <- seq(from=0, length.out=length(h1_which)) * 1e-8;
         if (direction < 0) {
            vseq <- rev(vseq);
         }
         h1h2_sorted[h1_which, "h1"] <- h1h2_sorted[h1_which, "h1"] + vseq;
      }
   }

   rownames(h1h2_sorted) <- seq_len(nrow(h1h2_sorted))
   h1 <- h1h2_sorted$h1;
   h2 <- h1h2_sorted$h2;
   list(h1=h1h2_sorted$h1,
      h2=h1h2_sorted$h2,
      direction=direction,
      default_step=default_step)
}

#' Plot colorjam preset
#'
#' Plot colorjam preset
#'
#' Plot the `h1`,`h2` color hue association, by default shown
#' relative to the `h2` (virtual hue) value on the x-axis,
#' and `h1` (actual hue) on the y-axis.
#'
#' Any duplicated `h2` values are indicated with a vertical line.
#'
#' @family colorjam hue warp
#'
#' @param colorize_borders `logical` indicating whether to display the color
#'    spectrum along the x-axis and y-axis borders.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @export
plot_colorjam_preset <- function
(preset=NULL,
 h1=NULL,
 h2=NULL,
 direction=1,
 xlim=c(0, 360),
 ylim=c(0, 360),
 digits=4,
 cex=1,
 col="#FFFFFF",
 pt_pch=20,
 pt_cex=1,
 pt_col="blue",
 pt_step=5,
 colorize_borders=TRUE,
 axis_fontfamily="Arial",
 axislabel_fontsize=24,
 axis_fontsize=18,
 style=c("base",
    "plotly"),
 verbose=FALSE,
 ...)
{
   #
   style <- match.arg(style);
   if ("plotly" %in% style && !jamba::check_pkg_installed("plotly")) {
      cli::cli_abort(
         "Argument {.code style=\"plotly\"} requires the {.pkg plotly} package.")
   }

   if (is.list(preset) && all(c("h1", "h2", "direction") %in% names(preset))) {
      h1 <- preset$h1;
      h2 <- preset$h2;
      direction <- preset$direction;
      preset <- NULL;
   }
   if (length(preset) > 0) {
      h1h2 <- validate_colorjam_preset(preset=preset);
   } else {
      h1h2 <- validate_colorjam_preset(
         h1=h1,
         h2=h2,
         direction=direction,
         default_step=default_step)
   }
   direction <- head(h1h2$direction, 1);
   default_step <- head(h1h2$default_step, 1);

   byCols <- c(1, -2);
   if (direction < 0) {
      byCols <- c(1, 2)
   }
   h1h2 <- jamba::mixedSortDF(as.data.frame(jamba::rmNULL(h1h2)),
      byCols=byCols);
   h1 <- h1h2$h1;
   h2 <- h1h2$h2;
   if (verbose) {
      jamba::printDebug("plot_colorjam_preset(): ",
         "h1h2:");
      print(as.data.frame(h1h2));
   }

   if ("base" %in% style) {
      # basic scatterplot
      plot(NULL,
         xlim=xlim,
         ylim=ylim,
         xaxt="n",
         yaxt="n",
         xlab="Virtual hue (h2)",
         ylab="Actual hue (h1)",
         ...)
      axis_at <- seq(from=0, to=360, by=30);
      axis(1, at=axis_at);
      axis(2, las=2, at=axis_at);
      rect(xleft=0,
         xright=360,
         ybottom=0,
         ytop=360,
         lty=1,
         border="grey35",
         col="grey95")
   } else if ("plotly" %in% style) {
      # prepare empty plotting region
      presetly <- plotly::plot_ly(
            mode="markers",
            type="scatter")
      # add shapes which can be edited
      axis_at <- seq(from=0, to=360, by=30);
      presetly <- plotly::layout(p=presetly,
         # width=750,
         # height=750,
         scene=list(
            aspectratio=list(
               x=1,
               y=1)),
         title=list(
            text="Color Hue Adjustment",
            y=1.15, x=0.5, xanchor='center', yanchor='top',
            font=list(
               family=axis_fontfamily,
               size=axislabel_fontsize + 4)),
         xaxis=list(
            scaleanchor="y",
            title=list(
               text="Virtual hue (h2)",
               font=list(
                  family=axis_fontfamily,
                  size=axislabel_fontsize)),
            tickfont=list(
               family=axis_fontfamily,
               size=axis_fontsize),
            # range=range(axis_at) + c(-10, 10),
            tick0=0,
            dtick=30),
         yaxis=list(
            scaleanchor="x",
            title=list(
               text="Actual hue (h1)",
               font=list(
                  family=axis_fontfamily,
                  size=axislabel_fontsize)),
            tickfont=list(
               family=axis_fontfamily,
               size=axis_fontsize),
            # range=range(axis_at) + c(-10, 10),
            tick0=0,
            dtick=30),
         shapes=lapply(seq_along(h1), function(i){
            list(
               type="circle",
               showlegend=FALSE,
               name=paste0("control", i),
               layer="above",
               xanchor=(h2[i]),
               yanchor=(h1[i]),
               # grey points with darker outline
               fillcolor="rgba(240, 240, 240, 0.6)",
               line=list(color="rgba(80, 80, 80, 1)"),
               # 4-pixel radius
               x0=-10, x1=10,
               y0=-10, y1=10,
               xsizemode="pixel",
               ysizemode="pixel")
         })
      );
      # enable editing only for shape positioning
      presetly <- plotly::config(p=presetly,
         displayModeBar=FALSE,
         edits=list(
            shapePosition=TRUE))
   }

   # check for, and indicate duplicates
   h1h2df <- data.frame(
      h1=round(h1, digits=digits) %% 360,
      h2=round(h2, digits=digits) %% 360)
   if (any(duplicated(h1h2df$h2))) {
      h2_duped <- unique(h1h2df$h2[duplicated(h1h2df$h2)])
      for (h2_dupe in h2_duped) {
         h2_which <- which(h1h2df$h2 %in% h2_dupe)
         h2_diff <- diff(h2_which)
         if ("base" %in% style) {
            lines(
               x=h1h2df$h2[h2_which],
               y=h1h2df$h1[h2_which],
               lwd=1,
               lty=1,
               col="grey30")
         } else if ("plotly" %in% style) {
            presetly <- plotly::add_trace(p=presetly,
               data=h1h2df[h2_which, , drop=FALSE],
               x=~h2,
               y=~h1,
               showlegend=FALSE,
               line=list(
                  color="rgba(80, 80, 80, 1)"),
               mode="lines")
         }
      }
   }

   # draw simulated points
   if (length(pt_pch) == 1 && nchar(pt_pch) > 0) {
      # sequence of virtual hues to display
      hseq <- seq(from=0, to=359.5, by=pt_step);
      h1new <- approx_degrees(h1=h2,
         h2=h1,
         direction=head(h1h2$direction, 1),
         h=hseq);
      # use HSL to approximate most saturated color per hue
      hseq_colors <- vibrant_color_by_hue(h1new);

      if ("base" %in% style) {
         points(x=hseq,
            y=h1new,
            pch=pt_pch,
            cex=pt_cex,
            col=hseq_colors);
            # col="blue")
      } else if ("plotly" %in% style) {
         # add scaled points
         h1h2new <- data.frame(x=hseq,
            y=h1new,
            color=pt_col)
         presetly <- plotly::add_trace(p=presetly,
            data=h1h2new,
            x=~x,
            y=~y,
            # marker=list(color=h1h2new$color),
            marker=list(color=hseq_colors),
            showlegend=FALSE,
            mode="markers")
      }
   }

   # optionally colorize the axis borders
   if (TRUE %in% colorize_borders) {
      hseq <- seq(from=0, to=359.5, by=1);
      hseq_colors <- vibrant_color_by_hue(hseq);
      h1new <- approx_degrees(h1=h2,
         h2=h1,
         direction=head(h1h2$direction, 1),
         h=hseq);
      h1new_colors <- vibrant_color_by_hue(h1new);
      # use HSL to approximate most saturated color per hue
      if ("base" %in% style) {
         # x-axis
         rect(
            xleft=rep(hseq, 2),
            xright=rep(hseq + 1, 2),
            ybottom=rep(c(-10, 362), each=length(hseq)),
            ytop=rep(c(-2, 370), each=length(hseq)),
            col=h1new_colors,
            border=NA)
         # y-axis
         rect(
            xleft=rep(c(-10, 362), each=length(hseq)),
            xright=rep(c(-2, 370), each=length(hseq)),
            ybottom=rep(hseq, 2),
            ytop=rep(hseq + 1, 2),
            col=hseq_colors,
            border=NA)
      } else if ("plotly" %in% style) {
         # x-axis
         h1h2axis <- data.frame(
            x=c(rep(hseq + 0.5, 2),
               rep(c(-2, 362), each=length(hseq))),
            y=c(rep(c(-2, 362), each=length(hseq)),
               rep(hseq + 0.5, 2)),
            color=c(rep(h1new_colors, 2),
               rep(hseq_colors, 2)))
         presetly <- plotly::add_trace(
            p=presetly,
            data=h1h2axis,
            x=~x,
            y=~y,
            marker=list(color=h1h2axis$color),
            showlegend=FALSE,
            symbol=2,
            mode="markers")
      }
   }


   # draw control points
   if ("base" %in% style) {
      # point open circles (unlabeled)
      points(
         x=h2,
         y=h1,
         pch=21,
         bg=col,
         cex=cex * 3)
      # point labels
      points(
         x=h2,
         y=h1,
         cex=cex,
         pch=as.character(seq_along(h1)))
   } else if ("plotly" %in% style) {
      return(presetly);
   }

}

#' Get vibrant reference color for a given HCL color hue
#'
#' Get vibrant reference color for a given HCL color hue
#'
#' This function converts each `h` HCL hue to a hex color using
#' `jamba::hcl2col()`. This hex color is converted to HSL color
#' space using `jamba::col2hsl()`, so the color saturation and
#' lightness can be adjusted to full saturation without
#' going outside the HCL color gamut. This HSL color is converted
#' back to hex color format.
#'
#' The reason for the two-step conversion is to honor the HCL color
#' hue, which is not the same numeric value as used by the HSL
#' color space.
#'
#' @family colorjam assignment
#'
#' @returns `character` vector of colors for each hue `h`.
#'
#' @param h `numeric` color hue in the range `c(0, 360)`.
#' @param C,S,L `numeric` values used during the color conversion:
#'    * `C` is the intermediate HCL color chroma
#'    * `S` is the HSL color saturation
#'    * `L` is the HSL lightness
#' @param ... additional arguments are ignored.
#'
#' @export
vibrant_color_by_hue <- function
(h,
 C=60,
 S=100,
 L=50)
{
   # get "best" HSL color for the given HCL color hue
   # (Note that HSL and HCL do not use the same Hue values)
   hsl_seq <- round(jamba::col2hsl(
      jamba::hcl2col(
         H=h,
         C=C,
         L=70)))["H",];
   hsl_hex_colors <- jamba::hsl2col(H=hsl_seq,
      S=S,
      L=L)
   return(hsl_hex_colors);
}

