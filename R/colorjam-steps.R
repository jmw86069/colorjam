

# define default environment to store colorjam step sequences
# Note: Some newer steps used `jamba::normScale()` to adjust
# the step sequence within a fixed range.
.colorjam_steps <- new.env();
.colorjam_steps[["v19"]] <- list(
   C=c(140, 150, 160, 130, 200, 100),
   L=c( 47,  85,  62,  42,  77,  54))
.colorjam_steps[["v20"]] <- list(
   C=c(200, 120, 160,  90, 180, 150),
   L=c( 44,  88,  74,  58,  80,  65))
.colorjam_steps[["v23"]] <- list(
   # C used v20 values scaled between 70 and 110
   C=c(110, 81, 95, 70, 103, 92),
   # L used v20 values scaled between 45 and 85
   L=c(45, 85, 72, 58, 78, 64))
.colorjam_steps[["v24"]] <- list(
   C=c(88, 110, 99,  80, 105, 97),
   # L values intended for gold-first sequence
   # slightly adjusted for each sequence of 6 steps
   L=c(
      c(80, 51, 65, 58, 76, 62),
      c(84, 55, 71, 61, 82, 66),
      c(90, 45, 61, 53, 71, 58)))

# .colorjam_steps[["v24a"]] <- list(
#    # C values intended for gold-first sequence
#    # scaled between 80 to 100
#    C=c(110, 88, 99,  80, 105, 97)[c(2,1,3,4,5,6)],
#    # L values intended for gold-first sequence
#    # slightly adjusted for each sequence of 6 steps:
#    # `+ rep(c(0, 5, -10), each=6)`
#    # scaled between 45 and 85
#    L=c(
#       c(51, 82, 70, 60, 77, 65)[c(2,1,3,4,5,6)],
#       c(55, 85, 73, 63, 80, 68)[c(2,1,3,4,5,6)],
#       c(45, 75, 63, 53, 70, 58)[c(2,1,3,4,5,6)]))

#' Colorjam chroma/luminance steps
#'
#' Colorjam chroma/luminance steps to adjust a series of color hues
#' to visibly distinct categorical colors.
#'
#' `colorjam_steps()`: list the names of available colorjam steps,
#' or when a `step` name is provided, it returns a `list` with elements
#' `"C"` and `"L"`.
#'
#' @returns `character` vector of recognized colorjam step names when
#'    `step` is `NULL`, or
#'    when `step` is defined a `list` is returned with elements
#'    * `C`: `numeric` values indicating HCL Chroma
#'    * `L`: `numeric` values indicating HCL Chroma
#'
#' @family colorjam hue warp
#'
#' @param steps `NULL` to return a `character` vector of all recognized
#'    steps, or `character` string to return specific data associated
#'    with a recognized steps name.
#' @param ... additional arguments are ignored.
#'
#' @export
colorjam_steps <- function
(step=NULL,
 ...)
{
   #
   if (length(step) == 0) {
      ls(.colorjam_steps)
   } else {
      .colorjam_steps[[step]]
   }
}


#' Add colorjam chroma/luminance step
#'
#' Add colorjam chroma/luminance steps to adjust a series of color hues
#' to visibly distinct categorical colors.
#'
#' @returns `TRUE`, invisibly.
#'
#' @family colorjam hue warp
#'
#' @param step `character` string with the step name.
#' @param step_list `list` with elements `"C"` and `"L"` which each contain
#'    a `numeric` vector with values in the range `c(0, 100)`.
#'    In future the `step_list` may permit other color channels, such
#'    as `"S"` and `"L"` for use with HSL color space, or `"S"` and `"V"`
#'    for use with HSV color space.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' colorjam_steps()
#' colorjam_steps("v24")
#'
#' Cvals <- c(110, 88, 99,  80, 105, 97)
#' Lvals <- c(51, 82, 70, 60, 77, 65)
#' add_colorjam_step("new_v24", step_list=list(C=Cvals, L=Lvals))
#' colorjam_steps()
#' colorjam_steps("new_v24")
#'
#' color_pie(rainbowJam(n=10,
#'    step="new_v24",
#'    phase=c(2,1,3,4,5,6)))
#'
#' add_colorjam_step("new_v24", step_list=NULL)
#' colorjam_steps()
#'
#' @export
add_colorjam_step <- function
(step,
 step_list,
 verbose=TRUE,
 ...)
{
   # validate input
   if (length(step_list) == 0 || all(lengths(step_list) == 0)) {
      if (step %in% ls(.colorjam_steps)) {
         # remove
         rm(list=as.character(step),
            envir=.colorjam_steps)
         if (TRUE %in% verbose) {
            cli::cli_alert_info("removed step '{.field {step}}'.")
         }
      }
   } else {
      assign(x=step,
         value=step_list,
         envir=.colorjam_steps)
      if (TRUE %in% verbose) {
         cli::cli_alert_info("added step '{.field {step}}'.")
      }
   }
   invisible(TRUE)
}

#' Plot colorjam steps
#'
#' Plot colorjam steps in simple scatterplot format
#'
#' Simple function to plot the series of Chroma/Luminance steps
#' for a given named step series. See steps with `colorjam_steps()`.
#'
#' @family colorjam hue warp
#'
#' @param step `character` name of colorjam step from `colorjam_steps()`
#' @param n `integer` number of steps to show, default NULL uses all steps.
#' @param hue `numeric` hue or `character` color to convert to color hue,
#'    used to colorize points using the actual C,L step values.
#'    * When no `hue` is supplied, the points are colored using a color
#'    gradient `"Reds"` in the order they appear.
#' @param ... additional arguments are ignored.
#'
#' @returns `list` with step values, invisibly.
#'
#' @examples
#' colorjam_steps()
#' step_name <- colorjam_steps()[1]
#' plot_colorjam_steps(step_name)
#'
#' step_name <- tail(colorjam_steps(), 1)
#' plot_colorjam_steps(step_name)
#'
#' plot_colorjam_steps(step_name, hue="orange")
#' plot_colorjam_steps(step_name, hue="gold")
#'
#' @export
plot_colorjam_steps <- function
(step,
 n=NULL,
 hue=NULL,
 lines=TRUE,
 return_type=c("ptcol", "step_data"),
 ...)
{
   #
   return_type <- match.arg(return_type);
   #
   step_data <- .colorjam_steps[[step]];
   maxlen <- max(lengths(step_data))
   if (length(n) == 1) {
      maxlen <- n
   }
   use_x <- rep(step_data[[1]], length.out=maxlen)
   use_y <- rep(step_data[[2]], length.out=maxlen)

   if (length(hue) == 1) {
      if (is.character(hue)) {
         hue <- jamba::col2hcl(hue)["H", ]
      }
      ptcol <- jamba::hcl2col(H=rep(hue, maxlen),
         C=use_x, L=use_y, alpha=1)
   } else {
      ptcol <- jamba::getColorRamp("Reds", n=maxlen);
   }
   ptype <- "p";
   if (TRUE %in% lines) {
      ptype <- "l";
   }
   plot(x=use_x,
      y=use_y,
      type=ptype,
      xlab=names(step_data)[1],
      ylab=names(step_data)[2],
      main=step,
      pch=21,
      bg=ptcol,
      cex=4,
      asp=1)
   points(x=use_x,
      y=use_y,
      pch=21,
      bg=ptcol,
      cex=4)
   text(x=use_x,
      y=use_y,
      col=jamba::setTextContrastColor(ptcol),
      label=seq_len(maxlen))

   if ("ptcol" %in% return_type) {
      return(invisible(setNames(ptcol, seq_len(maxlen))))
   }
   return(invisible(step_data));

}
