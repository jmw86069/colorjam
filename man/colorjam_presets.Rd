% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorjam-presets.R
\name{colorjam_presets}
\alias{colorjam_presets}
\title{Colorjam hue warp presets}
\usage{
colorjam_presets(preset = NULL, ...)
}
\arguments{
\item{preset}{\code{NULL} to return a \code{character} vector of all recognized
preset names, or \code{character} string to return specific data associated
with a recognized preset name.}

\item{...}{additional arguments are ignored.}
}
\value{
\code{character} vector of recognized colorjam presets, or when
\code{preset} is provided, it returns a \code{list} with elements:
\itemize{
\item \code{h1}: HCL color hue indicating the actual hue to be used
when generating a color.
\item \code{h2}: HCL color hue indicating the virtual hue assigned to each
actual hue in \code{h1}.
\item \code{direction}: with \code{1} indicating forward (increasing, clockwise)
progression around the color wheel, and \code{-1} indicating reverse
(decreasing, counter-clockwise) progression around the color
wheel.
\item \code{default_step}: \code{character} string with name matching a
value returned by \code{colorjam_steps()}
\item optional attribute \code{attr(x, "description")} which may contain
a text description for each color preset
}
}
\description{
Colorjam hue warp presets
}
\details{
\code{colorjam_presets()}: list the names of available colorjam presets,
or when a preset name is provided, it returns hue warp data as a
\code{list} with names \code{"h1"} and \code{"h2"} suitable for use by
\code{h2hw()} and \code{hw2h()}.
}
\seealso{
Other colorjam hue warp: 
\code{\link{add_colorjam_preset}()},
\code{\link{add_colorjam_step}()},
\code{\link{adjust_hue_warp}()},
\code{\link{approx_degrees}()},
\code{\link{colorjam_steps}()},
\code{\link{display_degrees}()},
\code{\link{h2hw}()},
\code{\link{h2hwOptions}()},
\code{\link{hcl_to_hsl_hue}()},
\code{\link{hsl_to_hcl_hue}()},
\code{\link{hw2h}()},
\code{\link{mean_angle}()},
\code{\link{plot_colorjam_preset}()},
\code{\link{remap_colorjam_preset}()},
\code{\link{validate_colorjam_preset}()}
}
\concept{colorjam hue warp}
