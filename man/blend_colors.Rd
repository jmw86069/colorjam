% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorjam-blendcolors.R
\name{blend_colors}
\alias{blend_colors}
\title{Blend multiple colors}
\usage{
blend_colors(
  x,
  preset = c("ryb", "none", "dichromat", "rgb", colorjam_presets()),
  h1 = NULL,
  h2 = NULL,
  do_plot = FALSE,
  lens = 0,
  c_weight = 0.2,
  c_floor = 12,
  apply_alpha = FALSE,
  flatten_alpha = FALSE,
  bg = NULL,
  ...
)
}
\arguments{
\item{x}{\code{character} vector of R colors in hex format, or \code{list}
of color vectors, where each vector will be independently
blended.}

\item{preset}{\code{character} value indicating the color wheel preset,
default \code{"ryb"} for red-yellow-blue paint-like color blending.
It is passed to \code{colorjam::h2hwOptions()}, and permits any value
returned by \code{colorjam_presets()}.}

\item{do_plot}{\code{logical} indicating whether to depict the color
blend operation using \code{jamba::showColors()}.}

\item{lens}{\code{numeric} value used to influence the color saturation
after averaging color wheel angles.}

\item{c_weight}{\code{numeric} value used to weight the average color
chroma (saturation) using the mean chroma values of the input
colors. When \code{c_weight=0} the chroma uses the radius returned
by the mean color wheel angle.}

\item{c_floor}{\code{numeric} value indicating the \code{C} chroma HCL value
below which a color is considered to be "grey" and unsaturated.
When this happens, the hue contribution is set to 0.001 relative
to other colors being blended. This correction is done because
every color is assigned one \code{H} hue value in HCL, even when
the \code{C} chroma (saturation) is zero, therefore these colors
effectively have no \code{H} hue.}

\item{apply_alpha}{\code{logical} default TRUE, whether to apply alpha
transparency to the output color. In other words, if the input
colors are transparent, the output will also contain transparency
when \code{apply_alpha=TRUE}.}

\item{flatten_alpha}{\code{logical} default FALSE, whether to "flatten" the
color transparency by blending with the current background color,
defined by \code{bg}.
This argument is only used when \code{apply_alpha=TRUE}.}

\item{bg}{\code{character} default NULL, used to define the default
background color, used only when \code{flatten_alpha=TRUE}.
When NULL, it checks for an open graphics device with \code{dev.list()}
and if open it open it uses \code{par("bg")}. However if no
graphics device is open, it does not call \code{par("bg")} because
that would open a new graphics device. Therefore when no graphics
device is open, NULL is converted to "white" background.}

\item{...}{additional arguments are ignored.}
}
\value{
\code{character} vector with blended color; when input \code{x}
is a \code{list} the returned vector will have length \code{length(x)}.
}
\description{
Blend multiple colors
}
\details{
This function is intended to blend two or more colors,
by default using "paint mixing" style, similar to subtractive
color mixing. It accomplishes this goal by using a red-yellow-blue
color wheel (very similar to cyan-yellow-magenta), then determines
the average color hue with appropriate loss of color saturation.

The process creates a unit vector for each color, whose length
is scaled relative to the saturation and alpha transparency,
with maximum length=1.
The average angle of these unit vectors is used as the final color
hue, and the distance from origin is used to derive the new color
chroma (similar to saturation). The end goal is for blue-yellow
to make green, blue-red to make purple, and red-yellow to make orange.
Current RGB color blending fails at least one of these criteria.

This approach enables blending more than
two colors, which is fairly unique for color functions in R.
Note that the approach, when used to blend multiple very different
colors, tends to "muddy" the output color, similar to using finger
paints. Eventually if you add enough colors, it turns "bleh".
\subsection{New transparency arguments in 0.0.30.900}{
\itemize{
\item \code{apply_alpha=TRUE} will
return a color with appropriate alpha transparency based upon the
input colors. For example, blending red with red should always
produce red. However, blending 50\% transparent red with 50\% transparent
red should produce 75\% transparent red. In effect, the redness
should build with more layers of transparent red.
\item \code{flatten_alpha=TRUE} (default is FALSE) will flatten a transparent
blended color to the background, which is useful for situations
where the alpha transparency would be ignored. In other words,
34\% transparent red would be flattened to \code{"#FFAAAAFF"} and should
appear nearly identical to \code{"#FF000057"} in R plots.
\item color wheel red-yellow-blue, subtractive color mixing
\item can blend more than two colors at once
\item accounts for transparency of individual colors
}

The basic design guide was to meet these expectations:
\itemize{
\item red + yellow = orange
\item blue + yellow = green
\item red + blue = purple
\item blue + red + yellow = some brown/gray substance
}

The input \code{x} can be a vector of colors, or a \code{list}. When
\code{x} is a \code{list} then the unique vectors are blended, returning
a vector with length \code{length(x)}.

The default additive color mixing, with red-green-blue colors
used in electronic monitors, does not meet these criteria.
(In no logical paint mixing exercise would someone expect that
mixing red and green would make yellow; or that
blue and yellow would make grey.)

In general the function performs well, with some exceptions
where the color hue angle is not well-normalized opposite
its complementary color, and therefore does not make the
expected "brownish/grey" output. Examples include
\code{blend_colors(c("yellow", "purple"))} which is closer
to blue + yellow = green, because purple is also composed
of blue with some red. Indeed, the R color hue for purple
is 283; the hue for blue is 266; the hue for red is 12 (372);
which means purple is substantially closer to blue than red.
A suitable workaround in this case is to use
\code{blend_colors(c("yellow", "deeppink4"))}.
}
}
\examples{
blend_colors(c("red", "yellow"), do_plot=TRUE)

blend_colors(c("blue", "gold"), do_plot=TRUE)

blend_colors(c("dodgerblue", "firebrick2"), do_plot=TRUE)

blend_colors(c("green", "dodgerblue"), do_plot=TRUE)

blend_colors(c("red", "gold", "blue"), do_plot=TRUE)

blend_colors(c("deeppink2", "yellow"), do_plot=TRUE)

blend_colors(c("blue4", "darkorange1"), do_plot=TRUE)

blend_colors(c("#FF000040", "#FF000080"), do_plot=TRUE)
title(main=paste0("blend identical transparent colors\n",
   "returning transparent colors\n"))

blend_colors(c("#FF000040", "#FF000080"), do_plot=TRUE, flatten_alpha=TRUE)
title(main=paste0("blend identical transparent colors\n",
   "then flatten alpha transparency\n(same visual result)"))

blend_colors(list(c("red", "yellow"), c("blue", "gold")), do_plot=FALSE)

}
\seealso{
Other colorjam core: 
\code{\link{closestRcolor}()},
\code{\link{color_complement}()},
\code{\link{colors_to_df}()},
\code{\link{group2colors}()},
\code{\link{rainbowJam}()},
\code{\link{sort_colors}()},
\code{\link{subset_colors}()}
}
\concept{colorjam core}
