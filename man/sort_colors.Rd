% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorjam-subset.R
\name{sort_colors}
\alias{sort_colors}
\title{Sort a vector of colors using one or more color attributes}
\usage{
sort_colors(
  x,
  ...,
  alpha = TRUE,
  byCols = c("H", "C", "L"),
  return_type = c("colors", "df")
)
}
\arguments{
\item{x}{\code{character} vector of R colors}

\item{...}{any valid criteria to subset the color \code{data.frame}.}

\item{alpha}{\code{logical} indicating whether to retain \code{alpha}
transparency in intermediate operations. When \code{alpha=FALSE} then
all color transparency is ignored, and colors will be returned
with no transparency.}

\item{byCols}{\code{character} vector of colnames to sort after the subset
operation.}

\item{return_type}{\code{character} string to define the return object,
where \code{return_type="colors"} returns a \code{character} vector of colors,
and \code{return_type="df"} returns a \code{data.frame} with the additional
subset colnames included for review.}
}
\value{
\code{character} vector of colors that meet the filter criteria.
When \code{return_type="df"} the returned object is a \code{data.frame} with
the subset columns included for review.
}
\description{
Sort a vector of colors using one or more color attributes
}
\details{
This function is an extension of \code{subset_colors()} except that
it applies a sort order to the results.

The input colors in \code{x} are converted internally to a \code{data.frame}
with colnames added by relevant helper functions.
\itemize{
\item \code{"num"} contains the integer index of the input vector \code{x}.
\item \code{"hex"} contains \code{character} values with hexadecimal colors
including alpha. If the input \code{x} is \code{"red"} then the hex value
will be converted to \code{"#FF0000FF"}.
}
}
\section{Added by \code{jamba::col2hcl()}}{
\itemize{
\item \code{"H"} contains color hue as values from \code{0} to \code{360}.
\item \code{"C"} contains color chroma (aka saturation) ranging from \code{0} up to \code{200},
where typical "full saturation" is represented as values above 100.
\item \code{"L"} contains color luminance (brightness/lightness) ranging from
\code{0} to \code{100}.
\item \code{"alpha"} the alpha transparency, ranging from \code{0} (fully transparent)
to \code{1} (fully opaque, not transparent).
}
}

\section{Added by \code{grDevices::col2rgb()}}{
\itemize{
\item \code{"red"} contains the red color channel, values range from \code{0} to \code{255}.
\item \code{"green"} contains the green color channel, values range from \code{0} to \code{255}.
\item \code{"blue"} contains the blue color channel, values range from \code{0} to \code{255}.
}
}

\section{Added by \code{jamba::col2hsv()}}{
\itemize{
\item \code{"h"} contains color hue as values from \code{0} to \code{1}. Note these values
may not map directly to color hue obtained from \code{jamba::col2hcl()}.
\item \code{"s"} contains color saturation with values from \code{0} to \code{1}.
\item \code{"v"} contains color vibrance (brightness/lightness) with values
from \code{0} to \code{1}.
}
}

\examples{
# subset for blue colors
jamba::showColors(subset_colors(colors(), H > 200 & H < 265 & C > 80))

# subset for saturated colors
jamba::showColors(subset_colors(colors(), C > 120))

# subset for saturated colors then sort by hue
jamba::showColors(subset_colors(colors(), C > 120, byCols=c("H", "-C", "-L")))

# review the data.frame itself
subset_colors(colors(), C > 135, return_type="df")

# for curiosity, compare H to h
colors_df <- subset_colors(colors(),
   C > 20,
   byCols=c("C"),
   return_type="df");
plot(colors_df$h, colors_df$H,
   xlab="hsv hue h",
   ylab="HCL hue H",
   pch=20,
   cex=colors_df$s * 1 + 1,
   col=colors_df$hex);
title("Comparison of HCL hue H\nwith hsv hue h")

plot(colors_df$s, colors_df$C,
   xlab="hsv saturation s",
   ylab="HCL chroma C",
   pch=20,
   cex=colors_df$s * 1 + 1,
   col=colors_df$hex);
title("Comparison of HCL C\nwith hsv s")

plot(colors_df$v, colors_df$L,
   xlab="hsv vibrance v",
   ylab="HCL luminance L",
   pch=20,
   cex=colors_df$s * 1 + 1,
   col=colors_df$hex);
title("Comparison of HCL L\nwith hsv v")

}
\seealso{
Other colorjam sort: 
\code{\link{colors_to_df}()},
\code{\link{subset_colors}()}

Other colorjam core: 
\code{\link{blend_colors}()},
\code{\link{closestRcolor}()},
\code{\link{color_complement}()},
\code{\link{colors_to_df}()},
\code{\link{group2colors}()},
\code{\link{rainbowJam}()},
\code{\link{subset_colors}()}
}
\concept{colorjam core}
\concept{colorjam sort}
