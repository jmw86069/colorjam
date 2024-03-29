% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorjam-shiny.R
\name{colorjamShinyServer}
\alias{colorjamShinyServer}
\title{Colorjam R-shiny app server}
\usage{
colorjamShinyServer(input, output, session)
}
\arguments{
\item{input}{provided by shiny}

\item{output}{provided by shiny}

\item{session}{provided by shiny}
}
\description{
Colorjam R-shiny app server
}
\details{
This function contains the server logic for the Colorjam R-shiny app.

The Colorjam R-shiny app is started by \code{launchColorjamShiny()}, which
calls \code{shiny::shinyApp()}, using arguments \code{server}, \code{ui},
\code{onStart}, and \code{options}. This function fulfills the
argument \code{ui}.
}
\seealso{
Other colorjam R-shiny functions: 
\code{\link{launchColorjamShiny}()}
}
\concept{colorjam R-shiny functions}
