\name{plotMeans}
\alias{plotMeans}
\title{Plot Means for One or Two-Way Layout}

\description{
Plots cell means for a numeric variable in each category of a factor or in each
combination of categories of two factors, optionally along with error bars based
on cell standard errors or standard deviations.
}

\usage{
plotMeans(response, factor1, factor2,
    error.bars = c("se", "sd", "conf.int", "none"), level=0.95,
    xlab = deparse(substitute(factor1)),
    ylab = paste("mean of", deparse(substitute(response))), 
    legend.lab = deparse(substitute(factor2)), main = "Plot of Means", 
    pch = 1:n.levs.2, lty = 1:n.levs.2, col = palette(), ...)
}

\arguments{
  \item{response}{Numeric variable for which means are to be computed.}
  \item{factor1}{Factor defining horizontal axis of the plot.}
  \item{factor2}{If present, factor defining profiles of means}
  \item{error.bars}{If \code{"se"}, the default, error bars around means
    give plus or minus one standard error of the mean; if \code{"sd"}, error
    bars give plus or minus one standard deviation; if \code{"conf.int"}, error
    bars give a confidence interval around each mean; if \code{"none"}, error
    bars are suppressed.}
  \item{level}{level of confidence for confidence intervals; default is .95}
  \item{xlab}{Label for horizontal axis.}
  \item{ylab}{Label for vertical axis.}
  \item{legend.lab}{Label for legend.}
  \item{main}{Label for the graph.}
  \item{pch}{Plotting characters for profiles of means.}
  \item{lty}{Line types for profiles of means.}
  \item{col}{Colours for profiles of means}
  \item{\ldots}{arguments to be passed to \code{plot}.}
}

\value{
  The function invisibly returns \code{NULL}.
}

\examples{
with(Moore, plotMeans(conformity, fcategory, partner.status, ylim=c(0, 25)))
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[stats]{interaction.plot}}}

\keyword{hplot}
