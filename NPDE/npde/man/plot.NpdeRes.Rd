\name{plot.NpdeRes}
\alias{plot.NpdeRes}
\title{Plots a NpdeRes object}
\usage{
  \method{plot}{NpdeRes} (x, y, ...)
}
\arguments{
  \item{x}{a NpdeRes object}
}
\description{
  Plots distribution and scatterplots for the npde in a
  NpdeRes object. Users are advised to use the plot()
  function on the NpdeObject object resulting from a call
  to npde() or autonpde() instead of trying to plot only
  the results element of this object.
}
\details{
  Four graphs are produced: \describe{ \item{a
  quantile-quantile plot}{plot of the npde versus the
  corresponding quantiles of a normal distribution, with
  the line y=x overlayed.} \item{a histogram of the
  npde}{the shape of the normal distribution is also shown}
  \item{two scatterplots of the npde}{a plot of the npde
  versus the independent variable X and a plot of the npde
  versus the empirical mean of the predicted distribution;
  for these last two graphs, we plot the lines
  corresponding to y=0 and to the 5% and 95% critical value
  of the normal distribution delimiting a 90% prediction
  interval for the npde} }
}
\examples{
data(theopp)
}
\references{
  K. Brendel, E. Comets, C. Laffont, C. Laveille, and
  F.Mentre. Metrics for external model evaluation with an
  application to the population pharmacokinetics of
  gliclazide. \emph{Pharmaceutical Research}, 23:2036--49,
  2006.
}
\seealso{
  \code{\link{set.plotoptions}}
}
\keyword{internal}
\keyword{plot}

