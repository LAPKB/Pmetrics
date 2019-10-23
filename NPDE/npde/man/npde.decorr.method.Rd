\name{npde.decorr.method}
\alias{npde.decorr.method}
\title{Method used to decorrelate vectors}
\description{
  Specifies the method used to decorrelate observed and simulated data
}
\value{ Decorrelation requires computing the square root of the inverse of the individual variance-covariance matrix Vi. The following methods are available in the npde library:
\describe{
  \item{cholesky}{decorrelation is performed through the Cholesky decomposition (default)}
  \item{inverse}{decorrelation is performed by inverting Vi through the \code{eigen} function}
  \item{polar}{the singular-value decomposition (\code{svd}) is used}
}
}
\details{More details can be found in the PDF documentation.
}
\references{K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.}
\keyword{methods}
