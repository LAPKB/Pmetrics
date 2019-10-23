\name{partial.cor}
\alias{partial.cor}

\title{Partial Correlations}
\description{
  Computes a matrix of partial correlations between each pair of variables
  controlling for the others.
}
\usage{
partial.cor(X, ...)
}

\arguments{
  \item{X}{data matrix.}
  \item{\dots}{arguments to be passed to \code{cor}.}
}

\value{
    Returns a matrix of partial correlations.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[stats]{cor}}}

\examples{
    library(car)
    data(DavisThin)
    partial.cor(DavisThin)
}
\keyword{misc}
