\name{reliability}
\alias{reliability}
\alias{print.reliability}

\title{Reliability of a Composite Scale}
\description{
  Calculates Cronbach's alpha and standardized alpha (lower bounds on reliability)
  for a composite (summated-rating) scale. Standardized alpha is for the sum of the
  standardized items. In addition, the function calculates alpha and standardized
  alpha for the scale with each item deleted in turn, and computes the correlation
  between each item and the sum of the other items.
}

\usage{
reliability(S)

\method{print}{reliability}(x, digits=4, ...)
}

\arguments{
  \item{S}{the covariance matrix of the items; normally, there should be at least 3 items
  and certainly no fewer than 2.}
  \item{x}{reliability object to be printed.}
  \item{digits}{number of decimal places.}
  \item{...}{not used: for compatibility with the print generic."}
}

\value{
    an object of class reliability, which normally would be printed.
}

\references{
    N. Cliff (1986) Psychological testing theory. Pp. 343--349 in
    S. Kotz and N. Johnson, eds., \emph{Encyclopedia of Statistical Sciences, Vol. 7}.
    Wiley.}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link{cov}}}

\examples{
library(car)
data(DavisThin)
reliability(cov(DavisThin))
}
\keyword{misc}
