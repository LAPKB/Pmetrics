\name{bin.var}
\alias{bin.var}

\title{Bin a Numeric Varisible}

\description{
  Create a factor dissecting the range of a numeric variable into
  bins of equal width, (roughly) equal frequency, or at "natural"
  cut points. The \code{\link[base]{cut}} function is used to create the factor.
}

\usage{
bin.var(x, bins = 4, method = c("intervals", "proportions", "natural"),
    labels = FALSE)
}

\arguments{
  \item{x}{numeric variable to be binned.}
  \item{bins}{number of bins.}
  \item{method}{one of \code{"intervals"} for equal-width bins;
    \code{"proportions"} for equal-count bins;
    \code{"natural"} for cut points between bins to be determined
    by a k-means clustering.}
  \item{labels}{if \code{FALSE}, numeric labels will be used for the factor levels;
    if \code{NULL}, the cut points are used to define labels; otherwise a character
    vector of level names.}
}

\value{
  A factor.
}

\author{Dan Putler, slightly modified by John Fox \email{jfox@mcmaster.ca}
    with the original author's permission.}

\seealso{\code{\link[base]{cut}}, \code{\link[stats]{kmeans}}.}

\examples{
summary(bin.var(rnorm(100), method="prop", labels=letters[1:4]))
}
  
\keyword{manip}
