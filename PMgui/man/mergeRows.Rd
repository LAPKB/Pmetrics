\name{mergeRows}
\Rdversion{1.1}
\alias{mergeRows}
\alias{mergeRows.data.frame}

\title{
Function to Merge Rows of Two Data Frames.
}
\description{
This function merges two data frames by combining their rows.
}
\usage{
mergeRows(X, Y, common.only = FALSE, ...)

\method{mergeRows}{data.frame}(X, Y, common.only = FALSE, ...)
}

\arguments{
  \item{X}{First data frame.}
  \item{Y}{Second data frame.}
  \item{common.only}{If \code{TRUE}, only variables (columns) common to the two
  	data frame are included in the merged data set; the default is \code{FALSE}.}
  \item{\dots}{Not used.}
}

\value{A data frame containing the rows from both input data frames.}

\author{John Fox}

\seealso{For column merges and more complex merges, see \code{\link[base]{merge}}.}

\examples{
require(car)
D1 <- Duncan[1:20,]
D2 <- Duncan[21:45,]
D <- mergeRows(D1, D2)
dim(D)
}

\keyword{manip}
