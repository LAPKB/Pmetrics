\name{CFA}
\alias{CFA}

\title{Rcmdr Confirmatory Factor Analysis Dialog}

\description{
The CFA dialog is used to create and fit a confirmatory factor analysis model via the
\code{\link[sem]{cfa}} and \code{\link[sem]{sem}} functions in the \pkg{sem} package.
}

\details{
Select two or more variables for each factor by \emph{Control}-clicking on their names in the variable-list box. Optionally
give the factor a name; this must be a valid R name. Then press the \emph{Define factor} button. 

Continue in this manner until all factors are specified. 
Note that if there are not at least two unique variables selected for each factor, the
model will probably be underidentified, causing \code{sem} to fail.

The radio buttons at the top of the dialog may be used to analyze either the correlation matrix or covariance matrix of
the observed variables; to specify either correlated or orthogonal factors; and to identify the model either by setting
the factor variance to 1 or by setting the first loading for each factor to 1 (establishing a ``reference indicator'' for
the factor). A check box is provided to compute robust standard errors and tests.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[sem]{sem}}, \code{\link[sem]{cfa}}}

\keyword{models}