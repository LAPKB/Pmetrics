\name{Compute}
\alias{Compute}

\title{Rcmdr Compute Dialog}
\description{
  The compute dialog is used to compute new variables.
}

\details{
The name of the new variable must be
a valid R object name (consisting only of upper and lower-case letters, numerals, and
periods, and not starting with a numeral).

Enter an R expression in the box at the right. The expression is evaluated using the active data set. 
You can double-click in the variable-list box to enter variable names in the expression.  The expression
must evaluate to a valid variable, which is added to the active data set.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[base]{Arithmetic}}}

\keyword{manip}
