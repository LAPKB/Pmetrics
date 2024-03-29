% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeCycle.R
\name{makeCycle}
\alias{makeCycle}
\title{Summarize Pmetrics Run Cycle Information}
\usage{
makeCycle(data)
}
\arguments{
\item{data}{A suitable data object of the \emph{NPAG} or \emph{IT2B} class (see \code{\link{NPparse}} or \code{\link{ITparse}}).}
}
\value{
The output of \code{makeCycle} is a list of class \emph{PMcycle},
which has 8 objects from NPAG or 6 objects from IT2B :
\item{names }{Vector of names of the random parameters}#' \item{names }{Vector of names of the random parameters}
\item{cycnum }{Vector cycle numbers, which may start at numbers greater than 1 if a non-uniform prior was specified for the run (NPAG only)}
\item{ll }{Matrix of cycle number and -2*Log-likelihood at each cycle}
\item{gamlam }{A matrix of cycle number and gamma or lambda at each cycle}
\item{mean }{A matrix of cycle number and the mean of each random parameter at each cycle,  normalized to initial mean}
\item{sd }{A matrix of cycle number and the standard deviation of each random parameter
at each cycle,  normalized to initial standard deviation}
\item{median }{A matrix of cycle number and the median of each random parameter at each cycle,  normalized to initial median}
\item{aic }{A matrix of cycle number and Akaike Information Criterion at each cycle}
\item{bic }{A matrix of cycle number and Bayesian (Schwartz) Information Criterion at each cycle}
A plot method exists in \code{\link{plot}} for \emph{PMcycle} objects.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Parses the cycle information from an NPAG or an IT2B object.
}
\details{
This function will parse the output of \code{\link{NPparse}} or \code{\link{ITparse}} to generate a
list suitable for analysis and plotting of NPAG or IT2B cycle information.
}
\examples{
cycle <- makeCycle(NPex$NPdata)
cycle
names(cycle)
}
\seealso{
\code{\link{NPparse}}, \code{\link{ITparse}},  \code{\link{plot.PMcycle}}
}
\author{
Michael Neely
}
