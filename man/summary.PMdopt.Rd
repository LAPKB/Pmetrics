% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaryPMdopt.R
\name{summary.PMdopt}
\alias{summary.PMdopt}
\title{Summarize PMdopt objects}
\usage{
\method{summary}{PMdopt}(object, ...)
}
\arguments{
\item{object}{A PMdopt object made by \link{Dopt}.}

\item{...}{Other parameters which are not necessary.}
}
\value{
The weighted mean D-optimal times.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Summarize a Pmetrics D-optimal object
}
\details{
Summarize observations, predictions and errors in a PMdopt object made by \link{Dopt}.
}
\seealso{
\link{makeOP}
}
\author{
Michael Neely
}
