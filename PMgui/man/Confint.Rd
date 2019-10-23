\name{Confint}
\alias{Confint}
\alias{Confint.glm}

\title{Confidence Intervals for Model Coefficients}

\description{
  Except for glm objects, where a method is provided that provides intervals optionally
  based on the Wald statistic, this generic function simply calls \code{\link[stats]{confint}}
  in the \code{stats} package via its default method.
}

\usage{
Confint(object, parm, level = 0.95, ...)

\method{Confint}{glm}(object, parm, level=0.95, type=c("LR", "Wald"), ...)
}

\arguments{
  \item{object}{a model object.}
  \item{parm}{which parameters to use, defaults to all.}
  \item{level}{level of confidence, defaulting to 0.95.}
  \item{type}{for a glm object, confidence interval based on the profile likelihood
    (the default) or the Wald statistic.}
  \item{\dots}{arguments to be passed down to methods.}
}

\value{
  dependent upon the method called.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[stats]{confint}}}

\keyword{htest}
\keyword{models}

