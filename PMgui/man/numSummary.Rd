\name{numSummary}
\alias{numSummary}
\alias{print.numSummary}

\title{Summary Statistics for Numeric Variables}
\description{
\code{numSummary} creates neatly formatted tables of means, standard deviations, coefficients of variation,
skewness, kurtosis, and quantiles of numeric variables.
}
\usage{
numSummary(data, 
	statistics=c("mean", "sd", "quantiles", "cv", "skewness", "kurtosis"),
	type=c("1", "2", "3"),
    quantiles=c(0, .25, .5, .75, 1), groups)
    
\method{print}{numSummary}(x, ...)
}

\arguments{
  \item{data}{a numeric vector, matrix, or data frame.}
  \item{statistics}{any of \code{"mean"}, \code{"sd"}, \code{"quantiles"}, 
  	\code{"cv"} (coefficient of variation --- sd/mean), \code{"skewness"}, or \code{"kurtosis"},
    defaulting to the first three.}
  \item{type}{definition to use in computing skewness and kurtosis; see the 
  	\code{link[e1071]{skewness}} and \code{link[e1071]{kurtosis}} functions in the \pkg{e1071}
  	package. The default is \code{"1"}.}
  \item{quantiles}{quantiles to report; default is \code{c(0, 0.25, 0.5, 0.75, 1)}.}
  \item{groups}{optional variable, typically a factor, to be used to partition the data.}
  \item{x}{object of class \code{"numSummary"} to print.}
  \item{\dots}{arguments to pass down from the print method.}
}

\value{
\code{numSummary} returns an object of class \code{"numSummary"} containing the table of 
statistics to be reported along with information on missing data, if there are any.
}

\author{John Fox \email{jfox@mcmaster.ca}}


\seealso{\code{\link[base]{mean}}, \code{\link[stats]{sd}}, \code{\link[stats]{quantile}},
	\code{link[e1071]{skewness}}, \code{link[e1071]{kurtosis}}.}

\examples{
	\dontrun{
library(car)
Prestige[1, "income"] <- NA
numSummary(Prestige[,c("income", "education")], 
	statistics=c("mean", "sd", "quantiles", "cv", "skewness", "kurtosis"))
numSummary(Prestige[,c("income", "education")], groups=Prestige$type)
remove(Prestige)
	}
}

\keyword{misc}

