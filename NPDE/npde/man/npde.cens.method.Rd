\name{npde.cens.method}
\alias{npde.cens.method}
\title{Method used to handle censored data}
\description{
  Specifies the method used to handle censored data (data below the limit of quantification LOQ)
}
\value{ The following methods are available in the npde library:
\describe{
  \item{omit}{pd and npde for censored data will be set to NA}
  \item{cdf}{for an observation ycens_ij under the LOQ, a pd_ij will be imputed in the uniform distribution [0-pLOQ_ij] where pLOQ_ij is the probability that y_ij is below LOQ, according to the model; the predictive distribution will then be used to obtain a corresponding y*_ij. This is also performed for all simulated data, and the npde are then computed on the completed dataset containing the observed y_ij for the uncensored data and the y*_ij imputed for the censored data. This method is the default.}
  \item{ipred}{an observation ycens_ij is replaced by the individual prediction according to the model (ipred, which must be present in the dataset). Simulated data are left untouched.}
  \item{ppred}{an observation ycens_ij is replaced by the population prediction according to the model. Simulated data are left untouched.}
  \item{loq}{an observation ycens_ij is replaced by the value of the LOQ. Simulated data are left untouched.}
}
}
\details{More details can be found in the PDF documentation.
}
\references{K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.}
\keyword{methods}
