\name{npdeData}
\alias{npdeData}
\title{Creates a NpdeData object}
\usage{
  npdeData(name.data,header=TRUE,sep="",na.strings=c(".","NA"),name.group,
    name.predictor,name.response,
    name.covariates,name.cens,name.miss,name.ipred,
    units=list(x="",y="",covariates=c()),detect=TRUE,verbose=FALSE)
}
\arguments{
  \item{name.data}{name of the file containing the observed
  data, or a dataframe containing the observed data}

  \item{sep}{field separator (for files on disk)}

  \item{na.strings}{strings to be considered as indicating
  NA}

  \item{header}{boolean indicating whether the file has a
  header (mandatory if detect is TRUE)}

  \item{name.group}{name/number of the column in the
  observed data containing the patient ID (if missing and
  detect is TRUE, columns named id, subject or sujet
  (regardless of case) will be assumed to contain this
  information)}

  \item{name.predictor}{name/number of the column in the
  observed data containing the independent variable X (if
  missing and detect is TRUE, columns named xobs, time,
  dose, x, temps, tim (regardless of case) will be assumed
  to contain this information)}

  \item{name.response}{name/number of the column in the
  observed data containing the dependent variable Y (if
  missing and detect is TRUE, columns named yobs, response,
  resp, conc, concentration (regardless of case) will be
  assumed to contain this information)}

  \item{name.miss}{name/number of the column containing
  information about missing data (MDV) (if missing and
  detect is TRUE, column called mdv or miss (regardless of
  case) will be assumed to contain this information)}

  \item{name.cens}{name/number of the column containing
  information about censored data (cens) (if missing and
  detect is TRUE, column with a name containing cens
  (regardless of case) will be assumed to contain this
  information)}

  \item{name.covariates}{name/number of the column(s)
  containing covariate information (optional)}

  \item{name.ipred}{name/number of the column(s) with
  individual predictions (ipred) (if missing and detect is
  TRUE, column with a name containing ipred (regardless of
  case) will be assumed to contain this information)}

  \item{units}{a list with components x, y and cov
  (optional), specifying the units respectively for the
  predictor (x), the response (y), and the covariates (a
  vector of length equal to the number of covariates).
  Units will default to (-) if not given.}

  \item{detect}{a boolean controlling whether automatic
  recognition of columns in the dataset is on, defaults to
  TRUE}

  \item{verbose}{whether to print warning messages,
  defaults to FALSE (set to TRUE to check how data is being
  handled)}
}
\value{
  an object of class NpdeData
}
\description{
  This function is used to create a NpdeData object,
  representing a longitudinal data structure, and fill it
  with data from a dataframe or a file on disk
}
\examples{
data(theopp)

x<-npdeData(theopp) # Automatic detection
print(x)
x<-npdeData(theopp,name.group="ID",name.predictor="Time",name.response="Conc",
name.covariates=c("Wt"),units=list(x="hr",y="mg/L",covariates="kg")) # Explicit
print(x)
plot(x)
}
\author{
  Emmanuelle Comets <emmanuelle.comets@bichat.inserm.fr>
}
\references{
  K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
  Mentre. Metrics for external model evaluation with an
  application to the population pharmacokinetics of
  gliclazide. \emph{Pharmaceutical Research}, 23:2036--49,
  2006.
}
\seealso{
  \code{\link{npde}}, \code{\link{autonpde}}
}
\keyword{models}

