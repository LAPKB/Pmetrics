\docType{class}
\name{NpdeData-class}
\alias{NpdeData}
\alias{NpdeData-class}
\alias{[<-,NpdeData-method}
\alias{[,NpdeData-method}
\alias{print,NpdeData-method}
\alias{show,NpdeData-method}
\alias{summary,NpdeData-method}
\title{Class "NpdeData" representing the structure of the longitudinal data}
\description{
  A longitudinal data structure
}
\section{Objects from the Class}{
  NpdeData objects are typically created by calls to
  \code{\link{npdeData}} and contain the following slots:

  \describe{ \item{name.data}{character string giving the
  name of the dataset} \item{name.group}{character string
  giving the name of the grouping term (ID)}
  \item{name.predictor}{character string giving the name of
  the predictor (X)} \item{name.response}{character string
  giving the name of the response (Y)}
  \item{name.cens}{character string giving the name of the
  censoring indicator} \item{name.mdv}{character string
  giving the name of the missing data indicator}
  \item{name.covariates}{vector of character string giving
  the name(s) of the covariates}
  \item{name.ipred}{character string giving the name of the
  individual predictions} \item{units}{(optional) a list
  with the units for X, Y, and covariates} \item{data}{a
  dataframe containing the data} \item{N}{number of
  subjects} \item{ntot.obs}{total number of non-missing
  observations} \item{nind.obs}{vector of size N giving the
  number of non-missing observations for each subject}
  \item{ind}{index of non-missing observations}
  \item{icens}{index of censored observations
  (non-missing)} \item{not.miss}{a vector of boolean
  indicating for each observation whether it is missing
  (FALSE) or available (TRUE)} \item{loq}{the censoring
  value} }
}

\section{Methods}{
  \describe{ \item{npdeData(name.data):}{Create a new
  \code{\linkS4class{NpdeData}} object from dataset
  name.data} \item{print(npde.data):}{Prints a summary of
  object npde.data} \item{show(npde.data):}{Prints a short
  summary of object npde.data}
  \item{showall(npde.data):}{Prints a detailed summary of
  object npde.data} \item{plot(npde.data):}{Plots the data
  in npde.data. More details can be found in
  \code{\link{plot.NpdeData}}}
  \item{summary(npde.data):}{Returns a summary of object
  npde.data in list format}
  \item{set.plotoptions(npde.data):}{Sets options for
  graphs of npde.data (internal method used in plots)} }
}
\examples{
methods(class="NpdeData")

showClass("NpdeData")
}
\seealso{
  \code{\link{npde}}, \code{\link{autonpde}},
  \code{\link{plot.NpdeData}}
}
\keyword{classes}

