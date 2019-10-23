\name{npde-package}
\alias{npde-package}
\docType{package}
\title{Normalised prediction distribution errors for nonlinear mixed-effect models}
\description{Routines to compute normalised prediction distribution errors, a metric designed to evaluate non-linear mixed effect models such as those used in pharmacokinetics and pharmacodynamics}
\details{
\tabular{ll}{
Package: \tab npde\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2012-08-15\cr
License: \tab GPL version 2 or later\cr
}
  See the documentation for npde or autonpde for details. A comprehensive user manual is provided in the inst directory of the package, along with a document illustrating the different graphs and graphical options. Please refer to these two guides for details, and send all comments and bug reports to Emmanuelle Comets (<emmanuelle.comets@bichat.inserm.fr>).
}
\author{
Emmanuelle Comets, Karl Brendel and France Mentre

Maintainer: Emmanuelle Comets <emmanuelle.comets@bichat.inserm.fr>
}
\references{
  K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.

  E. Comets, K. Brendel, and F. Mentre. Computing normalised prediction distribution errors to evaluate nonlinear mixed-effect models: the npde add-on package for R. \emph{Computer Methods and Programs in Biomedicine},90:154--66, 2008.

  E. Comets, K. Brendel, and F. Mentre. Model evaluation in nonlinear mixed effect models, with applications to pharmacokinetics. \emph{Journal de la Societe Francaise de Statistique},151:106--28, 2010.

  T.H. Nguyen, E. Comets, and F. Mentre. Extension of NPDE for evaluation of nonlinear mixed effect models in presence of data below the quantification limit with applications to HIV dynamic model. \emph{Journal of Pharmacokinetics and Pharmacodynamics}, in press, 2012.}

\keyword{models}
\examples{
data(theopp)
data(simtheopp)

# Calling autonpde with dataframes

# x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
# x$npde

}
