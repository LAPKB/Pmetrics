\name{npde}
\alias{autonpde}
\alias{npde}
\title{Compute normalised prediction distribution errors}
\usage{
  autonpde(namobs, namsim, iid, ix, iy, imdv = 0, icens =
    0, icov = 0, iipred = 0, boolsave = TRUE, namsav =
    "output", type.graph = "eps", verbose = FALSE,
    calc.npde=TRUE, calc.pd=TRUE, decorr.method =
    "cholesky", cens.method = "cdf", units =
    list(x="",y=""), detect=FALSE, ties=TRUE)
}
\arguments{
  \item{namobs}{name of the file containing the observed
  data, or a dataframe containing the observed data (in
  both cases, the column containing the various data
  required for the computation of the pde can be set using
  the arguments \code{iid},\code{ix} and \code{iy} below)}

  \item{namsim}{name of the file containing the simulated
  data, or a dataframe containing the simulated data (the
  program will assume that subject ID are in column 1 and
  simulated Y in column 3, see User Guide)}

  \item{iid}{name/number of the column in the observed data
  containing the patient ID; if missing, the program will
  attempt to detect a column named id}

  \item{ix}{name/number of the column in the observed data
  containing the independent variable (X); ; if missing,
  the program will attempt to detect a column named X}

  \item{iy}{name/number of the column in the observed data
  containing the dependent variable (Y); if missing, the
  program will attempt to detect a column with the
  response}

  \item{imdv}{name/number of the column containing
  information about missing data (MDV), defaults to 0
  (column not present)}

  \item{icens}{name/number of the column containing
  information about censored data (cens), defaults to 0
  (column not present)}

  \item{icov}{name/number of the column(s) containing
  covariate information defaults to 0 (no covariates)}

  \item{iipred}{name/number of the column(s) with
  individual predictions (ipred), defaults to 0 (individual
  predictions not available)}

  \item{units}{a list with components x, y and cov
  (optional), specifying the units respectively for the
  predictor (x), the response (y), and the covariates (a
  vector of length equal to the number of covariates).
  Units will default to (-) if not given.}

  \item{detect}{a boolean controlling whether automatic
  recognition of columns in the dataset is on, defaults to
  FALSE}

  \item{boolsave}{a boolean (TRUE if graphs and results are
  to be saved to a file, FALSE otherwise), defaults to
  TRUE}

  \item{namsav}{name of the files to which results are to
  be saved (defaults to "output", which will produce a file
  called output.eps (if the default format of postscript is
  kept, see type.graph) for the graphs and a file called
  output.npde for the numerical results (see value)}

  \item{type.graph}{type of graph (one of
  "eps","jpeg","png","pdf"), defaults to postscript
  ("eps")}

  \item{calc.npde}{a boolean (TRUE if npde are to be
  computed, FALSE otherwise), defaults to TRUE}

  \item{calc.pd}{a boolean (TRUE if pd are to be computed,
  FALSE otherwise), defaults to TRUE}

  \item{cens.method}{a character string indicating the
  method used to handle censored data (see
  \code{\link{npde.cens.method}}) defaults to cdf}

  \item{decorr.method}{a character string indicating the
  method used to decorrelate observed and simulated data in
  the computation of npde (see
  \code{\link{npde.decorr.method}}) defaults to cholesky}

  \item{ties}{a boolean (if FALSE, the distributions of pd
  and npde are smoothed by jittering the values so that
  there are no ties), defaults to TRUE}

  \item{verbose}{a boolean (TRUE if messages are to be
  printed as each subject is processed, FALSE otherwise),
  defaults to FALSE}
}
\value{
  An object of class \code{\link{NpdeObject}}
}
\description{
  These functions compute normalised prediction
  distribution errors (npde) and optionally prediction
  discrepancies (pd). \code{npde} asks the user the name
  and structure of the files containing the data, using
  \code{pdemenu}, while \code{autonpde} takes these
  variables and others as arguments.
}
\details{
  Both functions compute the normalised prediction
  distribution errors (and/or prediction discrepancies) in
  the same way. \code{npde} is an interactive function
  whereas \code{autonpde} takes all required input as
  arguments.

  When the computation of npde fails because of numerical
  problems, error messages are printed out, then pd are
  computed instead and graphs of pd are plotted so that the
  user may evaluate why the computation failed.

  The function also prints out the characteristics of the
  distribution of the npde (mean, variance, skewness and
  kurtosis) as well as the results of the statistical tests
  applied to npde. In addition, if boolsave is TRUE, two
  files are created: \describe{ \item{results file}{the
  numerical results are saved in a file with extension
  .npde (the name of which is given by the user). The file
  contains the components id, xobs, ypred, npde, pd stored
  in columns} \item{graph file}{the graphs are saved to a
  file with the same name as the results file, and with
  extension depending on the format.} }
}
\examples{
data(theopp)
data(simtheopp)

# Calling autonpde with dataframes

x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
x

# Calling autonpde with names of files to be read from disk

write.table(theopp,"theopp.tab",quote=FALSE,row.names=FALSE)
write.table(simtheopp,"simtheopp.tab",quote=FALSE,row.names=FALSE)
x<-autonpde(namobs="theopp.tab", namsim="simtheopp.tab", iid = 1,
ix = 3, iy = 4, imdv=0, boolsave = FALSE)

head(x["results"]["res"])
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
  \code{\link{npde.graphs}}, \code{\link{gof.test}}
}
\keyword{models}

