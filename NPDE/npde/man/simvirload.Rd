\name{simvirload}
\docType{data}
\alias{simvirload}
\title{Simulated data for the computation of normalised prediction distribution errors, viral load example}
\description{
  The \code{simvirload} dataset contains 1000 simulations using the design of
  dataset \code{\link{virload}}. These simulations are used to compute npde.}
\usage{simvirload}
\format{
  This data frame contains the following columns:
  \describe{
    \item{ID}{
      an ordered factor with levels \code{1}, \dots, \code{50}
      identifying the subject on whom the observation was made.  The
      ordering is first by simulation then by increasing time.
    }
    \item{xsim}{
      time (day).
    }
    \item{ysim}{
      simulated viral loads, in base 10 log-scale (cp/L).
    }
  }
}
\details{
  See \code{\link{virload}} for a description of the original dataset.
  
  The simulated data was obtained using the software \emph{R}, as described in Nguyen et al. (2011).
}
\source{
   Goujard, C., Barrail-Train, A., Duval, X., Nembot, G., Panhard, X., Savic, R., Descamps, D., Vrijens, B., Taburet, A., Mentre, F., and the ANRS 134 study group (2010). Virological response to atazanavir, ritonavir and tenofovir/emtricitabine: relation to individual pharmacokinetic parameters and adherence measured by medication events monitoring system (MEMS) in naive HIV-infected patients (ANRS134 trial). \emph{International AIDS Society 2010}, Abstr WEPE0094.

   Nguyen, T., Comets, E., Mentre, F. (2010). Prediction discrepancies (pd) for evaluation of
models with data under limit of quantification. \emph{20th meeting of the population approach group in Europe (PAGE), Athens, Greece}. Abstr 2182.
}
\seealso{\code{\link{virload}}}
\keyword{datasets}

