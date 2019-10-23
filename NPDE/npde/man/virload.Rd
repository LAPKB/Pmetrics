\name{virload}
\docType{data}
\alias{virload}
\alias{virload20}
\alias{virload50}
\title{Simulated HIV viral loads in HIV patients}
\description{
   This is simulated data, based on real data obtained in a phase II clinical trial supported by the French Agency for AIDS Research, the COPHAR 3-ANRS 134 trial (Goujard et al., 2010). The original study included 35 patients, who received a once daily dose containing atazanavir (300 mg), ritonavir (100 mg), tenofovir disoproxil (245 mg) and emtricitabine (200 mg) during 24 weeks. Viral loads were measured 6 times over a treatment period of 24 weeks (day 0, 28, 56, 84, 112, 168). 

   The datasets were generated in a simulation study designed to evaluate the new method proposed to handle BQL data (Nguyen et al., 2011). Data was simulated using a simple bi-exponential HIV dynamic model describing the two-phase decline of viral load during anti-retroviral treatment.

  The \code{virload} data frame has 300 rows and 4 columns of data. The dataset was then censored at two different LOQ levels (LOQ=20 or 50~copies/mL) to generate two datasets containing different proportions of BQL data, creating the data frames \code{virload20} and\code{virload50} respectively.}
\usage{virload}
\format{
  This data frame contains the following columns:
  \describe{
    \item{ID}{
      an ordered factor with levels \code{1}, \dots, \code{50}
      identifying the subject on whom the observation was made.  The
      ordering is by Time at which the observation was made.
    }
    \item{Time}{
      time since the beginning of the study (days).
    }
    \item{Log_VL}{
      logarithm (base 10) of the viral load (copies/L).
    }
    \item{cens}{
      indicator variable (cens=1 for censored data, cens=0 for observed data)
    }
    \item{ipred}{
      individual predictions)
    }
  }
}
\source{
   Goujard, C., Barrail-Train, A., Duval, X., Nembot, G., Panhard, X., Savic, R., Descamps, D., Vrijens, B., Taburet, A., Mentre, F., and the ANRS 134 study group (2010). Virological response to atazanavir, ritonavir and tenofovir/emtricitabine: relation to individual pharmacokinetic parameters and adherence measured by medication events monitoring system (MEMS) in naive HIV-infected patients (ANRS134 trial). \emph{International AIDS Society 2010}, Abstr WEPE0094.

   Nguyen, T., Comets, E., Mentre, F. (2010). Prediction discrepancies (pd) for evaluation of
models with data under limit of quantification. \emph{20th meeting of the population approach group in Europe (PAGE), Athens, Greece}. Abstr 2182.

}
\examples{
data(virload)
str(virload)
data(virload50)

#Plotting the data
plot(Log_VL~Time,data=virload,xlab="Time (d)",ylab="Viral loads, base 10 logarithmic scale  (cp/mL)")
plot(Log_VL~Time,data=virload50,xlab="Time (d)",ylab="Viral loads, base 10 logarithmic scale  (cp/mL)")

}
\keyword{datasets}

