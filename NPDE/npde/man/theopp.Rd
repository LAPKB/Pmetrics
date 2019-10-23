\name{theopp}
\docType{data}
\alias{theopp}
\title{Pharmacokinetics of theophylline}
\description{
  The \code{theopp} data frame has 132 rows and 5 columns of data from
  an experiment on the pharmacokinetics of theophylline.}
\usage{theopp}
\format{
  This data frame contains the following columns:
  \describe{
    \item{ID}{
      an ordered factor with levels \code{1}, \dots, \code{12}
      identifying the subject on whom the observation was made.  The
      ordering is by Time at which the observation was made.
    }
    \item{Dose}{
      dose of theophylline administered orally to the subject (mg).
    }
    \item{Time}{
      time since drug administration when the sample was drawn (hr).
    }
    \item{Conc}{
      theophylline concentration in the sample (mg/L).
    }
    \item{Wt}{
      weight of the subject (kg).
    }
  }
}
\details{
  Boeckmann, Sheiner and Beal (1994) report data from a study by
  Dr. Robert Upton of the kinetics of the anti-asthmatic drug
  theophylline.  Twelve subjects were given oral doses of theophylline
  then serum concentrations were measured at 11 time points over the
  next 25 hours. In the present package \emph{npde}, we removed the data
  at time 0.

  These data are analyzed in Davidian and Giltinan (1995) and Pinheiro
  and Bates (2000) using a two-compartment open pharmacokinetic model.
  
  These data are also available in the library \code{datasets} under the name
  \code{Theoph} in a slightly modified format and including the data at time 0.
  Here, we use the file in the format provided in the \emph{NONMEM} installation
  path (see the User Guide for that software for details).

}
\source{
  Boeckmann, A. J., Sheiner, L. B. and Beal, S. L. (1994), \emph{NONMEM
    Users Guide: Part V}, NONMEM Project Group, University of
  California, San Francisco.
    
  Davidian, M. and Giltinan, D. M. (1995) \emph{Nonlinear Models for
    Repeated Measurement Data}, Chapman & Hall (section 5.5, p. 145 and
  section 6.6, p. 176)

  Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
    S and S-PLUS}, Springer (Appendix A.29)
}
\examples{
data(theopp)
str(theopp)

#Plotting the theophylline data
plot(Conc~Time,data=theopp,xlab="Time after dose (hr)",
ylab="Theophylline concentration (mg/L)")

}
\keyword{datasets}

