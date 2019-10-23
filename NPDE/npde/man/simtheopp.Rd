\name{simtheopp}
\docType{data}
\alias{simtheopp}
\title{Simulated data for the computation of normalised prediction distribution errors}
\description{
  The \code{simtheopp} dataset contains 100 simulations using the design of
  dataset \code{\link{theopp}}. These simulations are used to compute npde. The
  control file used to perform the simulations can be found in the subdirectory
  'doc' within the library npde.}
\usage{simtheopp}
\format{
  This data frame contains the following columns:
  \describe{
    \item{ID}{
      an ordered factor with levels \code{1}, \dots, \code{12}
      identifying the subject on whom the observation was made.  The
      ordering is first by simulation then by increasing time.
    }
    \item{xsim}{
      time since drug administration when the sample was drawn (hr).
    }
    \item{ysim}{
      simulated theophylline concentration (mg/L).
    }
  }
}
\details{
  See \code{\link{theopp}} for a description of the original dataset.
  
  The simulated data was obtained using the software \emph{NONMEM}. A
  one-compartment model was fit to the data. An exponential interindividual
  variability was assumed for the three parameters (absorption rate constant ka,
  volume of distribution V and clearance CL) and a combined additive and
  proportional residual error model was usd. The estimated parameters were then
  used to simulate 100 datasets with the same structure as the original dataset.
  Thus, for each observation in the original dataset, the simulated dataset
  contains 100 simulations under the model used for the estimation.
  
  This dataset is provided so that users can figure out what type of data is
  needed for the computation of prediction distribution errors. More information
  can be found in the User Guide distributed along with this package, which
  contains a run-through of the theophylline example.
}
\source{
  Boeckmann, A. J., Sheiner, L. B. and Beal, S. L. (1994), \emph{NONMEM
    Users Guide: Part V}, NONMEM Project Group, University of
  California, San Francisco.
}
\seealso{\code{\link{theopp}}}
\examples{
data(simtheopp)

# Plotting the simulated data for subject 1 in the first simulation
plot(ysim[2:12]~xsim[2:12],data=simtheopp,xlab="Time after dose (hr)",
ylab="Theophylline concentration (mg/L)",type="l",
main="Example of simulated data for subject 1")

# Plotting a 90% prediction interval for the observations in theopp
# using the simulated data in simtheopp
# note : differences in doses between subjects are not taken into account
data(theopp)
xpl<-c(0,0.25,0.5,1,2,3.5,5,7,9,12,24)
xpl1<-list(c(0,0.1),c(0.2,0.4),c(0.5,0.65),c(0.9,1.2),c(1.9,2.2),c(3.4,4),
c(4.9,5.2),c(6.9,7.2),c(8.8,9.4),c(11.5,12.2),c(23.7,24.7))

ypl<-cbind(xpl=xpl,binf=xpl,median=xpl,bsup=xpl)
for(i in 1:(length(xpl))) {
  vec<-simtheopp$ysim[simtheopp$xsim>=xpl1[[i]][1] &simtheopp$xsim<=xpl1[[i]][2]] 
  ypl[i,2:4]<-quantile(vec,c(0.05,0.5,0.95))
}
plot(Conc~Time,data=theopp,xlab="Time after dose (hr)",
ylab="Theophylline concentration (mg/L)")
lines(ypl[,1],ypl[,3],lwd=2)
lines(ypl[,1],ypl[,2],lty=2)
lines(ypl[,1],ypl[,4],lty=2)

}
\keyword{datasets}

