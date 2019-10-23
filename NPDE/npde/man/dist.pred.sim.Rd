\name{dist.pred.sim}
\alias{calcnpde.sim}
\alias{dist.pred.sim}
\title{Compute distribution of pd/npde using simulations}
\usage{
  dist.pred.sim(npdeObject,nsamp, ...)
}
\arguments{
  \item{npdeObject}{an object returned by a call to
  \code{\link{npde}} or \code{\link{autonpde}}}

  \item{nsamp}{number of datasets (defaults to 100 or to
  the number of replications if it is smaller)}

  \item{\dots}{additional arguments. Currently only the
  value of calc.pd and calc.npde may be passed on, and will
  override their corresponding value in the "options" slot
  of npdeObject}
}
\value{
  an object of class NpdeObject; the ["results"] slot will
  contain pd and/or npde for a sample of the simulated
  datasets (depending on whether calc.pd/calc.npde are ),
  stored in pd.sim and/or npde.sim
}
\description{
  This function is used to built the distribution of
  pd/npde using the simulations under the model. The
  default is to build only the distribution of pd, and to
  sample from N(0,1) when building the distribution of npde
  under the null hypothesis.
}
\examples{
data(theopp)
data(simtheopp)
x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
# Use random samples from N(0,1) to obtain a prediction interval on the empirical cdf of the npde
plot(x,plot.type="ecdf",bands=TRUE,approx.pi=TRUE)
# defaults to computing the pd and npde for 100 simulated datasets (in the theophylline example, this uses all the simulated datasets)
x<-dist.pred.sim(x)
# Use the npde from the simulated datasets to obtain a prediction interval on the empirical cdf
plot(x,plot.type="ecdf",bands=TRUE,approx.pi=FALSE)
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

