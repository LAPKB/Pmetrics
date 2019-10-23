\name{mtsknn.eq}
\alias{mtsknn.eq}
\title{A robust multivariate two-sample test based on k-nearest neighbors against unbalanceness}
\description{
The function tests whether two samples share the same underlying distribution based on k-nearest-neighbors approach. This approach is robust in the unbalanced case. 
}
\usage{
mtsknn.eq(x,y,k,clevel=0.05,getpval=TRUE, print=TRUE)
}
\arguments{
  \item{x}{A matrix or data frame.}
  \item{y}{A matrix or data frame.}
  \item{k}{An integer.}
  \item{clevel}{The confidence level. Default value is 0.05.}
  \item{getpval}{Logic value. If it is set to be TRUE the p value of test will be calcuated and reported; if it is set to be false the p value will not be calculated.}
  \item{print}{Logic value. If it is set to be TRUE the test result will be reported; if it is set to be false the test result will not be reported.}
}
\details{
matrices or data frames x and y are the two samples to be tested. Each row consists of the coordinates of a data point. The integer k is the number of nearest neighbors to choose in the testing procedure. 
 }
\value{
The test result for a given confidence level. Reject or accept the null hypothesis. It can also calculate and report the p value. 
}
\references{
   Schilling, M. F. (1986). Multivariate two-sample tests based on nearest neighbors. \emph{J. Amer. Statist.
Assoc.}, 81 799-806.
    
   Henze, N. (1988). A multivariate two-sample test based on the number of nearest neighbor type coincidences.\emph{
Ann. Statist.}, 16 772-783.

   Chen, L. and Dou W. (2009). Robust multivariate two-sample tests based on k nearest neighbors for unbalanced designs. \emph{manuscripts}.  
}
\note{
This is appropriate for the unbalanced case where the two sample sizes are about the same level. Another robust test is\emph{mtsknn.neq}. 
}
\author{Lisha Chen\email{lisha.chen@yale.edu}, 
Peng Dai\email{peng.dai@yale.edu} and Wei Dou
\email{wei.dou@yale.edu}
}
\seealso{\code{\link{mtsknn}}, \code{\link{mtsknn.neq}} and \code{\link{mtsknn.discard}}}


\examples{

## Example of two samples from the same multivariate t distribution:

n <- 100

x <- matrix(rt(2*n, df=5),n,2)

y <- matrix(rt(2*10*n, df=5),(10*n),2)

mtsknn.eq(x,y,3)

## Example of two samples from different distributions:

n <- 100

x <- matrix(rt(2*n, df=10),n,2)

y <- matrix(rnorm(2*10*n),(10*n),2)


mtsknn.eq(x,y,3)

}
\keyword{nonparametric}
\keyword{robust}
\keyword{multivariate}
