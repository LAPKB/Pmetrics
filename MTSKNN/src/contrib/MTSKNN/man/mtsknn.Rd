\name{mtsknn}
\alias{mtsknn}
\title{Multivariate two-sample test based on k-nearest neighbors}
\description{
The function tests whether two samples share the same underlying distribution based on k-nearest-neighbors approach. 
}
\usage{
mtsknn(x,y,k)
}
\arguments{
  \item{x}{A matrix or data frame.}
  \item{y}{A matrix or data frame.}
  \item{k}{An integer.}
}
\details{
matrices or data frames x and y are the two samples to be tested. Each row consists of the coordinates of a data point. The integer k is the number of nearest neighbors to choose in the testing procedure. 
 }
\value{
A list consists of the test statistics, normalized Z score and corresponding P value.  
}
\references{
   Schilling, M. F. (1986). Multivariate two-sample tests based on nearest neighbors. \emph{J. Amer. Statist.
Assoc.}, 81 799-806.
    
   Henze, N. (1988). A multivariate two-sample test based on the number of nearest neighbor type coincidences.\emph{
Ann. Statist.}, 16 772-783.

   Chen, L. and Dou W. (2009). Robust multivariate two-sample tests based on k nearest neighbors for unbalanced designs. \emph{manuscripts}.  
}
\note{
This is appropriate for the balanced case where the two sample sizes are about the same level. For the unbalanced case where the two sample sizes deviate largely from each other, two more robust tests \emph{mtsknn.eq} and \emph{mtsknn.neq} are recommended. 
}
\author{Lisha Chen \email{lisha.chen@yale.edu}, 
Peng Dai \email{peng.dai@stonybrook.edu} and Wei Dou
\email{wei.dou@yale.edu}
}
\seealso{\code{\link{mtsknn.eq}} and \code{\link{mtsknn.neq}}} 


\examples{

## Example of two samples from the same multivariate t distribution:

n <- 100

x <- matrix(rt(2*n, df=5),n,2)

y <- matrix(rt(2*n, df=5),n,2)

mtsknn(x,y,3)

## Example of two samples from different distributions:

n <- 100

x <- matrix(rt(2*n, df=10),n,2)

y <- matrix(rnorm(2*n),n,2)


mtsknn(x,y,3)

}
\keyword{nonparametric}
\keyword{multivariate}
