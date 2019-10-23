\name{KMeans}
\alias{KMeans}

\title{K-Means Clustering Using Multiple Random Seeds}
\description{
  Finds a number of k-means clusting solutions using R's \code{kmeans} function,
  and selects as the final solution the one that has the minimum total
  within-cluster sum of squared distances.
}
\usage{
KMeans(x, centers, iter.max=10, num.seeds=10)
}

\arguments{
  \item{x}{A numeric matrix of data, or an object that can be coerced to such a
  matrix (such as a numeric vector or a dataframe with all numeric columns).}
  \item{centers}{The number of clusters in the solution.}
  \item{iter.max}{The maximum number of iterations allowed.}
  \item{num.seeds}{The number of different starting random seeds to use. Each
  random seed results in a different k-means solution.}
}

\value{
  A list with components:
  \item{cluster}{A vector of integers indicating the cluster to which each 
  point is allocated.}
  \item{centers}{A matrix of cluster centres (centroids).}
  \item{withinss}{The within-cluster sum of squares for each cluster.}
  \item{tot.withinss}{The within-cluster sum of squares summed across clusters.}
  \item{betweenss}{The between-cluster sum of squared distances.}
  \item{size}{The number of points in each cluster.}
}

\author{Dan Putler}

\seealso{\code{\link[stats]{kmeans}}}

\examples{
  data(USArrests)
  KMeans(USArrests, centers=3, iter.max=5, num.seeds=5)
}
\keyword{misc}
