\name{hierarchicalCluster}
\alias{hierarchicalCluster}

\title{Rcmdr Hierarchical Clustering Dialog}
\description{This dialog is used to specify a hierarchical cluster analysis 
solution using \code{\link{hclust}}, with the distance matrix calculated using
\code{\link[stats]{dist}}.}

\details{
  Enter a name for the hierarchical clustering solution to be created if you 
  want to retain more than one solution. The solution name must be a valid R 
  object name (consisting only of upper- and lower-case letters, numerials, 
  and periods, and not starting with a number).

  Select the variables to be included in the solution using the variable 
  selection box on the left side of the dialog box. A non-contiguous set of 
  variables can be selected by pressing your control key (ctrl) while 
  selecting variables.

  Specifying a subset expression (the field below the variable selection box) 
  allows you to obtain a clustering solution for a subset of observations in 
  the active data set. For example, assuming that gener is a variable in the 
  active data set, entering \code{gender == "Male"} would restrict the 
  solution to males.

  Select a clustering method and a distance measure if you are working with 
  raw data. There is often a relationship between the selection of these two 
  items. For example, squared-euclidian distance is appropriate for Ward's 
  method of cluster analysis. If your data \emph{is} a distance matrix, then 
  select "No Transformation" as the distance measure.

  The "Plot Dendrogram" option results in the dendrogram of the solution 
  being display by using the \code{plot} function.
}

\author{Dan Putler}

\seealso{\code{\link[stats]{hclust}}, \code{\link[stats]{dist}}}

\keyword{misc}
