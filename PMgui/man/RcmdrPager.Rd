\name{RcmdrPager}
\alias{RcmdrPager}

\title{Pager for Text Files}
\description{
  This is a slightly modified version of the tkpager, changed to use the Rcmdr monospaced font
  and a white background.
}
\usage{
RcmdrPager(file, header, title, delete.file)
}

\arguments{
  \item{file}{character vector of file(s) to be displayed.}
  \item{header}{for the beginning of each file.}
  \item{title}{for window}
  \item{delete.file}{delete file(s) on close.}
}

\seealso{\code{\link[tcltk]{tkpager}}}

\keyword{misc}
