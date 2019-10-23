\name{npdeControl}
\alias{check.control.options}
\alias{npdeControl}
\alias{replace.control.options}
\title{Set options for an NpdeObject}
\usage{
  npdeControl(boolsave = TRUE, namsav = "output",
    type.graph = "eps", verbose = FALSE, calc.npde = TRUE,
    calc.pd = TRUE, decorr.method = "cholesky", cens.method
    = "omit", ties = TRUE, sample = FALSE)
}
\arguments{
  \item{boolsave}{whether to save the results (a file
  containing the numerical results and a file with the
  graphs)}

  \item{namsav}{the root name of the files to save to (the
  file with the results will be named ROOTNAME.npde and the
  graphs will be saved to ROOTNAME.format where format is
  given by the type.graph argument)}

  \item{type.graph}{type of graph to save to (one of "eps",
  "pdf", "jpeg", "png")}

  \item{verbose}{a boolean; if TRUE, a message is printed
  as the computation of the npde begins for each new
  subject}

  \item{calc.pd}{a boolean; TRUE to compute pd}

  \item{calc.npde}{a boolean; TRUE to compute npde}

  \item{decorr.method}{the method used to decorrelate
  simulated and observed data (see
  \code{\link{npde.decorr.method}})}

  \item{cens.method}{the method used to handle censored
  data (see \code{\link{npde.cens.method}})}

  \item{ties}{if FALSE, a smoothing will be applied to
  prediction discrepancies to avoid ties}

  \item{sample}{if TRUE, the test on the pd will be
  performed after randomly sampling only pd per subject}
}
\description{
  Set, replace and check options for an NpdeObject
}
\keyword{methods}

