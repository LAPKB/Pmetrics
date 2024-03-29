% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeValid.R
\name{make_valid}
\alias{make_valid}
\title{Create a Pmetrics validation object}
\usage{
make_valid(result, tad = F, binCov, doseC, timeC, tadC, limits, ...)
}
\arguments{
\item{result}{The result of a prior run, loaded with \link{PM_load}.}

\item{tad}{Boolean operator to use time after dose rather than time after start.  Default is \code{FALSE}.}

\item{binCov}{A character vector of the names of covariates which are included in the model, i.e. in the
model equations and which need to be binned.  For example \code{binCov='wt'} if "wt" is included in a
model equation like V=V0*wt, or \code{binCov=c( 'wt', 'crcl')} if both "wt" and "crcl"
are included in model equations.}

\item{doseC}{An integer with the number of dose/covariate bins to cluster, if known from a previous run of
this function.  Including this value will skip the clustering portion for doses/covariates.}

\item{timeC}{An integer with the number of observation time bins to cluster, if known from a previous run of
this function.  Including this value will skip the clustering portion for observation times.}

\item{tadC}{An integer with the number of time after dose bins to cluster, if known from a previous run of
this function.  Including this value will skip the clustering portion for time after dose. This argument
will be ignored if \code{tad=FALSE}.}

\item{limits}{Limits on simulated parameters. See \link{SIMrun}.}

\item{\dots}{Other parameters to be passed to \link{SIMrun}, especially \code{limits}.}
}
\value{
The output of \code{make_valid} is a list of class \code{PMvalid}, which is a list with the following.
\itemize{
\item simdata The combined, simulated files for all subjects using the population mean values and each subject
as a template. See \link{SIMparse} This object will be automatically saved to the run, to be loaded with
\link{PM_load} next time.
\item timeBinMedian A data frame with the median times for each cluster bin.
\item tadBinMedian A data frame with the median time after dose (tad) for each cluster bin.
This will be \code{NA} if \code{tad = FALSE}.
\item opDF A data frame with observations, predicitons, and bin-corrected predictions for each subject.
\item ndpe An object with results of normalized distrubition of prediction errors analysis.
\item npde_tad NPDE with time after dose rather than absolute time, if \code{tad = TRUE}
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This function will create an object suitable for plotting visual predictive
checks (VPCs) and prediction-corrected visual
predictive checks (pcVPCs).
}
\details{
The function will guide the user
through appropriate clustering of doses, covariates and sample times for
prediction correction using the methods of Bergstrand et al (2011).
\emph{NOTE:} Including \code{tad} is only
valid if steady state conditions exist for each patient.
This means that dosing is stable and regular
for each patient, without changes in amount or timing, and that
sampling occurs after the average concentrations
are the same from dose to dose.  Otherwise observations are \emph{NOT}
superimposable and \code{tad} should
\emph{NOT} be used, i.e. should be set to \code{FALSE}.
}
\examples{
\dontrun{
valid <- NPex$validate(limits = c(0, 3))
}

}
\seealso{
\link{SIMrun}, \link{plot.PMvalid}
}
\author{
Michael Neely
}
