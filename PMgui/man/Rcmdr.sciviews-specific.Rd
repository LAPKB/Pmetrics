\name{Rcmdr.sciviews-specific}
\alias{Rcmdr.sciviews-specific}
\alias{is.SciViews}
\alias{is.SciViews.TclTk}
\alias{tkfocus}
\alias{svCommander}
\alias{svlogger}
\alias{activeDataSetEdit}
\alias{activeDataSetView}
\alias{optionLogCommand}
\alias{optionAttachDataSet}
\alias{optionSortVariables}
\alias{refreshStatus}

\title{Rcmdr SciViews-specific Functions}
\description{
  These functions provide compatibility with SciViews (\url{http://www.sciviews.org}). Thanks to them, Rcmdr is totally integrated
  into SciViews Insider. In this environment, the main 'R Commander' window is replaced by an 'R Commander menu' and log files are
  replaced by special R code editing windows with syntax highlighting. Most of these functions are not intended for direct use.
}

\usage{
is.SciViews()
is.SciViews.TclTk()
svlogger(command)
optionLogCommand()
optionAttachDataSet()
optionSortVariables()
refreshStatus()
}

\arguments{
  \item{command}{a character string that evaluates to an R command.}
}

\details{
  The functions \code{is.SciViews} tests if R is running under SciViews. If not, most of the other SciViews-specific functions do nothing. \code{is.SciViews.TclTk} test if the SciViews client communicates with R through Tcl/Tk (otherwise, it probably uses SciViews plugs).
  The function \code{svlogger} is similar to \code{logger}, but it records Rcmdr commands in the specific SciViews R script window
  and in the SciViews command history, instead of the log window and the default R command history. 
  \code{optionLogCommand}, \code{optionAttachDataSet} and \code{optionSortVariables} allow to change the command logging, automatic attachment of the active
  data set and sorting of variable names (equivalent options than those accessible by check boxes in the 'R Commander' window of Rcmdr outside of SciViews, or in the Options dialog box). In SciViews
  insider, the state of these options, as well as the names of the active data set and model are displayed in the status bar.
  \code{refreshStatus} make sure that this information in the status bar is updated according to the current internal state of Rcmdr.
}

\author{Philippe Grosjean \email{phgrosjean@sciviews.org}}

\keyword{misc}
