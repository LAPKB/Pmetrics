\name{Plugins}
\alias{Plugins}

\title{R Commander Plug-in Packages}
\description{
  Plug-ins are R packages that extend the R Command interface.
}


\details{
An R Commander plug-in is an ordinary R package that (1) provides extensions to 
the R Commander menus is a file named \code{menus.txt} located in the package's 
\code{etc} directory; (2) provides call-back functions required by these menus;
and (3) in an \code{Models:} field in the
package's \code{DESCRIPTION} file, augments the list of model objects 
recognized by the R Commander. The menus provided by a plug-in package are
merged with the standard Commander menus. It is also possible to remove menus
and menu items from the standard Commander menu file or from the files of
plug-ins installed before the current one.

Plug-in packages given in the R Commander \code{plugins} option 
(see \code{\link{Commander}}) are automatically loaded when the Commander
starts up. Plug-in packages may also be loaded via the Commander
\emph{Tools -> Load Rcmdr plug-in(s)} menu; a restart of the Commander is
required to install the new menus. Finally, loading a plug-in package when
the \pkg{Rcmdr} is not loaded will load the \pkg{Rcmdr} and activate the
plug-in.

An illustrative R Commander plug-in package, \pkg{RcmdrPlugin.TeachingDemos}, is
available on CRAN.

For more details, see my (slightly out-of-date) article on ``Extending the Rcmdr by 
Plug-in Packages'' in the December 2007 issue of \emph{R News} 
<\url{http://www.r-project.org/doc/Rnews/Rnews_2007-3.pdf}>.
}

\seealso{\code{\link{Commander}}}

\keyword{misc}