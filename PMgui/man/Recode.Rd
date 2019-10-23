\name{Recode}
\alias{Recode}

\title{Rcmdr Recode Dialog}
\description{
  The recode dialog is normally used to recode numeric variables and factors
  into factors, for example by combining values of numeric variables or levels
  of factors. It may also be used to produce new numeric variables. The Rcmdr
  recode dialog is based on the \code{\link[car]{recode}} function in the
  \code{car} package.
}

\details{
The name of each new variable must be
a valid R object name (consisting only of upper and lower-case letters, numerals, and
periods, and not starting with a numeral).

Enter recode directives in the box near the bottom of the dialog. 
Directives are normally entered one per line, 
but may also be separated by semicolons. Each directive is of the form \code{input = output} 
(see the examples below). If an input value satisfies more than one specification, then the 
first (from top to bottom, and left to right) applies. If no specification is satisfied, then 
the input value is carried over to the result. \code{NA} is allowed on input and output. Factor 
levels are enclosed in double-quotes on both input and output.

Several recode specifications are supported: 

\describe{
    \item{a single value}{For example, \code{"missing" = NA}.}
    \item{several values separated by commas}{For example, \code{7,8,9 = "high"}.}
    \item{a range of values indicated by a colon}{For example, \code{7:9 = "high"}. The
        special values \code{lo} and \code{hi} may appear in a range. For example,
        \code{lo:10=1}. Note that these values are unquoted.}
    \item{the special value \code{else}}{everything that does not fit a previous
        specification. For example, \code{else=NA}. Note that \code{else} matches \emph{all}    
        otherwise unspecified values on input, including \code{NA}.}
    }

If all of the output values are numeric, and the "Make new variable a factor" check box is 
unchecked, then a numeric result is returned.

If several variables are selected for recoding, then each is recoded using the same recode
directives. In this case, the name entered in the box labelled
"New variable name or prefix for multiple recodes" will be prefixed to the name of each
variable being recoded. Setting an empty prefix (i.e., "") will cause the recoded variables
to replace the original variables. 
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[car]{recode}}}

\keyword{manip}
