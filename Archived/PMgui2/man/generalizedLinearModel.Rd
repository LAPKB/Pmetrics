\name{generalizedLinearModel}
\alias{generalizedLinearModel}

\title{Rcmdr Generalized Linear Model Dialog}
\description{
  This dialog is used to specify a generalized linear model to be fit by the 
  \code{\link[stats]{glm}} function.
}

\details{
The left model-formula box specifies the response variable to be used in the model; it
may be a variable name or an expression evaluating to the response variable, such as
\code{working == "Fulltime"}.

The right model-formula box specifies the right-hand (i.e., predictor) side of the model.
See \code{\link[stats]{glm}} for details.

You can type directly in the model formula boxes. Alternatively, double-clicking the left mouse
button on a variable in the variable-list transfers it to the left-hand side of the model (if
it is empty or selected) or to the right-hand side. 
Factors are indicated in the variable list; all other variables are numeric.
You can also enter operators and parentheses using the buttons
above the formula. If you select several variables in the variable-list box, clicking on the
\code{+}, \code{*}, or \code{:} button will enter them into the model formula.

Double-click the left mouse button to select a family in the "Family" box and the corresponding
permissible link functions appear in the "Link function" box to the right. Initially, the
canonical link for the family is selected. See \code{\link[stats]{family}} for details.

Specifying a subset expression allows you to fit the model to a subset of observations
in the active data set. For example, assuming that \code{gender} is a variable in the
active data set, entering \code{gender == "Male"} would restrict the model to males.

If the active model is a generalized linear model, and the active data set has not changed, 
then the initial values of the left-hand-side,
right-hand-side, family, link, and subset fields are retained from the active model.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[stats]{glm}}, \code{\link[stats]{family}}, \code{\link[base]{Comparison}}}

\keyword{models}
