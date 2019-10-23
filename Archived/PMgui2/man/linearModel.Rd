\name{linearModel}
\alias{linearModel}

\title{Rcmdr Linear Model Dialog}
\description{
  This dialog is used to specify a linear model to be fit by the \code{\link[stats]{lm}} function.
}

\details{
The left model-formula box specifies the response variable to be used in the model; it
may be a variable name or an expression evaluating to the response variable, such as
\code{log(income)}.

The right model-formula box specifies the right-hand (i.e., predictor) side of the model.
See \code{\link[stats]{lm}} for details.

You can type directly in the model formula boxes. Alternatively, double-clicking the left mouse
button on a variable in the variable-list transfers it to the left-hand side of the model (if
it is empty or selected) or to the right-hand side. You can also enter operators and parentheses using the buttons
above the formula. If you select several variables in the variable-list box, clicking on the
\code{+}, \code{*}, or \code{:} button will enter them into the model formula.

Specifying a subset expression allows you to fit the model to a subset of observations
in the active data set. For example, assuming that \code{gender} is a variable in the
active data set, entering \code{gender == "Male"} would restrict the model to males.

If the active model is a linear model and the active data set has not changed, 
then the initial values of the left-hand-side,
right-hand-side, and subset fields are retained from the previous model.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[stats]{lm}}, \code{\link[base]{Comparison}}}

\keyword{models}
