% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_result.R
\name{PM_result}
\alias{PM_result}
\title{Results of a Pmetrics run}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This object contains all of the results after a Pmetrics runs. It is created
by using the \link{PM_load} function.
}
\details{
After a run completes, results are stored on your hard drive. They are loaded
back into R with \link{PM_load} to create the \link{PM_result} object, which contains both
the results and functions to analyze or plot the result.
}
\author{
Michael Neely, Julian Otalvaro
}
\keyword{internal}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{NPdata}}{List with all output from NPAG, made by \link{NPparse}}

\item{\code{ITdata}}{List with all output from IT2B, made by \link{ITparse}}

\item{\code{pop}}{A \link{PM_pop} object}

\item{\code{post}}{A \link{PM_post} object}

\item{\code{final}}{A \link{PM_final} object}

\item{\code{cycle}}{A \link{PM_cycle} object}

\item{\code{op}}{A \link{PM_op} object}

\item{\code{cov}}{A \link{PM_cov} object}

\item{\code{data}}{\link{PM_data} object representing the original .csv data file used in the run}

\item{\code{model}}{text string representing the original model file used in the run}

\item{\code{errfile}}{Name of error file if it exists}

\item{\code{success}}{Boolean if successful run}

\item{\code{valid}}{If the \verb{$validate} method has been executed after a run, this object will be added to
the \code{PM_result} object.  It contains the information required to plot visual predictive checks and normalized prediction
error discrepancies via the npde code developed by Comets et al. Use the
\verb{$save} method on the augmented \code{PM_result} object to save it with the
new validation results.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PM_result-new}{\code{PM_result$new()}}
\item \href{#method-PM_result-plot}{\code{PM_result$plot()}}
\item \href{#method-PM_result-summary}{\code{PM_result$summary()}}
\item \href{#method-PM_result-auc}{\code{PM_result$auc()}}
\item \href{#method-PM_result-nca}{\code{PM_result$nca()}}
\item \href{#method-PM_result-sim}{\code{PM_result$sim()}}
\item \href{#method-PM_result-save}{\code{PM_result$save()}}
\item \href{#method-PM_result-validate}{\code{PM_result$validate()}}
\item \href{#method-PM_result-step}{\code{PM_result$step()}}
\item \href{#method-PM_result-MM_opt}{\code{PM_result$MM_opt()}}
\item \href{#method-PM_result-load}{\code{PM_result$load()}}
\item \href{#method-PM_result-clone}{\code{PM_result$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-new"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-new}{}}}
\subsection{Method \code{new()}}{
Create new object populated with data from previous run
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$new(out, quiet = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{out}}{The parsed output from \link{PM_load}, which is
automatically generated. This is not a user-modifiable.}

\item{\code{quiet}}{Quietly validate. Default is \code{FALSE}.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Creation of new \code{PM_result} objects is via \link{PM_load}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-plot"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-plot}{}}}
\subsection{Method \code{plot()}}{
Plot generic function based on type
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$plot(type, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{Type of plot based on class of object}

\item{\code{...}}{Plot-specific arguments}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-summary"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-summary}{}}}
\subsection{Method \code{summary()}}{
Summary generic function based on type
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$summary(type, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{Type of summary based on class of object}

\item{\code{...}}{Summary-specific arguments}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-auc"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-auc}{}}}
\subsection{Method \code{auc()}}{
AUC generic function based on type
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$auc(type, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{Type of AUC based on class of object}

\item{\code{...}}{Summary-specific arguments}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-nca"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-nca}{}}}
\subsection{Method \code{nca()}}{
Perform non-compartmental analysis
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$nca(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{makeNCA}.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
See \link{makeNCA}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-sim"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-sim}{}}}
\subsection{Method \code{sim()}}{
Simulates using the self$final object.
For parameter information refer to \link{SIMrun}. It will return a \code{PM_sim} object
by running \link{SIMparse} at the end of the simulation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$sim(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Parameters passed to \link{SIMrun}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-save"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-save}{}}}
\subsection{Method \code{save()}}{
Save the current PM_result object to an .Rdata file.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$save(run, file)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{run}}{The run output folder number to save the revised result. If missing,
will save in the current working directory. For example, if folder "1" is in
your current working directory, specify \code{run = 1} to save the result to the "outputs"
subfolder of the "1" folder.}

\item{\code{file}}{Custom file name. Default is "PMout.Rdata".}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
This is useful if you have updated the result in some way, for example you
have run the \verb{$make_valid} method on the \code{PM_result} object, which returns
an internal simulation based validation as a new \code{valid} field. To save this
validation, use this \verb{$save} method. Note that unless a \code{file} name is provided,
the changes will overwrite the
previous run results, although unchanged items will be preserved. This is the
usual workflow. However, a custom file name may be useful to share the run
results with someone.

The saved object is an .Rdata file. When loaded, it should be assigned to an R
object, e.g. \code{run2 <- PM_result$new("filename")}. An equivalent statement would
be \code{run2 <- PM_load(file = "filename")}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-validate}{}}}
\subsection{Method \code{validate()}}{
Validate the result by internal simulation methods.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$validate(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{make_valid}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-step"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-step}{}}}
\subsection{Method \code{step()}}{
Conduct stepwise linear regression of Bayesian posterior parameter values
and covariates.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$step(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{PMstep}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-MM_opt"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-MM_opt}{}}}
\subsection{Method \code{MM_opt()}}{
Calculate optimal sample times from result and template data file.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$MM_opt(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{MM_opt}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-load"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-load}{}}}
\subsection{Method \code{load()}}{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

Deprecated method to load prior results saved with the \verb{$save} method.
Replaced by \link{PM_load}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$load(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Not used.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_result-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PM_result-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_result$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
