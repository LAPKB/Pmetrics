\name{NEWS}
\title{NEWS file for the Pmetrics package}
\section{Changes in version 0.20}{

\subsection{NEW FEATURES}{
\itemize{
\item Cleaned up bivariate plots in plot.PMfinal function.
\item Added target attainment function, summary and plot
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bug in simulator which caused it to crash when only one random variable simulated
\item Fixed minor bug in plot.PMcycle which did not allow full customization of gamma/lambda plot
}
}
}

\section{Changes in version 0.19}{

\subsection{NEW FEATURES}{
\itemize{
\item Added ADDL and II items to data matrix to support additional and steady state dosing.  Old format matrix files without ADDL and II will be automatically updated.  This change is propagated through NPAG, IT2B and the Simulator
\item Added the ability to specify ranges to truncate simulator output and avoid extreme, i.e. non-phyisologic, parameters
\item Added progress bar to PMdiag and cleaned display for SIMrun
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bug in PMmatrix plot that did not permit customization of X-axis label
\item Corrected a bug in makeErrorPoly that prevented reporting of 3rd degree polynomial coefficients
\item Fixed handling of .csv file creation during a simulation using SIMrun with multiple subjects as templates
\item Fixed bug in ERRrun that prevented completion on Windows machines
\item Corrected the PMcheckMatrix function to properly look for missing covariate data at time=0 for each subject
\item Suppressed confidence intervals in plot.PMsim if nsim <100 and enabled it to handle missing confidence intervals properly
}
}
}
\section{Changes in version 0.18}{

\subsection{NEW FEATURES}{
\itemize{
\item Cov objects are now loaded by default after NPAG and IT2B runs.
\item Added non-compartmental analysis to NPAG objects
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed matrix orientation of population means in simulator with multi-modal distributions
\item plot.PMcycle now works correctly if there are fewer than 10 cycles
\item Stopped makeErrorPoly from trying to open new graphics window
\item Updated makeErrorPoly to handle situations with too few points to fit 2nd and/or 3rd degree polynomials
\item Fixed errors in reading instruction files in ERRrun
\item Corrected mistake in IT2B runs with instruction files that caused end of file error
\item Took out AUC extraction with makeCov as it was too subject to error.  This function is better implemented in the existing makeAUC command.
}
}
}
\section{Changes in version 0.17}{

\subsection{NEW FEATURES}{
\itemize{
\item Pmetrics will now check for updates silently when it is first loaded into R with the library(Pmetrics) or require(Pmetrics) commands.  If an update is found, the user will be notified with a simple, unobtrusive message.
\item The scripts will now handle security features associated with Windows Vista and later which restrict installation of packages into certain folders.  Pmetrics now scans for the installation folder and sets this as an environment variable (which can be retrieved by Sys.getenv("PmetricsPath") for the remainder of the session.  This is invisible to the user.
\item 32 and 64 bit versions are now included.
}
}
\subsection{BUG FIXES}{
\itemize{
\item None
}
}
}
\section{Changes in version 0.16}{

\subsection{NEW FEATURES}{
\itemize{
\item Changed to compiled IT2B, NPAG, and Simulator objects.  Initial roll out to support gfortran.  More compilers added as needed by users.
\item Added an include argument to plot.PMmatrix to only plot a subset of the population.  This can be used to match the same subset specified in an instruction file (which will be the only one modeled out of all subjects in the .csv file) or to simply focus on a subset of the total modeled population.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed improper handling of block calculation by plot.PMmatrix and NPpostPred
\item Fixed bug in ITrun script that tried to use the tee logging command in Windows, which does not exist
\item Fixed another bug in ITrun which did not write correctly formatted integer to extnum file for run numbering purposes
}
}
}
\section{Changes in version 0.15}{

\subsection{NEW FEATURES}{
\itemize{
\item Added override flag to PMwriteMatrix to write .csv file even with errors.  Default is false.  This now allows a .csv file to be written by PMwrk2csv even if there are errors.  The errors will be reported, however.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed improper handling of .csv files with comments by PMreadMatrix
\item Fixed improper handling of 0 cycle runs
\item Corrected path in Windows version to Rscript on some machines
\item Corrected logic in checking data files for missing covariate data at time 0 when there is only one covariate
\item Restored ability to control line characteristics in PMmatrix plot
}
}
}
\section{Changes in version 0.14}{

\subsection{BUG FIXES}{
\itemize{
\item Normalized .Library path to account for path truncation in Windows.
\item Fixed cycle plot of gamma from IT2B run to allow for one gamma for each output equation
}
}
}
\section{Changes in version 0.13}{

\subsection{NEW FEATURES}{
\itemize{
\item Added npde, standardized visual predictive check with PMdiag command and object, which has a print method as well
}
}
\subsection{BUG FIXES}{
\itemize{
\item Corrected plotting of output equations >1 in PMmatrix objects
\item Changed .libpaths() to .Library in NPrun, ITrun, ERRrun, SIMrun to avoid multiple library paths
}
}
}
\section{Changes in version 0.12}{

\subsection{NEW FEATURES}{
\itemize{
\item Added additional items to ITparse and NPparse: tests for convergence, reporting of assay error model used, prior density used
\item Added support for different delimiters in PMreadMatrix
\item Added checking for column order and missing covariate data at time=0 to PMcheckMatrix
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed a bug in plot.PMmatrix that suppressed posterior plots from simulated data
\item Made Rscript work in NPrun and ITrun even if R not in user environment
}
}
}
\section{Changes in version 0.11}{

\subsection{NEW FEATURES}{
\itemize{
\item Summary and print methods for PMop objects
\item Plotting summary of PMop as weighted residual vs. pred and vs. time
\item Greater control over grids in plots
}
}
\subsection{BUG FIXES}{
\itemize{
\item Formula parsing in plot PMfinal 
\item Column name checking in PMcheckMatrix 
\item Regenerating seed file for simulations before each subject
\item Fixed axis ticks in log plots
}
}
}
\section{Changes in version 0.10}{

So it begins.
}




