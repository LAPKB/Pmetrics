\name{NEWS}
\title{NEWS file for the Pmetrics package}

\section{Changes in version 1.9.3}{

\subsection{BUG FIXES}{
\itemize{
\item A bug that made some tables in the report have wrong headers was fixed.
}
}
}

\section{Changes in version 1.9.2}{

\subsection{NEW FEATURES}{
\itemize{
\item Pmetrics will register each new installation.
}
}
}

\section{Changes in version 1.9.1}{

\subsection{NEW FEATURES}{
\itemize{
\item Now the local version number is compared with the version number in github after suggest to update Pmetrics.
\item Updated plot.PMvalid with error checking for icen and outEq.
\item PMupdate is deprecated.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fix makeValid and plot.PMvalid to handle multiple outEq. 
\item Depecrated functions were exported.
}
}
}

\section{Changes in version 1.9}{

\subsection{NEW FEATURES}{
\itemize{
\item This version is a pre-release beta for version 2.0
\item makeValid and plot.PMvalid added to replace makeNPDE and add pcVPC capacity
\item NPDE code folded into Pmetrics since original NPDE package no longer maintained
\item Windows users will now see terminal window open automatically
\item Added remote server run capabilities, currently only on LAPKB server
\item Restructured start up and installation to be self contained for all modules, including gfortran
\item Cleaned printing of summary.PMfinal object
\item Upated package NAMESPACE
}
}
\subsection{BUG FIXES}{
\itemize{
\item None
}
}
}

\section{Changes in version 1.5.3}{

\subsection{NEW FEATURES}{
\itemize{
\item MMopt now has multi-weighting option for any combo of auc, max, min
\item makeValid and plot.PMvalid added to replace makeNPDE and add pcVPC capacity
\item Updated makePTA to accept PMpost and PMmatrix objects
\item PMtree now checks for existing project and aborts if found to prevent overwriting.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed minor bug in npc to make calculated percentage between 5th and 95th percentile correct
}
}
}

\section{Changes in version 1.5.2}{

\subsection{NEW FEATURES}{
\itemize{
\item Updated for R version 3.5
}
}
\subsection{BUG FIXES}{
\itemize{
\item None
}
}
}

\section{Changes in version 1.5.1}{

\subsection{NEW FEATURES}{
\itemize{
\item Added the removal of lines with only spaces in model files that could cause an error
\item Clarified instruction line to launch batch file in Windows to execute NPAG/IT2B run.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bug in SIMrun when trying to used pre-loaded data file
\item Fixed bug with copying priors from previous run now that runs made inside new folders
\item makeAUC now returns NA when only one observation present for a subject
\item Models with more than 9 compartments detect compartments correctly
\item Restored split function to simulator, which was broken in 1.5.0
\item Recoded PMcompare to comply with deprecated recycling of vector length 1 in R, as of v. 3.4.3
\item Updated PMwrk2csv to properly handle files with 0 rates, i.e. only bolus dosing
}
}
}


\section{Changes in version 1.5.0}{

\subsection{NEW FEATURES}{
\itemize{
\item NPAG can now use variables which are fixed in the population, but estimated
\item Added ability to use external dataset for NPDE
\item Added shrinkage calculation to final objects as final.x$shrinkage.  Also added this to the summary table in PMreport.
\item makeNCA has new terminal argument (default=3) to control number of concentrations used for fitting k to terminal portion of time-concentration curve.
\item Added weighting to MMopt.
\item Removed dependency on rJava for PMcheck
\item Added ability to simulate with both covariates and split.  The mean covariate value is added to each support point in this case.
\item makePTA with target.type = 'time' now MUCH faster thanks to improvements by Jan Strojil
\item Added the ability to makePTA to recalculate success threshold and redefine labels using existing PMpta object
\item SIMparse can now operate in parallel and will check if older simulations are present.
\item Updated Shiny code for PMcode(run) 
}
}
\subsection{BUG FIXES}{
\itemize{
\item The mult argument works properly in plot.PMsim now.
\item Fixed bug in makePTA and simulated targets that miscalculated success
\item Fixed bug in makeNPDE that caused a crash when model and data files have same basename.
\item Corrected makeFinal postPoints when nsub>100.
\item Improved handling of missing subjects in makeNCA.
}
}
}



\section{Changes in version 1.4.1}{

\subsection{NEW FEATURES}{
\itemize{
\item Added a check to PMcheck for subjects with no observations
\item Added binSize argument to plot.PMsim to bin simulated concentrations, which is useful for VPCs.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed plot, summary, and print PMop to handle objects with no observations
\item PMtest updated for parallelized version of Pmetrics
\item fixed SIMrun ID bug 
}
}
}


\section{Changes in version 1.4.0}{

\subsection{NEW FEATURES}{
\itemize{
\item NPAG code parallelized over points introduced.  Default is serial for algebraic/exact and parallel for ODE models.  Can be overridden by user.
\item Post-run report now includes Processing mode (parallel | serial) and run time.
\item Added the ability to specify predInt argument to SIMrun as a list of multiple intervals to generate predictions.
\item When making a csv file from simulated output, missing obsNoise now defaults to model file values, not 0.
\item Added PMmb2csv to convert old .mb or USC*PACK files to .csv format
\item Added setPMoptions and getPMoptions for session options.  Currently they are sep (datafile field separator which can be "," or ";") and dec (decimal separator which can be "." or ",").  Removed PMreadDefaults and PMwriteDefaults as Defaults package no longer supported.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Bug with ADDL and II in NM2PM fixed
\item Allowed makeFinal to work with older NPdata objects that don't have postden items
\item Changed incorrectly formatted values <1E-100 to be 0 in output files
\item Fixed makeFinal and makePost to handle one subject data files properly
\item Fixed PMwrk2csv when no assay error coefficients present
\item General updating of help files, which isn't really a bug!
\item Fixed bug in simulator output when values very near 0 didn't format properly
}
}
}

\section{Changes in version 1.3.2}{

\subsection{NEW FEATURES}{
\itemize{
\item None
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed makeFinal bug regarding posterior density
\item Scaled bivariate NPAG final plots properly
}
}
}


\section{Changes in version 1.3.1}{

\subsection{NEW FEATURES}{
\itemize{
\item Added Bayesian posterior parameter points to final object as final$postden
\item Added a plot method to plot.PMfinal to plot posterior distributions for a given parameter and set of subjects.
}
}
\subsection{BUG FIXES}{
\itemize{
\item covariate argument now works properly for alphanumeric ID and when no covariates are fixed
}
}
}

\section{Changes in version 1.3.0}{

\subsection{NEW FEATURES}{
\itemize{
\item Added summary.PMfinal to provide non-parametric parameter summaries based on simulation or parametric parameter summaries based on central limit theorem
\item Added col.pred argument to plot.PMmatrix to allow separate coloring of predicted profiles
\item Added makePTAtarget to simulate distributions of targets and updated makePTA, plot.PMpta, and summary.PMpta accordingly.
\item Added the ability to do PDI (pharmacodynamic index) plots with plot.PMpta.
\item NPAG, IT2B and simulator updated logic for SS dosing to have observation at time x be the concentration x hours after the beginning of the last steady state dose
\item Added another argument to the covariate SIMrun list argument: fix, which permits some covariates to be fixed to values in the template and not simulated
\item Re-parameterized limits argument to the covariate SIMrun list argument so that it is a named list with the desired limits for each covariate
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bug in model file syntax checking with compilers other than gfortran.
\item summary.PMcov now handles non-numeric IDs correctly.
\item makePTA now calculates AUC correctly when start!=0 - it was adding in a time=0, conc=0 row, regardless of whether start=0 or not
}
}
}


\section{Changes in version 1.2.9}{

\subsection{NEW FEATURES}{
\itemize{
\item Added time after dose plotting option to plot.PMmatrix.
\item Added summary.PMmatrix and print.summary.PMmatrix
\item SIMrun now checks for covariance matrices that are not pos-def and offers several solutions
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bug in plot.PMmatrix where subjects with all missing observations caused an error.
\item add=T now functions properly in plot.PMsim when nsim<10.
\item Fixed error in makeNPDE causing problem with old instruction files.
\item Updated PMrun to work when data and model files have same root name
}
}
}

\section{Changes in version 1.2.8}{

\subsection{NEW FEATURES}{
\itemize{
\item None yet
}
}
\subsection{BUG FIXES}{
\itemize{
\item makeNPDE now works properly when the run to use was made with a non-uniform prior
\item SIMrun developed a bug after introducing covariate simulations such that fixed parameter values weren't handled properly.  This has been corrected.
\item PMcheck had a bug when TIME=0 observation requirement removed.  Fixed. 
}
}
}

\section{Changes in version 1.2.7}{

\subsection{NEW FEATURES}{
\itemize{
\item PMcheck now makes a color-coded .xlsx file with highlighted errors.  This code was co-developed with Patrick Nolain in Limoges, France.
\item More widespread checking occurs for packages with automatic downloading/installation for those that are missing 
\item makeNCA now takes a variety of arguments including a run number which can be used to do NCA on an data file or a post object, or a previously loaded data file, or a data file in the working directory
\item The post and pop objects after an NPAG run now contain the predictions at observation times in addition to the intervals specified by the idelta argument to NPrun
\item Model file format updated to TSMULTM
\item Removed PMcheck requirement for no observations at time=0.  Predictions will always be 0 unless non-zero initial conditions or steady-state dosing specified.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bug in covariate simulations when 0-variance covariates are not present
\item Corrected plot.PMmatrix to handle error bars on observations when C0,C1,C2,C3 missing from data file
\item Included gamma/lambda in SD for weighting in bias/imprecision - now matches option 6/7 results in old USC*PACK
\item PMstep now works properly with a datafile containing only one covariate
\item AIC calc changed in IT2B/NPAG from -TRULOG + (P + Q)  to   2*(-TRULOG + (P + Q)) 
}
}
}
\section{Changes in version 1.2.6}{

\subsection{NEW FEATURES}{
\itemize{
\item Added covariate argument to SIMrun to enable simulation with covariates
\item Internal moving of parseBlocks function to independent function
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed small bug in makeNCA that caused an error in Tmax when there were two successive simulated Cmax that were the same by rounding.
\item Corrected formulae for weighted prediction error to weight by SD not variance
}
}
}
\section{Changes in version 1.2.5}{

\subsection{NEW FEATURES}{
\itemize{
\item You can now plot any subset of output equations in plot.PMmatrix.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bugs related to making post and pop objects
}
}
}
\section{Changes in version 1.2.4}{

\subsection{NEW FEATURES}{
\itemize{
\item Just bug fixes from 1.2.3
}
}
\subsection{BUG FIXES}{
\itemize{
\item Numerous little bugs with make family and PMstep all related to adding mean and median predictions.
}
}
}

\section{Changes in version 1.2.3}{

\subsection{NEW FEATURES}{
\itemize{
\item Updated to reshape2
\item Enhanced algorithm for makePost and makePop to generate data frames with predictions based on means and medians
\item Updated makeAUC, makeNCA, plot.PMmatrix to include icen (mean or median) argument to use new output from makePost and makePop
\item Updated makeOP and makeCov to eliminate icen argument and make objects that include predictions based on means and medians
\item Updated plot.PMop to use icen and pred.type (post or pop) rather than type 
\item Updated plot.PMcov to use icen
\item Added PMsave as complement to PMload
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bug where prior DENxxxx file was moved rather than copied from earlier run when using the prior=run notation in NPrun
\item makeCov now handles multiple input models properly
\item SIMparse and makePTA now handle missing values in simulated files
\item Fixed PMupdate by setting up package repository on LAPK website.
}
}
}

\section{Changes in version 1.2.2}{

\subsection{NEW FEATURES}{
\itemize{
\item Added boolean option to suppress legend in makeErrorPoly
\item Added summary.PMcov to summarize covariates and Bayesian posterior parameter values for each subject
\item Added free.fraction argument to makePTA, as suggested by Joe Kuti
\item PMload can now load any number of results in a single command
}
}
\subsection{BUG FIXES}{
\itemize{
\item Corrected time above target calculations in makePTA to reflect proportion of stated time interval, i.e. end-start
}
}
}

\section{Changes in version 1.2.1}{

\subsection{NEW FEATURES}{
\itemize{
\item Added MMopt function for MM-optimal design from NPAG output
\item Added silent data file check when using NPrun, ITrun, or SIMrun.
\item Added error trap if the number of lines of output error coefficients in the model file is not equal to the number of output equations.
\item Added RMSE and \%RMSE to summary.PMop and pop\%RMSE and post\%RMSE to PMcompare
\item Added checks for EVID and TIME values in all rows to PMcheck
\item Added check for non-numeric columns to PMcheck
\item Removed requirement to have all subjects end with an EVID=0 observation event
\item Reversed axes so that x is now predicted and y is observed to be consistent with mathematical notation obs = pred + error

}
}
\subsection{BUG FIXES}{
\itemize{
\item Corrected handling of multiple output equations by PMcompare
\item Fixed creation of .config folder as needed on Mac/Linux systems
\item Stabilized name of directory for calling post-run processing script which couldn't be found by some Windows users
\item Corrected bug in simulator when multiplying limits with fixed variables present
}
}
}

\section{Changes in version 1.2.0}{

\subsection{NEW FEATURES}{
\itemize{
\item Pmetrics now works with Linux!  This is in prep for server mode and parallelized code.
\item Added NONMEM to Pmetrics translator: NM2PM.  It has been tested on several NONMEM files, but will probably not handle every possibility yet.
\item Cleaned up NPrun, ITrun, ERRrun code to use common source which is a hidden function.  This makes these functions more stable for changes moving forward.
\item Examples updated.
\item Documentation for PMcheck updated.
\item By entering nsim=0, you can now simulate one profile from each point in a non-parametric prior. This is prep for including the MMopt optimal sampling algorithm into Pmetrics.
\item A prior for NPrun can now be specified by simply typing the run number in addition to the existing methods of NPdata.x or DEN0001 file.
}
}
\subsection{BUG FIXES}{
\itemize{
\item PMbuild will now correctly detect missing packages and install them.
\item Fixed logic in PMreport to better handle error conditions
}
}
}

\section{Changes in version 1.1.7}{

\subsection{NEW FEATURES}{
\itemize{
\item Redesigned report and consolidated NPreport and ITreport into single PMreport function
\item Added tex file report to output (code by Alona Kryschenko)
\item Changed default behavior of obsNoise argument in SIMrun.  Now, if missing, will default to 0 for all coefficients/output equations (i.e. no noise).  To use values in data file first, and if missing, the model file, specify obsNoise=NA.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed a bug in simulator related to parsing multiple output equations
}
}
}

\section{Changes in version 1.1.6}{

\subsection{NEW FEATURES}{
\itemize{
\item None
}
}
\subsection{BUG FIXES}{
\itemize{
\item Corrected error in instructions causing a crash when fitting data with no covariates
}
}
}

\section{Changes in version 1.1.5}{

\subsection{NEW FEATURES}{
\itemize{
\item Added a check in NPrun, ITrun, ERRrun and SIMrun to ensure that model and data file names are 8 characters or fewer
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed more simulator bugs relating to dataframe format with lots of observations that continued on separate lines
\item Updated help for makeAUC to include method argument (linear or other)
\item Corrected handling of exclude argument to NPrun, ITrun and ERRrun
}
}
}
\section{Changes in version 1.1.4}{

\subsection{NEW FEATURES}{
\itemize{
\item Change simulator argument to clean rather than cleanUp to be consistent with other functions
\item Simplified start messaging
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed simulator bug with limits related to new dataframe structure of simulator output
\item Corrected bug in makePTA when using a single target with new dataframe structure of simulator output
\item Fixed directory handling in Windows version of PMupdate.
}
}
}

\section{Changes in version 1.1.3}{

\subsection{NEW FEATURES}{
\itemize{
\item Updated NPAG and IT2B to run off single instruction file rather than control stream plus instruction file
\item Added p-values to PMcompare based on nearest neighbors analysis of joint density for similarity.  Also eliminated need for list of NPdata or ITdata objects.
\item Changed simulator output to a simpler dataframe structure
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed the detection of output equation numbers in model file building to account for conditional outputs
\item Set compartment amts from SIMparse to NA if a 0 compartment model (explicit output)
\item Fixed bug in PMcheck which did not properly detect a PMmatrix object supplied as the object to check.
}
}
}

\section{Changes in version 1.1.2}{

\subsection{NEW FEATURES}{
\itemize{
\item Added exclude to list of arguments for SIMrun
\item Replaced PMdiag with makeNPDE which is a wrapper for npde::autonpde (Comets et al)
\item Added a fourth option to specify limits in SIMrun: a vector of 1 or 2 numbers to multiply the columns of the final$ab object used in the simulation.
\item Updated for Windows 8 and Mac OSX Mavericks
\item Internal changes to structure of PMsim objects from array to data.frame.  Also added compartment amounts to PMsim list obtained from SIMparse.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed a bug in PMupdate
}
}
}


\section{Changes in version 1.1.1}{

\subsection{NEW FEATURES}{
\itemize{
\item Added ability to use wildcards with makePTA to parse Simulator files on the fly
\item Combined NPload and ITload into single PMload function
\item Added ability to include comment lines prefixed with 'C ' in model files
\item Added include and exclude to makeAUC function
\item Changed predInt argument to SIMrun to allow for start, stop, interval times.
\item Reworked SIMparse to allow for combining across simulations with different times, as well as to include or exclude files returned when using wildcards.
\item Added weighted.t.test to summary of PMop objects to test for bias different than 0.  Code for this is courtesy of FelixS (http://www.r-bloggers.com/weighted-t-test-in-r/), also inspired by a function from Gavin Simpson on R-Help
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed incorrect handling of salt fractions for multiple drugs.
\item When outeq is specified with plot.PMsim and obs is included (for VPC), correct observations via outeq are now selected
\item Fixed bug that crept into PMcompare with plot=T option
\item Updated PMtree to generate template script with PMload rather than NPload
}
}
}

\section{Changes in version 1.1.0}{

\subsection{NEW FEATURES}{
\itemize{
\item Added ability to use old .ZMQ files to NPAG
\item Now extracts Bayesian posterior densities for first 100 subjects into NPdata
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed notification to run PMbuild()
}
}
}

\section{Changes in version 1.0}{

\subsection{NEW FEATURES}{
\itemize{
\item NEW! Pmetrics gets a GUI via the Shiny package.  Use PMcode("command").
\item Moved the directory for compiled Pmetrics fortran code to the folder pointed to by Sys.getenv('APPDATA') on Windows systems
\item Added out argument to all Pmetrics plots enabling output to any graphic device, e.g. pdf, eps, tiff, jpeg, etc, making it easy to generate publication-ready plots.
\item Replaced PMcheckMatrix and PMcheckModel with PMcheck to check data files and (optionally) model files for errors.
\item PMcheck will now look for non-monotonically increasing times within a subject ID and subject IDs which have been reused, i.e. are non-contiguous.
\item The errbar argument is added to plot.PMatrix to plot standard deviation error bars around observations, based on C0, C1, C2, C3 in the data file or user-supplied values.
\item Exclude now added as argument to plot.PMmatrix, plot.PMop, and plot.PMcov to exclude subject IDs, i.e. the complement to the include argument.
\item PMfixMatrix deprecated and fix=F/T argument added to PMcheck
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed a bug that caused model files to parse incorrectly if headers included with no information. Now they are ignored.
\item Corrected handling of more than one fixed parameter when writing instruction files.
\item PMcheck (formerly PMcheckMatrix) now correctly reports subjects with first record != EVID 1 or TIME 0.
\item Minor housekeeping issues with some of the plot functions.
}
}
}

\section{Changes in version 0.40}{

\subsection{NEW FEATURES}{
\itemize{
\item MAJOR update: the model file has been extended to eliminate the need for an instruction file by adding parameter ranges to PRI block and an ERRor block.
\item Added a new function to print errors in .csv files from PMcheckMatrix in a more easily digestible format.
\item Added a new function PMfixMatrix to attempt to automatically correct errors detected with PMcheckMatrix.
\item Added an 'extra' block to model file to allow for additional fortran subroutines to be added to the end of a model file
\item Changed makeOP and associated functions to always make OP objects for population and posterior predictions for all output equations, similar to the object loaded by NPload or ITload.  This combined object also has class PMop.  Functions can handle the larger object or the individual objects.
\item Reversed axes so that y is now predicted and x is observed to be consistent with mathematical notation that dependent variable is plotted on the y axis.
\item Redefined bias to be pred - obs, so that positive bias implies overprediction.
\item Changed output files to now be sequentially numbered rather than date-time stamped. This allows easier loading and examples.
\item Implemented system to compile Pmetrics objects upon first use, so that they do not have to be recompiled until there is a change in Fortran code.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bugs in SIMrun that mishandled multiple distributions manually specified and creation of .csv files 
\item Corrected calculations for bias (mean weighted obs-pred) and imprecision (bias-adjusted mean weighted squared obs-pred)
}
}
}

\section{Changes in version 0.30}{

\subsection{NEW FEATURES}{
\itemize{
\item BIG CHANGE:  Model file is now a much simpler text file which is parsed to make the fortran code.  Maintained backwards compatibility with existing fortran model files.
\item Added a growth dataset and qgrowth function to abstract CDC growth percentiles useful for simulating pediatric datasets.
\item Added bias and precision to observed vs. predicted regression plots
\item Clarified prediction errors on residual plots
\item Added bias and precision to PMcompare
\item Changed 'nsub' parameter to 'include' in SIMrun, so that non-contiguous subjects in a dataset may be used as simulation templates.  This is analogous to 'include' in plot.PMmatrix.
\item Added 'predInt' option to SIMrun to automatically generate simulated predictions for each template at the specified prediction interval, including predictions at times which are explicitly included in the template (i.e. EVID=0 events).
\item By default, makeCov now does not summarize covariate values, but returns all measured values and the time of measurement.  Bayesian posterior parameter values are replicated as necessary for each time/covariate entry.
\item plot.PMcov now accommodates the changes to makeCov above, so that when time is included as the x variable in a formula, one can plot changes in covariates or parameters over time, aggregated by subject.  Plotting two parameters or a parameter vs. covariate results in the customary behavior as before.
\item PMstep is also updated to handle the new PMcov format.
\item Added include argument to plot.PMcov, plot.PMop
\item Users must now specify a data file in NPrun, ITrun and ERRrun.  This enables more sophisticated error trapping. It also enables return of the data file as part of NPload or ITload.
\item Added checking for specified model, data, and (optional) instruction files and option to correct mistakes without aborting run.

}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bug in PMcheckMatrix when looking for missing covariate data in first entry for a patient
\item Fixed bug that caused crash when running population of 1 in NPAG
\item Fixed a bug in makeAUC for PMsim objects where the start parameter was ignored
\item Corrected plot.PMmatrix colors when single outeq > 1 plotted with prediction
\item Fixed another bug in plot.PMmatrix which did not handle block assignments correctly
\item Corrected subject IDs in PMcov objects
\item Corrected block (defined by EVID=4) handling in makeOP, makeAUC, makeNCA.   
}
}
}

\section{Changes in version 0.25}{

\subsection{NEW FEATURES}{
\itemize{
\item NPAG will no longer crash with a Hessian Matrix error, but will end 'nicely'
\item Changed 'denfile' argument in NPrun to 'prior' and enabled specification of either an NPdata object or a density file, e.g. DEN0001, for a non-uniform prior
\item Updated LAPK website with OS-specific instructions to download and install gfortran
\item Added intern=T option to NPrun, ITrun and ERRrun to run all three internally in R without a batch script.
\item Added PMstep function to carry out multiple linear regressions of covariates on Bayesian posterior parameters, with forward, backward, or forward/backward elimination of covariates
\item Users can now customize the defaults for any argument in any Pmetrics function using the setDefaults command from the Defaults package.  A new Pmetrics command, PMwriteDefaults, will make these session defaults permanent until undone by unsetDefaults.  See Defaults documentation for more details.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Corrected cycle numbering in plots when using a non-uniform prior for NPAG
\item Fixed bug in PMcheckMatrix if only one covariate
\item Corrected makeNCA to handle IV doses properly
}
}
}

\section{Changes in version 0.24}{

\subsection{NEW FEATURES}{
\itemize{
\item PMtree() added to create basic folder substructure and skeleton R script for new Pmetrics project
\item PMupdate() added to update Pmetrics from within R without launching internet browser
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed a bug that crept into plot.PMsim which scrambled the simulated outputs
}
}
}

\section{Changes in version 0.23}{

\subsection{NEW FEATURES}{
\itemize{
\item Error checking to NPreport() and ITreport() allowing creation/saving of partial data in case of error
\item Added PMnews() to simplify retrieval of Pmetrics changelog
}
}
\subsection{BUG FIXES}{
\itemize{
\item Fixed bug in makePop with multiple outputs
}
}
}
\section{Changes in version 0.22}{

\subsection{NEW FEATURES}{
\itemize{
\item Added makePop to generate predictions at frequent intervals for each subject using population parameter means, medians, or modes
\item Updated plot.PMmatrix, replacing "post" argument with "pred" to allow for either a post or pop object made by makePost or makePop, respectively
\item Added startup messages to inform users of news related to Pmetrics
}
}
\subsection{BUG FIXES}{
\itemize{
\item Enabled plot.PMsim to plot one simulated profile correctly.
}
}
}

\section{Changes in version 0.21}{

\subsection{NEW FEATURES}{
\itemize{
\item Added a warning to PMcheckMatrix if event times will exceed the maximum number of AUC intervals (48) in NPAG, based on the default interval of 24 hours.
\item Updated PMwrk2csv to handle new format with ADDL and II columns.
\item Added progress bars to parsing at the end of an NPAG or IT2B run
\item SIMparse can now open and combine several simulator output files at once.
\item Pmetrics now automatically checks for necessary fortran components and offers to download and install if missing.
\item Added a new command PMmanual() to launch the User's Manual and function catalogue.
}
}
\subsection{BUG FIXES}{
\itemize{
\item Made makePTA() stable to simulated values that are missing or not real numbers
\item Corrected plot.PMsim to handle circumstances better when zeros are encountered for log plots
\item Fixed some bugs in SIMrun related to manually specified population priors
\item Fixed a bug in PMwriteMatrix that used improper line endings in Windows, causing extra blank lines to be written
}
}
}

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
\itemize{
\item So it begins.
}
}




