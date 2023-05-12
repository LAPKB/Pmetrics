
# INTRODUCTION ------------------------------------------------------------

# Lines that start with "#" are comments and ignored by R.  Follow the
# directions in them. Execute each non-comment line in this script by
# putting your cursor on it and sending it to the R console.
# You can do this in several ways:
#     Windows
#        R-studio
#           1) The Run button at the top
#           2) Ctrl-Enter
#        R GUI - when the script window is active
#           1) The Run line or selection button at the top
#           2) Ctrl-R
#     Mac
#        R-studio
#           1) The Run button at the top
#           2) Command-Enter
#        R GUI
#           1) Command-Enter

# This script also serves to introduce several R programming functions
# and techniques. For any function, you can get help by typing ?function_name
# in the R console (the lower left window pane in RStudio).

# Load Pmetrics into memory.  You must include this line at the beginning
# of every script.

library(Pmetrics)

# EXERCISE 1 - NPAG RUN ------------------------------------------------

# EXAMPLE NPAG RUN - tlag, ka, kel, vol

# It is useful to annotate your runs as above, so that you can remember
# what you did later!


# Tell R where your working directory is going to be.
# Windows users:  Make sure that you separate directories with a
# forward slash "/" or double backslashes "\\". Unfortunately, Windows is the only OS that uses
# backslashes "\", so R conforms to Unix/Linux style.

wd <- "##WD##"

# change to the working directory to the Examples folder
setwd(wd)

# DATA OBJECT

# Pmetrics always needs data and a model to run
# create our first data object

# The working directory we want to move to can be specified as an absolute
# path (Line 44) or as a relative path (Line 56)
setwd(paste(wd, "/src", sep = ""))

# list the files inside the current working directory
list.files()

# create a new data object by reading a file
exData <- PM_data$new(data = "ex.csv")

# you can look at this file directly by opening it in
# a spreadsheet program like Excel, or a text editor

# exData is an R6 object, which means that contains both data and methods to
# process that data, for example:
exData$data # contains your original datafile
exData$standard_data # contains the standardized and validated data,
exData$summary() # prints the summary of the data to the terminal, or

# another way to do that is using the more common S3 framework in R:
summary(exData)

# To look at the contents of an object:
names(exData)

# other examples of things that can be done with this object are
exData # view the original data in the viewer
exData$print(standard = TRUE) # view the standardized data in the viewer
exData$print(viewer = FALSE) # view original data in console

# MODEL OBJECT
# You can specify a model by reading a file or directl as an object. We'll do both.
# The following code creates the same model as in /src/model.txt file.
# See PMmanual() for details on creating models in R compared to text files.
# The advantage of creating them in R is that one does not need to copy model
# files into folders to provide necessary inputs.

mod1 <- PM_model$new(list(
  pri = list(
    Ka = ab(0.1, 0.9),
    Ke = ab(0.001, 0.1),
    V = ab(30, 120),
    Tlag1 = ab(0, 4)
  ),
  cov = list(
    covariate("WT"), 
    covariate("AFRICA"), 
    covariate("AGE"), 
    covariate("GENDER"), 
    covariate("HEIGHT")
    ),
  lag = list("Tlag(1) = Tlag1"),
  out = list(
    Y1 = list(
      value = "X(2)/V",
      err = list(
        model = proportional(5),
        assay = errorPoly(c(0.02, 0.05, -0.0002, 0))
      )
    )
  )
))
# look at it
mod1

# in the working directory we have another file "model.txt" that contains the old
# representation of the same model we previously presented, let's take a look at it.
system("cat model.txt")

# PM_model$new() also accepts the path to a model file
# create the same model using this file
mod1b <- PM_model$new("model.txt")
mod1b

# PM_model provides a method to update the different elements of a model, for example:
mod1b$update(list(
  pri = list(
    Ka = ab(0.001, 5)
  )
))
mod1b

# to copy a model use the $clone() method.
mod1b <- mod1$clone()

# simply using mod1b <- mod1 will cause mod1b to be changed if mod1 is changed,
# as R6 objects use reference semantics. For more details you can refer to
# https://adv-r.hadley.nz/r6.html, Section 14.4.


# FIT OBJECT
# Now we define a new fit to be run as the combination of a dataset and a suitable model.
exFit <- PM_fit$new(model = mod1, data = exData)

# Let's analyze this object
exFit

# there are some methods we can execute over this object, like:
exFit$check()


# To keep everything tidy, let's move to another folder specific to store the runs
# notice that we didn't have to move any files...
setwd(paste(wd, "/Runs", sep = ""))
exFit$run() # execute the run with default arguments

# A terminal window will open and run; don't worry about pauses; the program has not crashed"

# After the run is complete you need get the extracted information back into R.
# They will be sequentially numbered as /1, /2, /3,... in your working directory.

# One benefit of having this fit object is that it is possible to run multiple
# fittings without needing to move datafiles around
getwd()
list.files()

# Result Object
exRes <- PM_load(1)

# Create a PM_result object by reading a run folder.  The "1" in the parentheses tells Pmetrics to
# look in the /1 folder.

# Plot the raw data using R6 with various options.  Type ?plot.PMmatrix in the R console for help.
exRes$data$plot()
exRes$data$plot(overlay = FALSE, xlim = c(119, 145))

# The following are the older S3 method with plot(...) for the first two examples
# You can use R6 or S3 for any Pmetrics object
# We will focus on R6 as the more modern way.
plot(exRes$data)
plot(exRes$data, xlim = c(119, 146), marker = list(color = "blue"))

# here's a summary of the original data file; ?summary.PMmatrix for help
exRes$data$summary()

# Plot some observed vs. predicted data.  Type ?plot.PMop in the R console for help.
exRes$op$plot()
exRes$op$plot(pred.type = "pop")
exRes$op$plot(line = list(lm = list(ci = 0, color = "red"), loess = FALSE))

# The OP plot can be disaggregated into a Tidy compatible format using the $data attribute (see https://www.tidyverse.org/)
library(tidyverse)
exRes$op$data %>% plot()
exRes$op$data %>%
  filter(pred > 5) %>%
  filter(pred < 10) %>%
  plot()

# the original op object data can be accessed via
exRes$op$data

# see a header with the first 10 rows of the op object
head(exRes$op$data, 10)

# get a summary with bias and imprecision of the population predictions;
# ?summary.PMop for help
exRes$op$summary(pred.type = "pop")

# the S3 way
summary(exRes$op, pred.type = "pop")

# look at the summary for the posterior predictions (default pred.type) based
# on means of parameter values
exRes$op$summary(icen = "mean")


# Plot final population joint density information.  Type ?plot.PMfinal in the R console for help.
exRes$final$plot()

# add a kernel density curve
exRes$final$plot(density = TRUE)

# A bivariate plot. Plotting formulae in R are of the form 'y~x'
exRes$final$plot(Ke ~ V,
  marker = list(color = "red", symbol = "diamond"),
  line = list(color = "purple", dash = "dash", width = 2)
)


# or the S3 way
plot(exRes$final)

# The original final object can be accessed using
exRes$final$data
names(exRes$final$data)

# see the population points
exRes$final$popPoints

# or
exRes$final$data$popPoints

# see the population mean parameter values
exRes$final$popMean

# see a summary with confidence intervals around the medians
# and the Median Absolute Weighted Difference (MAWD);
# ?summary.PMfinal for help
exRes$final$summary()

# Plot cycle information
# Type ?plot.PM_cycle in the R console for help.
exRes$cycle$plot()

# names of the cycle object; ?makeCycle for help
names(exRes$cycle$data)

# gamma/lamda value on last 6 cycles
tail(exRes$cycle$data$gamlam)

# Plot covariate information.  Type ?plot.PMcov in the R console for help.
# Recall that plotting formulae in R are of the form 'y~x'
exRes$cov$plot(V ~ wt)
exRes$cov$data %>% plot(V ~ wt)
exRes$cov$data %>%
  filter(age > 25) %>%
  plot(V ~ wt)

#will shortly be updated to plotly
exRes$cov$plot(Ke ~ age, lowess = FALSE, reg = TRUE, pch = 3)

# Same plot but with mean Bayesian posterior parameter and covariate values...
# Remember the 'icen' argument?
exRes$cov$plot(V ~ wt, icen = "mean")

# When time is the x variable, the y variable is aggregated by subject.
# In R plot formulae, calculations on the fly can be included using the I() function
exRes$cov$plot(I(V * wt) ~ time)

# The previous cov object can be seen via:
exRes$cov

# but to access individual elements, use:
exRes$cov$data[, 1:3] # for example
names(exRes$cov)

# summarize with mean covariates; ?summary.PMcov for help
exRes$cov$summary(icen = "mean")


# Look at all possible covariate-parameter relationships by multiple linear regression with forward
# and backward elimination - type ?PMstep in the R console for help.
exRes$step()
# icen works here too....
exRes$step(icen = "median")
# forward elimination only
exRes$step(direction = "forward")


# EXERCISE 2 - NPAG WITH COVARIATES ---------------------------------------

# Again, without copying files, let's create another run object, this time using
# a model that include covariates

# First clone mod1
mod2 <- mod1$clone()

# Then update it
mod2$update(list(
  pri = list(
    V0 = ab(30, 120),
    V = NULL
  ),
  sec = "V = V0*(WT/55)"
))
# we can also make a model object by loading a file
mod2b <- PM_model$new("../src/model2.txt")


exFit2 <- PM_fit$new(data = exData, model = mod2)
# You can build the PM_fit object with file sources directly, but this means
# that you have to copy files to the working directory, or specify paths relative
# to the working directory as below
# exFit2 <- PM_fit$new(data = "../src/ex.csv", model = "../src/model2.txt")

exFit2$check()
exFit2$run()

list.files()
exRes2 <- PM_load(2)


# EXERCISE 3 - COMPARING MODELS -------------------------------------------


# Let's compare model 1 and model 2.   You can compare any number of models.
# Type ?PM_compare for help.
PM_compare(exRes, exRes2)



# EXERCISE 4 - MODEL VALIDATION -------------------------------------------

# MODEL VALIDATION EXAMPLES
# Example of Pmetrics visual predictive check and prediction-corrected visual predictive check
# for model validation - be sure to have executed the NPAG run above
# Type ?makeValid in the R console for help.
# Choose wt as the covariate to bin. Accept all default bin sizes.
valid_2 <- exRes2$validate(limits = c(0, 3))

# To see what it contains, use:
valid_2

# Default visual predictive check; ?plot.PM_valid for help
valid_2$plot()

# or old S3
plot(valid_2)

# or take advantage of the valid object being added automatically to the
# result object
exRes2$valid$plot()

# or S3
plot(exRes2$valid)

# Generate a prediction-corrected visual predictive check; type ?plot.PMvalid in the R console for help.
valid_2$plot(type = "pcvpc")

# Create an npde plot
valid_2$plot(type = "npde")

# Here is another way to generate a visual predicive check...
npc_2 <- valid_2$simdata$plot(obs = exRes2$op, log = FALSE, binSize = 1)

# The jagged appearance of the plot when binSize=0 is because different subjects have
# different doses, covariates, and observation times, which are all combined in one simulation.
# Collapsing simulation times within 1 hour bins (binSize=1) smooths
# the plot, but can change the P-values in the numerical predictive check below.

npc_2
# ...and here is a numerical predictive check
# P-values are binomial test of proportion of observations less than
# the respective quantile


# EXERCISE 5 - SIMULATOR RUN ----------------------------------------------

# The following will simulate 100 sets of parameters/concentrations using the
# first subject in the data file as a template.
# Limits are put on the simulated parameter ranges to be the same as in the model.
# The population parameter values from the NPAG run in exercise 2 are used for the Monte Carlo Simulation.
simdata <- exRes2$sim(include = 1, limits = NA, nsim = 100)


# simulate from a model with new data
sim_new <- exRes2$sim(
  data = "../src/ptaex1.csv",
  include = 2, limits = NA,
  predInt = c(120, 144, 0.5)
)

sim_new$plot(log = FALSE)

# Plot it; ?plot.PMsim for help
simdata$plot()

# Simulate using multiple subjects as templates
simdata <- exRes2$sim(include = 1:4, limits = NA, nsim = 100)

# Plot the third simulation
simdata$plot(at = 3)

# or in S3
plot(simdata$data[[3]])

# Parse and combine multiple files and plot them.  Note that combining simulations from templates
# with different simulated observation times can lead to unpredictable plots
simdata2 <- exRes2$sim(include = 1:4, limits = NA, nsim = 100, combine = TRUE)
simdata2$plot()

# simulate with covariates
# in this case we use the covariate-parameter correlations from run 2, which
# are found in the cov.2 object; we re-define the mean weight to be 50 with
# SD of 20, and limits of 10 to 70 kg.  We fix africa, gender and height covariates,
# but allow age (the last covariate) to be simulated, using the mean, sd, and
# limits in the original population, since we didn't specify them.
# See ?SIMrun for more help on this and the Pmetrics manual.

covariate <- list(
  cov = exRes2$cov,
  mean = list(wt = 50),
  sd = list(wt = 20),
  limits = list(wt = c(10, 70)),
  fix = c("africa", "gender", "height")
)

# now simulate with this covariate list object
simdata3 <- exRes2$sim(include = 1:4, limits = NA, nsim = 100, covariate = covariate)

# compare difference in simulations without covariates simulated...
# PM_simlist's plot function defaults to the first simulation
simdata$plot()

# ...and with covariates simulated
simdata3$plot()

# Here are the simulated parameters and covariates for the first subject's
# template; note that both wt and age are simulated, using proper covariances
# with simulated PK parameters
simdata3$data[[1]]$parValues

# look in the working directory and find the "c_simdata.csv" and "c_simmodel.txt" files
# which were made when you simulated with covariates.  Compare to original
# "simdata.csv" and "simmoddel.txt" files to note that simulated covariates become
# Primary block variables, and are removed from the template data file.

# EXERCISE 6 - SAVING PMETRICS OBJECTS ------------------------------------

# The following objects have methods to save them to or load them from files:
# PM_fit
# PM_result
# PM_sim
# PM_pta

# Example - save the PM_result (exRes2) to the "2" folder
exRes2$save("2/exres2.rds") # rds is the recommended file extension
list.files("2")
copy_exRes2 <- PM_result$load("2/exres2.rds")
copy_exRes2

# If you want to save multiple objects into one single file, R provides the
# following functionality

save(exFit, exData, mod1, exRes, simdata, file = "2/test_drug.Rdata")
list.files("2")
load("2/test_drug.Rdata")

# or
save.image("2/workspace.Rdata") # This will save all variables in your environment
list.files("2")
load("2/workspace.Rdata")


# EXERCISE 7 - CONTINUING RUNS OR EXTERNAL VALIDATIONS --------------------

# Example of a run with a non-uniform density
# This is a good way to continue a previous run,
# in this case it continues where run 1 left off
setwd(paste(wd, "/Runs", sep = ""))

# note that we can supply a run number to model, data, and prior arguments.  The numbers do not
# have to be the same.  This will copy the appropriate files from the specified run to be used
# in the current run.  By specifying a prior, we are starting with the non-uniform density from the
# end of the specified fun.
exFit2$run(prior = 2)
exRes3 <- PM_load(3)

# We could also generate Bayesian posterior parameter estimates for a new population this
# way, and with 0 cycles:
# exFit3 <- PM_fit(data=PM_data("newPop.csv"), mod2)
# exFit3$run(prior = 2, cycles = 0)
# This won't run because we don't have a newPop.csv file,
# but shows you how it could be done.



# EXERCISE 8 - EXAMPLE PARAMETRIC IT2B RUN --------------------------------

# IT2B is our parametric population parameter estimator.  Population parameter value
# distributions are estimated as means and covariances.

# EXAMPLE IT2B run - tlag, ka, kel, vol

# Run IT2B.  Type ?ITrun in the R console for help.
# Remember how we created a PM_fit object way back in exercise 1?
# Now we can use it again, but change the engine.
exFit$run(engine = "IT2B")

# Type ?PM_load in the R console for help on PM_load.
# If you have done extra runs, you may be at more than 4 by now.
# Check your working directory to see the highest folder number,
# and replace the 4 below with that number if necessary.
run4 <- PM_load(4)

# Most of the commands above work the same for IT2B but some produce different plots.

run4$final$plot()
# in the following plot, we standardize the x-scales to enable
# comparisons of the widths of the normal distributions of the
# parameter values
run4$final$plot(standardize = "all")
run4$final$plot(standardize = c("Ke", "Ka", "Tlag"))

# here's a bivariate plot of IT2B population parameter value distributions
run4$final$plot(Ke ~ V)


# EXERCISE 9 - PROBABILITY OF TARGET ATTAINMENT ---------------------------

# Note: these can be computationally intense and take some time.

# Examples of probability of target attainment analysis
# Be sure to have executed the NPAG run above and used PM_load(2) in EXERCISE 2
# Type ?SIMrun, ?SIMparse, ?makePTA, or ?plot.PMpta into the R console for help.


# simulate with the template data file that contains different doses
# Look at ?SIMrun for help on arguments to this function, including predInt,
# seed, limits, nsim.

simlist1 <- exRes2$sim(
  limits = c(0, 3), data = "../src/ptaex1.csv",
  predInt = c(120, 144, 0.5), seed = rep(-17, 4)
)

# now simulate with covariates; make sure that you defined the covariate
# object first in Exercise 5 above and have loaded the results of Exercise 2
# with PM_load(2)
simlist2 <- exRes2$sim(
  limits = 5, data = "../src/ptaex1.csv",
  predInt = c(120, 144, 0.5), seed = rep(-17, 4),
  covariate = covariate
)

# make the first PMpta object to calculate the time above each target for at
# least 60% of the dosing
# interval from 120 to 144 hours.  Include labels for the simulations.
# ?makePTA for help
# define simulation labels first
simlabels <- c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid")

pta1_2 <- PM_pta$new(
  simdata = simlist1,
  targets = c(0.25, 0.5, 1, 2, 4, 8, 16, 32), target.type = "time",
  success = 0.6, start = 120, end = 144
)

pta1b_2 <- PM_pta$new(
  simdata = simlist2,
  simlabels = simlabels,
  targets = c(0.25, 0.5, 1, 2, 4, 8, 16, 32), target.type = "time",
  success = 0.6, start = 120, end = 144
)

# summarize the results
pta1_2$summary()
pta1_2$summary(ci = 0.8)

# in the summary()$pta, simnum is the simulation (dose) number;
# target is the MIC; prop.success is the proportion of the simulated
# profiles for each dose/MIC that are above the success threshold (0.6); pdi.mean and pdi.sd
# are the mean and standard deviation of the pharmacodynamic index (PDI), in this case proportion of the interval > MIC.
# In the $pdi, target and simnum are the same, but now the median and confidence
# intervals (default 95%) PDI are shown.
# ?summary.PMpta for help

# Plot the first without covariates.   We didn't include simulation
# labels in the makePTA command, so generics are used here, but we move it to
# the bottom left; ?legend for help on arguments to supply to the
# legend list argument to plot.PMpta.
pta1_2$plot(ylab = "Proportion with %T>MIC of at least 60%", grid = TRUE, legend = list(x = "bottomleft"))
pta1b_2$summary()

# Plot the second with covariates simulated. Note the regimen labels are included, but we move
# the legend to the bottom left.
pta1b_2$plot(
  ylab = "Proportion with %T>MIC of at least 60%", grid = TRUE,
  legend = list(x = "bottomleft")
)

# Now we'll define success as free auc:mic > 100 with a free drug fraction of 50%
pta2_2 <- PM_pta$new(
  simdata = simlist2,
  simlabels = simlabels, targets = c(0.25, 0.5, 1, 2, 4, 8, 16, 32),
  free.fraction = 0.7,
  target.type = "auc", success = 100, start = 120, end = 144
)
summary(pta2_2)
pta2_2$plot(
  ylab = "Proportion with AUC/MIC of at least 100", grid = TRUE,
  legend = list(x = "bottomleft")
)

# success is Cmax/MIC >=10
pta3_2 <- PM_pta$new(
  simdata = simlist2,
  simlabels = simlabels,
  targets = c(0.25, 0.5, 1, 2, 4, 8, 16, 32),
  target.type = "peak", success = 10, start = 120, end = 144
)
pta3_2$summary()
pta3_2$plot(ylab = "Proportion with peak/MIC of at least 10", grid = TRUE)

# success = Cmin:MIC > 1
pta4_2 <- PM_pta$new(
  simdata = simlist2,
  simlabels = simlabels,
  targets = c(0.25, 0.5, 1, 2, 4, 8, 16, 32),
  target.type = "min", success = 1, start = 120, end = 144
)
pta4_2$summary()
pta4_2$plot(ylab = "Proportion with Cmin/MIC of at least 1", grid = TRUE, legend = list(x = "bottomleft"))

# now plot the PDI (pharmacodynamic index) of each regimen, rather than the proportion
# of successful profiles.  A PDI plot is always available for PMpta objects.
pta4_2$plot(type = "pdi", ylab = "Cmin:MIC", grid = TRUE)

# Each regimen has the 90% confidence interval PDI around the median curve,
# in the corresponding, semi-transparent color.  Make the CI much narrower...
pta4_2$plot(type = "pdi", ci = 0.1)

# ...or gone altogether, put back the grid, redefine the colors, and make lines narrower
pta4_2$plot(
  type = "pdi", ci = 0, grid = TRUE,
  line = list(
    color = c("blue", "purple", "black", "brown"),
    width = 1
  )
)

# now let's repeat the analysis but simulate the distribution of MICs
# using susceptibility of Staphylococcus aureus to vancomycin contained
# in the mic1 dataset within Pmetrics (?mic1)

# see the source with ?mic1
pta4b_2 <- PM_pta$new(
  simdata = simlist2,
  simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
  targets = makePTAtarget(mic1), target.type = "min", success = 1, start = 120, end = 144
)
# plot it
pta4b_2$plot(
  grid = TRUE, ylab = "Proportion with Cmin/MIC of at least 1",
  marker = list(color = "red"), line = list(color = "black")
)
pta4b_2$plot(type = "pdi", grid = TRUE, ylab = "Proportion with Cmin/MIC of at least 1")

# note that the plot changes since target MICs are no longer discrete
# since most of the MICs are very low, the regimens all look very similar

# success = concentration at time 3 hours:MIC > 2
pta5_2 <- PM_pta$new(
  simdata = simlist2,
  simlabels = simlabels,
  targets = c(0.25, 0.5, 1, 2, 4, 8, 16, 32), target.type = 123, success = 2, start = 120, end = 144
)
pta5_2$summary()
pta5_2$plot(ylab = "Proportion with C3/MIC of at least 1", grid = TRUE, legend = list(x = .3, y = 0.1))


# success is trough >10
pta6_2 <- PM_pta$new(
  simdata = simlist2,
  simlabels = simlabels,
  targets = 10, target.type = 144, success = 1, start = 120, end = 144
)
plot(pta6_2)
pta6_2$summary()

# EXERCISE 10 - OPTIMAL SAMPLE TIMES --------------------------------------


# calculate MM-optimal sample times for Run 2, and the 1200 mg once daily dose in the PTA
# By specifying the predInt to start and stop at 120 and 144 hours, with an interval of 1 hour,
# we are sampling at steady state.  Including "subject 2", means only the 1200 mg once daily dose
# will serve as a simulation template.

mmopt_2 <- exRes2$MM_opt(
  data = "../src/ptaex1.csv",
  nsamp = 2, predInt = c(120, 140, 1),
  include = 2
)
# see the optimal sample times and the Bayes Risk of misclassification,
# which is only useful to compare optimal sampling regimens, i.e. the
# absolute value is less helpful, but is the statistic minimized by the
# selected optimal sample times for a given model

mmopt_2

# plot it, with the red lines indicating the optimal sample times.
# see ?plot.MMopt for help

plot(mmopt_2)
plot(mmopt_2, line = list(color = "slategrey"), times = list(color = "orange"))

# EXERCISE 11 - ASSAY ERROR -----------------------------------------------
# see ?makeErrorPoly for more help
# This will let you choose the best set of C0, C1, C2, C3 for your modeling,
# based on assay validation data which includes the "obs", which are the
# nominal concentrations of the standards, and "sd", which is the standard
# deviation of replicate measurements of each of the standards, i.e. the
# inter-day and/or intra-day standard deviation

obs <- c(0, 25, 50, 100, 250, 500, 1000, 2000, 5000)
sd <- c(0.5, 6.4, 8.6, 12, 8.6, 37.2, 60.1, 165.7, 483)

# See plots.pdf, page 50
makeErrorPoly(obs = obs, sd = sd)

# choose the one with the best R-squared that will never result in a
# negative value for the SD



# Ancillary functions -----------------------------------------------------

# Be sure to check out the help files for the following functions:
#
# makeAUC() - calculate AUC from a variety of inputs
# makeNCA() - non-compartmental analysis
# NM2PM() - convert NONMEM data files to Pmetrics data files
# qgrowth() - CDC growth charts
# zBMI() - CDC Pediatric BMI z-scores and percentiles
# ss.PK() - sample size for Phase 1 PK studies
