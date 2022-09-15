#Use PMmanual() for help
library(Pmetrics)

#Run 1 - add your run description here

setwd("/Users/mneely/LAPK/Development/Pmetrics/docs/reference/DrugX/Runs")
#Ensure your model and data files are in the /Runs folder
#Make the data object
dat <- PM_data$new("[replace with your data file name]")
#Make the model object
mod1 <- PM_model$new("[replace with your model file name]")
#Make the fit object
fit1 <- PM_fit$new(dat, mod1)
#Run fit1
fit1$run()
#Load the results after it completes
run1 <- PM_load(1)

#Plots
run1$op$plot()
run1$op$plot(pred.type="pop")
run1$final$plot()

#Summaries
run1$op$summary()
run1$final$summary()
