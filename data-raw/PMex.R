#code to prepare example datasets for Pmetrics
#re-run before building package when datasets change
#edit DataDescriptions.R for documentation of datasets

library(Pmetrics)
library(usethis)

setwd("~/LAPK/Development/Pmetrics/data-raw")

#model file
modEx <- PM_model$new("model.txt")
usethis::use_data(modEx)

#data
dataEx <- PM_data$new("ex.csv")
usethis::use_data(dataEx)

#bad data
badData<- PM_data$new("bad.csv")
usethis::use_data(badData)


setwd("Runs")
#NPAG
NPex <- PM_load(1)
usethis::use_data(NPex)

#IT2B
ITex <- PM_load(2)
usethis::use_data(ITex)

#simulator example
simEx <- PM_sim$load("sim.rds")
usethis::use_data(simEx)

#NPex with valid field
NPex_val <- PM_result$load("valid.rds")
usethis::use_data(NPex_val)


