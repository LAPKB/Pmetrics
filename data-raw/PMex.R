#code to prepare example datasets for Pmetrics
#re-run before building package when datasets change
#edit DataDescriptions.R for documentation of datasets

library(Pmetrics)
library(usethis)

setwd("~/LAPK/Development/Pmetrics/data-raw")

#model file
modEx <- PM_model$new("model.txt")
usethis::use_data(modEx, overwrite = T)

#data
dataEx <- PM_data$new("ex.csv")
usethis::use_data(dataEx, overwrite = T)

#bad data
badData<- PM_data$new("bad.csv")
usethis::use_data(badData, overwrite = T)


setwd("Runs")
#NPAG
NPex <- PM_load(1)
usethis::use_data(NPex, overwrite = T)

#IT2B
ITex <- PM_load(2)
usethis::use_data(ITex, overwrite = T)

setwd("..")

#simulator example
simEx <- NPex$sim(
  limits = c(0, 3), data = "ptaex1.csv",
  predInt = c(120, 144, 0.5), seed = rep(-17, 4)
)
usethis::use_data(simEx, overwrite = T)

#NPex with valid field
setwd("Valid")
NPex$validate()
NPex_val <- NPex
usethis::use_data(NPex_val, overwrite = T)


