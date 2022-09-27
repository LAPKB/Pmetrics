## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F, message=F-------------------------------------------------
library(Pmetrics)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  fit1 <- PM_fit$new(model, data)
#  fit1$run(options)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  run1 <- PM_load(1)
#  sim1 <- run1$sim(data = "new.csv")

## ----echo=T, eval=FALSE-------------------------------------------------------
#  sim1 <- PM_sim$run(poppar = list(...), model = "model.txt", data = "new.csv")

## ----echo=T, eval=FALSE-------------------------------------------------------
#  SIMrun(...)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  sim1 <- PM_result$sim(...)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  simdata <- SIMparse("simout1.txt")

## ----echo=T, eval=FALSE-------------------------------------------------------
#  sim.2 <- SIMparse("simout1.txt")
#  PMsave(2)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  res1 <- PM_load(1)
#  res1$op$plot()

## ----echo=T, eval=FALSE-------------------------------------------------------
#  sim1 <- PM_sim$load("sim.rds")

## ----echo=T, eval=F-----------------------------------------------------------
#  PMload(1)
#  plot(op.1)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  sim.2 <- SIMparse("simout1.txt")

## ----echo=F-------------------------------------------------------------------
data_comp <- read.csv("Data/RLcomp_data.csv",na.strings=".")
knitr::kable(data_comp)

## ----echo=F-------------------------------------------------------------------
mod_comp <- read.csv("Data/RLcomp_valid.csv",na.strings=".")
knitr::kable(mod_comp)

## ----echo=F-------------------------------------------------------------------
oth_comp <- read.csv("Data/RLcomp_other.csv",na.strings=".")
knitr::kable(oth_comp)

