## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F, message=F-------------------------------------------------
library(Pmetrics)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  #Run 1 - ensure the data, model files are in the working directory
#  data1 <- PM_data$new("data.csv")
#  mod1 <- PM_model$new("model.txt")
#  fit1 <- PM_fit$new(data1, mod1)
#  fit1$run()
#  res1 <- PM_load(1) #PM_load is an alias for PM_result$new()
#  
#  #Run 2 - update Ke range
#  mod2 <- mod1$clone #create an independent copy
#  mod2$update(list(pri = list(Ke = ab(0.5, 3))))
#  fit2 <- PM_fit$new(data1, mod2)
#  fit2$run()
#  
#  #Run 3 - continue run 2
#  fit2$run(prior = 2)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  #Run 1 - ensure the data, model files are in the working directory
#  NPrun("data.csv", "model.txt")
#  PMload(1)
#  
#  #Run 2 - update Ke range - edit file outside R, copy to working directory
#  #use same data as for run 1
#  NPrun(data = 1, "model2.txt")
#  
#  #Run 3 - continue run 2
#  NPrun(data = 1, model = 1, prior = 2)

