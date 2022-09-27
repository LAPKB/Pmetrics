## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F, message=F-------------------------------------------------
library(Pmetrics)
library(tidyverse)
library(plotly)

## ----echo = T, eval = F-------------------------------------------------------
#  setPMoptions(sep = ";", dec = ",")
#  #changes field separator to ";" from default ","
#  #and decimal separator from "." to ","

## ----echo=T, eval=FALSE-------------------------------------------------------
#  #assume that data.csv is in the working directory
#  data1 <- PM_data$new("data.csv")

## ----echo=T, eval=FALSE-------------------------------------------------------
#  #assume df is data frame with at least these columns:
#  #id, time, dose, out
#  data1 <- PM_data$new(df)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  #Run 1 - ensure that data.csv is in the working directory
#  NPrun("data.csv", "model.txt")
#  
#  #run 2 - use the data from run 1 in this run
#  #note that the file model.txt still has to be copied
#  # into the working directory in this example
#  NPrun(data = 1, "model.txt")

## ----echo=F, results='asis'---------------------------------------------------
library(knitr)
tab <- read.csv("Data/mdata.csv")
names(tab)[1] <- "#ID"
tab$OUT <- as.character(tab$OUT)
kable(tab)

## ----echo=F, results='asis'---------------------------------------------------
library(knitr)
tab <- read.csv("Data/RLcomp_data.csv")
kable(tab)

