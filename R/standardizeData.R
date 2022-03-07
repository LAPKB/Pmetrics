

standardize_data <- function(dataObj){
  
  dataNames <- names(dataObj)
  standardNames <- getFixedColNames()
  
  covNames <- dataNames[!dataNames %in% standardNames]
  if("date" %in% covNames){
    covNames <- covNames[-which(covNames == "date")]
  }
  
  mandatory <- c("id", "time", "dose", "out")
  missingMandatory <- sapply(mandatory, function(x) !x %in% dataNames)
  if(any(missingMandatory)){stop(paste0("Your data are missing these mandatory columns: ",mandatory[missingMandatory]))}
  
  msg <- ""
  
  if(!"evid" %in% dataNames){
    dataObj$evid <- ifelse(is.na(dataObj$dose),0,1)
    msg <- c(msg, "EVID inferred as 0 for observations, 1 for doses.\n")
  }
  
  if("date" %in% dataNames){
    
    relTime <- PMmatrixRelTime(dataObj)
    dataObj$time <- relTime$relTime
    dataObj <- dataObj %>% select(-date)
    msg <- c(msg, "Dates and clock times converted to relative decimal times.\n")
  }
  
  if(!"dur" %in% dataNames){
    dataObj$dur <- ifelse(is.na(dataObj$dose),NA,0)
    msg <- c(msg, "All doses assumed to be oral (DUR = 0).\n")
  }
  
  if(!"addl" %in% dataNames){
    dataObj$addl <- NA
    msg <- c(msg, "ADDL set to missing for all records.\n")
  }
  
  if(!"ii" %in% dataNames){
    dataObj$ii <- NA
    msg <- c(msg, "II set to missing for all records.\n")
  }
  
  if(!"input" %in% dataNames){
    dataObj$input <- ifelse(is.na(dataObj$dose),NA,1)
    msg <- c(msg, "All doses assumed to be INPUT = 1.\n")
  }
  
  if(!"outeq" %in% dataNames){
    dataObj$outeq <- ifelse(is.na(dataObj$out),NA,1)
    msg <- c(msg, "All observations assumed to be OUTEQ = 1.\n")
  }
  
  errorCoef <- c("c0", "c1", "c2", "c3")
  missingError <- sapply(errorCoef, function(x) !x %in% dataNames)
  if(any(missingError)){
    dataObj$c0 <- dataObj$c1 <- dataObj$c2 <- dataObj$c3 <- NA
    msg <- c(msg, "One or more error coefficients not specified. Error in model object will be used.\n")
  }
  
  dataObj <- dataObj %>% select(standardNames, all_of(covNames))
  cat(msg)

  
}