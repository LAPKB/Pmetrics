.safeExecution <- function(exec_function, error_function = function(e) { e <- NULL; cat("Generic error") }) {
  return(suppressWarnings(tryCatch(exec_function, error = error_function)))
}

GenAlData <- function(wd) {
  checkRequiredPackages("jsonlite")
  setwd(wd)
  filename <- "alquimiaData.json"
  data <- data.frame("ver" = NA)
  errors <- c()

  if (file.exists(filename)) file.remove(filename)

  success <- file.info("NP_RF0001.TXT")$size >= 1000
  PMdata <- .safeExecution(NPparse())
  post <- .safeExecution(makePost(NPdata = PMdata))
  pop <- .safeExecution(makePop(NPdata = PMdata))
  op <- .safeExecution(makeOP(PMdata))
  # PMdata <- suppressWarnings(tryCatch(NPparse(), error = function(e) { e <- NULL; errors <- append(errors, "WARNING: The run did not complete successfully.") }))
  # post <- suppressWarnings(tryCatch(makePost(NPdata = PMdata), error = function(e) { e <- NULL; append(errors, "WARNING: error in extraction of posterior Bayesian predictions at time ttpred; 'PMpost' object not saved.") }))
  # pop <- suppressWarnings(tryCatch(makePop(NPdata = PMdata), error = function(e) { e <- NULL; append(errors, "WARNING: error in extraction of population predictions at time tpred; 'PMpop' object not saved.") }))
  # op <- suppressWarnings(tryCatch(makeOP(PMdata), error = function(e) { e <- NULL; append(errors, "WARNING: error in extraction of observed vs. population predicted data; 'PMop' object not saved.") }))

  data$sucess <- success
  data$par <- I(list(PMdata$par))
  data$corden <- I(list(PMdata$corden))

  exportJSON <- toJSON(data)
  write(exportJSON, filename)
}
