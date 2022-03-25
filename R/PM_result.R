# PM_result ---------------------------------------------------------------

#' R6 object containing the results of a Pmetrics run
#'
PM_result <- R6::R6Class(
  "PM_result",
  public <- list(
    #' @field npdata List with all output from NPAG
    npdata = NULL,
    #' @field pop  NPAG only: Population predictions for each output equation
    pop = NULL,
    #' @field post  NPAG only: Individual posterior predictions for each output equation
    post = NULL,
    #' @field final Final cycle population support points and parameter summary statistics
    final = NULL,
    #' @field cycle Cycle log-likelihood, AIC, BIC, Gamma/lambda, and normalized parameter means, medians and SDs
    cycle = NULL,
    #' @field op List of observed vs. population and posterior predicted plots for each output equation
    op = NULL,
    #' @field cov Data frame of subject ID, covariate values, and Bayesian posterior parameter estimates
    cov = NULL,
    #' @field data \link{PM_data} object representing the original .csv data file used in the run
    data = NULL,
    #' @field model text string representing the original model file used in the run
    model = NULL,
    #' @field errfile Name of error file if it exists
    errfile = NULL,
    #' @field success Boolean if successful run
    success = NULL,
    #' @field valid If \code{\link{makeValid}} has been executed after a run, this object will be added to
    #' the save data.  It contains the information required to plot visual predictive checks and normalized prediction
    #' error discrepancies via the npde code developed by Comets et al
    valid = NULL,

    #' @description
    #' Create new object populated with data from previous run
    #' @details
    #' Creation of new \code{PM_result} objects is via
    #' \code{\link{PM_load}}
    #' @param out The parsed output from \code{\link{PM_load}}


    initialize = function(out, quiet = T) {
      self$npdata <- out$NPdata
      self$pop <- PM_pop$new(out$pop)
      self$post <- PM_post$new(out$post)
      self$final <- PM_final$new(out$final)
      self$cycle <- PM_cycle$new(out$cycle)
      self$op <- PM_op$new(out$op)
      self$cov <- PM_cov$new(out$cov)
      self$data <- PM_data$new(data = out$data, quiet = quiet) # no need to report
      self$model <- out$model
      self$errfile <- out$errfile
      self$success <- out$success
    },

    #' @description
    #' Plot generic function based on type
    #' @param type Type of plot based on class of object
    #' @param \dots Plot-specific arguments

    plot = function(type, ...) {
      stopifnot(!is.null(type), "please provide the type of plot you want to obtain")
      self[[type]]$plot(...)
    },

    #' @description
    #' Summary generic function based on type
    #' @param type Type of summary based on class of object
    #' @param \dots Summary-specific arguments

    summary = function(type, ...) {
      stopifnot(!is.null(type), "please provide the type of summary you want to obtain")
      self[[type]]$summary(...)
    },

    #' @description
    #' Simulates using the self$final object
    #' For parameter information refer to \code{\link{SIMrun}}

    sim = function(...) {
      PM_sim$new(self, ...)
    },
    #' @description
    #' Save the current PM_result object into a .rds file.
    #' @param file_name Name of the file to be created, the default is PMresult.rds
    save = function(file_name = "PMresult.rds") {
      saveRDS(self, file_name)
    },
    make_valid = function(...) {
      self$valid<-PM_valid$new(self, ...)
      self$valid
    },
    step = function(...){
      PMstep(self$cov$data, ...)
    }
  ) # end public
) # end PM_result

#' @description
#' Returns a PM_result object based on the information found in a specified rds file.
#' @param file_name Name of the file to be read, the default is PMresult.rds
PM_result$load <- function(file_name = "PMsim.rds") {
  readRDS(file_name)
}

PM_op <- R6Class(
  "PM_op",
  public <- list(
    id = NULL,
    time = NULL,
    obs = NULL,
    pred = NULL,
    pred.type = NULL,
    icen = NULL,
    outeq = NULL,
    block = NULL,
    obsSD = NULL,
    d = NULL,
    ds = NULL,
    wd = NULL,
    wds = NULL,
    initialize = function(op) {
      self$id <- op$id
      self$time <- op$time
      self$obs <- op$obs
      self$pred <- op$pred
      self$pred.type <- op$pred.typ
      self$icen <- op$icen
      self$outeq <- op$outeq
      self$block <- op$block
      self$obsSD <- op$obsSD
      self$d <- op$d
      self$ds <- op$ds
      self$wd <- op$wd
      self$wds <- op$wds
    },
    plot = function(...) {
      plot.PM_op(self$to_df(), ...)
    },
    summary = function(...) {
      summary.PMop(self$to_df(), ...)
    },
    #' @description
    #' AUC function
    #' @param \dots AUC-specific arguments
    auc = function() {
      makeAUC(data = self, ...)
    },
    to_df = function() {
      df<-list(
        id = self$id,
        time = self$time,
        obs = self$obs,
        pred = self$pred,
        pred.type = self$pred.type,
        icen = self$icen,
        outeq = self$outeq,
        block = self$block,
        obsSD = self$obsSD,
        d = self$d,
        ds = self$ds,
        wd = self$wd,
        wds = self$wds
      )%>%
      as.data.frame()
      class(df)<-c("PMop","data.frame")
      df
    }
  )
)

summary.PM_op <- function(obj, ...) {
  obj$summary(...)
}
PM_post <- R6Class(
  "PM_post",
  public <- list(
    id = NULL,
    time = NULL,
    ice = NULL,
    outeq = NULL,
    pred = NULL,
    block = NULL,
    initialize = function(post) {
      self$id <- post$id
      self$time <- post$time
      self$ice <- post$ice
      self$outeq <- post$outeq
      self$pred <- post$pred
      self$block <- post$block
    },
    #' @description
    #' AUC function
    #' @param \dots AUC-specific arguments
    auc = function() {
      makeAUC(data = self, ...)
    }
  )
)
PM_final <- R6Class(
  "PM_final",
  public <- list(
    popPoints = NULL,
    popMean = NULL,
    popSD = NULL,
    popCV = NULL,
    popVar = NULL,
    popCov = NULL,
    popCor = NULL,
    popMedian = NULL,
    popRanFix = NULL,
    postPoints = NULL,
    postMean = NULL,
    postSD = NULL,
    postVar = NULL,
    postCov = NULL,
    postCor = NULL,
    postMed = NULL,
    shrinkage = NULL,
    gridpts = NULL,
    nsub = NULL,
    ab = NULL,
    final = NULL,
    initialize = function(final) {
      self$popPoints <- final$popPoints
      self$popMean <- final$popMean
      self$popSD <- final$popSD
      self$popCV <- final$popCV
      self$popVar <- final$popVar
      self$popCov <- final$popCov
      self$popCor <- final$popCor
      self$popMedian <- final$popMedian
      self$popRanFix <- final$popRanFix
      self$postPoints <- final$postPoints
      self$postMean <- final$postMean
      self$postSD <- final$postSD
      self$postVar <- final$postVar
      self$postCov <- final$postCov
      self$postCor <- final$postCor
      self$postMed <- final$postMed
      self$shrinkage <- final$shrinkage
      self$gridpts <- final$gridpts
      self$nsub <- final$nsub
      self$ab <- final$ab
      self$final <- final
      class(self) <- c("NPAG", class(self))
    },
    summary = function(...) {
      summary.PMfinal(self, ...)
    },
    plot = function(...) {
      plot.PMfinal(self, ...)
    }
  )
)
#' @export
summary.PM_final <- function(obj, ...) {
  obj$summary(...)
}
PM_cycle <- R6Class(
  "PM_cycle",
  public <- list(
    names = NULL,
    cynum = NULL,
    ll = NULL,
    gamlam = NULL,
    mean = NULL,
    sd = NULL,
    median = NULL,
    aic = NULL,
    bic = NULL,
    initialize = function(cycle) {
      self$names <- cycle$names
      self$cynum <- cycle$cynum
      self$ll <- cycle$ll
      self$gamlam <- cycle$gamlam
      self$mean <- cycle$mean
      self$sd <- cycle$sd
      self$median <- cycle$median
      self$aic <- cycle$aic
      self$bic <- cycle$bic
    },
    plot = function(...) {
      plot.PMcycle(self, ...)
    }
  )
)
PM_pop <- R6Class(
  "PM_pop",
  public <- list(
    id = NULL,
    time = NULL,
    ice = NULL,
    outeq = NULL,
    pred = NULL,
    block = NULL,
    initialize = function(pop) {
      self$id <- pop$id
      self$time <- pop$time
      self$ice <- pop$ice
      self$outeq <- pop$outeq
      self$pred <- pop$pred
      self$block <- pop$block
    },
    #' @description
    #' AUC function
    #' @param \dots AUC-specific arguments
    auc = function() {
      makeAUC(data = self, ...)
    }
  )
)
PM_cov <- R6Class(
  "PM_cov",
  public <- list(
    data = NULL,
    initialize = function(cov) {
      self$data <- cov
    },
    summary = function(...) {
      summary.PMcov(self$data, ...)
    },
    plot = function(...) {
      plot.PMcov(self$data, ...)
    },
    print = function(...){
      print(x = self$data, ...)
    }
  )
)
#' @export
summary.PM_cov <- function(obj, ...) {
  obj$summary(...)
}