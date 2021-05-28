library(R6)

pm_fit <- R6Class("pm_fit",
              public = list(
                data = NULL,
                model = NULL,
                initialize = function(data, model){
                  stopifnot(is.character(data), length(data) == 1)
                  stopifnot(inherits(model,"pm_model"))
                  self$data <- data
                  self$model <- model
                },
                print = function(...){
                  cat("Data:",self$data,"\nModel:",self$model$name)
                  invisible(self)
                }
              ))

pm_model <- R6Class("pm_model",
                    public = list(
                      name = NULL,
                      random = NULL,
                      fixed = NULL,
                      constant = NULL,
                      covariates = NULL,
                      library_model = NULL,
                      equations = NULL,
                      output = NULL,
                      error = NULL,
                      initialize = function(name, random, covariates){
                        self$name <- name
                        self$random <- random
                        self$covariates <- covariates
                      },
                      print = function(...){
                        rvNames <- names(self$random)
                        rvRanges <- self$random %>% as.character() %>% str_replace("c\\(","") %>%
                          str_replace("\\)","")
                        rvMean <- self$random %>% modify(mean)
                        rvSD <- self$random %>% modify(function(x) diff(x)/4)
                        rv <- self$random %>% 
                          glue_data("{rvNames}: Range {rvRanges}; Mean {rvMean}; SD {rvSD}")
                        cat("Random Variables\n________________\n\n",paste0(rv,sep="\n"))
                        
                        cov <- paste0(self$covariates, sep = "\n")
                        cat("\nCovariates\n________________\n\n",cov)
                        invisible(self)
                      }
                      
                    ))

mod1 <- pm_model$new(name = "drug A",
                     random = list(Ka = c(0,5),
                                   Ke = c(0,5),
                                   V = c(0,100)),
                     covariates = c("wt","crcl")
                     )

mod1
mod1$random



test <- pm_fit$new("data.csv",mod1)
test
test$model


