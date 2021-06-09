library(R6)
library(JuliaCall)
j<-julia_setup()
# julia_library("npag")
j$library("npag")
#public classes
PM.fit <- R6Class("PM.fit", 
    public = list(
        initialize = function(data, model, ...){
            stopifnot(is.character(data), length(data) == 1)
            private$data = data
            private$model <- if(inherits(model, "PM._model")) model else PM.model(model, ...)
            stopifnot(inherits(private$model, "PM._model"))
        },
        run = function(){
            if (inherits(private$model, "PM.model_legacy")) {
                cat(sprintf("Runing Legacy: %s-%s\n", private$data, private$model$name))
                Pmetrics::NPrun(private$model$legacy_file_path, private$data)
            } else if(inherits(private$model, "PM.model_julia")){
                cat(sprintf("Runing Julia: %s-%s\n", private$data, private$model$name))
                return(
                    julia_call("npag.run",
                        private$model$model_function,
                        private$data,
                        private$model$min,
                        private$model$max,
                        private$model$error[1],
                        private$model$error[2],
                        private$model$error[3],
                        private$model$n_theta0)
                )
            }
        }
    ),
    private = list(
        data = NULL,
        model = NULL,
        method = "NPAG"
    )
)

#Factory pattern
PM.model <- function(model,...,julia=F){
    #Now we have multiple options for the model:
    #The model can be a String -> legacy run
    #The model can be a Function -> julia run
    #The model can be a String holding a julia function -> julia run
    if (is.function(model)) {
        return(PM.model_julia$new(model,...))
    } else if(is.character(model) && length(model) == 1){
        if(julia){
            return(PM.model_julia$new(model,...))
        } else {
            return(PM.model_legacy$new(model))
        }
        
    } else{
        stop(sprintf("Non supported model type: %s", typeof(model)))
    }
   
            
}

#private classes

#Virtual Class
#it seems that protected does not exist in R
PM._model <- R6Class("PM._model", 
    public = list(
        name = NULL,
        error = NULL,
        initialize = function() stop("Unable to initialize abstract class")
    ),
    private = list(
        random = NULL,
        fixed = NULL,
        constant = NULL,
        covariates = NULL,
        library_model = NULL,
        equations = NULL,
        output = NULL        
    )
)
PM.model_legacy <- R6Class("PM.model_legacy", 
    inherit = PM._model,
    public = list(
        legacy_file_path = NULL,
        initialize = function(model_path){
            self$name <- basename(model_path)[1]
            self$legacy_file_path <- model_path
        },
        print = function(){}
    )
)
PM.model_julia <- R6Class("PM.model_julia", 
    inherit = PM._model,
    public = list(
        model_function = NULL,
        min = NULL,
        max = NULL,
        n_theta0 = NULL,
        initialize = function(model, ...){
            dots = list(...)
            if(!exists("max", where=dots) || !exists("min", where=dots)){
                stop("Error: Running using the Julia solver requires at least the following arguments: max, min.")
            }
            self$min <- dots$min
            self$max <- dots$max
            self$error <- if(is.null(dots$error)) c(0.1, 0.01, 0) else dots$error
            self$n_theta0 <- if(is.null(dots$n_theta0)) 100 else dots$n_theta0
            if(is.function(model)){
                private$julia_type <- "function"
                self$name <- "Dyn function(...){...}"
                self$model_function <- model
            } else {
                private$julia_type <- "Str function"
                self$name <- "Str function(...){...}"
            }
        },
        print = function(){}
    ),
    private = list(
        julia_type = NULL
    )
)





setwd("~/Desktop/simulaciones")
#examples


run1 <- PM.fit$new("data.csv","model.txt")
run1$run()
#run1$load()





f_model<-function(theta){
    k =theta[1]
    v= theta[2]
    f<-function(u,p,t){-k * u}
    u0 = 20/v
    return(c(f,u0))
}
a = c(0.4, 0.4)
b = c(2, 2)
n_theta0 = 2130
c0 = 0
c1 = 0
c2 = 0.5
run2 <- PM.fit$new("/Users/julianotalvaro/Dev/NPAGjl/data/example_data.csv",
                    f_model, min=a, max=b, error=c(c0,c1,c2), n_theta0=n_theta0)
run2$run()


# run3 <- PM.fit$new("data.csv",function(a){a+2})





# run3$run()

# model<-function(theta){
#     k =theta[1]
#     v= theta[2]
#     f<-function(u,p,t){-k * u}
#     u0 = 20/v
#     return(c(f,u0))
# }
# pkdata_file = "/Users/julianotalvaro/Dev/NPAGjl/data/example_data.csv"
# a = c(0.4, 0.4)
# b = c(2, 2)
# n_theta0 = 2130
# c0 = 0
# c1 = 0
# c2 = 0.5
# a<-julia_call("npag.run", model, pkdata_file, a, b, c0, c1, c2, n_theta0)


# model = 1
#     pkdata_file = "/Users/julianotalvaro/Dev/NPAGjl/data/data_1comp_neely.csv"
#     a = c(0.000826,24.730583)
#     b = c(2.000000,388.075827)
#     c0 = 0
#     c1 = 0.1
#     c2 = 0
#     n_theta0 = 51
# a<-julia_call("npag.run", 1, pkdata_file, a, b, c0, c1, c2, n_theta0)

# # julia_eval("function model(theta)
# #         # Parameter definition
# #         k = theta[1]
# #         v = theta[2]
# #         # Model definition
# #         f(u, p, t) = -k * u
# #         u0 = 20 / v
# #         return((f, u0))
# #     end")


