# library(R6)
# library(JuliaCall)
# j <- julia_setup()
# julia_library("npag")
# j$library("npag")
#public classes

#may need to separate out error model as separate, e.g. need data, model, error to run
#' @export
PM_fit <- R6Class("PM_fit", 
    public = list(
        initialize = function(data, model, ...){
            stopifnot(is.character(data), length(data) == 1)
            private$data = data
            private$model <- if(inherits(model, "PM_Vmodel")) model else PM_model(model, ...)
            stopifnot(inherits(private$model, "PM_Vmodel"))
        },
        run = function(){
            if (inherits(private$model, "PM_model_legacy")) {
                cat(sprintf("Runing Legacy: %s-%s\n", private$data, private$model$name))
                Pmetrics::NPrun(private$model$legacy_file_path, private$data)
            } else if(inherits(private$model, "PM_model_list")) {
                model_path <-private$model$write_model_file()
                cat(sprintf("Creating model file at: %s\n", model_path))
                Pmetrics::NPrun(model_path, private$data)
            } else if(inherits(private$model, "PM_model_julia")){
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
        engine = "NPAG" #eventually should be public
    )
)

#Factory pattern
#' @export
PM_model <- function(model, ..., julia = F){
    #Now we have multiple options for the model:
    #The model can be a String -> legacy run
    #The model can be a Function -> julia run
    #The model can be a String holding a julia function (julia = T) -> julia run
    if (is.function(model)) {
        return(PM_model_julia$new(model,...))
    } else if(is.character(model) && length(model) == 1){
        if(julia){
            return(PM_model_julia$new(model,...))
        } else {
            return(PM_model_legacy$new(model))
        }
    
    } else if(is.list(model)){
        return(PM_model_list$new(model))
    } else{
        stop(sprintf("Non supported model type: %s", typeof(model)))
    }
   
            
}

#private classes

#Virtual Class
#it seems that protected does not exist in R
PM_Vmodel <- R6Class("PM_Vmodel", 
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
PM_model_list <- R6Class("PM_model_list",
    inherit = PM_Vmodel,
    public = list(
        model_list = NULL,
        initialize = function(model_list){
            names(model_list) = lapply(names(model_list), tolower)
            #Should I guarantee that all the keys are only 3 characters long?
            stopifnot(
                "pri" %in% names(model_list),
                "out" %in% names(model_list),
                "err" %in% names(model_list),
                "L" %in% names(model_list$err) || "G" %in% names(model_list$err)
            )

            self$model_list = model_list
        }, 
        write_model_file = function(){
            model_path<-"genmodel.txt"#paste(tempdir(),"model.txt", sep="/")
            keys <- names(self$model_list)
            lines <- c()
            for(i in 1:length(keys)){
                lines<-private$write_block(lines,keys[i],self$model_list[[i]])
            }
            fileConn<-file(model_path)
            writeLines(lines, fileConn)
            close(fileConn)

            return(model_path)
        }
        
    ),
    private = list(
        write_block = function(lines, key, block){
            lines<-append(lines,sprintf("#%s", key))
            if(key == "pri"){
                i<-1
                for(param in names(block)){
                    lines<-append(lines,sprintf("%s, %f, %f", param, block[[i]][1], block[[i]][2]))
                    i<-i+1
                }
            } else if(key == "out"){
                for(out in block){
                    lines<-append(lines,out)
                }
            } else if(key == "err"){
                lines<-append(lines,sprintf("%s=%f",names(block)[1], block[[1]]))
                lines<-append(lines,sprintf("%f,%f,%f,%f",block[[2]][1],block[[2]][2],block[[2]][3],block[[2]][4]))
            } else {
                stop(sprintf("Error: Unsupported block name: %s", key))
            }
            lines<-append(lines,"")
            return(lines)
        }
    )
)
PM_model_legacy <- R6Class("PM_model_legacy", 
    inherit = PM_Vmodel,
    public = list(
        legacy_file_path = NULL,
        initialize = function(model_path){
            self$name <- basename(model_path)[1]
            self$legacy_file_path <- model_path
        },
        print = function(){}
    )
)
PM_model_julia <- R6Class("PM_model_julia", 
    inherit = PM_Vmodel,
    public = list(
        model_function = NULL,
        #prior:  created based on user input that needs to include possible values
        #for means, SDs, mins, maxes, and initial support points (which could be a function)
        min = NULL, #this will be folded into prior bin
        max = NULL, #this will be folded into prior bin
        n_points0 = NULL, #this will be folded into prior bin
        initialize = function(model, ...){
            dots = list(...)
            if(!exists("max", where = dots) || !exists("min", where = sdots)){
                stop("Error: Running using the Julia solver requires sufficient information to create a prior, e.g. min, max or mean/SD.")
            }
            self$min <- dots$min
            self$max <- dots$max
            self$error <- if(is.null(dots$error)) c(0.1, 0.01, 0) else dots$error #will need dynamic function to detect poisson, etc.
            self$n_points0 <- if(is.null(dots$n_points0)) 100 else dots$n_points0
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

# #Examples
# model <- list(
#     pri=list(
#         ke=c(0.001,2),
#         V=c(50, 250)
#     ),
#     out=c("Y(1)=x(1)/V"),
#     err=list(
#         L=0,
#         coeff=c(0,0.1,0,0)
#     )

# )
# bimodal_ke <- PM_fit$new("data.csv", model)

# bimodal_ke$run()











































# setwd("~/Desktop/simulaciones")
# #examples


# run1 <- PM.fit$new("data.csv","model.txt")
# run1$run()
# #run1$load()





# f_model<-function(theta){
#     k =theta[1]
#     v= theta[2]
#     f<-function(u,p,t){-k * u}
#     u0 = 20/v
#     return(c(f,u0))
# }
# a = c(0.4, 0.4)
# b = c(2, 2)
# n_theta0 = 2130
# c0 = 0
# c1 = 0
# c2 = 0.5
# run2 <- PM.fit$new("/Users/julianotalvaro/Dev/NPAGjl/data/example_data.csv",
#                     f_model, min=a, max=b, error=c(c0,c1,c2), n_theta0=n_theta0)
# run2$run()


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


