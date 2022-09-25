#' @title
#' Defines the PM_model class
#' 
#' @description
#' PM_model objects contain the variables, covariates, equations and error models
#' necessary to run a population analysis.
#' 
#' @details
#' PM_model objects are passed to \code{\link{PM_fit}} objects to initiate a
#' population analysis. The object is created by defining a list of lists
#' directly in R, or by reading a model text file in the current working directory.
#' 
#' @export
PM_model <- R6::R6Class("PM_Vmodel", 
                        public = list(
                          #' @description 
                          #' Build a new PM_model from a variety of inputs.
                          #' @param model This can be a quoted name of a model text file in the 
                          #' working directory which will be read and passed to Fortran engines unless
                          #' \code{julia = TRUE} in which case is will be passed as a Julia model.
                          #' It can be a list of lists that defines the model directly in R. Similarly,
                          #' it can be a function that defines a Julia model directly in R. See the user
                          #' manual for more help on directly defining models in R.
                          #' @param julia Controls whether a filename defines a Fortran or Julia model.
                          #' Default is \code{FALSE}, i.e. using Fortran.
                          #' @param ... Additional arguments passed to creation of new Julia model.
                          
                          #the following functions are dummy to permit documentation
                          new = function(model, ..., julia = F){
                            return(invisible())
                          },
                          #' @description 
                          #' Print a model object to the console in readable format
                          #' @param ... Not used currently.
                          print = function(...){
                            return(invisible())
                          },
                          #' @description 
                          #' Update selected elements of a model object
                          #' @param changes_list The named list containing elements and values to update.
                          #' Because R6 objects are mini-environments, using typical
                          #' R notation to copy an object like mod2 <- mod1 can lead to unexpected
                          #' results since this syntax simply creates a copied object in the same
                          #' environment. Therefore updating any one object (e.g., mod1 or mod2)
                          #' will update the other. To avoid this behavior, use the $clone() function 
                          #' first if you want to create a copied, yet independent new model.
                          #' @examples 
                          #' mod2 <- modEx$clone() #create an independent copy of modEx called mod2
                          #' mod2$update(list(
                          #'   pri = list(
                          #'    Ke = ab(0, 1), #change the range
                          #'    V = NULL, #this deletes the variable
                          #'    V0 = ab(10, 100) #add a new variable
                          #'   ),
                          #'   sec = "V = V0 * WT" #add a new secondary equation
                          #' )) 
                          #' #note that they are different now
                          #' mod2
                          #' modEx
                          update = function(changes_list){
                            return(invisible())
                          },
                          #' @description Write a `PM_model` object to a text file
                          #' @param model_path Full name of the file to be created, including the path
                          #' relative to the current working directory
                          #' @param engine Currently only "npag".
                          #' @examples 
                          #' \dontrun{
                          #' modEx$write("model.txt")
                          #' }
                          write = function(model_path = "genmodel.txt", engine = "npag"){
                            return(invisible())
                          }
                        )
)


#' @export
PM_model$new <- function(model, ..., julia = F) {
  # print(model)
  # Now we have multiple options for the model:
  # The model can be a String -> legacy run
  # The model can be a Function -> julia run
  # The model can be a String holding a julia function (julia = T) -> julia run
  if (is.function(model)) {
    return(PM_model_julia$new(model, ...))
  } else if (is.character(model) && length(model) == 1) {
    if (julia) {
      return(PM_model_julia$new(model, ...))
    } else {
      return(PM_model_file$new(model))
    }
  } else if (is.list(model)) {
    return(PM_model_list$new(model))
  } else {
    stop(sprintf("Non supported model type: %s", typeof(model)))
  }
}

#' @export
additive <- function(add, constant = F) {
  PM_Vinput$new(add, add, "additive", constant)
}

#' @export
proportional <- function(prop, constant = F) {
  PM_Vinput$new(prop, prop, "proportional", constant)
}

#' @export
combination <- function(add, prop, constant = F) {
  PM_Vinput$new(add, prop, "combination", constant)
}

#' @export
ab <- function(min, max, gtz = F) {
  PM_Vinput$new(min, max, "ab", constant = F, gtz)
}

#' @export
msd <- function(mean, sd, gtz = F) {
  PM_Vinput$new(mean, sd, "msd", constant = F, gtz)
}

#' @export
fixed <- function(fixed, constant = F, gtz = F) {
  PM_Vinput$new(fixed, fixed, "fixed", constant, gtz)
}

# PM_Vmodel ---------------------------------------------------------------


# Virtual Class
# it seems that protected does not exist in R
PM_Vmodel <- R6::R6Class("PM_model",
                         public = list(
                           name = NULL, # used by PM_model_legacy
                           # error = NULL,
                           initialize = function() {
                             stop("Unable to initialize abstract class")
                           },
                           print = function(...) {
                             cat("$model_list\n")
                             mlist <- self$model_list
                             blockNames <- names(mlist)
                             
                             #internal function to add space blocks
                             sp <- function(n){paste0(rep("   ",n), collapse="")}
                             
                             sapply(blockNames, function(x) {
                               if (x == "pri") {
                                 # cat("\t$pri\n")
                                 cat(sp(1),"$pri\n")
                                 for (i in 1:length(mlist$pri)) {
                                   thispri <- mlist$pri[[i]]
                                   thisname <- names(mlist$pri)[i]
                                   if(is.null(thispri$fixed)){
                                     cat(paste0(
                                       sp(2),"$", thisname, "\n",sp(3),"$min: ", round(thispri$min, 3),
                                       "\n",sp(3),"$max: ", round(thispri$max, 3),
                                       "\n",sp(3),"$mean: ", round(thispri$mean, 3),
                                       "\n",sp(3),"$sd: ", round(thispri$sd, 3),
                                       "\n",sp(3),"$gtz: ", thispri$gtz, "\n"
                                     ))
                                   } else {
                                     cat(paste0(
                                       sp(2),"$", thisname, "\n",sp(3),"$fixed: ", round(thispri$fixed, 3),
                                       "\n",sp(3),"$contant: ", thispri$constant,
                                       "\n"
                                     ))
                                   }
                                   
                                 }
                               } else if (x == "cov") {
                                 cat("\n",sp(1),"$cov\n", paste0(sp(2), "[", 1:length(mlist$cov), "] \"", mlist$cov, "\"", collapse = "\n "))
                                 cat("\n")
                               } else if (x == "sec") {
                                 cat("\n",sp(1),"$sec\n", paste0(sp(2), "[", 1:length(mlist$sec), "] \"", mlist$sec, "\"", collapse = "\n "))
                                 cat("\n")
                               } else if (x == "dif") {
                                 cat("\n",sp(1),"$dif\n", paste0(sp(2), "[", 1:length(mlist$dif), "] \"", mlist$dif, "\"", collapse = "\n "))
                                 cat("\n")
                               } else if (x == "lag") {
                                 cat("\n",sp(1),"$lag\n", paste0(sp(2), "[", 1:length(mlist$lag), "] \"", mlist$lag, "\"", collapse = "\n "))
                                 cat("\n")
                               } else if (x == "bol") {
                                 cat("\n",sp(1),"$bol\n", paste0(sp(2), "[", 1:length(mlist$bol), "] \"", mlist$bol, "\"", collapse = "\n "))
                                 cat("\n")
                               } else if (x == "fa") {
                                 cat("\n",sp(1),"$fa\n", paste0(sp(2), "[", 1:length(mlist$fa), "] \"", mlist$fa, "\"", collapse = "\n "))
                                 cat("\n")
                               } else if (x == "ini") {
                                 cat("\n",sp(1),"$ini\n", paste0(sp(2), "[", 1:length(mlist$ini), "] \"", mlist$ini, "\"", collapse = "\n "))
                                 cat("\n")
                               } else if (x == "out") {
                                 cat("\n",sp(1),"$out\n")
                                 for (i in 1:length(mlist$out)) {
                                   thisout <- mlist$out[[i]]
                                   cat(paste0(
                                     sp(2),"$Y", i,"\n",
                                     sp(3),"$val: \"", thisout[[1]], "\"\n",
                                     sp(3),"$err\n",
                                     sp(4),"$model\n",
                                     sp(5),"$additive: ", thisout$err$model$additive, "\n",
                                     sp(5),"$proportional: ", thisout$err$model$proportional, "\n",
                                     sp(5),"$constant: ", thisout$err$model$constant, "\n",
                                     sp(4),"$assay: ",
                                     paste0("[", 1:length(thisout$err$assay), "] ", thisout$err$assay, collapse = ", "),
                                     "\n"
                                   ))
                                 }
                                 cat("\n")
                               }
                             }) # end sapply
                             
                             invisible(self)
                           }
                         ),
                         private = list(
                           validate = function() {
                             # add checks here
                           }
                         )
)


# PM_input ----------------------------------------------------------------


# private classes
# TODO: Should I make these fields private?
PM_Vinput <- R6::R6Class(
  "PM_Vinput",
  public <- list(
    mode = NULL,
    min = NULL,
    max = NULL,
    mean = NULL,
    sd = NULL,
    fixed = NULL,
    constant = NULL,
    param = NULL, # is this used?
    additive = NULL,
    proportional = NULL,
    gtz = NULL,
    initialize = function(a, b, mode, constant = F, gtz = F) {
      stopifnot(mode %in% c("ab", "msd", "fixed", "additive", "proportional", "combination"))
      self$gtz <- gtz
      self$constant <- constant
      self$mode <- mode
      if (mode %in% c("ab")) {
        self$min <- a
        self$max <- b
        self$mean <- a + round((b - a) / 2, 3)
        self$sd <- round((b - a) / 6, 3)
      } else if (mode %in% c("msd")) {
        self$mean <- a
        self$sd <- b
        self$min <- a - 3 * b
        self$max <- a + 3 * b
      } else if (mode == "fixed") {
        self$fixed <- a
        # note that a fixed input with constant=T becomes a constant
      } else if (mode == "additive") {
        self$additive <- a
      } else if (mode == "proportional") {
        self$proportional <- a
      } else if (mode == "combination") {
        stop(sprintf("Combination error models are not supported yet"))
        self$additive <- a
        self$proportional <- b
      }
    },
    print_to = function(mode_not_used, engine) {
      # TODO:use mode and self$mode to translate to the right set of outputs
      if (engine == "npag" | engine == "it2b") {
        if (self$mode == "ab" | self$mode == "msd") {
          if (self$gtz) {
            return(sprintf("+%f, %f", self$mind, self$max))
          } else {
            return(sprintf("%f, %f", self$min, self$max))
          }
        } else if (self$mode == "fixed") {
          if (self$constant) {
            return(sprintf("%f!", self$fixed))
          } else {
            return(sprintf("%f", self$fixed))
          }
        } else if (self$mode == "additive") {
          if(engine == "it2b"){stop("Lambda is not defined in IT2B.")}
          if (self$constant) {
            return(sprintf("L=%f!", self$additive))
          } else {
            return(sprintf("L=%f", self$additive))
          }
        } else if (self$mode == "proportional") {
          if (self$constant) {
            return(sprintf("G=%f!", self$proportional))
          } else {
            return(sprintf("G=%f", self$proportional))
          }
        }
      } else if (engine == "rpem") {
        if (self$mode == "ab") {
          if (self$gtz) {
            return(sprintf("+%f, %f", self$min, self$max))
          } else {
            return(sprintf("%f, %f", self$min, self$max))
          }
        } else if (self$mode == "msd") {
          if (self$gtz) {
            return(sprintf("+%%%f, %f", self$mean, self$sd))
          } else {
            return(sprintf("%%%f, %f", self$mean, self$sd))
          }
        } else if (self$mode == "fixed") {
          if (self$constant) {
            return(sprintf("%f!", self$fixed))
          } else {
            return(sprintf("%f", self$fixed))
          }
        } else if (self$mode == "additive") {
          if (self$constant) {
            return(sprintf("L=%f!", self$additive))
          } else {
            return(sprintf("L=%f", self$additive))
          }
        } else if (self$mode == "proportional") {
          if (self$constant) {
            return(sprintf("G=%f!", self$proportional))
          } else {
            return(sprintf("G=%f", self$proportional))
          }
        }
      }
    }
  )
)




# PM_model_list -----------------------------------------------------------


PM_model_list <- R6::R6Class("PM_model_list",
                             inherit = PM_Vmodel,
                             public = list(
                               model_list = NULL,
                               initialize = function(model_list) {
                                 
                                 #guarantees primary keys are lowercase and max first 3 characters
                                 orig_names <- names(model_list) 
                                 names(model_list) <- private$lower3(names(model_list))
                                 model_blocks <- names(model_list)
                                 if(!identical(model_blocks, orig_names)) cat("Model block names standardized to 3 lowercase characters.\n")
                                 if(!"pri" %in% model_blocks) stop("Model must have a PRImary block.")
                                 if(!"out" %in% model_blocks) stop("Model must have an OUTput block.")
                                 n_out <- length(names(model_list$out))
                                 for(i in 1:n_out){
                                   out_names <- private$lower3(names(model_list$out[[i]]))
                                   names(model_list$out[[i]]) <- out_names
                                   if(!"err" %in% out_names){
                                     stop("Ensure all outputs have an ERRor block.")}
                                   if(!"model" %in% names(model_list$out[[i]]$err) ||
                                      !"assay" %in% names(model_list$out[[i]]$err)){
                                     stop("ERRor blocks need 'model' and 'assay' components.")
                                   }
                                   if(!"proportional" %in% names(model_list$out[[i]]$err$model) ||
                                      !"additive" %in% names(model_list$out[[i]]$err$model)){
                                     stop("ERRor model block must be either proportional or additive.")
                                   }
                                   
                                 }
                                 
                                 # stopifnot(
                                 #   "pri" %in% model_blocks,
                                 #   "out" %in% model_blocks,
                                 #   "err" %in% private$lower3(names(model_list$out[[1]])),
                                 #   "model" %in% private$lower3(names(model_list$out[[1]]$err)),
                                 #   "assay" %in% private$lower3(names(model_list$out[[1]]$err)),
                                 #   "proportional" %in% private$lower3(names(model_list$out[[1]]$err$model)) || 
                                 #     "additive" %in% private$lower3(names(model_list$out[[1]]$err$model))
                                 # )
                                 
                                 self$model_list <- model_list
                               },
                               write = function(model_path = "genmodel.txt", engine = "npag") {
                                 engine <- tolower(engine)
                                 keys <- names(self$model_list)
                                 lines <- c()
                                 for (i in 1:length(keys)) {
                                   lines <- private$write_block(lines, keys[i], self$model_list[[i]], engine)
                                 }
                                 fileConn <- file(model_path)
                                 writeLines(lines, fileConn)
                                 close(fileConn)
                                 
                                 return(model_path)
                               },
                               update = function(changes_list) {
                                 keys <- names(changes_list)
                                 stopifnot(private$lower3(keys) %in% c("pri", "sec", "dif", "ini", "cov", "out", "err", "fa")) # TODO: add all supported blocks
                                 self$model_list <- modifyList(self$model_list, changes_list)
                               }
                             ),
                             private = list(
                               lower3 = function(chr){
                                 purrr::map_chr(chr,function(x){
                                   substr(tolower(x),1,3)
                                 })
                               },
                               write_block = function(lines, key, block, engine) {
                                 if (private$lower3(key) == "fa") {
                                   key <- "f"
                                 }
                                 lines <- append(lines, sprintf("#%s", key))
                                 if (private$lower3(key) == "pri") {
                                   i <- 1
                                   for (param in names(block)) {
                                     lines <- append(
                                       lines,
                                       if (is.numeric(block[[i]])) {
                                         sprintf("%s, %f", param, block[[i]])
                                       } else {
                                         sprintf("%s, %s", param, block[[i]]$print_to("ab", engine))
                                       }
                                     )
                                     i <- i + 1
                                   }
                                 } else if (private$lower3(key) %in% c("cov", "bol", "lag", "ext")) {
                                   stopifnot(is.null(names(block)))
                                   for (i in 1:length(block)) {
                                     lines <- append(lines, sprintf("%s", block[[i]]))
                                   }
                                 } else if (private$lower3(key) %in% c("sec")) {
                                   names <- names(block)
                                   for (i in 1:length(block)) {
                                     key <- toupper(names[i])
                                     lines <- append(
                                       lines,
                                       if (is.null(names[i]) || nchar(names[i]) == 0) {
                                         sprintf("%s", block[[i]])
                                       } else {
                                         sprintf("%s=%s", key, block[[i]][1])
                                       }
                                     )
                                   }
                                 } else if (private$lower3(key) == "ini") {
                                   names <- names(block)
                                   for (i in 1:length(block)) {
                                     key <- toupper(names[i])
                                     lines <- append(
                                       lines,
                                       if (is.null(names[i]) || nchar(names[i]) == 0) {
                                         sprintf("%s", block[[i]][1])
                                       } else if (nchar(names[i]) == 2) {
                                         sprintf("%s(%s)=%s", substr(key, 1, 1), substr(key, 2, 2), block[[i]][1])
                                       } else {
                                         stop(sprintf("Error: Unsupported key named: %s", key))
                                       }
                                     )
                                   }
                                 } else if (private$lower3(key) %in% c("dif", "f")) {
                                   names <- names(block)
                                   for (i in 1:length(block)) {
                                     key <- toupper(names[i])
                                     lines <- append(
                                       lines,
                                       if (is.null(names[i]) || nchar(names[i]) == 0) {
                                         sprintf("%s", block[[i]][1])
                                       } else if (nchar(names[i]) == 3) {
                                         sprintf("%s(%s)=%s", substr(key, 1, 2), substr(key, 3, 3), block[[i]][1])
                                       } else {
                                         stop(sprintf("Error: Unsupported key named: %s", key))
                                       }
                                     )
                                   }
                                 } else if (private$lower3(key) == "out") {
                                   i <- 1 # keep track of the first outeq
                                   err_lines <- "#err"
                                   for (param in names(block)) {
                                     stopifnot(nchar(param) == 2 || nchar(param) == 0)
                                     key <- toupper(names(block)[i])
                                     lines <- append(
                                       lines,
                                       if (nchar(param) == 2) {
                                         sprintf("%s(%s)=%s", substr(key, 1, 1), substr(key, 2, 2), block[[i]][1])
                                       } else {
                                         sprintf("%s", block[[i]][1])
                                       }
                                     )
                                     err_block <- block[[i]]$err
                                     if (i == 1) {
                                       err_lines <- append(err_lines, err_block$model$print_to("ab", engine))
                                     }
                                     err_lines <- append(
                                       err_lines,
                                       sprintf(
                                         "%f,%f,%f,%f",
                                         err_block$assay[1],
                                         err_block$assay[2],
                                         err_block$assay[3],
                                         err_block$assay[4]
                                       )
                                     )
                                     i <- i + 1
                                   }
                                   lines <- append(lines, "")
                                   lines <- append(lines, err_lines)
                                 }
                                 
                                 else {
                                   stop(sprintf("Error: Unsupported block named: %s", key))
                                 }
                                 lines <- append(lines, "")
                                 return(lines)
                               }
                             )
)


# # PM_model_legacy ---------------------------------------------------------


# PM_model_legacy <- R6::R6Class("PM_model_legacy",
#   inherit = PM_Vmodel,
#   public = list(
#     legacy_file_path = NULL,
#     content = NULL,
#     name = NULL,
#     initialize = function(model_path) {
#       self$name <- basename(model_path)[1]
#       self$legacy_file_path <- model_path
#       self$content <- readChar(model_path, file.info(model_path)$size)
#     },
#     print = function() {}
#   )
# )

PM_model_file <- R6::R6Class("PM_model_file",
                             inherit = PM_model_list,
                             public = list(
                               content = NULL,
                               initialize = function(model_filename) {
                                 self$name <- basename(model_filename)[1]
                                 self$model_list <- private$makeR6model(model_filename)
                                 self$content <- readChar(model_filename, file.info(model_filename)$size)
                               }
                             ),
                             private = list(
                               makeR6model = function(file) {
                                 msg <- ""
                                 
                                 blocks <- parseBlocks(file) # this function is in PMutilities
                                 
                                 # check for reserved variable names
                                 reserved <- c(
                                   "ndim", "t", "x", "xp", "rpar", "ipar", "p", "r", "b", "npl", "numeqt", "ndrug", "nadd", "rateiv", "cv",
                                   "n", "nd", "ni", "nup", "nuic", "np", "nbcomp", "psym", "fa", "lag", "tin", "tout"
                                 )
                                 conflict <- c(match(tolower(blocks$primVar), reserved, nomatch = -99), match(tolower(blocks$secVar), reserved, nomatch = -99), match(tolower(blocks$covar), reserved, nomatch = -99))
                                 nconflict <- sum(conflict != -99)
                                 if (nconflict > 0) {
                                   msg <- paste("\n", paste(paste("'", reserved[conflict[conflict != -99]], "'", sep = ""), collapse = ", "), " ", c("is a", "are")[1 + as.numeric(nconflict > 1)], " reserved ", c("name", "names")[1 + as.numeric(nconflict > 1)], ", regardless of case.\nPlease choose non-reserved parameter/covariate names.\n", sep = "")
                                   return(list(status = -1, msg = msg))
                                 }
                                 
                                 if (length(grep(";", blocks$primVar)) > 0) {
                                   # using ';' as separator
                                   sep <- ";"
                                 } else {
                                   if (length(grep(",", blocks$primVar)) > 0) {
                                     # using ',' as separator
                                     sep <- ","
                                   } else {
                                     return(list(status = -1, msg = "\nPrimary variables should be defined as 'var,lower_val,upper_val' or 'var,fixed_val'.\n"))
                                   }
                                 }
                                 
                                 
                                 model_list <- list()
                                 # this function makes pri for PM_model
                                 model_list$pri <- sapply(strsplit(blocks$primVar, sep), function(x) {
                                   # find out if constrained to be positive
                                   const_pos <- any(grepl("\\+", x))
                                   if (const_pos) {
                                     x <- gsub("\\+", "", x)
                                     gtz <- T
                                     msg <- c(msg, "Truncating variables to positive ranges is not recommended.\n
               Consider log transformation instead.\n")
                                   } else {
                                     gtz <- F
                                   }
                                   
                                   # find out if constant
                                   const_var <- any(grepl("!", x))
                                   if (const_var) {
                                     x <- gsub("!", "", x)
                                   }
                                   
                                   values <- as.numeric(x[-1])
                                   
                                   if (length(x[-1]) == 1) { # fixed
                                     thisItem <- list(fixed(values[1], constant = const_var, gtz = gtz))
                                   } else { # range
                                     thisItem <- list(ab(values[1], values[2], gtz = gtz))
                                   }
                                   names(thisItem) <- x[1]
                                   thisItem
                                 }) # end sapply
                                 
                                 # covariates
                                 blocks$covar <- gsub("!", "", blocks$covar) # for now remove "!" indicating step, default interpolate
                                 if (blocks$covar[1] != "") {
                                   model_list$cov <- blocks$covar
                                 }
                                 
                                 # secondary variables
                                 if (blocks$secVar[1] != "") {
                                   model_list$sec <- blocks$secVar
                                 }
                                 
                                 # bioavailability
                                 if (blocks$f[1] != "") {
                                   model_list$fa <- blocks$f
                                 }
                                 
                                 # bolus
                                 if (blocks$bol[1] != "") {
                                   model_list$bol <- blocks$bol
                                 }
                                 
                                 # initial conditions
                                 if (blocks$ini[1] != "") {
                                   model_list$ini <- blocks$ini
                                 }
                                 
                                 # lag time
                                 if (blocks$lag[1] != "") {
                                   model_list$lag <- blocks$lag
                                 }
                                 
                                 # differential equations
                                 if (blocks$diffeq[1] != "") {
                                   model_list$dif <- blocks$diffeq
                                 }
                                 
                                 # out/err
                                 n_outputLines <- length(blocks$output)
                                 outputLines <- grep("Y\\([[:digit:]]+\\)", blocks$output)
                                 if (length(outputLines) == 0) {
                                   return(list(status = -1, msg = "\nYou must have at least one output equation of the form 'Y(1) = ...'\n"))
                                 }
                                 otherLines <- (1:n_outputLines)[!(1:n_outputLines) %in% outputLines] #find other lines
                                 if(length(otherLines)>0){
                                   model_list$sec <- c(model_list$sec, blocks$output[otherLines]) #append to #sec block
                                 }
                                 output <- blocks$output[outputLines]
                                 remParen <- stringr::str_replace(output, "Y\\((\\d+)\\)", "Y\\1")
                                 diffeq <- stringr::str_split(remParen, "\\s*=\\s*")
                                 diffList <- sapply(diffeq, function(x) x[2])
                                 num_out <- length(diffList)
                                 
                                 err <- tolower(gsub("[[:space:]]", "", blocks$error))
                                 gamma <- grepl("^g", err[1])
                                 const_gamlam <- grepl("!", err[1])
                                 gamlam_value <- as.numeric(stringr::str_match(err[1], "\\d+.*"))
                                 
                                 out <- list()
                                 for (i in 1:num_out) {
                                   out[[i]] <- list(
                                     val = diffList[i],
                                     err = list(
                                       model = if ((1 + as.numeric(gamma)) == 1) {
                                         additive(gamlam_value, constant = const_gamlam)
                                       } else {
                                         proportional(gamlam_value, constant = const_gamlam)
                                       },
                                       assay = stringr::str_split(err[i + 1], ",")[[1]] %>% as.numeric()
                                     )
                                   )
                                 }
                                 names(out) <- sapply(diffeq, function(x) x[1])
                                 model_list$out <- out
                                 
                                 cat(msg)
                                 flush.console()
                                 
                                 return(model_list)
                               }
                             ) # end private list
)


# PM_model_julia ----------------------------------------------------------


PM_model_julia <- R6::R6Class("PM_model_julia",
                              inherit = PM_Vmodel,
                              public = list(
                                model_function = NULL,
                                # prior:  created based on user input that needs to include possible values
                                # for means, SDs, mins, maxes, and initial support points (which could be a function)
                                min = NULL, # this will be folded into prior bin
                                max = NULL, # this will be folded into prior bin
                                n_points0 = NULL, # this will be folded into prior bin
                                initialize = function(model, ...) {
                                  dots <- list(...)
                                  if (!exists("max", where = dots) || !exists("min", where = sdots)) {
                                    stop("Error: Running using the Julia solver requires sufficient information to create a prior, e.g. min, max or mean/SD.")
                                  }
                                  self$min <- dots$min
                                  self$max <- dots$max
                                  self$error <- if (is.null(dots$error)) c(0.1, 0.01, 0) else dots$error # will need dynamic function to detect poisson, etc.
                                  self$n_points0 <- if (is.null(dots$n_points0)) 100 else dots$n_points0
                                  if (is.function(model)) {
                                    private$julia_type <- "function"
                                    self$name <- "Dyn function(...){...}"
                                    self$model_function <- model
                                  } else {
                                    private$julia_type <- "Str function"
                                    self$name <- "Str function(...){...}"
                                  }
                                },
                                print = function() {}
                              ),
                              private = list(
                                julia_type = NULL
                              )
)


# Examples ----------------------------------------------------------------

# simple_model <- PM_model(list(
#   pri=list(
#     Ke=ab(0.001,2,gtz=F),
#     V=msd(50, 250)
#   ),
#   out=list(
#     y1=list(
#       "X(1)/V",
#       err=list(
#         model= proportional(1,fixed=T),
#         assay=c(0,0.1,0,0)
#       )
#     )
#   )
# ))

# simple_model$update(list(
#   pri = list(
#     Ke = ab(0,1)
#   )
# ))


# full_model <- PM_model(list(
#     pri=list(
#         ke=ab(0.001,2),
#         V=ab(50, 250),
#         ka=fixed(5),
#         Kcp=ab(0.01, 10, gtz=F),
#         Kpc=5
#         # alpha = msd(0,0.3, gtz=F)
#     ),
#     ini= list("X(1)=0",x2="0"),
#     #default gtz=T
#     dif=list(
#         xp1="-ke*X(1)",
#         "XP(2)=ke*X(1)"
#     ),
#     out=list(
#         y1=list(
#             "X(1)/V",
#             err=list(
#                 model=list(
#                     additive=0#Y=Y+ASSAY   SD=?  Yobs=Ypred+E1+ASSAY(Yobs)
#                     # proportional=fixed(1)#Y=Y+Y*ASSAY     Yobs=Ypred*(1+E2)+ASSAY(Yobs)
#                     #Combination: Y=Y+ASSAY+Y*ASSAY Yobs=Ypred*(1+E2)+E1+ASSAY(Yobs)
#                 ),
#                 assay=c(0,0.1,0,0)
#             )
#         ),
#         y2=list(
#             "X(2)/V"
#         ),
#         "Y(3)=X(3)/V"
#     )
# ))
# 600
# # # ke=abs(0.001,2),
# # # V=meansd(50, 250)
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