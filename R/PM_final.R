# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 ----------------------------------------------------------------


#' @title Final Cycle Population Values
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains final cycle information from run.
#'
#' @details
#' The [PM_final] object is both a data field within a [PM_result], and itself an R6 object
#' comprising data fields and associated methods suitable for analysis and plotting of
#' final cycle parameters.
#'
#' Because [PM_final] objects are automatically added to the [PM_result] at the end of a
#' successful run, it is generally not necessary for users to generate [PM_final] objects
#' themselves.
#'
#' The main results are contained in the `$data` field,
#' and it is this field which is passed to the `$plot` and `$summary` methods.
#' You can use this `$data` field for custom manipulations, e.g. `probs <- run1$final$data$popPoints %>% select(prob)`.
#' This will select the probabilities of the support points.
#' If you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.
#'
#' To provide a more traditional experience in R,
#' the `$data` field is also separated by list items into the other data fields within the R6 object,
#' e.g. `popMean` or `nsub`. This
#' allows you to access them in an S3 way, e.g. `run1$final$popMean` if `run1` is a
#' [PM_result] object.
#'
#' @author Michael Neely, Julian Otalvaro
#' @export

PM_final <- R6::R6Class(
  "PM_final",
  public = list(
    #' @field data A list with the following elements, which can also be extracted by name.
    #' * **popPoints** (NPAG only) Data frame of the final cycle joint population density of grid points
    #' with column names equal to the name of each random parameter plus *prob* for the
    #' associated probability of that point
    #' * **popMean** The final cycle mean for each random parameter distribution
    #' * **popSD** The final cycle standard deviation for each random parameter distribution
    #' * **popCV** The final cycle coefficient of variation (SD/Mean) for each random parameter distribution
    #' * **popVar** The final cycle variance for each random parameter distribution
    #' * **popCov** The final cycle random parameter covariance matrix
    #' * **popCor** The final cycle random parameter correlation matrix
    #' * **popMed** The final cycle median values for each random parameter,
    #' i.e. those that have unknown mean and unknown variance, both of which are
    #' fitted during the run
    #' * **postPoints** (NPAG only) Data frame of posterior population points for each of the first 100 subject,
    #' with columns id, point, parameters and probability.  The first column is the subject, the second column has the population
    #' point number, followed by the values for the parameters in that point and the probability.
    #' * **postMean** A *nsub* x *npar* data frame containing
    #' the means of the posterior distributions for each parameter.
    #' * **postSD** A *nsub* x *npar* data frame containing
    #' the SDs of the posterior distributions for each parameter.
    #' * **postVar** A *nsub* x *npar* data frame containing
    #' the variances of the posterior distributions for each parameter.
    #' * **postCov** NPAG only: An list of length *nsub*, each element with an *npar* x *npar* data frame
    #' that contains the posterior parameter value covariances for that subject.
    #' * **postCor** NPAG only: An list of length *nsub*, each element with an *npar* x *npar* data frame
    #' that contains the posterior parameter value correlations for that subject.
    #' * **postMed** A *nsub* x *npar* data frame containing
    #' the medians of the posterior distributions for each parameter.
    #' * **shrinkage** A data frame with the shrinkage for each parameter.
    #' * **gridpts** (NPAG only) Initial number of support points
    #' * **nsub** Number of subjects
    #' * **ab** Tibble/data frame of boundaries for random parameter values with columns: name, lower, upper.
    #'
    data = NULL,
    #' @description
    #' Create new object populated with final cycle information
    #' @details
    #' Creation of new `PM_final` object is automatic and not generally necessary
    #' for the user to do.
    #' @param PMdata include `r template("PMdata")`.
    #' @param path include `r template("path")`.
    #' @param ... Not currently used.
    initialize = function(PMdata = NULL, path = ".", ...) {
      self$data <- private$make(PMdata, path)
      class(self) <- c(c("NPAG", "IT2B")[1 + as.numeric(is.null(self$popPoints))], class(self))
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_final].
    #' @param ... Arguments passed to [plot.PM_final]
    plot = function(...) {
      plot.PM_final(self, ...)
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PM_final].
    #' @param ... Arguments passed to [summary.PM_final]
    summary = function(...) {
      summary.PM_final(self, ...)
    }
  ), # end public
  active = list(
    #' @field popPoints (NPAG only) Data frame of the final cycle joint population density of grid points
    #'  with column names equal to the name of each random parameter plus *prob* for the
    #'  associated probability of that point
    popPoints = function() {
      self$data$popPoints
    },
    #' @field popMean The final cycle mean for each random parameter distribution
    popMean = function() {
      self$data$popMean
    },
    #' @field popSD The final cycle standard deviation for each random parameter distribution
    popSD = function() {
      self$data$popSD
    },
    #' @field popCV The final cycle coefficient of variation (SD/Mean) for each random parameter distribution
    popCV = function() {
      self$data$popCV
    },
    #' @field popVar The final cycle variance for each random parameter distribution
    popVar = function() {
      self$data$popVar
    },
    #' @field popCov The final cycle random parameter covariance matrix
    popCov = function() {
      self$data$popCov
    },
    #' @field popCor The final cycle random parameter correlation matrix
    popCor = function() {
      self$data$popCor
    },
    #' @field popMed The final cycle median values for each random parameter,
    #' i.e. those that have unknown mean and unknown variance, both of which are
    #' fitted during the run
    popMed = function() {
      self$data$popMed
    },
    #' @field postPoints (NPAG only) Data frame of posterior population points for each of the first 100 subject,
    #' with columns id, point, parameters and probability.  The first column is the subject, the second column has the population
    #' point number, followed by the values for the parameters in that point and the probability.
    postPoints = function() {
      self$data$postPoints
    },
    #' @field postMean A *nsub* x *npar* data frame containing
    #' the means of the posterior distributions for each parameter.
    postMean = function() {
      self$data$postMean
    },
    #' @field postSD A *nsub* x *npar* data frame containing
    #' the SDs of the posterior distributions for each parameter.
    postSD = function() {
      self$data$postSD
    },
    #' @field postVar A *nsub* x *npar* data frame containing
    #' the variances of the posterior distributions for each parameter.
    postVar = function() {
      self$data$postVar
    },
    #' @field postCov NPAG only: An list of length *nsub*, each element with an *npar* x *npar* data frame
    #' that contains the posterior parameter value covariances for that subject.
    postCov = function() {
      self$data$postCov
    },
    #' @field postCor NPAG only: An list of length *nsub*, each element with an *npar* x *npar* data frame
    #' that contains the posterior parameter value correlations for that subject.
    postCor = function() {
      self$data$postCor
    },
    #' @field postMed A *nsub* x *npar* data frame containing
    #' the medians of the posterior distributions for each parameter.*
    postMed = function() {
      self$data$postMed
    },
    #' @field shrinkage A data frame with the shrinkage for each parameter.
    #' The total population variance for a parameter
    #' is comprised of variance(EBE) plus average variance(EBD),
    #' where each subject's EBE is the Empirical Bayes Estimate or mean posterior value
    #' for the parameter. EBD is the Empirical Bayes Distribution, or
    #' the full Bayesian posterior parameter value distribution for each subject.
    #'
    #' The typical definition of \eqn{\eta} shrinkage is
    #' \eqn{[1 - \frac{SD(\eta)}{\omega}]} or \eqn{[1 - \frac{var(\eta)}{\omega^2}]},
    #' where \eqn{\eta} is the EBE and \eqn{\omega^2} is the population variance of \eqn{\eta}.
    #'
    #' In parametric modeling approaches \eqn{\eta} is the interindividual variability around
    #' the typical (mean) value of the parameter in the population, usually referred to
    #' as \eqn{\theta}. In nonparametric approaches, there is no assumption of normality, so
    #' \eqn{\eta} simply becomes each subject's mean parameter value estimate.
    #'
    #' Here is how Pmetrics derives and then calculates shrinkage for a given parameter.
    #' \deqn{popVar = var(EBE) + mean(var(EBD))}
    #' \deqn{1 = \frac{var(EBE)}{popVar} + \frac{mean(var(EBD)}{popVar}}
    #' \deqn{1 - \frac{var(EBE)}{popVar} = \frac{mean(var(EBD))}{popVar}}
    #' \deqn{shrinkage = \frac{mean(var(EBD))}{popVar}}
    #' Shrinkage is therefore a fraction between 0 and 1. If Bayesian posterior distributions are wide
    #' for a given parameter and \eqn{mean(var(EBD))} is high due to sparse or uninformative sampling,
    #' then most of the population variance is due
    #' to this variance and shrinkage is high, i.e., individual posterior estimates (EBE)
    #' shrink towards the population mean. Be aware, however, that a Bayesian posterior
    #' parameter value distribution for a given subject who is sparsely sampled may also
    #' be a single support point with no variance. Therefore EBD under nonparametric
    #' assumptions is not always large with uninformative sampling. This means that shrinkage
    #' is not as readily interpretable in nonparametric population modeling.
    #'
    #' An alternative is to consider the number of support points relative to the number
    #' of subjects. Highly informed, distinct subjects will result in the maximum possible
    #' number of support points, *N*, which is the same as the number of subjects.
    #' In contrast, badly undersampled subjects can result in only one support point.
    #' There is no formal criterion for this statistic, but it can be used in combination
    #' with shrinkage to assess the information content of the data.
    shrinkage = function() {
      self$data$shrinkage
    },
    #' @field gridpts (NPAG only) Initial number of support points
    gridpts = function() {
      self$data$gridpts
    },
    #' @field nsub Number of subjects
    nsub = function() {
      self$data$nsub
    },
    #' @field ab Matrix of boundaries for random parameter values
    ab = function() {
      self$data$ab
    }
  ), # end active
  private = list(
    make = function(data, path) {
      if (file.exists(file.path(path, "theta.csv"))) {
        theta <- readr::read_csv(file = file.path(path, "theta.csv"), show_col_types = FALSE)
      } else if (inherits(data, "PM_final") & !is.null(data$data)) { # file not there, and already PM_final
        class(data$data) <- c("PM_final_data", "list")
        return(data$data)
      } else {
        cli::cli_warn(c(
          "!" = "Unable to generate final cycle information.",
          "i" = "{.file {file.path(path, 'theta.csv')}} does not exist, and result does not have valid {.code PM_final} object."
        ))
        return(NULL)
      }
      
      if (file.exists(file.path(path, "posterior.csv"))) {
        post <- readr::read_csv(file = file.path(path, "posterior.csv"), show_col_types = FALSE)
      } else if (inherits(data, "PM_final") & !is.null(data$data)) { # file not there, and already PM_final
        class(data$data) <- c("PM_final_data", "data.frame")
        return(data$data)
      } else {
        cli::cli_warn(c(
          "!" = "Unable to generate final cycle information.",
          "i" = "{.file {file.path(path, 'posterior.csv')}} does not exist, and result does not have valid {.code PM_final} object."
        ))
        return(NULL)
      }
      
      if (file.exists(file.path(path, "settings.json"))) {
        config <- jsonlite::fromJSON(file.path(path, "settings.json"))
      } else if (inherits(data, "PM_final")) { # file not there, and already PM_final
        class(data$data) <- c("PM_final_data", "data.frame")
        return(data$data)
      } else {
        cli::cli_warn(c(
          "!" = "Unable to generate final cycle information.",
          "i" = "{.file {file.path(path, 'settings.json')}} does not exist, and result does not have valid {.code PM_final} object."
        ))
        return(NULL)
      }
      
      par_names <- names(theta)[names(theta) != "prob"]
      
      # Pop
      popMean <- theta %>%
      dplyr::summarize(across(.cols = -prob, .fns = function(x) {
        wtd.mean(x = x, weights = prob)
      }))
      
      popVar <- theta %>%
      dplyr::summarize(across(.cols = -prob, .fns = function(x) {
        wtd.var(x = x, weights = prob) # in PMutilities
      }))
      
      popSD <- sqrt(popVar)
      
      
      popCov <- stats::cov.wt(theta %>%
        select(-prob), theta$prob, cor = TRUE)$cov
        
        
        popCor <- stats::cov.wt(theta %>%
          select(-prob), theta$prob, cor = TRUE)$cor
          
          popMed <- theta %>%
          dplyr::summarize(across(-prob, \(x) wtd.quantile(x, prob, 0.5))) # in PMutilities
          
          # Posterior
          
          postMean <- post %>%
          group_by(id) %>%
          dplyr::summarize(across(.cols = -c(point, prob), .fns = function(x) {
            wtd.mean(x = x, weights = prob) # in PMutilities
          }))
          
          postVar <- post %>%
          group_by(id) %>%
          dplyr::summarize(across(.cols = -c(point, prob), .fns = function(x) {
            wtd.var(x = x, weights = prob) # in PMutilities
          }))
          
          postSD <- postVar %>%
          rowwise() %>%
          dplyr::summarize(across(.cols = -c(id), .fns = function(x) {
            sqrt(x)
          }))
          
          cov_cor_post <- post %>%
          split(post$id) %>%
          map(\(x){
            wt <- x$prob
            mat <- x %>% select(-c(id, point, prob))
            stats::cov.wt(mat, wt, cor = TRUE)
          })
          
          
          postCov <- cov_cor_post %>%
          map(\(x) as.data.frame(x$cov))
          
          postCor <- cov_cor_post %>%
          map(\(x) as.data.frame(x$cor))
          
          postMed <- post %>%
          group_by(id) %>%
          summarize(
            across(
              -c(point, prob),
              ~ suppressWarnings(wtd.quantile(.x, weights = prob, probs = 0.5, na.rm = TRUE)) # in PMutilities, from Hmisc package
            ),
            .groups = "drop"
          )
          
          # shrinkage
          varEBD <- postVar %>% summarize(across(-id, \(x) mean(x, na.rm = TRUE)))
          sh <- varEBD / popSD**2
          
          # ranges
          ab <- config$parameters[[1]] %>% tibble::as_tibble() %>% dplyr::rename(par = name, min = lower, max = upper)
          
          
          gridpts <- config$prior$Sobol[1]
          
          final <- list(
            popPoints = theta,
            postPoints = post,
            popMean = popMean,
            popSD = popSD,
            popCV = popSD / popMean,
            popVar = popVar,
            popCov = popCov,
            popCor = popCor,
            popMed = popMed,
            postMean = postMean,
            postSD = postSD,
            postVar = postVar,
            postMed = postMed,
            postCov = postCov,
            postCor = postCor,
            shrinkage = sh,
            gridpts = gridpts,
            nsub = length(unique(post$id)),
            ab = ab
          )
          class(final) <- c("PM_final_data", "NPAG", "list")
          
          return(final)
        }
      ) # end private
    )
    
    
    
    # PLOT --------------------------------------------------------------------
    #' @title Plot Pmetrics Final Cycle Parameter Value Distributions
    #' @description
    #' `r lifecycle::badge("stable")`
    #'
    #' Plot R6 [PM_final] objects loaded as a field in the
    #' [PM_result] object, e.g. `PM_result$final`.
    #'
    #' @details
    #' This is a function usually called by the `$plot()` method for [PM_final] objects
    #' within a [PM_result] to generate a plot the parameter value probability densities
    #' after completion of a model fitting.
    #' The function can be called directly on a [PM_final] object.
    #'
    #' If `formula` is omitted, this will generate a marginal plot for each parameter.
    #' For NPAG data, this will be a histogram of marginal values for each parameter and the associated probability
    #' of that value.  For IT2B, this will be a series of normal distributions with mean and standard deviation
    #' equal to the mean and standard deviation of each parameter marginal distribution.
    #'
    #' On the other hand, if `formula` is specified as two parameters, e.g. CL~V, this will generate a bivariate plot.
    #' For NPAG data, it will be support point with size proportional to the probability
    #' of each point.  For IT2B, it will be an elliptical distribution of a bivariate normal distribution centered at the mean
    #' of each plotted variable and surrounding quantiles of the bivariate distribution plotted in decreasing shades of grey.
    #' Mulitvariate normal density code is borrowed from the mvtnorm package.
    #' @method plot PM_final
    #' @param x The name of an [PM_final] data object as a field in a [PM_result] R6
    #' object, e.g `PM_result$final`.
    #' @param formula An optional formula of the form `y ~ x`, where `y` and `x`
    #' are two model parameters to plot in a 3-dimensional bivariate plot.  See details.
    #' @param marker See details for which objects the `marker` argument controls.
    #' This argument maps to the plotly marker object.
    #' It can be boolean or a list.
    #' `TRUE` will plot the marker with default characteristics.
    #' `FALSE` will suppress marker plotting.
    #' If a list, can control many marker characteristics, including overriding defaults.
    #' Use the plotly `plotly::schema()` command in the console and navigate
    #' to traces > scatter > attributes > marker to see all the ways the marker
    #' can be formatted. Most common will be:
    #' * `color` Fill color for NPAG bars, marker color for bivariate NPAG plots.
    #' Ignored for IT2B plots.
    #' * `symbol` Plotting character. See `plotly::schema()`, traces > scatter >
    #' attributes > marker > symbol > values. Only relevant for bivariate NPAG plots.
    #' * `size` Character size in points. Only relevant for bivariate NPAG plots.
    #' * `opacity` Bar fill color for univariate NPAG plots or marker opacity for
    #' bivariate NPAG plots. Ignored for IT2B plots.
    #' Ranges between 0 (fully transparent) to 1 (fully opaque).
    #' * `line` A list of  additional attributes governing the outline for bars in
    #' univariate NPAG plots or markers in bivariate NPAG plots. Ignored for IT2B plots.
    #'     - `color` Outline color. Default is "black".
    #'     - `width` Outline width. Default is 1.
    #' Example: `marker = list(color = "red", symbol = "triangle", opacity = 0.8, line = list(color = "black", width = 2))`
    #' @param line `r template("line")` The `line` argument is used to format:
    #' * the density line drawn over an NPAG [PM_final] object. Default is `FALSE`,
    #' which means no density line will be drawn. Use `TRUE` to draw the default line,
    #' which is black, solid, and of width 1, or specify as a list to control these
    #' elements, e.g. `line = list(color = "red", width = 2, dash = "dash")`
    #' * the drop lines from point to lower surface when a `formula` is specified
    #' to generate a bivariate plot from an NPAG [PM_final] object.
    #' In this case, default is `line = TRUE`. The default
    #' format is black, dashed, and of width 1.
    #' * the lines drawing the normal distribution of parameter values from an IT2B [PM_Final] object.
    #' Again, here the default is `line = TRUE`, and the format is black, solid, of width 1.
    #' See [density].  Ignored for IT2B output.
    #' @param grid `r template("grid")`
    #' @param xlab `r template("xlab")` Default is the name of the plotted x-variable.
    #' Formatting can be controlled, but the text is recommended to not be changed.
    #' @param ylab `r template("ylab")` Default is "Probability" for univariate plots,
    #' and the name of the plotted y-variable for bivariate plots.
    #' Formatting can be controlled, but the text is recommended to not be changed.
    #' @param zlab Only for bivariate plots. Default is "Probability". Can be a list
    #' to control formatting or default text, as for `xlab` and `zlab`.
    #' @param standardize Standardize the normal parameter distribution plots from IT2B to the same
    #' scale x-axis.  Ignored for NPAG output.
    #' @param legend Ignored for this plot.
    #' @param log `r template("log")`
    #' @param title `r template("title")` Default is to have no title on plots.
    #' @param xlim `r template("xlim")`
    #' @param ylim `r template("ylim")`
    #' @param print If `TRUE`, will print the plotly object and return it. If `FALSE`, will only return the plotly object.
    #' @param ... `r template("dotsPlotly")`
    #' @return Plots the object.
    #' @author Michael Neely
    #' @seealso [PM_final], [schema]
    #' @export
    #' @examples
    #' \dontrun{
    #' # NPAG
    #' NPex$final$plot()
    #' NPex$final$plot(line = TRUE)
    #' NPex$final$plot(Ke ~ V)
    #' # IT2B
    #' ITex$final$plot()
    #' ITex$final$plot(Ke ~ V)
    #' }
    
    #' @family PMplots
    
    plot.PM_final <- function(x,
      formula = NULL,
      line,
      marker = TRUE,
      standardize,
      legend,
      log,
      grid = TRUE,
      xlab, ylab, zlab,
      title,
      xlim, ylim,
      print = TRUE,
      ...) {
        # housekeeping
        
        # fixed for now
        static = FALSE
        
        if (inherits(x, "PM_final")) {
          data <- x$data
        } else {
          data <- x # PM_final_data
        }
        
        
        if (inherits(x, "NPAG")) {
          type <- "NPAG"
          
          if (missing(formula)) { # univariate
            if (missing(line)) {
              line <- amendLine(FALSE) # no density
              density <- FALSE
            } else {
              line <- amendLine(line, default = list(color = "black")) # density
              density <- TRUE
            }
          } else { # bivariate
            density <- FALSE
            if (missing(line)) {
              line <- amendLine(TRUE, default = list(color = "black", dash = "dash")) # bivariate, need drop lines
            } else {
              line <- amendLine(line, default = list(color = "black", dash = "dash"))
            }
          }
          
          spike <- list() # formatting for spikes 
          
          spike$head <- amendMarker(marker, default = list(
            color = "dodgerblue", size = 7,
            symbol = "circle", width = 1, opacity = 0.8
          ))
          
          spike$line <- list(color = spike$head$color, width = 3*spike$head$width)
          
          
        } else {
          type <- "IT2B"
          if (missing(line)) line <- TRUE
          line <- amendLine(line)
          bar <- NULL
        }
        
        
        # grab columns from formula, NULL if missing
        yCol <- tryCatch(as.character(attr(terms(formula), "variables")[2]),
        error = function(e) NULL)
        xCol <- tryCatch(as.character(attr(terms(formula), "variables")[3]),
        error = function(e) NULL)
        
        
        # unnecessary arguments
        if (!missing(legend)) {
          notNeeded("legend", "plot.PM_final") # in plotlyUtils.R
        }
        if (!missing(log)) {
          notNeeded("log", "plot.PM_final")
        }
        
        # process dots
        layout <- amendDots(list(...))
        
        
        # ranges
        ab <- data.frame(data$ab)
        # names(ab) <- c("min", "max")
        # ab$par <- names(data$popMean)
        
        # axis labels and title
        if (missing(xlab)) {
          xlab <- NULL
        }
        if (missing(ylab)) {
          ylab <- NULL
        }
        if (missing(zlab)) {
          zlab <- NULL
        }
        if (missing(title)) {
          title <- NULL
        }
        
        # choose the right plot
        if (is.null(yCol) || xCol == "prob") { # univariate or prob plot
          
          # NPAG
          if (type == "NPAG") {
            
            if (is.null(xCol)) { # regular marginal
              ab_alpha <- ab %>% arrange(par)
              p <- data$popPoints %>%
              pivot_longer(cols = !prob, names_to = "par") %>%
              dplyr::nest_by(par) %>%
              dplyr::full_join(ab_alpha, by = "par") %>%
              dplyr::mutate(panel = list(uniPlot(data, par, min, max,
                type = "NPAG",
                spike = spike, 
                xlab = xlab, ylab = ylab, title = title, 
                height = 1500, density = density, line = line, layout = layout
              ))) %>%
              plotly::subplot(margin = 0.02, nrows = nrow(.), titleX = TRUE, titleY = TRUE)
            } else { # prob plot
              ab_alpha <- ab %>% filter(par == yCol)
              p1 <- data$popPoints %>%
              tidyr::pivot_longer(cols = !prob, names_to = "par") %>%
              dplyr::filter(par == yCol) %>%
              dplyr::nest_by(par) %>%
              dplyr::full_join(ab_alpha, by = "par")
              
              p <- data$postPoints %>%
              select(id, point, value = !!yCol, prob) %>%
              nest(data = -id) %>%
              dplyr::mutate(panel = trelliscopejs::map_plot(data, \(x) uniPlot(x, yCol, ab_alpha$min, ab_alpha$max,
                type = "NPAG",
                spike = spike, xlab = xlab, ylab = ylab, title = title, .prior = p1$data[[1]], density = density, line = line, layout = layout
              ))) %>%
              trelliscopejs::trelliscope(name = "Posterior/Prior", self_contained = FALSE)
            }
          } else {
            
            # IT2B
            if (!missing(standardize)) { # standardize plot
              if (tolower(standardize[1]) == "all") {
                to_standardize <- 1:nrow(ab)
              } else {
                to_standardize <- which(names(data$popMean) %in% standardize)
              }
              if (length(to_standardize) > 0) {
                ab[to_standardize, 1] <- min(ab[to_standardize, 1])
                ab[to_standardize, 2] <- max(ab[to_standardize, 2])
              } else {
                cli::cli_abort(c("x" = "Requested standardization parameters are not in model."))
              }
            }
            ab_alpha <- ab %>% arrange(par)
            p <- data.frame(mean = purrr::as_vector(data$popMean), sd = purrr::as_vector(data$popSD), min = ab[, 1], max = ab[, 2]) %>%
            purrr::pmap(.f = function(mean, sd, min, max) {
              dplyr::tibble(
                value = seq(min, max, (max - min) / 1000),
                prob = dnorm(value, mean, sd)
              )
            }) %>%
            purrr::set_names(names(data$popMean)) %>%
            dplyr::bind_rows(.id = "par") %>%
            dplyr::nest_by(par) %>%
            dplyr::full_join(ab_alpha, by = "par") %>%
            dplyr::mutate(panel = list(uniPlot(data, par, min, max,
              type = "IT2B", xlab = xlab, ylab = ylab, title = title
            ))) %>%
            plotly::subplot(margin = 0.02, nrows = nrow(.), titleX = TRUE, titleY = TRUE)
          }
        } else { # bivariate
          p <- biPlot(xCol, yCol, ab, data, xlab, ylab, zlab, title, spike, layout, type = "NPAG")
        }
        if (print) print(p)
        return(invisible(p))
      }
      
      ####### plotting functions for plot.PM_final
      
      # plot functions for univariate
      uniPlot <- function(.data, .par, .min, .max, type, spike, xlab, ylab, title, .prior = NULL, height = NULL, density, line, layout) {
        .data$text_label <- glue::glue("{.par}: {round(.data$value,4)}\nProb: {round(.data$prob,4)}")
        
        p <- .data %>%
        plotly::plot_ly(x = ~value, y = ~prob, height = height, opacity = spike$head$opacity)
        
        if (type == "NPAG") {
          p <- p %>%
          plotly::add_markers(
            marker = spike$head,
            text = ~text_label,
            hoverinfo = "text"
          )
          p <- p %>%
          plotly::add_segments(
            xend = ~value, yend = 0,
            line = spike$line
          )
          
          
          
          if (!is.null(.prior)) {
            .prior$text_label <- glue::glue("{.par}: {round(.prior$value,4)}\nProb: {round(.prior$prob,4)}")
            spike2 <- spike
            spike2$head$color <- spike2$line$color <- "black"
            
            p <- p %>%
            plotly::add_markers(
              x = ~value, y = ~prob,
              data = .prior,
              marker = spike2$head,
              text = ~text_label,
              hoverinfo = "text"
            )
            p <- p %>%
            plotly::add_segments(
              xend = ~value, yend = 0,
              line = spike2$line
            )
          }
          
          if (density) {
            if (!is.null(.prior)) {
              denData <- .prior
            } else {
              denData <- .data
            }
            densList <- tryCatch(density(denData$value, weights = denData$prob, bw = density(denData$value, bw = "sj")$bw),
            error = function(e) NULL
          )
          if (!is.null(densList)) {
            dens <- data.frame(x = densList$x, y = densList$y)
            normalize <- max(denData$prob)
            p <- p %>% plotly::add_lines(
              data = dens, x = ~x, y = ~ y / max(y) * I(normalize),
              line = line,
              text = round(dens$y, 4),
              hovertemplate = "Value: %{x:0.2f}<br>Prob: %{text}<extra></extra>"
            )
          }
        }
      } else { # IT2B
        p <- p %>%
        plotly::add_lines(
          line = line,
          hovertemplate = "Value: %{x:0.2f}<br>Prob: %{y:0.2f}<extra></extra>"
        )
      }
      
      # common to both
      
      # axis labels
      if (is.null(xlab)) {
        xlb <- .par
      } else {
        if (is.character(xlab)) { # specified as fixed character, not recommended
          xlb <- list(text = xlab)
        } else { # specified as list,likely to update formatting
          if (is.null(purrr::pluck(xlab, "text"))) { # text not included in list
            xlb <- modifyList(xlab, list(text = .par)) # so add it
          }
        }
      }
      
      if (is.null(ylab)) {
        ylb <- "Probability"
      } else {
        if (is.character(ylab)) {
          ylb <- list(text = ylab)
        } else { # specified as list,likely to update formatting
          if (is.null(purrr::pluck(ylab, "text"))) { # text not included in list
            ylb <- modifyList(ylab, list(text = "Probability")) # so add it
          }
        }
      }
      
      # title
      if (is.null(title)) {
        titl <- ""
      } else {
        if (is.character(title)) {
          titl <- list(text = title)
        } else { # specified as list,likely to update formatting
          if (is.null(purrr::pluck(title, "text"))) { # text not included in list
            titl <- modifyList(title, list(text = "Marginal")) # so add it
          } else {
            titl <- title
          }
        }
      }
      
      layout$title <- amendTitle(titl, default = list(size = 20))
      layout$xaxis$title <- amendTitle(xlb)
      layout$yaxis$title <- amendTitle(ylb)
      if (is.character(ylb)) {
        layout$yaxis$title <- amendTitle(ylb, layout$xaxis$title$font)
      } else {
        layout$yaxis$title <- amendTitle(ylb)
      }
      
      
      p <- p %>%
      plotly::layout(
        showlegend = F,
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        shapes = list(
          ab_line(v = .min, line = list(width = 3, dash = "dash")),
          ab_line(v = .max, line = list(width = 3, dash = "dash"))
        ),
        barmode = "overlay",
        title = layout$title
      )
      
      return(p)
    }
    
    biPlot <- function(xCol, yCol, ab, data, xlab, ylab, zlab, title, spike, layout, type) {
      whichX <- which(ab$par == xCol)
      whichY <- which(ab$par == yCol)
      
      # axes labels
      if (is.null(xlab)) {
        xlb <- xCol
      } else {
        if (is.character(xlab)) { # specified as fixed character
          xlb <- list(text = xlab)
        } else { # specified as list,likely to update formatting
          if (is.null(purrr::pluck(xlab, "text"))) { # text not included in list
            xlb <- modifyList(xlab, list(text = xCol)) # so add it
          }
        }
      }
      
      if (is.null(ylab)) {
        ylb <- yCol
      } else {
        if (is.character(ylab)) {
          ylb <- list(text = ylab)
        } else { # specified as list,likely to update formatting
          if (is.null(purrr::pluck(ylab, "text"))) { # text not included in list
            ylb <- modifyList(ylab, list(text = yCol)) # so add it
          }
        }
      }
      
      if (is.null(zlab)) {
        zlb <- "Probability"
      } else {
        if (is.character(zlab)) {
          zlb <- list(text = zlab)
        } else { # specified as list,likely to update formatting
          if (is.null(purrr::pluck(zlab, "text"))) { # text not included in list
            zlb <- modifyList(zlab, list(text = "Probability")) # so add it
          }
        }
      }
      
      # title
      if (is.null(title)) {
        titl <- ""
      } else {
        if (is.character(title)) {
          titl <- list(text = title)
        } else { # specified as list,likely to update formatting
          if (is.null(purrr::pluck(title, "text"))) { # text not included in list
            titl <- modifyList(title, list(text = paste0(yCol, " x ", xCol))) # so add it
          } else {
            titl <- title
          }
        }
      }
      
      layout$title <- amendTitle(titl, default = list(size = 20))
      layout$xaxis$title <- amendTitle(xlb)
      layout$yaxis$title <- amendTitle(ylb)
      layout$zaxis$title <- amendTitle(zlb)
      if (is.character(ylb)) {
        layout$yaxis$title <- amendTitle(ylb, layout$xaxis$title$font)
      } else {
        layout$yaxis$title <- amendTitle(ylb)
      }
      if (is.character(zlb)) {
        layout$zaxis$title <- amendTitle(zlb, layout$xaxis$title$font)
      } else {
        layout$zaxis$title <- amendTitle(zlb)
      }
      
      
      if (type == "IT2B") {
        rangeX <- as.numeric(ab[whichX, 1:2])
        rangeY <- as.numeric(ab[whichY, 1:2])
        coords <- data.frame(
          x = seq(
            rangeX[1], rangeX[2],
            (rangeX[2] - rangeX[1]) / 100
          ),
          y = seq(
            rangeY[1], rangeY[2],
            (rangeY[2] - rangeY[1]) / 100
          )
        ) %>%
        tidyr::expand(x, y)
        
        # dmv_norm in PMutilities
        z <- dmv_norm(coords,
          mean = as.numeric(data$popMean[1, c(whichX, whichY)]),
          sigma = as.matrix(data$popCov[c(whichX, whichY), c(whichX, whichY)])
        )
        z <- matrix(z, nrow = 101)
        
        p <- plot_ly(x = ~ unique(coords$x), y = ~ unique(coords$y), z = ~z) %>%
        plotly::add_surface(
          hovertemplate = paste0(xlb, ": %{x:0.4f}<br>", ylb, ":%{y:0.4f}<br>", zlb, ":%{z:0.4f}<extra></extra>")
        ) %>%
        plotly::layout(
          scene = list(
            xaxis = layout$xaxis,
            yaxis = layout$yaxis,
            zaxis = layout$zaxis
          ),
          title = layout$title
        ) %>%
        plotly::hide_colorbar()
        return(p)
      } else { # NPAG
        data$popPoints$id <- seq_len(nrow(data$popPoints))
        pp <- replicate(3, data$popPoints, simplify = FALSE)
        data$popPoints <- data$popPoints %>% select(-id) # undo modification
        
        # make object for drop lines
        pp[[2]]$prob <- min(pp[[1]]$prob)
        pp2 <- dplyr::bind_rows(pp, .id = "key") %>% dplyr::arrange(id, key)
        pp2[pp2$key == 3, ] <- NA
        pp2 <- pp2 %>%
        dplyr::select(x = whichX[1] + 1, y = whichY[1] + 1, prob = prob)
        p <- data$popPoints %>%
        select(x = whichX[1], y = whichY[1], prob = prob) %>%
        plotly::plot_ly(
          x = ~x, y = ~y, z = ~prob,
          hovertemplate = paste0(xlb, ": %{x:0.4f}<br>", ylb, ": %{y:0.4f}<br>", zlb, ": %{z:0.4f}<extra></extra>")
        ) %>%
        plotly::add_markers(marker = spike$head)
        
        if (spike$line$width > 0) {
          p <- p %>% plotly::add_paths(
            data = pp2, x = ~x, y = ~y, z = ~prob,
            line = spike$line
          )
        }
        p <- p %>%
        plotly::layout(
          showlegend = F,
          scene = list(
            xaxis = layout$xaxis,
            yaxis = layout$yaxis,
            zaxis = layout$zaxis
          ),
          title = layout$title
        )
        return(p)
      }
    } # end bivariate plot function
    
    ##############################################
    
    #' @title Plot PM_final_data objects
    #' @description
    #' `r lifecycle::badge("stable")`
    #' Plots the raw data (`class: PM_final_data`) from a [PM_final] object in the same way as plotting a [PM_final] object.
    #' Both use [plot.PM_final].
    #' @method plot PM_final_data
    #' @param x A `PM_final_data`` object
    #' @param ... Additional arguments passed to [plot.PM_final]
    #' @examples
    #' NPex$final$data %>% plot()
    #' @export
    #' 
    plot.PM_final_data <- function(x,...){
      plot.PM_final(x, ...)
    }
    
    # SUMMARY -----------------------------------------------------------------
    
    #' @title Summary Statistics for Final Cycle
    #' @description
    #' `r lifecycle::badge("stable")`
    #'
    #' Generates summary statistics of final population model parameters.
    #'
    #' @details
    #' #' This is a function usually called by the `$summary()` method for [PM_final] objects
    #' within a [PM_result]. The function can be called directly on a [PM_final] object.
    #' For NPAG runs, this function will generate weighted medians as central tendencies of the
    #' population points with a 95% confidence interval (95% CI) around the median,
    #' and the median absolute weighted deviation (MAWD) from the median as a measure
    #' of the variance, with its 95% CI.  These estimates correspond to weighted mean,
    #' 95% CI of the mean, variance, and 95% CI of the variance, respectively, for a
    #' sample from a normal distribution.
    #'
    #' To estimate these non-parametric summaries,
    #' the function uses a Monte Carlo simulation approach, creating  1000 x npoint samples
    #' with replacement from the weighted marginal distribution of each parameter,
    #' where npoint is the number of support points in the model.  As an example,
    #' if there are 100 support points, npoint = 100, and for Ka, there will be
    #' 1000 sets of 100 samples drawn from the weighted marginal distribution of the
    #' values for Ka.  For each of the 1,000 sets of npoint values, the median and MAWD are
    #' calculated, with MAWD equal to the median absolute difference between each point
    #' and the median of that set.  The output is npoint estimates of the weighted median
    #' and npoint estimates of the MAWD for each parameter, from which the median, 2.5th,
    #' and 97.5th percentiles can be found as point estimates and 95% confidence
    #' interval limits, respectively, of both the weighted median and MAWD.
    #'
    #' For IT2B runs, the function will return the mean and variance of each parameter,
    #' and the standard errors of these terms, using
    #' \deqn{SE_mean = SD/\sqrt(nsub)}
    #' \deqn{SE_var = var * \sqrt(2/(nsub-1))}.
    #'
    #' @method summary PM_final
    #' @param object The PM_final object made after an NPAG or IT2B run
    #' @param lower Desired lower confidence interval boundary.  Default is 0.025. Ignored for IT2B objects.
    #' @param upper Desired upper confidence interval boundary.  Default is 0.975. Ignored for IT2B objects.
    #' @param file Filename to save the summary. Include path if necessary.
    #' @param ... Not used.
    #' @return The output is a data frame.
    #' For NPAG this has 4 columns:
    #' * **value** The value of the summary statistic
    #' * **par** The name of the parameter
    #' * **type** Either *WtMed* for weighted median, or *MAWD* for MAWD (see details)
    #' * **percentile** Requested `lower`, 0.5 (median), and `upper` quantiles
    #' For IT2B this has 6 columns:
    #' * **mean** Parameter mean value
    #' * **se.mean** Standard error of the mean
    #' * **cv.mean** Error of the mean divided by mean
    #' * **var** Variance of the parameter values
    #' * **se.var** Standard error of the variance
    #' * **summary** Name of the summary statistic
    #' @author Michael Neely
    #' @seealso [PM_final]
    #' @examples
    #' \dontrun{
    #' NPex$final$summary() # preferred
    #' ITex$final$summary()
    #' summary(NPex$final) # alternate
    #' }
    
    #' @export
    
    summary.PM_final <- function(object, lower = 0.025, upper = 0.975, file = NULL, ...) {
      if (inherits(object, "PM_final")) { # user called summary(PM_final)
        object <- object$data
      }
      
      if (inherits(object, "IT2B")) { # IT2B object
        if (is.null(object$nsub)) {
          nsub <- as.numeric(readline("Your IT2B run is very old. Please re-run.\nFor now, enter the number of subjects. "))
        } else {
          nsub <- object$nsub
        }
        mean <- object$popMean
        se.mean <- object$popSD / sqrt(nsub)
        cv.mean <- se.mean / mean
        var <- object$popVar
        se.var <- object$popVar * sqrt(2 / (nsub - 1))
        sumstat <- dplyr::bind_rows(
          mean, se.mean,
          cv.mean,
          var, se.var
        ) %>%
        dplyr::mutate(summary = c("Mean", "StdErr Mean", "CV% Mean", "Variance", "StdErr Var"))
        return(sumstat)
      } else { # NPAG object
        medMAD <- function(x) {
          med <- median(x)
          MAD <- median(abs(x - med))
          # MAD <- sqrt(sum((x-med)^2)/length(x))
          return(list(med, MAD))
        }
        
        mcsim <- function(x, prob) {
          set.seed(17)
          sim <- apply(matrix(sample(x, replace = TRUE, size = 10^3 * length(x), prob = prob), nrow = 10^3), 1, medMAD)
          ciMed <- quantile(sapply(sim, function(x) x[[1]]), c(lower, 0.5, upper))
          ciMAD <- quantile(sapply(sim, function(x) x[[2]]), c(lower, 0.5, upper))
          return(list(ciMed, ciMAD))
        }
        if (inherits(object, "NPAG") || inherits(object, "PM_final")) {
          popPoints <- object$popPoints
        } else {
          popPoints <- object
        }
        
        nvar <- ncol(popPoints) - 1 # subtract prob
        
        # trick it if there is only one point
        if (nrow(popPoints) == 1) {
          popPoints <- rbind(popPoints, popPoints)
          popPoints$prob <- c(0.5, 0.5)
        }
        
        sumstat <- apply(popPoints[, 1:nvar], 2, function(x) mcsim(x, popPoints$prob)) %>%
        dplyr::as_tibble() %>%
        unnest(cols = everything()) %>%
        mutate(percentile = rep(c(lower, 0.5, upper), 2)) %>%
        mutate(parameter = rep(c("WtMed", "MAWD"), each = 3))
        
        attr(sumstat, "CI") <- c(lower, upper)
        attr(sumstat, "file") <- file
        
        
        class(sumstat) <- c("summary.PM_final", "tbl_df", "tbl", "data.frame")
        return(sumstat)
      }
    }
    
    
    #' @title Summarize PM_final_data objects
    #' @description
    #' `r lifecycle::badge("stable")`
    #' Summarizes the raw data (`class: PM_final_data`) from a [PM_final] object in the same way as summarizing a [PM_final] object.
    #' Both use [summary.PM_final].
    #' @method summary PM_final_data
    #' @param object A `PM_final_data` object
    #' @param ... Additional arguments passed to [summary.PM_final]
    #' @examples
    #' NPex$final$data %>% summary()
    #' @export
    #' 
    summary.PM_final_data <- function(object,...){
      summary.PM_final(object, ...)
    }
    # PRINT SUMMARY ----------------------------------------------------------------
    
    
    #' @title Print Summary of Parameter Values and Credibility Intervals
    #' @description
    #' `r lifecycle::badge("stable")`
    #'
    #' Print a Pmetrics Final Summary Object
    #' @details
    #' Print a summary of parameter medians and MAWD, with point estimates and credibilty intervals
    #' from an object made by [summary.PM_final]. Users do not normally need to call this
    #' function directly, as it will be the default method to display the object.
    #'
    #' @method print summary.PM_final
    #' @param x A summary.PM_final object made by [summary.PM_final].
    #' @param digits Integer, used for number of digits to print.
    #' @param ... Not used.
    #' @return A printed object.
    #' @author Michael Neely
    #' @seealso [summary.PM_final]
    #' @examples
    #' \dontrun{
    #' NPex$final$summary
    #' }
    
    #' @export
    
    print.summary.PM_final <- function(x,
      digits = 3,
      ...) {
        keys <- c("lo", "med", "up")
        values <- c(attr(x, "CI")[1], 0.5, attr(x, "CI")[2])
        ci <- 100 * (values[3] - values[1])
        
        df <- x %>%
        mutate(percentile = dplyr::case_when(
          percentile == values[1] ~ "lo",
          percentile == 0.5 ~ "med",
          percentile == values[3] ~ "up"
        )) %>%
        tidyr::pivot_longer(cols = c(-percentile, -parameter)) %>%
        tidyr::pivot_wider(id_cols = c(name), values_from = value, names_from = c(parameter, percentile)) %>%
        dplyr::mutate(across(-name, ~ round(.x, digits = digits)))
        
        ret <- tibble::tibble(
          Parameter = df$name,
          Median = glue::glue("{df$WtMed_med} ({df$WtMed_lo} - {df$WtMed_up})"),
          MAWD = glue::glue("{df$MAWD_med} ({df$MAWD_lo} - {df$MAWD_up})")
        )
        
        flextable::set_flextable_defaults(font.family = "Arial")
        ft <- flextable::flextable(ret) %>%
        flextable::set_header_labels(values = list(Median = glue::glue("Median ({ci}% CI)"), MAWD = glue::glue("MAWD ({ci}% CI)"))) %>%
        flextable::set_table_properties(width = .5) %>%
        flextable::footnote(
          i = 1, j = 3,
          value = flextable::as_paragraph("MAWD: Mean Absolute Weighted Deviation, a nonparametric measure of dispersion similar to variance"),
          ref_symbols = "1",
          part = "header"
        ) %>%
        flextable::theme_zebra() %>%
        flextable::bold(bold = FALSE, part = "footer") %>%
        flextable::align_text_col(align = "center", header = TRUE, footer = FALSE) %>%
        flextable::autofit()
        
        print(ft)
        save_flextable(ft) # will only save if file specified in summary, function in PMutilities
      }
      