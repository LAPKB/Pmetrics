#Use menu item Code -> Jump To... for rapid navigation
#Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 -----------------------------------------------------------------


#' @title Individual Bayesian posterior predictions at short intervals
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains the Bayesian posterior predictions at short intervals
#' specified as an argument to the $run method of [PM_fit]. Default is every 12 minutes.
#'
#' @details
#' #' The [PM_post] object is both a data field within a [PM_result], and itself an R6 object
#' comprising data fields and associated methods suitable for 
#' analysis and plotting of posterior predictions generated during the run. 
#' 
#' Because [PM_post] objects are automatically added to the [PM_result] at the end of a
#' successful run, it is generally not necessary for users to generate [PM_post] objects
#' themselves.
#'  
#' The main results are contained in the `$data` field,
#' and it is this field which is passed to the `$plot` and `$summary` methods.
#' data frame with population predicted outputs for all subjects.
#' 
#' To provide a more traditional experience in R,
#' the data frame is separated by columns into fields, e.g. `id` or `time`. This
#' allows you to access them in an S3 way, e.g. `run1$post$time` if `run1` is a
#' `PM_result` object.
#'
#' However, if you wish to manipulate the entire data frame,
#' use the `data` field, e.g. `trough <- run1$post$data %>% filter(time == 24)`. If
#' you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_post <- R6::R6Class(
  "PM_post",
  public = list(
    #' @field id Subject id
    id = NULL,
    #' @field time Time of predictions in decimal hours
    time = NULL,
    #' @field icen Prediction based on mean or median of Bayesian posterior parameter distribution
    icen = NULL,
    #' @field outeq Output equation number
    outeq = NULL,
    #' @field pred Predicted output for each outeq
    pred = NULL,
    #' @field block Observation blocks within subjects as defined by *EVID=4* dosing events
    block = NULL,
    #' @field data A data frame combining all the above fields as its columns
    data = NULL,
    #' @description
    #' Create new object populated with Bayesian posterior predicted data at
    #' regular, frequent intervals
    #' @details
    #' Creation of new `PM_post` object is automatic and not generally necessary
    #' for the user to do.
    #' @param PMdata If backend is Fortran, the parsed output from [NPparse] or [ITparse]. Not needed when the backend is Rust.
    #' @param run If backend is Fortran, run number to find PRTB0001 file with
    #' posterior predictions. If missing, will look for this file in working
    #' directory. Not needed when the backend is Rust.
    #' @param ... Not currently used.
    initialize = function(PMdata, run, ...) {
      post <- private$make(PMdata, run)
      self$data <- post
      if(length(post)>1){ #all the objects were made
        self$id <- post$id
        self$time <- post$time
        self$icen <- post$icen
        self$outeq <- post$outeq
        self$pred <- post$pred
        self$block <- post$block
      }
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_pop].
    #' @param ... Arguments passed to [plot.PM_pop]
    plot = function(...){
      tryCatch(plot.PM_post(self, ...), error = function(e){
        cat(crayon::red("Error:"), e$message, "\n")
      }
      )
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PM_post].
    #' @param ... Arguments passed to [summary.PM_pop]
    summary = function(...) {
      tryCatch(summary.PM_pop(self, ...), error = function(e){
        cat(crayon::red("Error:"), e$message, "\n")
      }
      )
    },
    #' @description
    #' Calculate AUC
    #' @details
    #' See [makeAUC]
    #' @param data The object to use for AUC calculation
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      makeAUC(data = self$data, ...)
    }
  ), # end public
  private = list(
    make = function(data, run) {
      if(getPMoptions("backend") == "rust"){
        
        raw <- tryCatch(readr::read_csv(file = "pred.csv", show_col_types = FALSE),
                             error = function(e) {
                               e <- NULL
                               cat(crayon::red("Error:"),
                                   "The run did not complete and the pred.csv file was not created.\n")
                             })
       
        
        if(is.null(raw)){
          return(NA)
        }
        
        post <- raw %>%
          select(-popMedian, -popMean) %>%
          pivot_longer(
            cols = c(postMedian, postMean),
            values_to = "pred"
          ) %>%
          dplyr::rename(icen = name) %>%
          mutate(icen = case_when(
            icen == "postMedian" ~ "median",
            icen == "postMean" ~ "mean"
          )) %>%
          # Hardcoded for now
          mutate(block = 1) %>%
          relocate(id, time, icen, outeq, pred, block)
        
        class(post) <- c("PM_post_data", "data.frame")
        return(post)
        
      } else { #fortran
        
        if(inherits(data,"PMpost")){ #old format
          return(data) #nothing to do
        }
        
        if(inherits(data,"PM_post")){ #R6 format
          return(data$data) #return raw to rebuild
        }
        
        # get data
        if (missing(run)) { # look in current wd
          # run <- "."
          # predfile <- paste(run, "PRTB0001", sep = "/")
          predfile <- "PRTB0001"
        } else { # look in run folder/outputs
          if (!file.exists(as.character(run))) stop(paste(run, " not found in the current working directory.\n", sep = ""))
          predfile <- paste(run, "outputs/PRTB0001", sep = "/")
        }
        
        
        # read PRTB file
        preddata <- readLines(predfile)
        
        
        predLines <- grep("[[:space:]]*TIMES[[:space:]]+PREDICTED VALUES", preddata)
        
        if (length(predLines) > 0) { # we are dealing with the old format
          stop("Rerun NPAG to generate current format.\n")
        } else { # we are dealing with the new format
          predLines <- grep("BASED, IN ORDER, ON THE POSTERIOR MEANS, MEDIANS, AND MODES:", preddata)
          raw <- list()
          if (data$nsub > 1) pb <- txtProgressBar(min = 1, max = data$nsub, style = 3)
          cat("\nObtaining posterior predicted time-observation profiles for each subject.\n")
          flush.console()
          for (i in 1:data$nsub) {
            if (data$nsub > 1) setTxtProgressBar(pb, i)
            raw[[i]] <- data.frame(matrix(scan(predfile, skip = predLines[i] + 1, nlines = data$numt[i], quiet = T), ncol = 1 + 3 * data$numeqt, byrow = T))
            names(raw[[i]]) <- c("time", unlist(lapply(1:data$numeqt, function(x) paste(c("mean", "median", "mode"), x, sep = ""))))
            raw[[i]]$id <- data$sdata$id[i]
          }
          totalRaw <- do.call(rbind, raw)
          
          # add blocks
          blocks <- totalRaw %>%
            group_by(.data$id) %>%
            filter(.data$time == 0) %>%
            transmute(blocknum = row_number())
          
          
          totalRaw$block <- NA
          totalRaw$block[totalRaw$time == 0] <- blocks$blocknum
          totalRaw <- fill(totalRaw, block)
          
          
          post <- totalRaw %>%
            pivot_longer(cols = c(-time, -id, -block), names_to = "icen", values_to = "pred") %>%
            arrange(.data$id, .data$icen, .data$block, .data$time) %>%
            extract(icen, into = "outeq", regex = "([[:digit:]])+$", remove = F, convert = T) %>%
            separate(icen, into = c("icen", NA), sep = "[[:digit:]]+") %>%
            select(.data$id, .data$time, .data$icen, .data$outeq, .data$pred, .data$block) %>%
            filter(.data$icen != "mode") # suppress mode
        }
        
        # add predictions at observed times
        op <- PM_op$new(data)$data
        opPost <- op[op$pred.type == "post", ]
        post <- rbind(post, opPost[, c("id", "time", "icen", "outeq", "pred", "block")])
        
        # remove duplicates
        dupTime <- which(duplicated(post[, c("id", "time", "icen", "outeq", "block")]))
        if (length(dupTime) > 0) post <- post[-dupTime, ]
        
        # sort by icen, id, block, time, outeq
        post <- post[order(post$icen, post$id, post$block, post$time, post$outeq), ]
        
        # assign class
        class(post) <- c("PM_post_data", "data.frame")
        
        return(post)
      }
      
    }
  ) # end private
)




# PLOT --------------------------------------------------------------------

#' @title Plot PM_post Prediction Data
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plots [PM_post] objects
#' @details
#' This is a function usually called by the `$plot()` method for [PM_post] objects
#' within a [PM_result] to generate the plot.
#' However, the function can be called directly on a [PM_post] object.
#' This function will plot time and population predictions with a variety of options.
#' By default markers are included and  have the following plotly properties:
#' `list(symbol = "circle", color = "red", size = 10, opacity = 0.5, line = list(color = "black", width = 1))`.
#' Markers can be joined by lines, default is `TRUE`. If `TRUE`,
#' the joining lines will have the following properties:
#' `list(color = "dodgerblue", width = 1, dash = "solid"`.
#' The grid and legend are omitted by default.
#'
#' @method plot PM_post
#' @param x The name of a [PM_post]  object, e.g. `PmetricsData::NPex$post`.
#' in a [PM_result] object
#' @param include `r template("include")`
#' @param exclude `r template("exclude")`
#' @param line Controls characteristics of lines as for all plotly plots.
#' It can either be a boolean or a list. If set to `TRUE` or
#' a list of plotly line attributes, it
#' will generate line segments joining observations. If set to
#' `FALSE`, no segments will be generated. The default
#' values for the elements of formatting list, all of which can be
#' overriden are:
#'     - `color` Color of the segments. Default is "dodgerblue".
#'     - `width `Width of the segments, default 1.
#'     - `dash` See `plotly::schema()`, traces > scatter > attributes >
#' line > dash > values. Default is "solid".
#' Example: `line = list(color = "red", dash = "longdash", width = 2)`
#' @param marker Formats the symbols plotting observations. `r template("marker")`
#' @param colors to use for **groups** when there are multiple `outeq`, `block`, or `icen`. 
#' This can be a palette or a vector of colors.
#' For accepted palette names see `RColorBrewer::brewer.pal.info`. Examples include
#' "BrBG", or "Set2". An example vector could be `c("red", "green", "blue")`. It is not
#' necessary to specify the same number of colors as groups within `color`, as colors
#' will be interpolated to generate the correct number. The default when `color`
#' is specified is the "Set1" palette.
#' @param names A character vector of names to label the **groups** if `legend = TRUE`.
#' This vector does need to be the same length as the number of groups.
#' Example: `c("Mean", "Median")` if `icen = c("mean", "median")`.
#' @param mult `r template("mult")`
#' @param icen Can be "median" for the predictions based on median of 
#' parameter value distributions, "mean", or both.  Default is "median".
#' @param outeq `r template("outeq")` Default is 1, but can be multiple if present in the data, e.g. `1:2` or `c(1, 3)`.
#' @param block `r template("block")` Default is 1, but can be multiple if present in the data, as for `outeq`.
#' @param overlay Operator to overlay all time prediction profiles in a single plot.
#' The default is `TRUE`. If `FALSE`, will trellisplot subjects one at a time. Can also be
#' specified as a vector with number of rows and columns, e.g. `c(3, 2)` for 3 rows and
#' 2 columns of subject splots to include in each trellis.
#' @param legend Not used for this plot.
#' @param log `r template("log")` 
#' @param grid `r template("grid")` 
#' @param xlim `r template("xlim")` 
#' @param ylim `r template("ylim")` 
#' @param xlab `r template("xlab")` Default is "Time".
#' @param ylab `r template("ylab")` Default is "Output".
#' @param title `r template("title")` Default is to have no title.
#' @param ... `r template("dotsPlotly")`
#' @return Plots the object.
#' @author Michael Neely
#' @seealso [PM_post], [PM_result]
#' @export
#' @examples
#' library(PmetricsData)
#' # basic spaghetti plot
#' NPex$post$plot()
#' # format line and marker
#' NPex$post$plot(
#'   marker = list(color = "blue", symbol = "square", size = 12, opacity = 0.4),
#'   line = list(color = "orange")
#' )
#' 
#' @family PMplots

plot.PM_post <- function(x,
                        include = NA,
                        exclude = NA,
                        line = TRUE,
                        marker = TRUE,
                        color = NULL,
                        colors = "Set1",
                        names = NULL,
                        mult = 1,
                        icen = "median",
                        outeq = 1,
                        block = 1,
                        overlay = TRUE,
                        legend = FALSE, 
                        log = FALSE, 
                        grid = FALSE,
                        xlab = "Time",
                        ylab = "Output",
                        title = "",
                        xlim, ylim, ...) {
  # Plot parameters ---------------------------------------------------------
  
  x <- if (inherits(x, "PM_post")) {
    x$data
  } else {
    stop("Please supply a PM_post object for plotting.\n")
  }
  
  # process marker
  marker <- amendMarker(marker)
  
  # process line
  line <- amendLine(line)
  
  
  # get the rest of the dots
  layout <- amendDots(list(...))
  
  #legend
  legendList <- amendLegend(legend)
  layout <- modifyList(layout, list(showlegend = legendList$showlegend))
  if (length(legendList) > 1) {
    layout <- modifyList(layout, list(legend = within(legendList, rm(showlegend))))
  }
  
  # grid
  layout$xaxis <- setGrid(layout$xaxis, grid)
  layout$yaxis <- setGrid(layout$yaxis, grid)
  
  # axis labels if needed
  layout$xaxis$title <- amendTitle(xlab)
  if (is.character(ylab)) {
    layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
  } else {
    layout$yaxis$title <- amendTitle(ylab)
  }
  
  
  # axis ranges
  if (!missing(xlim)) {
    layout$xaxis <- modifyList(layout$xaxis, list(range = xlim))
  }
  if (!missing(ylim)) {
    layout$yaxis <- modifyList(layout$yaxis, list(range = ylim))
  }
  
  # log y axis
  if (log) {
    layout$yaxis <- modifyList(layout$yaxis, list(type = "log"))
  }
  
  # title
  layout$title <- amendTitle(title, default = list(size = 20))
  
  # overlay
  if (is.logical(overlay)) { # T/F
    if (!overlay) { # F,default
      nrows <- 1
      ncols <- 1
    } # if T, no need to set nrows or ncols
  } else { # specified as c(rows, cols)
    nrows <- overlay[1]
    ncols <- overlay[2]
    overlay <- FALSE
  }
  
  # Data processing ---------------------------------------------------------
  
  
  # filter
  presub <- x %>%
    filter(outeq %in% !!outeq, block %in% !!block, icen %in% !!icen) %>%
    mutate(group = "") %>%
    includeExclude(include, exclude) 
  
  # group
  if (outeq[1] != 1 | length(outeq) > 1) {
    presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", outeq: ", outeq))
  }
  if (block[1] != 1 | length(block) > 1) {
    presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", block: ", block))
  }
  
  if (length(icen) > 1) {
    presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", ", icen))
  }
  
  presub$group <- stringr::str_replace(presub$group, "^\\s*,*\\s*", "")
  if (!is.null(names)) {
    presub$group <- factor(presub$group, labels = names)
  } else {
    presub$group <- factor(presub$group)
  }
  
  # select relevant columns
  sub <- presub %>%
    select(id, time, pred, outeq, group) %>%
    ungroup()
  sub$group <- factor(sub$group)
  
  # remove missing
  sub <- sub %>% filter(pred != -99)
  
  
  
  # Plot function ----------------------------------------------------------
  
  dataPlot <- function(allsub, overlay) {
    # set appropriate pop up text
    if (!overlay) {
      hovertemplate <- "Time: %{x}<br>Pred: %{y}<extra></extra>"
      text <- ""
    } else {
      hovertemplate <- "Time: %{x}<br>Pred: %{y}<br>ID: %{text}<extra></extra>"
      text <- ~id
    }
    
    if (!all(is.na(allsub$group)) && any(allsub$group != "")) { # there was grouping
      n_colors <- length(levels(allsub$group))
      if(checkRequiredPackages("RColorBrewer")){
        palettes <- RColorBrewer::brewer.pal.info %>% mutate(name = rownames(.))
        if (length(colors) == 1 && colors %in% palettes$name) {
          max_colors <- palettes$maxcolors[match(colors, palettes$name)]
          colors <- colorRampPalette(RColorBrewer::brewer.pal(max_colors, colors))(n_colors)
        }
      } else {
        cat(paste0(crayon::green("Note: "), "Group colors are better with RColorBrewer package installed.\n"))
        colors <- getDefaultColors(n_colors) #in plotly_Utils
      }
      
      marker$color <- NULL
      line$color <- NULL
    } else { # no grouping
      allsub$group <- factor(1, labels = "Predicted")
    }
    
    p <- allsub %>%
      plotly::plot_ly(
        x = ~time, y = ~ pred * mult,
        color = ~group,
        colors = colors,
        name = ~group) %>%
      plotly::add_markers(
        marker = marker,
        text = text,
        hovertemplate = hovertemplate
      ) %>%
      plotly::add_lines(
        line = line,
        showlegend = FALSE
      )
    p <- p %>% plotly::layout(
      xaxis = layout$xaxis,
      yaxis = layout$yaxis,
      title = layout$title,
      showlegend = layout$showlegend,
      legend = layout$legend
    )
    return(p)
  } # end dataPlot
  
  
  # Call plot ---------------------------------------------------------------
  
  # call the plot function and display appropriately
  if (overlay) {
    sub <- sub %>% dplyr::group_by(id)
    p <- dataPlot(sub, overlay = TRUE)
    print(p)
  } else { # overlay = FALSE, ie. split them
    
    if(!checkRequiredPackages("trelliscopejs")){
      stop(paste0("Package trelliscopejs required to plot when overlay = ", crayon::red("FALSE")))
    }
    sub_split <- x %>%
      nest(data = -id) %>%
      mutate(panel = trelliscopejs::map_plot(data, \(x) dataPlot(x, overlay = FALSE)))
    p <- sub_split %>%
      ungroup() %>%
      trelliscopejs::trelliscope(name = "Data", nrow = nrows, ncol = ncols)
    print(p)
  }
  
  return(p)
}


# SUMMARY -------------------------------------------------------------------


#' @title Summarize Observations and Predictions
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Summarize a Pmetrics Observed vs. Predicted object
#'
#' @details This is a function usually called by the `$summary()` method for [PM_op] objects
#' within a [PM_result] to summarize observations, predictions and errors. The function can
#' be called directly on a [PM_op] object. See examples.
#'
#' @method summary PM_post
#' @param object A [PM_post] object 
#' @param digits Integer, used for number of digits to print.
#' @param icen Can be either "median" for the predictions based on medians of the posterior parameter value
#' distributions, or "mean".  Default is "median".
#' @param outeq Output equation number.  Default is 1.
#' @param ... Not used.
#' @return A data frame with the minimum, first quartile, median, third quartile, maximum,
#' mean and standard deviation for times and predictions in `x`.
#' 
#' @author Michael Neely
#' @examples
#' library(PmetricsData)
#' NPex$post$summary() #preferred
#' summary(NPex$post) #alternative
#' @seealso [PM_post]
#' @export

summary.PM_post <- function(object, digits = max(3, getOption("digits") - 3),
                          icen = "median",
                          outeq = 1, ...) {

  sumWrk <- function(data) {
    sumstat <- matrix(NA, nrow = 7, ncol = 2, dimnames = list(c("Min", "25%", "Median", "75%", "Max", "Mean", "SD"), c("Time", "Pred")))
    # min
    sumstat[1, ] <- round(apply(data[, c(2,5)], 2, min, na.rm = T), digits)
    # 25th percentile
    sumstat[2, ] <- round(apply(data[, c(2,5)], 2, quantile, 0.25, na.rm = T), digits)
    # median
    sumstat[3, ] <- round(apply(data[, c(2,5)], 2, median, na.rm = T), digits)
    # 75th percentil
    sumstat[4, ] <- round(apply(data[, c(2,5)], 2, quantile, 0.75, na.rm = T), digits)
    # max
    sumstat[5, ] <- round(apply(data[, c(2,5)], 2, max, na.rm = T), digits)
    # mean
    sumstat[6, ] <- round(apply(data[, c(2,5)], 2, mean, na.rm = T), digits)
    # SD
    sumstat[7, ] <- round(apply(data[, c(2,5)], 2, sd, na.rm = T), digits)
    sumstat <- data.frame(sumstat)
    # N
    N <- length(data$pred[!is.na(data$pred)])
    
    return(sumstat)
  } # end sumWrk
  
  # make summary
  if(inherits(object, "PM_post")){ 
    object <- object$data
  }  
  
  object <- object %>% filter(outeq == !!outeq, icen == !!icen)
  if (all(is.na(object$pred))) {
    result <- NA
  } else {
    result <- sumWrk(object)
  }
  return(result)
}
