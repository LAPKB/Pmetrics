#' Generates a summary of a Pmetrics NPAG or IT2B run
#'
#' Creates an HTML page and several files summarizing an NPAG or IT2B run.  This report is generated
#' automatically at the end of a successful run.
#'
#' @title Summarize NPAG or IT2B Run
#'
#' @param wd The working directory containing the NP_RFxxxx.TXT or IT_RFxxxx.TXT file
#' @param rdata The processed output of an IT2B or NPAG run, depending on local or server runs.
#' @param icen Median (default), mean or mode of Bayesian posterior to be used to calculate predictions.
#' @param type \dQuote{NPAG} (default) or \dQuote{IT2B} report type
#' @param parallel Boolean parameter which indicates the type of run done.  Default is \code{FALSE} for serial.
#' @return Several files are placed in the \code{wd}
#' \item{NPAGreport.html or IT2Breport.html }{An .html file containing a summary of all the results}
#' \item{poppoints.csv }{NPAG only: A .csv file containing the population support points and probabilities}
#' \item{poparam.csv }{A .csv file containing a summary of the population parameter values, including
#' mean, standard deviation, coefficient of variation, variance, and median}
#' \item{popcor.csv }{A .csv file containing the population parameter correlation matrix}
#' \item{popcov.csv }{A .csv file containing the population parameter covariance matrix}
#' \item{cycle.pdf }{A .pdf file containing the run cycle information (see \code{\link{plot.PMcycle}})}
#' \item{cycle.png }{A thumbnail of the run cycle information for the .html file}
#' \item{final.pdf }{A .pdf file containing the population final cycle information (see \code{\link{plot.PMfinal}})}
#' \item{final.png }{A thumbnail of the population final cycle information for the .html file}
#' \item{opx.pdf }{One or more .pdf files, where \emph{x} is the number of the output equation, each containing
#' two observed vs. predicted plots: population and individual Bayesian posterior predictions (see \code{\link{plot.PMop}})}
#' \item{opx.png }{One or more thumnails of the observed vs. predicted plots for the .html file}
#' \item{NPAGout.Rdata or IT2Bout.Rdata }{An R data file containing the output of \code{\link{NPparse}} or \code{\link{ITparse}}, \code{\link{makeFinal}},
#' \code{\link{makeCycle}}, \code{\link{makeOP}}, \code{\link{makeCov}}, \code{\link{makePop}}, \code{\link{makePost}}, and
#' the data file for the run read by \code{\link{PMreadMatrix}}.
#' This file can be loaded using \code{\link{PMload}}.}
#' @author Michael Neely
#' @export

PMreport <- function(wd, rdata, icen = "median", type = "NPAG", parallel = F) {
  cwd <- getwd()
  reportType <- which(c("NPAG", "IT2B") == type)

  if (missing(rdata)) rdata <- makeRdata(wd, remote = F, reportType)
  # get elapsed time if available
  if (file.exists("time.txt")) {
    execTime <- readLines("time.txt")
    OS <- switch(gsub("[[:blank:]]", "", execTime[1]),
      Unix = 1,
      Windows = 2,
      Linux = 3
    )
    if (OS == 1 | OS == 3) {
      elapsed <- difftime(as.POSIXct(execTime[3], format = "%s"), as.POSIXct(execTime[2], format = "%s"))
    }
    if (OS == 2) {
      elapsed <- difftime(as.POSIXct(execTime[3], format = "%T"), as.POSIXct(execTime[2], format = "%T"))
    }
  } else {
    elapsed <- NA
  }
  # check for error file
  errfile <- rdata$errfile
  # errfile <- list.files(pattern = "^ERROR")
  error <- length(errfile) > 0
  # #see if NP_RF or IT_RF made anyway (i.e. is >1MB in size)
  success <- rdata$success
  # reportType <- 1
  # success <- file.info(c("NP_RF0001.TXT", "IT_RF0001.TXT")[reportType])$size >= 1000
  if (success) {
    # TODO:create r6 object

    report_file <- system.file("report/report.html", package = "Pmetrics")
    manual_file <- system.file("manual/index.html", package = "Pmetrics")
    html <- readr::read_file(report_file)
    html <- gsub("</manual_link>", manual_file, html)


    # red Summary
    if (error) {
      html <- gsub("</red>", "red", html)
    }
    # Generate plots
    thisData <- switch(reportType,
      rdata$NPdata,
      rdata$ITdata
    )
    for (i in 1:thisData$numeqt) {
      plot.PM_op(rdata$op, outeq = i, pred.type = "pop") %>%
        plotly::as_widget() %>%
        htmlwidgets::saveWidget(sprintf("op_pop%i.html", i), libdir = "deps", selfcontained = F)

      plot.PM_op(rdata$op, outeq = i) %>%
        plotly::as_widget() %>%
        htmlwidgets::saveWidget(sprintf("op_ind%i.html", i), libdir = "deps", selfcontained = F)

      html <- gsub("</op>", sprintf('<div class="col-lg-12">
                    <div class="home-content text-center">
                        <h3 class="home-title mb-4 text-white">Output %i</h3>
                    </div>
                </div><div class="col-md-6">
                    <div class="home-content text-center">
                        <h1 class="home-title mb-4 text-white">Population</h1>
                        <iframe width="95%%" height="500" src="op_pop%i.html"></iframe>
                    </div>
                </div>
                <div class="col-md-6">
                    <div class="home-content text-center">
                        <h1 class="home-title mb-4 text-white">Individual</h1>
                        <iframe width="95%%" height="500" src="op_ind%i.html"></iframe>
                    </div>
                </div></op>', i, i, i), html)
    }


    plot.PMcycle(rdata$cycle) %>%
      plotly::as_widget(height = "1060px", width = "500px") %>%
      htmlwidgets::saveWidget("cycle.html", libdir = "deps", selfcontained = F)


    pmfinal_height <- ((length(names(rdata$final$popMean)) - 1) %/% 2) * 500
    plot.PM_final(rdata$final) %>%
      plotly::as_widget(final, height = sprintf("%ipx", pmfinal_height), width = "1420px") %>%
      htmlwidgets::saveWidget("final.html", libdir = "deps", selfcontained = F)
    html <- gsub("</pmfinal_height>", sprintf("%ipx", pmfinal_height), html)

    # Edit HTML

    if (reportType == 1) {
      html <- gsub("</parameter_values>", paste0(
        '<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Support Points</h2>',
        makeHTMLdf(rdata$final$popPoints, 3), "</div></parameter_values>"
      ), html)
    }
    report.table <- data.frame(
      mean = t(rdata$final$popMean),
      sd = t(rdata$final$popSD),
      CV = t(rdata$final$popCV),
      var = t(rdata$final$popVar),
      median = t(rdata$final$popMedian),
      shrink = t(100 * rdata$final$shrinkage)
    )
    names(report.table) <- c("Mean", "SD", "CV%", "Var", "Median", "Shrink%")
    html <- gsub("</parameter_values>", paste0(
      '<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Parameter Values Summary</h2>',
      makeHTMLdf(report.table, 3), "</div></parameter_values>"
    ), html)

    if (thisData$nranfix > 0) {
      ranfixdf <- data.frame(Parameter = thisData$parranfix, Value = thisData$valranfix)
      html <- gsub("</parameter_values>", paste0(
        '<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Population Fixed (but Random) Values</h2>',
        makeHTMLdf(ranfixdf, 3), "</div></parameter_values>"
      ), html)
    }
    if (thisData$nofix > 0) {
      fixdf <- data.frame(Parameter = thisData$parfix, Value = thisData$valfix)
      html <- gsub("</parameter_values>", paste0(
        '<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Population Fixed (and Constant) Values</h2>',
        makeHTMLdf(fixdf, 3), "</div></parameter_values>"
      ), html)
    }
    # covariance matrix
    html <- gsub("</parameter_values>", paste0(
      '<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Covariance Matrix</h2>',
      makeHTMLdf(rdata$final$popCov, 3), "</div></parameter_values>"
    ), html)

    # correlation matrix
    html <- gsub("</parameter_values>", paste0(
      '<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Correlation Matrix</h2>',
      makeHTMLdf(rdata$final$popCor, 3), "</div></parameter_values>"
    ), html)



    if (thisData$nofix == 0) {
      parfix <- "There were no constant fixed parameters."
    } else {
      parfix <- paste("Constant fixed parameters:", paste(thisData$parfix, collapse = ", "))
    }
    if (thisData$nranfix == 0) {
      parranfix <- "There were no random fixed parameters."
    } else {
      parranfix <- paste("Random fixed parameters:", paste(thisData$parranfix, collapse = ", "))
    }
    ilog <- thisData$ilog
    if (is.null(thisData$converge)) {
      same <- 0
      for (i in 2:length(ilog)) {
        if ((ilog[i] - ilog[i - 1]) < 1e-04) same <- same + 1
      }
      if (same %% 11 == 0) {
        coninterp <- " - The run converged."
        confor1 <- ""
        confor2 <- ""
      } else {
        coninterp <- " *The run did not converge before the last cycle."
        confor1 <- "<span class=\"alert\">"
        confor2 <- "</span>"
        html <- gsub("</red>", "red", html)
      }
    } else {
      coninterp <- switch(1 + thisData$converge,
        " *The run did not converge before the last cycle.",
        " - The run converged.",
        "",
        " *The run ended with a Hessian Error."
      )
      confor1 <- switch(1 + thisData$converge,
        "<span class=\"alert\">",
        "",
        "",
        "<span class=\"alert\">"
      )
      confor2 <- switch(1 + thisData$converge,
        "</span>",
        "",
        "",
        "</span>"
      )
      html <- gsub("</red>", "red", html)
    }
    if (reportType == 1 && !is.null(thisData$prior)) {
      # this will only be for NPAG
      extra <- paste("Prior density: ", c("Non-uniform (prior.txt)", "Uniform")[1 + as.numeric(thisData$prior == "UNIFORM")], "<br>",
        "Assay error model: ", switch(thisData$ERRmod,
          "SD",
          "SD, gamma",
          "SD, lambda",
          "gamma"
        ), "<br>",
        sep = ""
      )
    } else {
      extra <- ""
    }

    # no error, but alert if no convergence
    # TODO: This code detects if the run does not converged, how are we going to show that?
    # writeHTML(ifelse(coninterp == " - The run converged.",
    #                   "<label for=\"tab5\">Summary</label>",
    #                   "<label for=\"tab5\">Summary<span class=\"alert\">*</span></label>"))


    fixedvar <- ""
    html <- gsub("</summary>", paste("Engine: ", c("NPAG", "IT2B")[reportType], "<br>",
      "Computation mode: ", c("Serial", "Parallel")[1 + as.numeric(parallel)], "<br>",
      "Output file: <a href=", file.path(wd), c("/NP_RF0001.TXT target=_blank>", "/IT_RF0001.TXT target=_blank>")[reportType], file.path(wd), c("/NP_RF0001.TXT</a><br>", "/IT_RF0001.TXT</a><br>")[reportType],
      "Random parameters: ", paste(paste(thisData$par, collapse = ", "), fixedvar, sep = " "), "<br>",
      parranfix, "<br>", parfix, "<br>",
      "Number of analyzed subjects: ", thisData$nsub, "<br>",
      "Number of output equations: ", thisData$numeqt, "<br>",
      "Number of cycles: ", thisData$icyctot, "  ", confor1, coninterp, confor2, "<br>",
      "Additional covariates: ", paste(thisData$covnames, collapse = ", "), "<br>", extra, "</summary>",
      sep = ""
    ), html)


    if (thisData$negflag) {
      html <- gsub("</summary>", "WARNING: There were negative pop/post predictions.<br></summary>", html)
    }
    if (!is.na(elapsed)) {
      html <- gsub("</summary>", paste("Elapsed time for this run was", elapsed, attr(elapsed, "units"), "<br></summary>"), html)
    }

    # system(paste0("cp ",report_file," /NPAGreport.html"))
    readr::write_file(html, c("NPAGreport.html", "IT2Breport.html")[reportType])
  } else {
    error_file <- paste(path.package("Pmetrics"), "/report/error.html", sep = "")
    html <- readr::read_file(error_file)

    errmessage <- readLines(errfile)
    errmessage <- paste(errmessage, collapse = "")
    errmessage <- gsub("  ", " ", errmessage)
    errmessage <- sub("^ *", "", errmessage)

    html <- gsub("</summary>", paste0("<h2>ERROR REPORT<h2>", errmessage), html)
    readr::write_file(html, c("NPAGreport.html", "IT2Breport.html")[reportType])
  }
  setwd(cwd)
}


makeRdata <- function(wd, remote, reportType) {
  setwd(wd)
  errfile <- list.files(pattern = "^ERROR")
  # error <- length(errfile) > 0
  # see if NP_RF or IT_RF made anyway (i.e. is >1MB in size)
  success <- file.info(c("NP_RF0001.TXT", "IT_RF0001.TXT")[reportType])$size >= 1000

  if (success) {
    # run completed
    # open and parse the output
    if (reportType == 1) {
      # NPAG
      PMdata <- suppressWarnings(tryCatch(NPparse(), error = function(e) {
        e <- NULL
        cat("\nWARNING: The run did not complete successfully.\n")
      }))
      # make the posterior predictions
      post <- suppressWarnings(tryCatch(makePost(NPdata = PMdata), error = function(e) {
        e <- NULL
        cat("\nWARNING: error in extraction of posterior Bayesian predictions at time ttpred; 'PMpost' object not saved.\n\n")
      }))
      # make the population predictions
      pop <- suppressWarnings(tryCatch(makePop(NPdata = PMdata), error = function(e) {
        e <- NULL
        cat("\nWARNING: error in extraction of population predictions at time tpred; 'PMpop' object not saved.\n\n")
      }))
    } else { # IT2B
      PMdata <- suppressWarnings(tryCatch(ITparse(), error = function(e) {
        e <- NULL
        cat("\nWARNING: The run did not complete successfully.\n")
      }))
    }
    # both NPAG and IT2B
    cat("\n\n")
    flush.console()
    if (is.null(PMdata$nranfix)) PMdata$nranfix <- 0
    op <- suppressWarnings(tryCatch(makeOP(PMdata), error = function(e) {
      e <- NULL
      cat("\nWARNING: error in extraction of observed vs. population predicted data; 'PMop' object not saved.\n\n")
    }))
    cycle <- suppressWarnings(tryCatch(makeCycle(PMdata), error = function(e) {
      e <- NULL
      cat("\nWARNING: error in extraction of cycle information; 'PMcycle' object not saved.\n\n")
    }))
    final <- suppressWarnings(tryCatch(makeFinal(PMdata), error = function(e) {
      e <- NULL
      cat("\nWARNING: error in extraction of final cycle parameter values; 'PMfinal' object not saved.\n\n")
    }))
    if (PMdata$mdata != "NA") {
      mdata <- PMreadMatrix(paste("../inputs/", PMdata$mdata, sep = ""), quiet = T)
    } else {
      mdata <- NA
    }
    cov <- suppressWarnings(tryCatch(makeCov(PMdata), error = function(e) {
      e <- NULL
      cat("\nWARNING: error in extraction of covariate-parameter data; 'PMcov' object not saved.\n\n")
    }))
    if (PMdata$mdata != "NA") {
      mdata <- PMreadMatrix(paste("../inputs/", PMdata$mdata, sep = ""), quiet = T)
    } else {
      mdata <- NA
    }
    model <- list.files("../inputs") %>%
      .[grepl(".txt$", .)] %>%
      paste0("../inputs/", .) %>%
      .[[1]] %>%
      PM_model$new(.)

    if (reportType == 1) {
      NPAGout <- list(NPdata = PMdata, pop = pop, post = post, final = final, cycle = cycle, op = op, cov = cov, data = mdata, model = model, errfile = errfile, success = success)
      save(NPAGout, file = "NPAGout.Rdata")
      # Hacky return to deal with Rservex bug T.T
      if (remote) {
        return("ok")
      }
      return(NPAGout)
    }
    if (reportType == 2) {
      IT2Bout <- list(ITdata = PMdata, final = final, cycle = cycle, op = op, cov = cov, data = mdata, errfile = errfile, success = success)
      save(IT2Bout, file = "IT2Bout.Rdata")
      if (remote) {
        return("ok")
      }
      return(IT2Bout)
    }
  }
}

# HTML tools --------------------------------------------------------------

# function to process data.frames
makeHTMLdf <- function(df, ndigit) {
  Nrow <- nrow(df)
  Ncol <- ncol(df)
  dfScript <- vector("character")
  dfScript <- append(dfScript, '<table border=0 class="table table-striped table-hover">')
  dfScript <- append(dfScript, "<tbody>")
  dfScript <- append(dfScript, "<tr class=firstline>")
  dfScript <- append(dfScript, "<th></th>")
  for (j in 1:Ncol) {
    dfScript <- append(dfScript, paste('<th scope="col">', colnames(df)[j], "</th>", sep = ""))
  }
  dfScript <- append(dfScript, "</tr>") # end first row headers
  for (i in 1:Nrow) {
    dfScript <- append(dfScript, "<tr>")
    dfScript <- append(dfScript, paste("<td class=firstcolumn>", rownames(df)[i], "</td>", sep = ""))
    for (j in 1:Ncol) {
      dfScript <- append(dfScript, ifelse(inherits(df[i, j], "numeric"),
        paste("<td class=cellinside>", sprintf(paste("%.", ndigit, "f", sep = ""), round(as.numeric(df[i, j]), ndigit)), "</td>", sep = ""),
        paste("<td class=cellinside>", df[i, j])
      ))
    }
    dfScript <- append(dfScript, "</tr>")
  }
  dfScript <- append(dfScript, "</tbody>")
  dfScript <- append(dfScript, "</table>")

  return(paste(dfScript, collapse = ""))
}
# end makeHTMLdf function

writeHTML <- function(x) {
  .HTMLfile <- get(".HTMLfile", pos = parent.frame())
  cat(c(paste(x, collapse = "\n"), "\n"), file = .HTMLfile, append = T)
}


# Tex functions -----------------------------------------------------------
# written by Alona Kryschenko

TEX <- function(x) {
  .TEXfile <- get(".TEXfile", pos = parent.frame())
  cat(paste(x, collapse = ""), "\n", file = .TEXfile, append = T)
}

# TEXstart <- function(outdir, filename) {
#   file <- file.path(outdir, paste(filename, ".tex",
#                                   sep = ""))
#   CurrentTEXfile <<- file
#   s <- paste('\\documentclass{article}
#              \\usepackage{graphicx}
#              \\usepackage[colorlinks]{hyperref}
#              \\usepackage{url}
#              \\usepackage{float}
#              \\usepackage[landscape]{geometry}')
#   #\\begin{document}')
#   cat(s, '\n', file = file, append = FALSE)
#
#   CurrentTEXfile
# }

# TEXend <- function() {
#   cat('\\end{document}', '\n', file = CurrentTEXfile, append = TRUE)
# }
