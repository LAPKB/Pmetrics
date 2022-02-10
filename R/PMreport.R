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

PMreport<-function(wd, rdata, icen = "median", type = "NPAG", parallel = F) {
  setwd(wd)
  if (missing(rdata)) rdata <- makeRdata(wd, remote=F, 1)
  #get elapsed time if available
  if (file.exists("time.txt")) {
    execTime <- readLines("time.txt")
    OS <- switch(gsub("[[:blank:]]", "", execTime[1]), Unix = 1, Windows = 2, Linux = 3)
    if (OS == 1 | OS == 3) {
      elapsed <- difftime(as.POSIXct(execTime[3], format = "%s"), as.POSIXct(execTime[2], format = "%s"))
    }
    if (OS == 2) {
      elapsed <- difftime(as.POSIXct(execTime[3], format = "%T"), as.POSIXct(execTime[2], format = "%T"))
    }
  } else { elapsed <- NA }
  #check for error file
  errfile <- rdata$errfile
  # errfile <- list.files(pattern = "^ERROR")
  error <- length(errfile) > 0
  # #see if NP_RF or IT_RF made anyway (i.e. is >1MB in size)
  success <- rdata$success
  reportType<-1
  # success <- file.info(c("NP_RF0001.TXT", "IT_RF0001.TXT")[reportType])$size >= 1000
  if(success){
    #TODO:create r6 object

    report_file<- paste(path.package("Pmetrics"), "/report/report.html", sep = "")
    html<-readr::read_file(report_file)

    #red Summary
    if(error){
      html<-gsub("</red>","red",html)
    }
    #Generate plots
    for (i in 1:rdata$NPdata$numeqt) {
      plot.PMfit(rdata$op, outeq = i, pred.type="pop") %>%
      plotly::as_widget() %>%
      htmlwidgets::saveWidget(sprintf("op_pop%i.html",i), libdir = "deps", selfcontained = F)

      plot.PMfit(rdata$op, outeq = i) %>%
      plotly::as_widget() %>%
      htmlwidgets::saveWidget(sprintf("op_ind%i.html",i), libdir = "deps", selfcontained = F)

      html<-gsub("</op>",sprintf('<div class="col-lg-12">
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
                </div></op>',i,i,i),html)
    }
    

    plot.PMcycle(rdata$cycle) %>%
    plotly::as_widget(height="1060px", width="500px") %>%
    htmlwidgets::saveWidget("cycle.html", libdir = "deps", selfcontained = F)

    
    pmfinal_height=((length(names(rdata$final$popPoints))-1)%/%2)*500
    plot.PMfinal(rdata$final) %>%
    plotly::as_widget(final, height=sprintf("%ipx",pmfinal_height), width="1420px") %>%
    htmlwidgets::saveWidget("final.html", libdir = "deps", selfcontained = F)
    html<-gsub("</pmfinal_height>",sprintf("%ipx",pmfinal_height),html)

    #Edit HTML
    
    html<-gsub("</parameter_values>",paste0('<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Support Points</h2>',makeHTMLdf(rdata$final$popPoints, 3),'</div></parameter_values>'),html)
    report.table <- data.frame(mean = t(rdata$final$popMean), sd = t(rdata$final$popSD), CV = t(rdata$final$popCV), var = t(rdata$final$popVar), median = t(rdata$final$popMedian), shrink = t(100 * rdata$final$shrinkage))
    names(report.table) <- c("Mean", "SD", "CV%", "Var", "Median", "Shrink%")
    html<-gsub("</parameter_values>",paste0('<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Parameter Values Summaries</h2>',makeHTMLdf(report.table, 3),'</div></parameter_values>'),html)
      
    if (rdata$NPdata$nranfix > 0) {
      ranfixdf <- data.frame(Parameter = rdata$NPdata$parranfix, Value = rdata$NPdata$valranfix)
      html<-gsub("</parameter_values>",paste0('<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Population Fixed (but Random) Values</h2>',makeHTMLdf(ranfixdf, 3),'</div></parameter_values>'),html)
    }
    if (rdata$NPdata$nofix > 0) {
      fixdf <- data.frame(Parameter = rdata$NPdata$parfix, Value = rdata$NPdata$valfix)
      html<-gsub("</parameter_values>",paste0('<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Population Fixed (and Constant) Values</h2>',makeHTMLdf(fixdf, 3),'</div></parameter_values>'),html)
    }
    #covariance matrix
    html<-gsub("</parameter_values>",paste0('<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Population Parameter Value Covariance Matrix</h2>',makeHTMLdf(rdata$final$popCov, 3),'</div></parameter_values>'),html)

    #correlation matrix
    html<-gsub("</parameter_values>",paste0('<div class="col-md-6 col-sm-12" style="padding-right: 10px;padding-left: 10px;"><h2>Population Parameter Value Correlation Matrix</h2>',makeHTMLdf(rdata$final$popCor, 3),'</div></parameter_values>'),html)



    if (rdata$NPdata$nofix == 0) {
      parfix <- "There were no constant fixed parameters."
    } else { parfix <- paste("Constant fixed parameters:", paste(rdata$NPdata$parfix, collapse = ", ")) }
    if (rdata$NPdata$nranfix == 0) {
      parranfix <- "There were no random fixed parameters."
    } else { parranfix <- paste("Random fixed parameters:", paste(rdata$NPdata$parranfix, collapse = ", ")) }
    ilog <- rdata$NPdata$ilog
    if (is.null(rdata$NPdata$converge)) {
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
      }
    } else {
      coninterp <- switch(1 + rdata$NPdata$converge, " *The run did not converge before the last cycle.", " - The run converged.", "", " *The run ended with a Hessian Error.")
      confor1 <- switch(1 + rdata$NPdata$converge, "<span class=\"alert\">", "", "", "<span class=\"alert\">")
      confor2 <- switch(1 + rdata$NPdata$converge, "</span>", "", "", "</span>")

    }
    if (reportType == 1 && !is.null(rdata$NPdata$prior)) {
      #this will only be for NPAG
      extra <- paste("Prior density: ", c("Non-uniform (prior.txt)", "Uniform")[1 + as.numeric(rdata$NPdata$prior == "UNIFORM")], "<br>",
                     "Assay error model: ", switch(rdata$NPdata$ERRmod, "SD", "SD, gamma", "SD, lambda", "gamma"), "<br>", sep = "")
    } else extra <- ""
   
    #no error, but alert if no convergence
    #TODO: This code detects if the run does not converged, how are we going to show that?
    # writeHTML(ifelse(coninterp == " - The run converged.",
    #                   "<label for=\"tab5\">Summary</label>",
    #                   "<label for=\"tab5\">Summary<span class=\"alert\">*</span></label>"))
    

    fixedvar <- ""
    html<-gsub("</summary>",paste("Engine: ", c("NPAG", "IT2B")[reportType], "<br>",
                    "Computation mode: ", c("Serial", "Parallel")[1 + as.numeric(parallel)], "<br>",
                    "Output file: <a href=", file.path(wd), c("/NP_RF0001.TXT target=_blank>", "/IT_RF0001.TXT target=_blank>")[reportType], file.path(wd), c("/NP_RF0001.TXT</a><br>", "/IT_RF0001.TXT</a><br>")[reportType],
                    "Random parameters: ", paste(paste(rdata$NPdata$par, collapse = ", "), fixedvar, sep = " "), "<br>",
                    parranfix, "<br>", parfix, "<br>",
                    "Number of analyzed subjects: ", rdata$NPdata$nsub, "<br>",
                    "Number of output equations: ", rdata$NPdata$numeqt, "<br>",
                    "Number of cycles: ", rdata$NPdata$icyctot, "  ", confor1, coninterp, confor2, "<br>",
                    "Additional covariates: ", paste(rdata$NPdata$covnames, collapse = ", "), "<br>", extra,"</summary>", sep = ""),html) 
    

    if (rdata$NPdata$negflag) { html<-gsub("</red>","WARNING: There were negative pop/post predictions.<br></summary>",html) }
    if (!is.na(elapsed)) { html<-gsub("</red>",paste("Elapsed time for this run was", elapsed, attr(elapsed, "units"), "<br></summary>"),html) }
    
    # system(paste0("cp ",report_file," /NPAGreport.html"))
    readr::write_file(html, "NPAGreport.html")

  } else{
    error_file<- paste(path.package("Pmetrics"), "/report/error.html", sep = "")
    html<-readr::read_file(error_file)
    
    errmessage <- readLines(errfile)
    errmessage <- paste(errmessage, collapse = "")
    errmessage <- gsub("  ", " ", errmessage)
    errmessage <- sub("^ *", "", errmessage)

    html<-gsub("</summary>",paste0("<h2>ERROR REPORT<h2>",errmessage),html)
    readr::write_file(html, "NPAGreport.html")
  }

}

PMreport_old <- function(wd, rdata, icen = "median", type = "NPAG", parallel = F) {
  #1 for NPAG, 2 for IT2B, 1 for anything else
  reportType <- switch(type, NPAG = 1, IT2B = 2, 1)

  #checkRequiredPackages("xtable")

  setwd(wd)
  if (missing(rdata)) rdata <- makeRdata(wd, remote=F, reportType)

  #get elapsed time if available
  if (file.exists("time.txt")) {
    execTime <- readLines("time.txt")
    OS <- switch(gsub("[[:blank:]]", "", execTime[1]), Unix = 1, Windows = 2, Linux = 3)
    if (OS == 1 | OS == 3) {
      elapsed <- difftime(as.POSIXct(execTime[3], format = "%s"), as.POSIXct(execTime[2], format = "%s"))
    }
    if (OS == 2) {
      elapsed <- difftime(as.POSIXct(execTime[3], format = "%T"), as.POSIXct(execTime[2], format = "%T"))
    }
  } else { elapsed <- NA }

  #initiate HTML file
  .CSSfile <- paste(path.package("Pmetrics"), "/report/Pmetrics.css", sep = "")
  HTMLfileName <- paste(getwd(), c("/NPAGreport.html", "/IT2Breport.html")[reportType], sep = "")
  if (file.exists(HTMLfileName)) file.remove(HTMLfileName)
  #open HTML file 
  .HTMLfile <- file(HTMLfileName, open = "a+")
  #HTML header
  writeHTML(c("<html>",
              "<head>",
              "<title>Pmetrics Report</title>",
              paste("<link rel=stylesheet href=\"http://lapk.org/software/Pmetrics/Report/Pmetrics.css\" type=text/css>", sep = ""),
              paste("<link rel=stylesheet href=\"", .CSSfile, "\" type=text/css>", sep = ""),
              "</head>",
              "<body>"))
  #header
  writeHTML("<div class='wrap'>") #wrapper around header
  writeHTML("<div class='header'>")
  dtstamp <- format(Sys.time(), "%d %b %Y at %H:%M")
  writeHTML("<div class='wrap'>") #wrapper around columns
  writeHTML("<div class='leftcol'>")
  writeHTML(paste("<img src=\"", path.package("Pmetrics"), "/report/pmetrics-logo.png\" class=\"pmetrics\">", sep = ""))
  writeHTML(paste("<h4>Report generated ", dtstamp, "</h4>", sep = ""))
  writeHTML("</div>") #end left column
  writeHTML("<div class='fullrightcol'>")
  writeHTML("<h4>Laboratory of Applied Pharmacokinetics<br>4650 Sunset Blvd., Los Angeles, CA 90027<br>Tel: +01 323 361 2509
            <br><a href='http://www.lapk.org' target='_blank'>www.lapk.org</a><br>")
  writeHTML("</div>") #end right column
  writeHTML("</div>") #end column wrap
  writeHTML("</div>") #end header
  writeHTML("</div>") #end header wrap

  #check for error file
  errfile <- rdata$errfile
  error <- length(errfile) > 0
  # #see if NP_RF or IT_RF made anyway (i.e. is >1MB in size)
  success <- rdata$success
  # success <- file.info(c("NP_RF0001.TXT", "IT_RF0001.TXT")[reportType])$size >= 1000

  if (success) {

    #run completed
    #open and parse the output
    if (reportType == 1) {
      PMdata <- rdata$NPdata
      post <- rdata$post
      pop <- rdata$pop
    }
    if (reportType == 2) {
      PMdata <- rdata$ITdata
    }
 

    #HTML body
    writeHTML("<ul class=\"tabs\">")

    #Tab 2 - Observed vs. Predicted
    writeHTML("<li>")
    writeHTML("<input type=\"radio\" checked name=\"tabs\" id=\"tab1\">")
    writeHTML("<label for=\"tab1\">Obs-Pred Plots</label>")
    writeHTML("<div id=\"tab-content1\" class=\"tab-content\">")
    #Plot
    op <- rdata$op
    #op <- suppressWarnings(tryCatch(makeOP(PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of observed vs. population predicted data; 'PMop' object not saved.\n\n") }))
    for (i in 1:PMdata$numeqt) {
      if (!all(is.null(op))) {
        OPpng <- paste("op", i, ".png", sep = "")
        OPpdf <- paste("op", i, ".pdf", sep = "")
        writeHTML(paste("<h2>Output", i, "&nbsp<a href='", OPpdf, "'>Open image file</a></h2>"))
        png(filename = OPpng, width = 1200, height = 600)
        par(mfrow = c(1, 2))
        plot(op, xlab = "Population Predicted", outeq = i, pred.type = "pop", x.stat = 0.4, cex.stat = 1.2, font = 2)
        plot(op, xlab = "Individual Predicted", outeq = i, x.stat = 0.4, cex.stat = 1.2, font = 2)
        par(mfrow = c(1, 1))
        dev.off()
        pdf(file = OPpdf, width = 12, height = 6)
        par(mfrow = c(1, 2))
        plot(op, xlab = "Population Predicted", outeq = i, pred.type = "pop", x.stat = 0.4, cex.stat = 0.7)
        plot(op, xlab = "Individual Posterior Predicted", outeq = i, x.stat = 0.4, cex.stat = 0.7)
        par(mfrow = c(1, 1))
        dev.off()
        writeHTML(paste("<img border='0' src='", OPpng, "' alt='OPplot' />", sep = ""))

      }
    }

    writeHTML("</div>") #end tab content
    writeHTML("</li>")

    #Tab 2 - Convergence plots
    writeHTML("<li>")
    writeHTML("<input type=\"radio\" name=\"tabs\" id=\"tab2\">")
    writeHTML("<label for=\"tab2\">Convergence Plots</label>")
    writeHTML("<div id=\"tab-content2\" class=\"tab-content\">")
    #icycle plot
    if (PMdata$icyctot > 0) {
      cycle <- rdata$cycle
      #cycle <- suppressWarnings(tryCatch(makeCycle(PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of cycle information; 'PMcycle' object not saved.\n\n") }))
      if (!all(is.null(cycle))) {
        pdf(file = "cycle.pdf", width = 12, height = 10);
        plot(cycle, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2);
        dev.off()
        png(filename = "cycle.png", width = 1200, height = 1000);
        plot(cycle, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2);
        dev.off()
        writeHTML("<h2><a href='cycle.pdf'>Open image file</a></h2>")
        writeHTML("<img border='0' src='cycle.png' alt='Cycle Information' />")
      } else {
        writeHTML("No cycles in this run.")
      }
    }
    writeHTML("</div>") #end tab content
    writeHTML("</li>")

    #Tab 3 - Marginal plots
    writeHTML("<li>")
    writeHTML("<input type=\"radio\" name=\"tabs\" id=\"tab3\">")
    writeHTML("<label for=\"tab3\">Marginal Plots</label>")
    writeHTML("<div id=\"tab-content3\" class=\"tab-content\">")
    #marginals
    final <- rdata$final
    #final <- suppressWarnings(tryCatch(makeFinal(PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of final cycle parameter values; 'PMfinal' object not saved.\n\n") }))
    if (!all(is.null(final))) {
      pdf(file = "final.pdf", width = 12, height = 4 * ceiling(PMdata$nvar / 3));
      plot(final, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5);
      dev.off()
      png(filename = "final.png", width = 1200, height = 400 * ceiling(PMdata$nvar / 3));
      plot(final, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5);
      dev.off()
      writeHTML("<h2><a href='final.pdf'>Open image file</a></h2>")
      writeHTML("<img border='0' src='final.png' alt='Marginal Densities' />")
    }
    writeHTML("</div>") #end tab content
    writeHTML("</li>")

    #Tab 4 - Parameter values
    writeHTML("<li>")
    writeHTML("<input type=\"radio\" name=\"tabs\" id=\"tab4\">")
    writeHTML("<label for=\"tab4\">Parameter Values</label>")
    writeHTML("<div id=\"tab-content4\" class=\"tab-content\">")
    if (!all(is.null(final))) {
      report.table <- data.frame(mean = t(final$popMean), sd = t(final$popSD), CV = t(final$popCV), var = t(final$popVar), median = t(final$popMedian), shrink = t(100 * final$shrinkage))
      names(report.table) <- c("Mean", "SD", "CV%", "Var", "Median", "Shrink%")
      if (reportType == 1) {
        #only for NPAG
        #Support point matrix
        writeHTML("<h2>Support Points</h2>")
        writeHTML(makeHTMLdf(final$popPoints, 3))
      }
      #popparar
      writeHTML("<h2>Population Parameter Value Summaries</h2>")
      writeHTML(makeHTMLdf(report.table, 3))
      if (PMdata$nranfix > 0) {
        ranfixdf <- data.frame(Parameter = PMdata$parranfix, Value = PMdata$valranfix)
        writeHTML("<h2>Population Fixed (but Random) Values</h2>")
        writeHTML(makeHTMLdf(ranfixdf, 3))
      }
      if (PMdata$nofix > 0) {
        fixdf <- data.frame(Parameter = PMdata$parfix, Value = PMdata$valfix)
        writeHTML("<h2>Population Fixed (and Constant) Values</h2>")
        writeHTML(makeHTMLdf(fixdf, 3))
      }
      #covariance matrix
      writeHTML("<h2>Population Parameter Value Covariance Matrix</h2>")
      writeHTML(makeHTMLdf(final$popCov, 3))
      #correlation matrix
      writeHTML("<h2>Population Parameter Value Correlation Matrix</h2>")
      writeHTML(makeHTMLdf(final$popCor, 3))
    }
    writeHTML("</div>") #end tab content
    writeHTML("</li>")

    #Tab 5 - Summary

    #summary
    if (PMdata$nofix == 0) {
      parfix <- "There were no constant fixed parameters."
    } else { parfix <- paste("Constant fixed parameters:", paste(PMdata$parfix, collapse = ", ")) }
    if (PMdata$nranfix == 0) {
      parranfix <- "There were no random fixed parameters."
    } else { parranfix <- paste("Random fixed parameters:", paste(PMdata$parranfix, collapse = ", ")) }
    ilog <- PMdata$ilog
    if (is.null(PMdata$converge)) {
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
      }
    } else {
      coninterp <- switch(1 + PMdata$converge, " *The run did not converge before the last cycle.", " - The run converged.", "", " *The run ended with a Hessian Error.")
      confor1 <- switch(1 + PMdata$converge, "<span class=\"alert\">", "", "", "<span class=\"alert\">")
      confor2 <- switch(1 + PMdata$converge, "</span>", "", "", "</span>")

    }
    if (reportType == 1 && !is.null(PMdata$prior)) {
      #this will only be for NPAG
      extra <- paste("Prior density: ", c("Non-uniform (prior.txt)", "Uniform")[1 + as.numeric(PMdata$prior == "UNIFORM")], "<br>",
                     "Assay error model: ", switch(PMdata$ERRmod, "SD", "SD, gamma", "SD, lambda", "gamma"), "<br>", sep = "")
    } else extra <- ""
    writeHTML("<li>")
    writeHTML("<input type=\"radio\" name=\"tabs\" id=\"tab5\">")
    if (error) {
      #there was an error, so make the tab with an alert
      writeHTML("<label for=\"tab5\">Summary<span class=\"alert\">*</span></label>")
    } else {
      #no error, but alert if no convergence
      writeHTML(ifelse(coninterp == " - The run converged.",
                       "<label for=\"tab5\">Summary</label>",
                       "<label for=\"tab5\">Summary<span class=\"alert\">*</span></label>"))
    }
    writeHTML("<div id=\"tab-content1\" class=\"tab-content\">")

    if (reportType == 2 && length(PMdata$fixedpos) > 0) {
      #this is only for IT2B
      fixedvar <- paste("(", paste(PMdata$par[PMdata$fixedpos], collapse = ", "), " fixed to be positive)", sep = "")
    } else { fixedvar <- "" }
    writeHTML(paste("Engine: ", c("NPAG", "IT2B")[reportType], "<br>",
                    "Computation mode: ", c("Serial", "Parallel")[1 + as.numeric(parallel)], "<br>",
                    "Output file: <a href=", file.path(wd), c("/NP_RF0001.TXT target=_blank>", "/IT_RF0001.TXT target=_blank>")[reportType], file.path(wd), c("/NP_RF0001.TXT</a><br>", "/IT_RF0001.TXT</a><br>")[reportType],
                    "Random parameters: ", paste(paste(PMdata$par, collapse = ", "), fixedvar, sep = " "), "<br>",
                    parranfix, "<br>", parfix, "<br>",
                    "Number of analyzed subjects: ", PMdata$nsub, "<br>",
                    "Number of output equations: ", PMdata$numeqt, "<br>",
                    "Number of cycles: ", PMdata$icyctot, "  ", confor1, coninterp, confor2, "<br>",
                    "Additional covariates: ", paste(PMdata$covnames, collapse = ", "), "<br>", extra, sep = ""))

    if (PMdata$negflag) { writeHTML("WARNING: There were negative pop/post predictions.<br>") }
    if (!is.na(elapsed)) { writeHTML(paste("Elapsed time for this run was", elapsed, attr(elapsed, "units"), "<br>")) }
    if (error) {
      errmessage <- readLines(errfile)
      errmessage <- paste(errmessage, collapse = "")
      errmessage <- gsub("  ", " ", errmessage)
      errmessage <- sub("^ *", "", errmessage)

      writeHTML("<h2>ERROR REPORT<h2>")
      writeHTML(errmessage)
    }
    writeHTML("</div>") #end tab content
    writeHTML("</li>")

    writeHTML("</ul>")
    writeHTML("</body></html>")


    close(.HTMLfile)

    ####### TEX section ###############
    if (success) {
      #open TEX file 
      TEXfileName <- paste(getwd(), c("/NPAGreport.tex", "/IT2Breport.tex")[reportType], sep = "")
      if (file.exists(TEXfileName)) file.remove(TEXfileName)
      .TEXfile <- file(TEXfileName, open = "a+")
      
      #TEX header
      TEX(paste("\\documentclass{article}
             \\usepackage{graphicx}
             \\usepackage[colorlinks]{hyperref} 
             \\usepackage{url}
             \\usepackage{float}
             \\usepackage[landscape]{geometry}"))
      TEX(paste("\\title{Report generated by Pmetrics package for R on", dtstamp, "}"))
      TEX(paste("\\date{}
              \\begin{document}
              \\maketitle"))
      TEX("Laboratory of Applied Pharmacokinetics $\\cdot$ 4650 Sunset Blvd. MS\\#51, Los Angeles, CA 90027 $\\cdot$ (323) 361-5046 $\\cdot$ \\href{http://www.lapk.org}{www.lapk.org}")

      #TEX table of contents
      TEX("\\hypertarget{tableofcontents}{}
        \\tableofcontents
        \\newpage")

      TEX("\\section{Summary} \\hyperlink{tableofcontents}{Back to Contents} $\\cdot$ \\hyperlink{cycleinfo}{Next Section}\\newline
      \\newline")

      TEX(paste("Engine: ", c("NPAG", "IT2B")[reportType], "\\newline", sep = ""))
      TEX(paste("Output file: ", file.path(wd), c("/NP\\_RF0001.TXT", "/IT\\_RF0001.TXT")[reportType], "\\newline", sep = ""))
      TEX(paste("Random parameters:", paste(PMdata$par, collapse = ", "), "\\newline", sep = ""))
      TEX(paste(parranfix, "\\newline"))
      TEX(paste(parfix, "\\newline"))
      TEX(paste("Number of analyzed subjects: ", PMdata$nsub, "\\newline"))
      TEX(paste("Number of output equations: ", PMdata$numeqt, "\\newline"))
      TEX(paste("Number of cycles: ", PMdata$icyctot, "  ", coninterp, "\\newline"))
      TEX(paste("Additional covariates: ", paste(PMdata$covnames, collapse = ", "), "\\newline"))
      TEX(paste(gsub("<br>", " \\\\newline ", extra), "\\newline"))

      if (PMdata$negflag) { TEX("WARNING: There were negative pop/post predictions.") }
      if (error) {
        errmessage <- readLines(errfile)
        errmessage <- paste(errmessage, collapse = "")
        errmessage <- gsub("  ", " ", errmessage)
        errmessage <- sub("^ *", "", errmessage)

        TEX("\\textbf{ERROR REPORT}\\newline")
        TEX(errmessage)
      }

      #icycle plot
      if (PMdata$icyctot > 0) {
        if (!all(is.null(cycle))) {
          #setEPS()
          #postscript("cycle.eps",width=12,height=10); plot(cycle,cex.lab=1.4,font=2,cex.axis=1.2); dev.off()
          TEX("\\newpage
            \\hypertarget{cycleinfo}{}
            
            \\section{Cycle information} \\ ")
          TEX(" $\\cdot$ \\hyperlink{tableofcontents}{Back to Contents} $\\cdot$ \\hyperlink{mppd}{Next Section} \\newline")
          TEX("\\begin{figure}[H] 
            \\includegraphics[height=4.99in,width=7in]{cycle.pdf}
            \\end{figure}")
        } else {
          TEX("\\section{Cycle information}")
          TEX("No cycles in this run.")
        }
      }
      #marginals
      if (!all(is.null(final))) {
        #setEPS()
        #postscript("final.eps",width=12,height=4*ceiling(PMdata$nvar/3)); plot(final,density=F,cex.lab=2.0,font=2,cex.axis=1.5); dev.off()
        TEX("\\hypertarget{mppd}{}
          \\section{Marginal population parameter distributions}")
        TEX(" $\\cdot$ \\hyperlink{tableofcontents}{Back to Contents} $\\cdot$ \\hyperlink{suppoint}{Next Section} \\newline
          ")
        TEX("\\begin{figure}[H] 
          \\includegraphics[height=4.5in,width=8in]{final.pdf}
          \\end{figure}")

        if (reportType == 1) {
          #NPAG only
          #Support point matrix
          TEX("              
          \\hypertarget{suppoint}{}
          
          \\section{Support points}")
          TEX(" $\\cdot$ \\hyperlink{tableofcontents}{Back to Contents} $\\cdot$ \\hyperlink{ppe}{Next Section} \\newline
          \\newline")
          s1 <- final$popPoints
          tab1 <- xtable::xtable(s1)
          # if (xtable.installed) { tab1 <- xtable(s1) } else { tab1 <- "Package xtable not installed" }
          print(tab1, file = .TEXfile, append = T, floating = FALSE)
        }

        #popparam
        TEX("              
          \\hypertarget{ppe}{}
          
          \\section{Population parameter estimate summary statistics}")
        TEX(" \\hyperlink{tableofcontents}{Back to Contents} $\\cdot$ \\hyperlink{covforppe}{Next Section} \\newline
          \\newline")
        s2 <- report.table
        tab2 <- xtable::xtable(s2)
        # if (xtable.installed) { tab2 <- xtable(s2) } else { tab2 <- "Package xtable not installed." }
        print(tab2, file = .TEXfile, append = T, floating = FALSE)

        TEX("\\newpage
          
          \\hypertarget{covforppe}{}
          
          \\section{Covariance matrix for population parameter estimates}")
        TEX(" \\hyperlink{tableofcontents}{Back to Contents} $\\cdot$ \\hyperlink{corforppe}{Next Section} \\newline
          \\newline")
        s3 <- final$popCov
        tab3 <- xtable::xtable(s3)
        # if (xtable.installed) { tab3 <- xtable(s3) } else { tab3 <- "Package xtable not installed." }
        print(tab3, file = .TEXfile, append = T, floating = FALSE)
        #correlation matrix
        TEX("
          \\hypertarget{corforppe}{}
          
          \\section{Correlation matrix for population parameter estimates}")
        TEX(" \\hyperlink{tableofcontents}{Back to Contents} $\\cdot$ \\hyperlink{opp}{Next Section} \\newline
          \\newline")
        s4 <- final$popCor
        tab4 <- xtable::xtable(s4)
        #if (xtable.installed) { tab4 <- xtable(s4) } else { tab4 <- "Package xtable not installed." }
        print(tab4, file = .TEXfile, append = T, floating = FALSE)
      }

      #OP
      TEX("\\newpage
        
        \\hypertarget{opp}{}
        
        \\section{Observed vs. predicted plots}
        \\hyperlink{tableofcontents}{Back to Contents} \\newline
        \\newline")
      for (i in 1:PMdata$numeqt) {
        if (!all(is.null(op))) {
          #OPeps <-paste("op",i/2,".eps",sep="")
          TEX(paste("Output ", i, sep = ""))
          #setEPS()
          #postscript(OPeps,width=12,height=6)
          #par(mfrow=c(1,2))
          #plot(op[[i-1]],ylab="Population Predicted",x.stat=0.4,cex.stat=1.2,font=2)
          #plot(op[[i]],ylab="Individual Predicted",x.stat=0.4,cex.stat=1.2,font=2)
          #par(mfrow=c(1,1))
          #dev.off()

          TEX(paste("\\begin{figure}[H] 
              \\includegraphics[height=4.5in,width=8in]{op", i, ".pdf} 
              \\end{figure}", sep = ""))
        }
      }
      # TEXend()
      TEX("\\end{document}\n")
    }
    #end if success block

    ##### END TEX section #########


    #make the covariate object but not included in report
    # cov <- suppressWarnings(tryCatch(makeCov(PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of covariate-parameter data; 'PMcov' object not saved.\n\n") }))
    if (!all(is.null(final))) {
      write.csv(report.table, "popparam.csv", row.names = T)
      if (reportType == 1) { write.csv(final$popPoints, "poppoints.csv", row.names = T) }
      write.csv(final$popCov, "popcov.csv", row.names = T)
      write.csv(final$popCor, "popcor.csv", row.names = T)
    }
    # if (PMdata$mdata != "NA") { mdata <- PMreadMatrix(paste("../inputs/", PMdata$mdata, sep = ""), quiet = T) } else { mdata <- NA }

    #summary of saved objects
    # cat(paste("\n\n\nSaving R data objects to ", wd, "......\n\n", sep = ""))
    # cat("\nUse PMload() to load them.\n")
    # cat("\nThe following objects have been saved:\n")
    # cat(c("\nNPdata: All output from NPAG\n", "\nITdata: All output from IT2B\n")[reportType])
    # if (reportType == 1 && !all(is.null(pop))) cat("pop: Population predictions at regular, frequent intervals\n")
    # if (reportType == 1 && !all(is.null(post))) cat("post: Posterior predictions at regular, frequent inteverals\n")
    # if (!all(is.null(final))) cat("final: Final cycle parameters and summary statistics\n")
    # if (!all(is.null(cycle))) cat("cycle: Cycle information\n")
    # if (!all(is.null(op))) cat("op: Observed vs. population and posterior predicted\n")
    # if (!all(is.null(cov))) cat("cov: Individual covariates and Bayesian posterior parameters\n")
    # if (length(mdata) > 1) cat("mdata: The data file used for the run\n")

    # if (reportType == 1) {
    #   NPAGout <- list(NPdata = PMdata, pop = pop, post = post, final = final, cycle = cycle, op = op, cov = cov, mdata = mdata)
    #   save(NPAGout, file = "NPAGout.Rdata")
    # }
    # if (reportType == 2) {
    #   IT2Bout <- list(ITdata = PMdata, final = final, cycle = cycle, op = op, cov = cov, mdata = mdata)
    #   save(IT2Bout, file = "IT2Bout.Rdata")
    # }

    return(invisible(NULL))
  } else {
    #yes there was an errorfile made
    errmessage <- readLines(errfile)
    errmessage <- paste(errmessage, collapse = "")
    errmessage <- gsub("  ", " ", errmessage)
    errmessage <- sub("^ *", "", errmessage)

    writeHTML("<h2>ERROR REPORT<h2>")
    writeHTML(errmessage)
    close(.HTMLfile)
    return(invisible(NULL))
  }
}
#end function

makeRdata <- function(wd, remote, reportType = 1) {
  setwd(wd)
  errfile <- list.files(pattern = "^ERROR")
  #error <- length(errfile) > 0
  #see if NP_RF or IT_RF made anyway (i.e. is >1MB in size)
  success <- file.info(c("NP_RF0001.TXT", "IT_RF0001.TXT")[reportType])$size >= 1000

  if (success) {
    #run completed
    #open and parse the output
    if (reportType == 1) {
      #only for NPAG
      PMdata <- suppressWarnings(tryCatch(NPparse(), error = function(e) { e <- NULL; cat("\nWARNING: The run did not complete successfully.\n") }))
      #make the posterior predictions
      post <- suppressWarnings(tryCatch(makePost(NPdata = PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of posterior Bayesian predictions at time ttpred; 'PMpost' object not saved.\n\n") }))
      #make the population predictions
      pop <- suppressWarnings(tryCatch(makePop(NPdata = PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of population predictions at time tpred; 'PMpop' object not saved.\n\n") }))
    } else {
      PMdata <- suppressWarnings(tryCatch(ITparse(), error = function(e) { e <- NULL; cat("\nWARNING: The run did not complete successfully.\n") }))
    }
    cat("\n\n")
    flush.console()
    if (is.null(PMdata$nranfix)) PMdata$nranfix <- 0
    op <- suppressWarnings(tryCatch(makeOP(PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of observed vs. population predicted data; 'PMop' object not saved.\n\n") }))
    cycle <- suppressWarnings(tryCatch(makeCycle(PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of cycle information; 'PMcycle' object not saved.\n\n") }))
    final <- suppressWarnings(tryCatch(makeFinal(PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of final cycle parameter values; 'PMfinal' object not saved.\n\n") }))
    if (PMdata$mdata != "NA") { mdata <- PMreadMatrix(paste("../inputs/", PMdata$mdata, sep = ""), quiet = T) } else { mdata <- NA }
    cov <- suppressWarnings(tryCatch(makeCov(PMdata), error = function(e) { e <- NULL; cat("\nWARNING: error in extraction of covariate-parameter data; 'PMcov' object not saved.\n\n") }))
    if (PMdata$mdata != "NA") { mdata <- PMreadMatrix(paste("../inputs/", PMdata$mdata, sep = ""), quiet = T) } else { mdata <- NA }

    cat(paste("\n\n\nSaving R data objects to ", wd, "......\n\n", sep = ""))
    cat("\nUse PMload() to load them.\n")
    cat("\nThe following objects have been saved:\n")
    cat(c("\nNPdata: All output from NPAG\n", "\nITdata: All output from IT2B\n")[reportType])
    if (reportType == 1 && !all(is.null(pop))) cat("pop: Population predictions at regular, frequent intervals\n")
    if (reportType == 1 && !all(is.null(post))) cat("post: Posterior predictions at regular, frequent inteverals\n")
    if (!all(is.null(final))) cat("final: Final cycle parameters and summary statistics\n")
    if (!all(is.null(cycle))) cat("cycle: Cycle information\n")
    if (!all(is.null(op))) cat("op: Observed vs. population and posterior predicted\n")
    if (!all(is.null(cov))) cat("cov: Individual covariates and Bayesian posterior parameters\n")
    if (length(mdata) > 1) cat("mdata: The data file used for the run\n")


    if (reportType == 1) {
      NPAGout <- list(NPdata = PMdata, pop = pop, post = post, final = final, cycle = cycle, op = op, cov = cov, mdata = mdata, errfile = errfile, success = success)
      save(NPAGout, file = "NPAGout.Rdata")
      #Hacky return to deal with Rservex bug T.T
      if (remote) {
        return("ok")
      }
      return(NPAGout)
    }
    if (reportType == 2) {
      IT2Bout <- list(ITdata = PMdata, final = final, cycle = cycle, op = op, cov = cov, mdata = mdata, errfile = errfile, success = success)
      save(IT2Bout, file = "IT2Bout.Rdata")
      if (remote) {
        return("ok")
      }
      return(IT2Bout)
    }
  }
}

# HTML tools --------------------------------------------------------------

#function to process data.frames
makeHTMLdf <- function(df, ndigit) {
  print(df)
  Nrow <- nrow(df)
  Ncol <- ncol(df)
  dfScript <- vector("character")
  dfScript <- append(dfScript,'<table border=0 class="table table-striped table-hover">')
  dfScript <- append(dfScript,"<tbody>")
  dfScript <- append(dfScript,"<tr class=firstline>")
  dfScript <- append(dfScript,"<th></th>")
  for (j in 1:Ncol) {
    dfScript <- append(dfScript,paste('<th scope="col">', colnames(df)[j], "</th>", sep = ""))
  }
  dfScript <- append(dfScript,"</tr>") #end first row headers
  for (i in 1:Nrow) {
    dfScript <- append(dfScript,"<tr>")
    dfScript <- append(dfScript,paste("<td class=firstcolumn>", rownames(df)[i], "</td>", sep = ""))
    for (j in 1:Ncol) {
      dfScript <- append(dfScript,ifelse(inherits(df[i, j], "numeric"),
                                            paste("<td class=cellinside>", sprintf(paste("%.", ndigit, "f", sep = ""), round(as.numeric(df[i, j]), ndigit)), "</td>", sep = ""),
                                            paste("<td class=cellinside>", df[i, j])))
    }
    dfScript <- append(dfScript,"</tr>")
  }
  dfScript <- append(dfScript,"</tbody>")
  dfScript <- append(dfScript,"</table>")
  
  return(paste(dfScript, collapse=""))
  
}
#end makeHTMLdf function

writeHTML <- function(x) {
  .HTMLfile <- get(".HTMLfile",pos=parent.frame())
  cat(c(paste(x, collapse = "\n"), "\n"), file = .HTMLfile, append = T)
}


# Tex functions -----------------------------------------------------------
#written by Alona Kryschenko

TEX <- function(x) {
  .TEXfile <- get(".TEXfile",pos=parent.frame())
  cat(paste(x, collapse = ''), '\n', file = .TEXfile, append = T)
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




