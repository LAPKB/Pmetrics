# This file contains internal Pmetrics utility functions
# BUG FIX wmy.2017.04.12 The Femke error polynomial quandry.
#
# wmy.2017.04.12
#
# Symptoms:
# For NPrun(...data=RunNo...) the RunNo/wrk/working_files are copied to the new
# NPAG run. The working files include an error polynomial. NPAG
# uses this error polynomial instead of the error function requested in the
# model.txt file for the new optimization.
#
# Relevant observations:
# 1) NPprep() reads the name of the model.txt file, just prior to
# determining if data=RunNo or a new *.csv file.
#
# Solution: Immediately after determining that data=RunNo AND is.integer(RunNo) == T,
# compare the #ERR block of /RunNo/input/model.txt to the model.txt for the current
# run. If equivalent, no problem -- if different, then use the /RunNo/input/*.csv
# as a "new" dataset and generate working files from it.  Soution reqs the following
# two utility functions:
# 1) compareTwoModelERRs <- function(mod1,mod2)
# 2)

#
# This function compares two model.txt file #ERR blocks and returns T/F if
# the same or different, respectively.
#
# Below is MN's code as of 12/3/18

compareTwoModelERRs <- function(mod1, mod2) {
  getAssErr <- function(model) {
    blocks <- parseBlocks(model)
    if (length(grep(";", blocks$primVar)) > 0) {
      # using ';' as separator
      sep <- ";"
    } else {
      if (length(grep(",", blocks$primVar)) > 0) {
        # using ',' as separator
        sep <- ","
      } else {
        return(F)
      }
    }
    blocks$error <- tolower(gsub("[[:space:]]", "", blocks$error))
    gamlam <- grep("^g|^l", blocks$error)
    asserr <- gsub(sep, "  ", blocks$error[-gamlam])
    return(asserr)
  }
  asserr1 <- getAssErr(mod1)
  asserr2 <- getAssErr(mod2)
  return(identical(asserr1, asserr2))
}

# This utility gets a block of information from a past run instr.inx file.
#
# TODO
#  1) add parameter for pathToInstFile -- currently just assumes this is "../
#    which should be fine b/c the only reason to use this utility is
#    to compare old instructions to a new NPAG run instructions.  Therefore,
#    the user should be calling this utility from a /.../Runs/new_run/ directory,
#    and the old instructions will be in ../data/etc/instr.inx, where data is
#    the interger identifier of the old run.
#  2) Write error checking code for: if a file or directory exists,
#    if an instr is a valid phrase, if old.data is an integer, REM* is the
#    first line and is not preceded by a HEADER line, PAR(I) is treated as
#    regex instead of as plain text,
#
getNPinstr <- function(old.data, instr) {
  Ifile <- paste("../", old.data, "/etc/instr.inx", sep = "")
  # The header lines for all instruction blocks in old.data (lines that begin w/a " ")
  HLs <- grep(" ", readLines(Ifile))
  lines <- readLines(Ifile)
  FirstChar <- substring(lines, 1, 1)
  HLs <- which(FirstChar == " ")
  # The header line for the requested instruction block
  dataHeaderLine <- (grep(instr, readLines(Ifile), ignore.case = T))
  # The desired block is in instr.inx[dataHeaderLine:HLs[dataHeaderLine+1]]
  instrBlockStart <- HLs[which(HLs == dataHeaderLine)] + 1
  instrBlockStop <- HLs[which(HLs == dataHeaderLine) + 1] - 1
  if ((instrBlockStop - instrBlockStart) > -1) {
    read1 <- readLines(Ifile, n = instrBlockStop)
    InstrBlock <- read1[instrBlockStart:instrBlockStop]
  } else {
    InstrBlock <- -99
  }
  return(InstrBlock)
}
# wmy ########### END of Femke Error Polynomial Quandry

# make pretty log axes
logAxis <- function(side, grid = F, ...) {
  pow <- log10(axTicks(side))
  pow[1] <- ceiling(pow[1])
  pow[length(pow)] <- floor(pow[length(pow)])
  pow <- round(pow, 0)
  pow <- unique(pow)
  ticksat1 <- 10^pow
  ticksat2 <- as.vector(sapply(pow, function(p) (1:9) * 10^p))
  labels <- formatC(ticksat1, digits = 5, format = "g")
  for (i in 1:length(labels)) {
    if (length(grep("e", labels[i])) > 0) labels[i] <- as.expression(substitute(10^x, list(x = log10(ticksat1[i]))))
  }
  axis(side, ticksat1, labels = labels, tcl = -0.5, lwd = 0, lwd.ticks = 1, ...)
  axis(side, ticksat2, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1, ...)
  
  if (grid & (side == 1 | side == 3)) abline(v = ticksat2, col = "lightgray", lty = 1)
  if (grid & (side == 2 | side == 4)) abline(h = ticksat2, col = "lightgray", lty = 1)
}


# sample from multivariate normal distribution, code modified from mvtnorm package
rmnorm <- function(n, mean, sigma) {
  sigma1 <- sigma
  ev <- eigen(sigma, symmetric = TRUE)
  retval <- ev$vectors %*% diag(sqrt(ev$values), length(ev$values)) %*%
  t(ev$vectors)
  retval <- matrix(rnorm(n * ncol(sigma)), nrow = n) %*% retval
  retval <- sweep(retval, 2, mean, "+")
  colnames(retval) <- names(mean)
  retval
}

# density function for the multivariate normal distribution, code from mvtnorm package
dmv_norm <- function(
  x, mean = rep(0, p), sigma = diag(p), log = FALSE,
  checkSymmetry = TRUE
) {
  if (is.vector(x)) {
    x <- matrix(x, ncol = length(x))
  }
  p <- ncol(x)
  if (!missing(mean)) {
    if (!is.null(dim(mean))) {
      dim(mean) <- NULL
    }
    if (length(mean) != p) {
      stop("x and mean have non-conforming size")
    }
  }
  if (!missing(sigma)) {
    if (p != ncol(sigma)) {
      stop("x and sigma have non-conforming size")
    }
    if (checkSymmetry && !isSymmetric(sigma,
      tol = sqrt(.Machine$double.eps),
      check.attributes = FALSE
    )) {
      stop("sigma must be a symmetric matrix")
    }
  }
  dec <- tryCatch(base::chol(sigma), error = function(e) e)
  if (inherits(dec, "error")) {
    x.is.mu <- colSums(t(x) != mean) == 0
    logretval <- rep.int(-Inf, nrow(x))
    logretval[x.is.mu] <- Inf
  } else {
    tmp <- backsolve(dec, t(x) - mean, transpose = TRUE)
    rss <- colSums(tmp^2)
    logretval <- -sum(log(diag(dec))) - 0.5 * p * log(2 *
      pi) - 0.5 * rss
    }
    names(logretval) <- rownames(x)
    if (log) {
      logretval
    } else {
      exp(logretval)
    }
  }
  
  openHTML <- function(x) {
    if (file.exists(x)) {
      utils::browseURL(normalizePath(x, winslash = "/", mustWork = FALSE))
    } else {
      cli::cli_warn(c(
        "!" = "HTML file not found: {.val {x}}.",
        "i" = "Please check the path and try again."
      ))
    }
  }
  # parse NP_RF file only for final cycle information; used for bootstrapping
  # indpts,ab,corden,nvar,nactve,iaddl,icyctot,par
  
  
  random_name <- function() {
    n <- 1
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }
  
  
  # check for numeric id and convert to number if necessary
  checkID <- function(id) {
    id <- gsub("^[[:blank:]]+", "", id)
    id <- gsub("[[:blank:]]+$", "", id)
    idNonNum <- suppressWarnings(any(is.na(as.numeric(id))))
    if (!idNonNum) id <- as.numeric(id)
    return(id)
  }
  
  # extract pattern from strings
  strparse <- function(pattern, x) {
    match <- regexpr(pattern, x, ignore.case = T)
    start <- match[1]
    stop <- match[1] + attr(match, "match.length") - 1
    return(substr(x, start, stop))
  }
  
  
  # parse blocks in new model template
  parseBlocks <- function(model) {
    modelFile <- scan(model, what = "character", sep = "\n", blank.lines.skip = T, quiet = T)
    # remove comment lines
    commLn <- grep("^C ", modelFile)
    if (length(commLn) > 0) modelFile <- modelFile[-commLn]
    if (length(grep("TSTMULT", modelFile)) > 0) {
      return(list(status = 0, model = model))
    }
    # stop, we already have a fortran model file
    blockStart <- grep("#", modelFile)
    blockStop <- c(blockStart[-1] - 1, length(modelFile))
    headers <- tolower(modelFile[blockStart])
    primVar <- blockStart[grep("#pri", headers)]
    covar <- blockStart[grep("#cov", headers)]
    secVar <- blockStart[grep("#sec", headers)]
    bolus <- blockStart[grep("#bol", headers)]
    ini <- blockStart[grep("#ini", headers)]
    f <- blockStart[grep("#f", headers)]
    lag <- blockStart[grep("#lag", headers)]
    diffeq <- blockStart[grep("#dif", headers)]
    eqn <- blockStart[grep("#eqn", headers)]
    output <- blockStart[grep("#out", headers)]
    error <- blockStart[grep("#err", headers)]
    extra <- blockStart[grep("#ext", headers)]
    
    if (length(diffeq) > 0) {
      eqn <- diffeq
    } # change diffeq block to eqn for more general
    
    headerPresent <- which(c(
      length(primVar) > 0, length(covar) > 0, length(secVar) > 0, length(bolus) > 0, length(ini) > 0,
      length(f) > 0, length(lag) > 0, length(eqn) > 0, length(output) > 0, length(error) > 0, length(extra) > 0
    ))
    missing_mandatory <- which(!c(1, 8:10) %in% headerPresent)
    if (length(missing_mandatory)) {
      # missing mandatory headers
      missing_headers <- c("#PRI", "#EQN", "#OUT", "#ERR")[missing_mandatory]
      cli::cli_abort(c("x" = "Model file is missing mandatory header{?s}: {missing_headers} "))
    }
    
    headerOrder <- c(primVar, covar, secVar, bolus, ini, f, lag, eqn, output, error, extra)
    blockStart <- blockStart[rank(headerOrder)]
    blockStop <- blockStop[rank(headerOrder)]
    
    # remove headers that have no information
    ok <- mapply(function(x, y) x != y, blockStart, blockStop)
    blockStart <- blockStart[ok]
    blockStop <- blockStop[ok]
    headerPresent <- headerPresent[ok]
    
    # get blocks
    blocks <- list(primVar = NA, covar = NA, secVar = NA, bolus = NA, ini = NA, f = NA, lag = NA, eqn = NA, output = NA, error = NA, extra = NA)
    for (i in 1:length(headerPresent)) {
      temp <- modelFile[(blockStart[i] + 1):blockStop[i]]
      allblank <- grep("^[[:blank:]]+$", temp)
      if (length(allblank) > 0) temp <- temp[-allblank]
      blocks[[headerPresent[i]]] <- tolower(temp)
    }
    emptyHeaders <- which(is.na(blocks))
    if (length(emptyHeaders) > 0) blocks[emptyHeaders] <- ""
    return(blocks)
  } # end parseBlocks
  
  # check all blocks statements for more than maxwidth characters and insert line break if necessary
  chunks <- function(x, maxwidth = 60) {
    for (i in 1:length(x)) {
      for (j in 1:length(x[[i]])) {
        temp <- x[[i]][j]
        if (nchar(temp) > maxwidth) {
          numchunks <- floor(nchar(temp) / maxwidth)
          if (nchar(temp) %% maxwidth > 0) numchunks <- numchunks + 1
          splitchunks <- vector("character", numchunks)
          chunkindex <- c(seq(0, numchunks * maxwidth, maxwidth), nchar(temp))
          for (k in 1:numchunks) {
            splitchunks[k] <- substr(temp, chunkindex[k] + 1, chunkindex[k + 1])
          }
          x[[i]][j] <- paste(splitchunks, collapse = "\n     &   ")
        }
      }
    }
    return(x)
  } # end chunks
  
  # change dX[digit] to XP(digit) and X[digit] to X(digit)
  fortranize <- function(block) {
    block <- purrr::map_chr(block, ~ gsub("dX\\[(\\d+)\\]", "XP\\(\\1\\)", .x, ignore.case = T, perl = T))
    block <- purrr::map_chr(block, ~ gsub("BOLUS\\[\\d+\\]", "", .x, ignore.case = T, perl = T))
    block <- purrr::map_chr(block, ~ gsub("\\[(\\d+)\\]", "\\(\\1\\)", .x, ignore.case = T, perl = T))
    return(block)
  }
  
  
  # get the next line when building a file like instructions
  getNext <- function(build) {
    return(length(build) + 1)
  }
  
  
  # Get OS version ----------------------------------------------------------
  
  getOS <- function() {
    OS <- switch(Sys.info()[1],
    Darwin = 1,
    Windows = 2,
    Linux = 3
  )
  return(OS)
}


# makePMmatrixBlock -------------------------------------------------------

makePMmatrixBlock <- function(mdata) {
  # make event blocks, delimited by evid=4
  mdata$block <- 1
  if (any(mdata$evid == 4)) {
    blocks <- tapply(mdata$time[mdata$evid == 1 | mdata$evid == 4], mdata$id[mdata$evid == 1 | mdata$evid == 4], function(x) sum(x == 0))
    blocks <- blocks[rank(unique(mdata$id))] # sort blocks back into id order in mdata
    blocks2 <- unlist(mapply(function(x) 1:x, blocks))
    time0 <- c(which(mdata$time == 0 & mdata$evid != 0), nrow(mdata))
    blocks3 <- rep(blocks2, times = diff(time0))
    mdata$block <- c(blocks3, tail(blocks3, 1))
  }
  return(mdata)
}


# getCov ------------------------------------------------------------------

#' @title Extract covariate information
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Extracts covariate information from a Pmetrics data object.
#' @details
#' The function subtracts the number of fixed columns in a standard data format,
#' currently 14, from the total number of columns in `mdata` and queries
#' the remaining columns.  When given a [PM_data] object, will return a list
#' with the number of covariates, their names, and the starting and
#' ending column numbers
#' @param mdata A [PM_data] object. It can be other data frames but the results
#' will likely not be meaningful.
#' @return A list with named items: *ncov, covnames, covstart, covend*.
#'
#' @export
#' @examples
#' \dontrun{
#' getCov(dataEx)
#' }

#' @author Michael Neely

getCov <- function(mdata) {
  if (inherits(mdata, "PM_data")) {
    mdata <- mdata$standard_data
  }
  nfixed <- getFixedColNum()
  ncolData <- ncol(mdata)
  ncov <- ncolData - nfixed
  if (ncov > 0) {
    covnames <- names(mdata)[(nfixed + 1):ncolData]
    covstart <- nfixed + 1
    covend <- ncolData
  } else {
    covnames <- NA
    covstart <- NA
    covend <- NA
  }
  
  return(list(ncov = ncov, covnames = covnames, covstart = covstart, covend = covend))
}


# Time after dose ---------------------------------------------------------

# calculate time after dose
calcTAD <- function(rawData) {
  rawData$tad <- NA
  for (i in 1:nrow(rawData)) {
    if (rawData$evid[i] != 0) {
      if (!is.na(rawData$addl[i]) && rawData$addl[i] > 0) {
        doseTime <- rawData$time[i] + rawData$addl[i] * rawData$ii[i]
      } else {
        doseTime <- rawData$time[i]
      }
      prevDose <- rawData$dose[i]
      rawData$tad[i] <- 0
    } else {
      rawData$tad[i] <- rawData$time[i] - doseTime
    }
  }
  return(rawData$tad)
}


#' Import recycled text into documentation
#' @param name The name of the text file
#' @export
#' @keywords internal
#'
template <- function(name) {
  insert <- readLines(paste0("man-roxygen/", name, ".R")) |>
  stringr::str_replace("#' ", "") |>
  stringr::str_replace("<br>", "  \n")
  insert <- c(insert, "  \n")
  insert <- paste(insert, collapse = " ")
  return(insert)
}


# WEIGHTED STAT FUNCTIONS ---------------------------------------------------------------

# the following two functions come from FelixS (http://www.r-bloggers.com/weighted-t-test-in-r/)
# weighted variance, inspired by a function from Gavin Simpson on R-Help
var.wt <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  return((sum(w * x^2) * sum.w - sum(w * x)^2) / (sum.w^2 - sum(w^2)))
}

# weighted t test
weighted.t.test <- function(x, w, mu, conf.level = 0.95, alternative = "two.sided", na.rm = TRUE) {
  if (!missing(conf.level) &
  (length(conf.level) != 1 || !is.finite(conf.level) ||
  conf.level < 0 || conf.level > 1)) {
    stop("'conf.level' must be a single number between 0 and 1")
  }
  # see if x came from PM_op object
  if (all(inherits(x, c("PM_op_data", "data.frame"), which = T))) {
    w <- 1 / x$obsSD**2
    x <- x$d
    mu <- 0
  }
  
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  
  # to achieve consistent behavior in loops, return NA-structure in case of complete missings
  if (sum(is.na(x)) == length(x)) {
    return(list(estimate = NA, se = NA, conf.int = NA, statistic = NA, df = NA, p.value = NA))
  }
  
  # if only one value is present: this is the best estimate, no significance test provided
  if (sum(!is.na(x)) == 1) {
    warning("Warning weighted.t.test: only one value provided; this value is returned without test of significance!", call. = FALSE)
    return(list(estimate = x[which(!is.na(x))], se = NA, conf.int = NA, statistic = NA, df = NA, p.value = NA))
  }
  
  x.w <- weighted.mean(x, w, na.rm = na.rm)
  var.w <- var.wt(x, w, na.rm = na.rm)
  df <- length(x) - 1
  t.value <- sqrt(length(x)) * ((x.w - mu) / sqrt(var.w))
  se <- sqrt(var.w) / sqrt(length(x))
  
  if (alternative == "less") {
    pval <- pt(t.value, df)
    cint <- c(-Inf, x.w + se * qt(conf.level, df))
  } else if (alternative == "greater") {
    pval <- pt(t.value, df, lower.tail = FALSE)
    cint <- c(x.w - se * qt(conf.level, df), Inf)
  } else {
    pval <- 2 * pt(-abs(t.value), df)
    alpha <- 1 - conf.level
    cint <- x.w + se * qt(1 - alpha / 2, df) * c(-1, 1)
  }
  
  names(t.value) <- "t"
  return(list(estimate = x.w, se = se, conf.int = cint, statistic = t.value, df = df, p.value = pval))
}

# modified from Hmisc functions

wtd.table <- function(
  x, weights = NULL,
  type = c("list", "table"),
  normwt = TRUE,
  na.rm = TRUE
) {
  type <- match.arg(type)
  if (!length(weights)) {
    weights <- rep(1, length(x))
  }
  isdate <- lubridate::is.Date(x)
  ax <- attributes(x)
  ax$names <- NULL
  if (is.character(x)) {
    x <- as.factor(x)
  }
  lev <- levels(x)
  x <- unclass(x)
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s, drop = FALSE]
    weights <- weights[s]
  }
  n <- length(x)
  if (normwt) {
    weights <- weights * length(x) / sum(weights)
  }
  i <- order(x)
  x <- x[i]
  weights <- weights[i]
  if (anyDuplicated(x)) {
    weights <- tapply(weights, x, sum)
    if (length(lev)) {
      levused <- lev[sort(unique(x))]
      if ((length(weights) > length(levused)) && any(is.na(weights))) {
        weights <- weights[!is.na(weights)]
      }
      if (length(weights) != length(levused)) {
        stop("program logic error")
      }
      names(weights) <- levused
    }
    if (!length(names(weights))) {
      stop("program logic error")
    }
    if (type == "table") {
      return(weights)
    }
    x <- all_is_numeric(names(weights), "vector")
    if (isdate) {
      attributes(x) <- c(attributes(x), ax)
    }
    names(weights) <- NULL
    return(list(x = x, sum.of.weights = weights))
  }
  xx <- x
  if (isdate) {
    attributes(xx) <- c(attributes(xx), ax)
  }
  if (type == "list") {
    list(x = if (length(lev)) lev[x] else xx, sum.of.weights = weights)
  } else {
    names(weights) <- if (length(lev)) {
      lev[x]
    } else {
      xx
    }
    weights
  }
}

wtd.mean <- function(x, weights = NULL, normwt = "ignored", na.rm = TRUE) {
  if (!length(weights)) {
    return(mean(x, na.rm = na.rm))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  sum(weights * x) / sum(weights)
}


wtd.quantile <- function(
  x, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1),
  normwt = TRUE,
  na.rm = TRUE
) {
  if (!length(weights)) {
    return(quantile(x, probs = probs, na.rm = na.rm))
  }
  
  if (any(probs < 0 | probs > 1)) {
    cli::cli_abort("Probabilities must be between 0 and 1 inclusive")
  }
  nams <- paste(format(round(probs * 100, if (length(probs) >
  1) {
    2 - log10(diff(range(probs)))
  } else {
    2
  })), "%", sep = "")
  i <- is.na(weights) | weights == 0
  if (any(i)) {
    x <- x[!i]
    weights <- weights[!i]
  }
  
  w <- wtd.table(x, weights,
    na.rm = na.rm, normwt = normwt,
    type = "list"
  )
  x <- w$x
  wts <- w$sum.of.weights
  n <- sum(wts)
  order <- 1 + (n - 1) * probs
  low <- pmax(floor(order), 1)
  high <- pmin(low + 1, n)
  order <- order %% 1
  allq <- approx(cumsum(wts), x,
  xout = c(low, high), method = "constant",
  f = 1, rule = 2
)$y
k <- length(probs)
quantiles <- (1 - order) * allq[1:k] + order * allq[-(1:k)]
names(quantiles) <- nams
return(quantiles)
}

wtd.var <- function(
  x, weights = NULL,
  normwt = TRUE,
  na.rm = TRUE,
  method = c("unbiased", "ML")
) {
  if (any(weights == 1)) {
    return(0)
  }
  
  method <- match.arg(method)
  if (!length(weights)) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    return(var(x))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt) {
    weights <- weights * length(x) / sum(weights)
  }
  if (normwt || method == "ML") {
    return(as.numeric(stats::cov.wt(cbind(x), weights, method = method)$cov))
  }
  sw <- sum(weights)
  if (sw <= 1) {
    cli::cli_warn("only one effective observation; variance estimate undefined")
  }
  xbar <- sum(weights * x) / sw
  sum(weights * ((x - xbar)^2)) / (sw - 1)
}


# Check if all values numeric ---------------------------------------------

#' @title Check if all values are numeric
#' @description
#' `r lifecycle::badge("stable")`
#' Checks if all values in a vector are numeric.
#' @details
#' The function checks if all values in a vector are numeric.
#' It can be used to check if a vector contains only numeric values.
#' It can also be used to check if a vector contains any non-numeric values.
#' @param x A vector to check.
#' @param what A character string indicating what to return.
#' Can be "test", "vector", or "nonnum".
#' The default is "test".
#' @param extras A character vector of extra values to exclude from the check.
#' The default is c(".", "NA").
#' @return A logical value indicating if all values are numeric.
#' If `what` is "vector", a numeric vector is returned.
#' If `what` is "nonnum", a character vector of non-numeric values is returned.
#' If `what` is "test", a logical value is returned.
#' @export
#' @examples
#' \dontrun{
#' all_is_numeric(c("1", "2", "3"))
#' all_is_numeric(c("1", "2", "a"))
#' all_is_numeric(c("1", "2", "3"), what = "vector")
#' all_is_numeric(c("1", "2", "a"), what = "nonnum")
#' }
all_is_numeric <- function(x, what = c("test", "vector", "nonnum"), extras = c(
  ".",
  "NA"
)) {
  what <- match.arg(what)
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  xs <- x[!x %in% c("", extras)]
  if (!length(xs) || all(is.na(x))) {
    return(switch(what,
      test = FALSE,
      vector = x,
      nonnum = x[0]
    ))
  }
  isnon <- suppressWarnings(!is.na(xs) & is.na(as.numeric(xs)))
  isnum <- !any(isnon)
  switch(what,
    test = isnum,
    vector = if (isnum) suppressWarnings(as.numeric(x)) else x,
    nonnum = xs[isnon]
  )
}


# Save Flextable ---------------------------------------------------------

#' @title Save a flextable object to a file
#' @description
#' `r lifecycle::badge("stable")`
#' Saves flextable objects to a file based on the `file` attribute
#' in the object, set when the flextable generator function is called.
#' Allowable file types are 'docx', 'pptx', 'html', 'png', and 'svg'.
#' @param x A [flextable::flextable] object.
#' @return A message indicating the file was saved.
#' @export
#' @keywords internal

save_flextable <- function(x) {
  # check if x is a flextable
  if (!inherits(x, "flextable")) {
    cli::cli_abort("{.arg x} must be a flextable object.")
  }
  
  file <- attr(x, "file")
  if (!is.null(file)) {
    # get file extension
    ext <- stringr::str_match(file, "\\.(.*)$")[2]
    
    # save flextable based on file extension
    if (ext %in% c("docx", "doc")) {
      flextable::save_as_docx(x, path = file)
    } else if (ext %in% c("pptx", "ppt")) {
      flextable::save_as_pptx(x, path = file)
    } else if (ext %in% c("html", "htm")) {
      flextable::save_as_html(x, path = file)
    } else if (ext %in% c("png", "svg")) {
      flextable::save_as_image(x, path = file)
    } else {
      cli::cli_abort("File type not recognized. Choose from 'docx', 'pptx', 'html', 'png', or 'svg'.")
    }
    
    cli::cli_inform(paste("The file", file, "was saved to", getwd(), "."))
  }
  
  return(invisible(x))
}


# Ask with warning --------------------------------------------------------

#' @title Ask with warning
#' @description Get user input in warning situation
#' @details Combines the [cli::cli_text] function with [readline].
#' @param text The warning text.
#' @param prompt The prompt preceding user input. Default is ">>".
#' @param ... Additional parameters which could be passed to [cli::cli_text].
#' @return The value of the user response
#' @export
#' @keywords internal
#'
cli_ask <- function(text, prompt = ">> ", ...) {
  cli::cli_text(text, ...)
  ans <- readline(prompt = prompt)
  return(ans)
}


# Function to Character ---------------------------------------------------

#' @title Convert a function to a character string
#' @keywords internal
func_to_char <- function(fun) {
  deparse(fun, width.cutoff = 500L) |>
  stringr::str_trim("left") |>
  purrr::discard(\(x) stringr::str_detect(x, "function|\\{|\\}"))
}


# Round to x digits  ---------------------------------------------------

#' @title Round to x digits
#' @description
#' `r lifecycle::badge("stable")`
#' Rounds a numeric value to a specified number of digits for display in flextables and plots.
#' @details Uses [base::format] and [base::round] to round a numeric value to a specified number of digits.
#' @param x A numeric value to be rounded.
#' @param digits The number of digits to round to. Default is set using [setPMoptions].
#' @return A character string representing the rounded value with the specified number of digits.
#' @export
#' @keywords internal

round2 <- function(x, digits = getPMoptions("digits")) {
  if (is.null(digits) || !is.numeric(digits)) digits <- 2
  format(round(x, digits), nsmall = digits)
}


# Print data frame in CLI format ------------------------------------------
#' @title Print data frame in CLI format
#' @description
#' `r lifecycle::badge("stable")`
#' Prints a data frame in a format suitable for the command line interface (CLI).
#' @details
#' Uses [dplyr::mutate] to convert all columns to character, rounds numeric values using [round2],
#' and formats the output using [knitr::kable] for a simple table format.
#' The function replaces spaces with non-breaking spaces for better alignment in the CLI.
#' @param df A data frame to be printed.
#' @return A formatted text output of the data frame.
#' @export
#' @keywords internal
cli_df <- function(df) {
  # Convert all columns to character for uniform formatting
  df_chr <- df |>
  mutate(across(where(is.double), ~ round2(.x))) |>
  mutate(across(everything(), ~ as.character(.x, stringsAsFactors = FALSE)))
  
  
  # create table
  df_tab <- knitr::kable(df_chr, format = "simple")
  
  # print header
  header <- df_tab[1] |> stringr::str_replace_all(" ", "\u00A0")
  cli::cli_text("{.strong {header}}")
  
  # print each row
  for (i in 2:length(df_tab)) {
    cli::cli_text(df_tab[i] |> stringr::str_replace_all(" ", "\u00A0"))
  }
}

#' @title Convert correlation matrix to covariance matrix
#' @description
#' `r lifecycle::badge("stable")`
#' Converts a correlation matrix to a covariance matrix using standard deviations.
#' @details
#' Uses matrix multiplication to convert a correlation matrix to a covariance matrix.
#' @param cor A correlation matrix.
#' @param sd A vector of standard deviations corresponding to the variables in the correlation matrix.
#' @return A covariance matrix.
#' @export
#' @author Michael Neely
#'
cor2cov <- function(cor, sd) {
  cov_matrix <- diag(sd) %*% cor %*% diag(sd)
  return(cov_matrix)
}


#' @title Check if a matrix is positive definite
#' @description
#' `r lifecycle::badge("stable")`
#' Checks if a matrix is positive definite and attempts to fix it if necessary.
#' @param mat A covariance matrix to check.
#' @return A positive definite covariance matrix, 1 if aborting, or -1 if unable to fix
#' @export
#' @author Michael Neely
#' @keywords internal
pos_def <- function(mat, id, source) {
  # check to make sure mat (within 15 sig digits, which is in file) is pos-def and fix if necessary
  posdef <- rlang::try_fetch(eigen(signif(mat, 15)),
  error = function(e) {
    return(list(values = 0))
  }
)
ans <- NULL
if (any(posdef$values < 0)) {
  mat_names <- dimnames(mat)[[1]] # store for later
  if (is.null(ans)) {
    cli::cli_alert_warning("Warning: your covariance matrix is not positive definite. This is typically due to small population size.\nChoose one of the following:\n1) end simulation\n2) fix covariances\n3) set covariances to 0\n ")
    ans <- readline("\n")
  }
  if (ans == 1) {
    cli::cli_alert_info("Aborting.")
    return(1)
  }
  if (ans == 2) {
    # eigen decomposition to fix the matrix
    for (j in 1:10) { # try up to 10 times
      eps <- 1e-8 # threshold for small eigenvalues
      eig <- eigen(mat)
      eig$values[eig$values < eps] <- eps # threshold small eigenvalues
      mat <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
      
      
      posdef <- eigen(signif(mat, 15))
      
      if (all(posdef$values >= 0)) { # success, break out of loop
        break
      }
      if (j == 10) browser()
    }
    posdef <- eigen(signif(mat, 15)) # last check
    if (any(posdef$values < 0)) {
      return(-1)
    }
    mat <- data.frame(mat)
    names(mat) <- mat_names
    row.names(mat) <- mat_names
  }
  if (ans == 3) {
    mat2 <- diag(0, nrow(mat))
    diag(mat2) <- diag(as.matrix(mat))
    mat2 <- data.frame(mat2)
    names(mat2) <- mat_names
    mat <- mat2
  }
}

return(mat)
}


#' @title Modify a list with another list, allowing NULL values
#' @description
#' `r lifecycle::badge("stable")`
#' Version of [utils::modifyList()] that works with lists which have unnamed elements.
#' @param x A list to be modified.
#' @param val A list of values to modify `x`.
#' @param keep.null A logical value indicating whether to keep NULL values in `val`.
#' Default is FALSE.
#' @return A modified list, as in [utils::modifyList()].
#' @export
#' @keywords internal
modifyList2 <- function(x, val, keep.null = FALSE) {
  stopifnot(is.list(x), is.list(val))
  names(x) <- tolower(names(x))
  names(val) <- tolower(names(val))
  xnames <- names(x)
  vnames <- names(val)
  # handle unnamed lists
  if (is.null(xnames)) xnames <- 1:length(x)
  if (is.null(vnames)) vnames <- 1:length(val)
  # handle unnamed elements
  xnames <- ifelse(xnames == "", as.character(seq_along(xnames)), xnames)
  vnames <- ifelse(vnames == "", as.character(seq_along(vnames)), vnames)
  
  vnames <- vnames[nzchar(vnames)]
  if (keep.null) {
    for (v in vnames) {
      x[v] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) {
        list(modifyList(x[[v]], val[[v]], keep.null = keep.null))
      } else {
        val[v]
      }
    }
  } else {
    for (v in vnames) {
      x[[v]] <- if (v %in% xnames && is.list(x[[v]]) &&
      is.list(val[[v]])) {
        modifyList2(x[[v]], val[[v]], keep.null = keep.null)
      } else {
        val[[v]]
      }
    }
  }
  x
}


#' @title Clear build files
#' @description
#' `r lifecycle::badge("stable")`
#' Deletes rust template files from the Pmetrics template directory.
#' @return NULL
#' @export
#' @keywords internal
clear_build <- function() {
  template_file <- system.file("template", package = "Pmetrics")
  if (file.exists(template_file)) {
    unlink(template_file, recursive = TRUE)
  }
}

#' @title Get latest platform-specific R release metadata
#' @description
#' `r lifecycle::badge("stable")`
#' Retrieves metadata for the latest R release available for the current
#' platform from the r-hub rversions API.
#' @return A list containing all fields returned by the API response.
#' @export
latestR <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])
  r_arch <- tolower(R.version$arch)
  r_release_endpoint <- switch(sysname,
    windows = "r-release-win",
    darwin = if (grepl("arm64|aarch64", r_arch)) "r-release-macos-arm64" else "r-release-macos",
    linux = "r-release-tarball",
    "r-release"
  )
  
  jsonlite::fromJSON(sprintf("https://api.r-hub.io/rversions/%s", r_release_endpoint))
}


pm_updates_cache_file <- function() {
  file.path(tools::R_user_dir("Pmetrics", which = "cache"), "updates.rds")
}


pm_read_updates_cache <- function() {
  cache_file <- pm_updates_cache_file()
  if (!file.exists(cache_file)) {
    return(NULL)
  }
  
  tryCatch(readRDS(cache_file), error = function(e) NULL)
}


pm_write_updates_cache <- function(result) {
  cache_file <- pm_updates_cache_file()
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  tryCatch(saveRDS(result, cache_file), error = function(e) invisible(NULL))
  invisible(result)
}


pm_update_interval_days <- function() {
  mode <- getPMoptions("update_check", warn = FALSE, quiet = TRUE)
  if (is.null(mode)) {
    mode <- getOption("Pmetrics.update_check", "weekly")
  }
  mode <- tolower(as.character(mode))
  
  switch(mode,
    always = 0,
    daily = 1,
    weekly = 7,
    monthly = 30,
    manual = Inf,
    never = Inf,
    Inf
  )
}


pm_notify_outdated <- function(result) {
  if (is.null(result)) {
    return(invisible(NULL))
  }
  
  pmetrics_outdated <- isTRUE(result$pmetrics_outdated)
  r_outdated <- isTRUE(result$r_outdated)
  
  if (!pmetrics_outdated && !r_outdated) {
    return(invisible(NULL))
  }
  
  ul <- cli::cli_ul()
  
  if (pmetrics_outdated) {
    cli::cli_li("{.red Update available:} Pmetrics {result$latest_pmetrics} (installed: {result$installed_pmetrics}).")
  }
  
  if (r_outdated) {
    cli::cli_li("{.red Update available:} R {result$latest_r} (installed: {result$current_r}). Use {.help downloadR}.")
  }
  
  cli::cli_end(ul)
  invisible(result)
}


pm_maybe_notify_updates <- function() {
  if (!interactive()) {
    return(invisible(NULL))
  }
  
  cache <- pm_read_updates_cache()
  interval_days <- pm_update_interval_days()
  
  needs_refresh <- is.null(cache) ||
  is.null(cache$checked_at) ||
  (is.finite(interval_days) &&
  as.numeric(difftime(Sys.time(), cache$checked_at, units = "days")) >= interval_days)
  
  if (is.finite(interval_days) && isTRUE(needs_refresh)) {
    timeout <- getPMoptions("update_timeout", warn = FALSE, quiet = TRUE)
    if (is.null(timeout)) {
      timeout <- getOption("Pmetrics.update_timeout", 1)
    }
    cache <- tryCatch(check_updates(verbose = FALSE, timeout = timeout), error = function(e) cache)
  }
  
  pm_notify_outdated(cache)
  invisible(cache)
}


#' @title Check for Pmetrics and R updates
#' @description
#' `r lifecycle::badge("stable")`
#' Performs an on-demand check for newer Pmetrics and R releases.
#' This function is intended for interactive use and avoids running network
#' checks automatically during package attach.
#' @param verbose Logical. If `TRUE`, emits a user-facing CLI summary.
#' @param timeout Numeric scalar. Network timeout in seconds used for this check.
#' @return An invisible list with installed/latest versions and outdated flags.
#' @export
check_updates <- function(verbose = interactive(), timeout = 2) {
  timeout <- as.numeric(timeout)
  if (!is.finite(timeout) || timeout <= 0) {
    timeout <- 2
  }
  
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)
  
  installed_pmetrics <- packageVersion("Pmetrics")
  latest_pmetrics <- tryCatch(
    package_version(
      jsonlite::fromJSON("https://lapkb.r-universe.dev/api/packages/Pmetrics")$Version
    ),
    error = function(e) NA
  )
  
  current_r <- getRversion()
  latest_r_info <- tryCatch(latestR(), error = function(e) NULL)
  latest_r <- if (!is.null(latest_r_info) && !is.null(latest_r_info$version)) {
    package_version(latest_r_info$version)
  } else {
    NA
  }
  
  pmetrics_outdated <- !is.na(latest_pmetrics) && installed_pmetrics < latest_pmetrics
  r_outdated <- !is.na(latest_r) && current_r < latest_r
  
  result <- list(
    installed_pmetrics = installed_pmetrics,
    latest_pmetrics = latest_pmetrics,
    pmetrics_outdated = pmetrics_outdated,
    current_r = current_r,
    latest_r = latest_r,
    r_outdated = r_outdated,
    checked_at = Sys.time()
  )
  
  pm_write_updates_cache(result)
  
  if (isTRUE(verbose)) {
    ul <- cli::cli_ul()
    
    if (is.na(latest_pmetrics)) {
      cli::cli_li("Unable to check latest Pmetrics version (network unavailable or endpoint unreachable).")
    } else if (pmetrics_outdated) {
      cli::cli_li("{.red Warning:} Your Pmetrics version ({installed_pmetrics}) is older than the latest release ({latest_pmetrics}). Update instructions are at https://github.com/LAPKB/Pmetrics.")
    } else {
      cli::cli_li("You are using the latest Pmetrics version: {installed_pmetrics}.")
    }
    
    if (is.na(latest_r)) {
      cli::cli_li("Unable to check latest R version (network unavailable or endpoint unreachable).")
    } else if (r_outdated) {
      cli::cli_li("{.red Warning:} Your R version ({current_r}) is older than the latest release ({latest_r}). Use {.help downloadR} to download the latest platform-specific installer.")
    } else {
      cli::cli_li("You are using the latest R version: {current_r}.")
    }
    
    cli::cli_end(ul)
  }
  
  invisible(result)
}


#' @title Download the latest platform-specific R installer
#' @description
#' `r lifecycle::badge("stable")`
#' Downloads the latest R installer (or source tarball on Linux) for the current
#' platform to the user's Downloads folder.
#' @param r_info Optional API response list. Defaults to [latestR()].
#' @param destdir Destination directory. Defaults to the user's Downloads folder.
#' @return The file path of the downloaded installer/tarball.
#' @export
downloadR <- function(r_info = latestR(), destdir = path.expand("~/Downloads")) {
  download_url <- r_info$URL
  if (is.null(download_url) || length(download_url) == 0 || is.na(download_url)) {
    download_url <- r_info$url
  }
  
  if (is.null(download_url) || length(download_url) == 0 || is.na(download_url)) {
    cli::cli_abort("No downloadable URL was returned by the rversions API for this platform.")
  }
  
  if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
  }
  
  destfile <- file.path(destdir, basename(download_url))
  utils::download.file(download_url, destfile = destfile, mode = "wb")
  destfile
}



# Internal lower-bounded MVN sampler (lb only), using rejection sampling

PM_rtmvnorm <- function(n, mean, sigma, lb, max_draws = 5000000L) {
  mean <- as.numeric(mean)
  sigma <- as.matrix(sigma)
  lb <- as.numeric(lb)
  
  d <- length(mean)
  if (length(lb) != d) stop("lb length must equal length(mean)")
  if (!all(dim(sigma) == c(d, d))) stop("sigma must be d x d")
  
  out <- matrix(NA_real_, nrow = n, ncol = d)
  filled <- 0L
  draws <- 0L
  batch <- max(2000L, 4L * n)
  
  while (filled < n) {
    x <- MASS::mvrnorm(n = batch, mu = mean, Sigma = sigma)
    if (d == 1L) x <- matrix(x, ncol = 1)
    
    keep <- rowSums(sweep(x, 2, lb, `>=`)) == d
    n_keep <- sum(keep)
    
    if (n_keep > 0L) {
      take <- min(n_keep, n - filled)
      out[(filled + 1L):(filled + take), ] <- x[which(keep)[seq_len(take)], , drop = FALSE]
      filled <- filled + take
    }
    
    draws <- draws + batch
    if (draws > max_draws) {
      stop("Exceeded max_draws before filling sample; truncation region may be too restrictive")
    }
    
    if (filled > 0L) {
      acc <- filled / draws
      batch <- as.integer(max(2000L, min(200000L, ceiling((n - filled) / max(acc, 1e-5) * 1.2))))
    }
  }
  
  out
}
