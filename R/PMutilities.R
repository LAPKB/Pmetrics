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
dmv_norm <- function(x, mean = rep(0, p), sigma = diag(p), log = FALSE,
checkSymmetry = TRUE) {
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
  
  openHTML <- function(x) pander::openFileInOS(x)
  
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
    if(length(missing_mandatory)){
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
  
  
  # convert new model template to model fortran file
  makeModel <- function(model = "model.txt", data = "data.csv", engine, backend = getPMoptions("backend"), write = T, quiet = F) {
    blocks <- parseBlocks(model)
    
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
    
    # check all blocks statements for more than maxwidth characters and insert line break if necessary
    maxwidth <- 60
    blocks <- chunks(x = blocks, maxwidth = maxwidth)
    
    # ensure in fortran format: dX -> XP and [] -> ()
    blocks <- purrr::map(blocks, fortranize)
    
    # primary variable definitions
    npvar <- length(blocks$primVar)
    psym <- vector("character", npvar)
    pvardef <- psym
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
    
    # find out if any are fixed to be positive only for IT2B
    fixedpos <- grep("\\+", blocks$primVar)
    if (length(fixedpos) > 0) blocks$primVar <- gsub("\\+", "", blocks$primVar)
    
    # find out if any are to be fixed (constant)
    fixcon <- grep("!", blocks$primVar)
    nofix <- length(fixcon)
    if (nofix > 0) blocks$primVar <- gsub("!", "", blocks$primVar)
    
    
    # get limits [a,b] on primary variables
    splitprimVar <- strsplit(blocks$primVar, sep)
    a <- as.numeric(unlist(lapply(splitprimVar, function(x) x[2])))
    b <- as.numeric(unlist(lapply(splitprimVar, function(x) x[3])))
    
    # set parameter type: 1 for random, 0 for constant, -1 for random but pos (IT2B only) and 2 for fixed random
    ptype <- c(1, 2)[1 + as.numeric(is.na(b))]
    # if any fixed constant variables are present, set ptype to 0
    if (nofix > 0) ptype[fixcon] <- 0
    
    # npvar is total number of parameters
    # nvar is number of random (estimated) parameters
    # nranfix is number of fixed (but unknown) parameters
    # nofix is number of constant parameters
    nranfix <- sum(as.numeric(is.na(b))) - nofix
    nvar <- npvar - nofix - nranfix
    
    if ((engine$alg == "IT" | engine$alg == "ERR") & length(fixedpos) > 0) ptype[fixedpos] <- -1
    
    if (nofix > 0) {
      valfix <- a[which(ptype == 0)]
    } else {
      valfix <- NA
    }
    
    if (nranfix > 0) {
      valranfix <- a[which(ptype == 2)]
    } else {
      valranfix <- NA
    }
    
    ab.df <- data.frame(a = a[which(ptype == 1)], b = b[which(ptype == 1)])
    
    
    # replace a,b with SIM limits argument if it is present
    if (engine$alg == "SIM" & !all(is.na(engine$limits))) {
      if (nrow(engine$limits) == nvar) {
        # make sure same row number
        replA <- engine$limits[, 1]
        replB <- engine$limits[, 2]
        ab.df$a[!is.na(replA)] <- replA[!is.na(replA)]
        ab.df$b[!is.na(replB)] <- replB[!is.na(replB)]
      } else {
        return(list(status = -1, msg = "Your limit block does not have the same number of parameters as the model file.\n"))
      }
    }
    
    if (nofix > 0 & any(is.na(valfix))) {
      return(list(status = -1, msg = "One or more variables did not have any boundaries.\n"))
    }
    if (nranfix > 0 & any(is.na(valranfix))) {
      return(list(status = -1, msg = "One or more variables did not have any boundaries.\n"))
    }
    
    # set grid point index for NPAG if not supplied
    if (engine$indpts == -99) {
      indpts <- switch(nvar,
        1,
        1,
        3,
        4,
        6
      )
      if (is.null(indpts)) indpts <- 100 + nvar - 5
      if (indpts > 108) indpts <- 108
    } else {
      indpts <- engine$indpts
    }
    
    
    # transform ab
    if (nrow(ab.df) > 0) {
      ab <- paste(t(as.matrix(ab.df)))
      ab <- c(paste(ab[1:(length(ab) - 1)], "t", sep = ""), ab[length(ab)])
      ab[seq(1, 2 * nvar, 2)] <- sub("t", "    ", ab[seq(1, 2 * nvar, 2)])
      ab[seq(2, 2 * nvar, 2)] <- sub("t", "\n", ab[seq(2, 2 * nvar, 2)])
      ab <- paste(ab, collapse = "")
    }
    
    blocks$primVar <- unlist(lapply(splitprimVar, function(x) x[1]))
    
    for (i in 1:npvar) {
      psym[i] <- paste("PSYM(", i, ")='", blocks$primVar[i], "'", sep = "")
      pvardef[i] <- paste(blocks$primVar[i], "=P(", i, ")", sep = "")
    }
    
    
    # covariate definitions
    if (blocks$covar[1] != "") {
      ncov <- length(blocks$covar)
      interpol <- grep("!", blocks$covar)
      blocks$covar <- gsub("!", "", blocks$covar)
      covardef <- vector("character", ncov)
      for (i in 1:ncov) {
        covardef[i] <- paste(blocks$covar[i], "=CV(", i, ")", sep = "")
      }
      if (!identical(1:ncov, which(tolower(engine$covnames) %in% tolower(blocks$covar)))) {
        return(list(status = -1, msg = "The covariate set in your model file was not in the same order as in your data file.\n"))
      }
    } else {
      covardef <- ""
      interpol <- grep("!", blocks$covar)
    }
    if (engine$ncov > 0) {
      ctype <- rep(2, engine$ncov)
    } else {
      ctype <- -99
    }
    # set covariate type based on number of covariates in data file, default is 2, interpolated
    if (length(interpol) > 0) ctype[interpol] <- 1 # change those in model file with "!" to constant
    
    # secondary variable definitions
    svardef <- blocks$secVar
    
    # get secondary variables and remove continuation lines beginning with "&"
    secVarNames <- gsub("[[:blank:]]", "", unlist(lapply(strsplit(svardef, "="), function(x) x[1])))
    secVarNames[is.na(secVarNames)] <- ""
    oldContLines <- grep("^\\+", secVarNames)
    if (length(oldContLines > 0)) {
      return(list(status = -1, msg = "\nThe model file format has changed.  Please replace '+' with '&' in all continuation lines.\n"))
    }
    contLines <- grep("^&", secVarNames)
    
    if (length(contLines) > 0) {
      secVarNames <- secVarNames[-contLines]
      svardef <- gsub("^&", "", svardef)
    }
    
    # take out any extra declarations in eqn to add to declarations in subroutine
    diffdec <- grep("COMMON|EXTERNAL|DIMENSION", blocks$eqn, ignore.case = T)
    if (length(diffdec) > 0) {
      diffstate <- blocks$eqn[diffdec]
      blocks$eqn <- blocks$eqn[-diffdec]
    } else {
      diffstate <- ""
    }
    
    # detect N
    if (blocks$eqn[1] == "" | grepl("^\\{algebraic:", blocks$eqn[1])) {
      if ("KE" %in% toupper(secVarNames) | "KE" %in% toupper(blocks$primVar)) {
        N <- -1
      } else {
        N <- 0
      }
    } else {
      # get number of equations and verify with data file
      # find statements with XP(digit) or dX[digit]
      compLines <- grep("XP\\([[:digit:]]+\\)|dX\\[[[:digit:]]+\\]", blocks$eqn, ignore.case = T)
      if (length(compLines) == 0) {
        N <- 0
      } else {
        compStatements <- sapply(blocks$eqn[compLines], function(x) strparse("XP\\([[:digit:]]+\\)|dX\\[[[:digit:]]+\\]", x))
        compNumbers <- sapply(compStatements, function(x) strparse("[[:digit:]]+", x))
        # get max number
        N <- max(as.numeric(compNumbers))
      }
    }
    
    # figure out model if N = -1 and if so, assign values to required KA,KE,V,KCP,KPC
    # in future, use {algebraic: xx} which is in model files now to select correct algebraic model
    # for now, comment the eqn lines in fortran if present
    if (length(grep("^\\{algebraic:", blocks$eqn[1])) > 0) {
      blocks$eqn[1] <- "This model uses algebraic solutions. Differential equations provided here for reference only."
      blocks$eqn <- purrr::map_chr(blocks$eqn, \(x) paste0("! ", x))
    }
    
    
    reqVars <- c("KA", "KE", "KCP", "KPC", "V")
    matchVars <- match(reqVars, toupper(c(blocks$primVar, secVarNames)))
    if (N == -1) {
      if (any(is.na(matchVars))) {
        missVars <- reqVars[is.na(matchVars)]
        if ("KE" %in% toupper(missVars)) {
          return(list(status = -1, msg = "\nYou have specified an algebraic model, which requires a variable named 'KE'\n"))
        }
        if ("V" %in% toupper(missVars)) {
          return(list(status = -1, msg = "\nYou have specified an algebraic model, which requires a variable named 'V'\n"))
        }
        missVarValues <- paste(missVars, "=0", sep = "")
        if (length(missVarValues) > 0) svardef <- c(svardef, missVarValues)
        svardef <- svardef[svardef != ""]
        # add new secondary variables that won't be estimated
        secVarNames <- c(secVarNames, missVars)
        secVarNames <- secVarNames[secVarNames != ""]
      } else {
        missVars <- NA
      }
    } else {
      if (any(is.na(matchVars))) {
        missVars <- reqVars[is.na(matchVars)]
      } else {
        missVars <- NA
      }
    }
    
    # extract bolus inputs and create bolus block, then remove bolus[x] from equations
    bolus <- purrr::map(blocks$eqn, ~ stringr::str_extract_all(.x, regex("B[\\[\\(]\\d+|BOL[\\[\\(]\\d+|BOLUS[\\[\\(]\\d+", ignore_case = TRUE), simplify = FALSE))
    blocks$bolus <- purrr::imap(bolus, \(x, idx){
      if (length(x[[1]]) > 0) {
        paste0("NBCOMP(", stringr::str_extract(x[[1]], "\\d+$"), ") = ", idx)
      }
    }) %>% unlist()
    blocks$eqn <- purrr::map(blocks$eqn, \(x) stringr::str_replace_all(x, regex("(\\+*|-*|\\**)\\s*B[\\[\\(]\\d+[\\]\\)]|(\\+*|-*|\\**)\\s*BOL[\\[\\(]\\d+[\\]\\)]|(\\+*|-*|\\**)\\s*BOLUS[\\[\\(]\\d+[\\]\\)]", ignore_case = TRUE), "")) %>%
    unlist()
    
    # replace R[x] or R(x) with RATEIV(x)
    blocks$eqn <- purrr::map(blocks$eqn, \(x) stringr::str_replace_all(x, regex("R[\\[\\(](\\d+)[\\]\\)]", ignore_case = TRUE), "RATEIV\\(\\1\\)")) %>%
    unlist()
    
    # get number of equations and verify with data file
    # find statements with Y(digit) or Y[digit]
    outputLines <- grep("Y\\([[:digit:]]+\\)|Y\\[[[:digit:]]+\\]", blocks$output, ignore.case = T)
    if (length(outputLines) == 0) {
      return(list(status = -1, msg = "\nYou must have at least one output equation of the form 'Y[1] = ...'\n"))
    }
    # extract numbers
    outputStatements <- sapply(blocks$output[outputLines], function(x) strparse("Y\\([[:digit:]]+\\)|Y\\[[[:digit:]]+\\]", x))
    outputNumbers <- sapply(outputStatements, function(x) strparse("[[:digit:]]", x))
    # get max number
    modelnumeqt <- max(as.numeric(outputNumbers))
    if (modelnumeqt != engine$numeqt) {
      return(list(status = -1, msg = "\nThe number of output equations in the model file\ndoes not match the maximum value of outeq in your datafile.\n"))
    }
    
    # remove leading ampersands from getfa, getix, gettlag if present
    oldContLines <- grep("^\\+", c(blocks$f, blocks$ini, blocks$lag))
    if (length(oldContLines > 0)) {
      return(list(status = -1, msg = "\nThe model file format has changed.  Please replace '+' with '&' in all continuation lines.\n"))
    }
    if (length(grep("^&", blocks$f) > 0)) blocks$f <- gsub("^&", "", blocks$f)
    if (length(grep("^&", blocks$ini) > 0)) blocks$ini <- gsub("^&", "", blocks$ini)
    if (length(grep("^&", blocks$lag) > 0)) blocks$lag <- gsub("^&", "", blocks$lag)
    
    # variable declarations for fortran and make sure not >maxwidth characters
    if (secVarNames[1] != "") {
      vardec <- paste("REAL*8 ", paste(blocks$primVar, collapse = ","), ",", paste(secVarNames, collapse = ","), sep = "")
    } else {
      vardec <- paste("REAL*8 ", paste(blocks$primVar, collapse = ","), sep = "")
    }
    if (blocks$covar[1] != "") {
      vardec <- paste(vardec, ",", paste(blocks$covar, collapse = ","), sep = "")
    }
    if (nchar(vardec) > maxwidth) {
      vardec <- paste(unlist(strsplit(vardec, ",")), collapse = ",\n     &  ")
    }
    
    # error
    blocks$error <- tolower(gsub("[[:space:]]", "", blocks$error))
    # check to make sure coefficient lines are the same number as outputs
    nErrCoeff <- length(blocks$error) - 1
    if (nErrCoeff != modelnumeqt) {
      return(list(status = -1, msg = paste("\nThere ", c("is", "are")[1 + as.numeric(nErrCoeff > 1)], " ",
      nErrCoeff, c(" line", " lines")[1 + as.numeric(nErrCoeff > 1)],
      " of error coefficients in the model file, but ",
      modelnumeqt, " output ", c("equation", "equations")[1 + as.numeric(modelnumeqt > 1)],
      ".\nThese must be the same.\n",
      sep = ""
    )))
  }
  # get assay error only if SIMrun
  if (engine$alg == "SIM") {
    gamlam <- grep("^g|^l", blocks$error)
    if (length(gamlam) > 0) {
      # gamma/lambda is specified in file
      asserr <- unlist(strsplit(blocks$error[-gamlam], sep))
    } else {
      asserr <- unlist(strsplit(blocks$error, sep))
    }
  } else {
    # this is not a SIMrun
    # get gamma or lambda if present and assay error coefficients
    gamlam <- grep("^g|^l", blocks$error)
    if (length(gamlam) > 0) {
      # gamma/lambda is specified in file
      fixed <- grep("!", blocks$error[gamlam[1]])
      ierr <- unlist(strsplit(blocks$error[gamlam[1]], "="))
      ierrtype <- gsub("[[:space:]]", "", tolower(substr(ierr[1], 1, 1)))
      
      # NPAG error parameters
      # IERRMOD
      # 1 SD WITH GAMMA(IEQ) FIXED
      # 2 SD*GAMMA, GAMMA(IEQ) IS TO BE ESTIMATED EACH CYCLE.
      # 3 SD+LAMBDA, LAMBDA(IEQ) IS TO BE ESTIMATED EACH CYCLE.
      #
      #
      # IASS
      # 0 IF Cs ENTERED PATIENT x PATIENT;
      # 2 IF ONE SET OF ABOVE Cs USED FOR ALL PATIENTS;
      # 1 IF Cs ALREADY IN PATIENT FILES WILL BE USED; IF A
      # PATIENT HAS NO C'S, THEN POPULATION C'S WILL BE USED.
      
      if (engine$alg == "NP") {
        if (length(fixed) > 0) {
          # gamma is fixed (error for lambda)
          if (ierrtype == "l") {
            return(list(status = -1, msg = "\nFixed lambda is not currently implemented in NPAG.\nPlease correct the error block in your model file.\n"))
          }
          blocks$error[gamlam[1]] <- gsub("!", "", blocks$error[gamlam[1]])
          ierrmod <- 1 # set to fixed gamma
          gamlam0 <- as.numeric(gsub("!", "", ierr[2])) # but multiply assay error coefficients by fixed amount later
          asserr <- gsub(sep, "  ", blocks$error[-gamlam])
          iass <- rep(1, engine$numeqt)
          assfix <- grep("!", asserr) # check for asserr override
          if (length(assfix) > 0) {
            iass[assfix] <- 2
            asserr <- paste(gsub("!", "", asserr), collapse = "\n")
          }
        } else {
          # gamma/lambda are to be estimated
          ierrmod <- switch(ierrtype,
            g = 2,
            l = 3
          )
          gamlam0 <- as.numeric(ierr[2])
          asserr <- gsub(sep, "  ", blocks$error[-gamlam])
          iass <- rep(1, engine$numeqt)
          assfix <- grep("!", asserr) # check for asserr override
          if (length(assfix) > 0) {
            iass[assfix] <- 2
            asserr <- paste(gsub("!", "", asserr), collapse = "\n")
          }
        }
        asserr <- paste(gsub("!", "", asserr), collapse = "\n") # clean up asserr
        iass <- paste(iass, collapse = "     ") # clean up iass
      }
      
      # IT2B error parameters
      # IERRMOD
      # 1 IF GAMMA(IEQ) IS TO REMAIN 1.0 THROUGHOUT THE ANALYSIS;
      # 0 IF THE UPDATED ESTIMATE OF GAMMA(IEQ) IS TO BE ESTIMATED EACH CYCLE.
      # 2 IF Cs ARE TO BE ESTIMATED.
      #
      #
      # IASS
      # 0 IF Cs ENTERED PATIENT x PATIENT;
      # 2 IF ONE SET OF ABOVE Cs USED FOR ALL PATIENTS;
      # 1 IF Cs ALREADY IN PATIENT FILES WILL BE USED; IF A
      # PATIENT HAS NO C'S, THEN POPULATION C'S WILL BE USED.
      
      # IQVAL
      # 0 IF OUTPUT EQ. HAS ITS Cs ENTERED BY USER (NOT
      # ESTIMATED BY assbigxx.exe) AND IERRTYPE(IEQ) = 1
      # 1 IF OUTPUT EQ. HAS ITS Cs ENTERED BY USER (NOT
      # ESTIMATED BY assbigxx.exe) AND IERRTYPE(IEQ) = 0
      # 4 IF OUTPUT EQ. HAD ITS Cs ESTIMATED PREVIOUSLY
      # BY assbigxx.exe.
      
      if (engine$alg == "IT") {
        if (ierrtype == "l") {
          return(list(status = -1, msg = "\nLambda is not currently implemented in IT2B\nPlease correct the error block in your model file.\n"))
        }
        if (length(fixed) > 0) {
          ierrmod <- rep(1, engine$numeqt) # set to fixed gamma, always 1
          iqval <- rep(0, engine$numeqt)
          asserr <- gsub(sep, "  ", blocks$error[-gamlam])
        } else {
          ierrmod <- rep(0, engine$numeqt) # set to estimated gamma, always starting at 1
          iqval <- rep(1, engine$numeqt)
          asserr <- gsub(sep, "  ", blocks$error[-gamlam])
        }
        iass <- rep(1, engine$numeqt) # 1 for datafile or general source for asserr
        assfix <- grep("!", asserr) # check for general only (override)
        if (length(assfix) > 0) {
          asserr <- gsub("!", "", asserr)
          iass[assfix] <- 2
        }
        comberr <- paste(ierrmod, asserr, iass, iqval, sep = "\n", collapse = "\n")
      }
      if (engine$alg == "ERR") {
        if (ierrtype == "l") {
          return(list(status = -1, msg = "\nLambda is not currently implemented in IT2B/Error.\nPlease correct the error block in your model file.\n"))
        }
        ierrmod <- rep(2, engine$numeqt) # set to estimated gamma, always starting at 1
        iqval <- rep(0, engine$numeqt)
        asserr <- gsub(sep, "  ", blocks$error[-gamlam])
        iass <- rep(1, engine$numeqt) # 1 for datafile or general source for asserr
        assfix <- grep("!", asserr) # check for general only (override)
        if (length(assfix) > 0) {
          iass[assfix] <- 2
          asserr <- gsub("!", "", asserr)
        }
        comberr <- paste(ierrmod, asserr, iass, iqval, sep = "\n", collapse = "\n")
      }
    } else {
      # gamma/lambda was omitted from file
      return(list(status = -1, msg = "Please specify a gamma or lambda error model in your\nmodel file error block."))
    }
  }
  
  
  
  
  
  # write report
  if (!quiet) {
    cat(paste("\nModel solver mode: ", switch(letters[N + 2],
      a = "Algebraic",
      b = "Exact",
      "ODE"
    ), sep = ""))
    if (N > 0) {
      numcomp <- N
      absorb <- F
    }
    if (N == 0) {
      numcomp <- 1
      absorb <- F
    }
    if (N == -1) {
      if (identical(missVars, c("KA", "KCP", "KPC"))) {
        numcomp <- 1
        absorb <- F
      }
      if (identical(missVars, c("KCP", "KPC"))) {
        numcomp <- 2
        absorb <- T
      }
      if (identical(missVars, "KA")) {
        numcomp <- 2
        absorb <- F
      }
      if (identical(missVars, NA)) {
        numcomp <- 3
        absorb <- T
      }
    }
    cat(paste("\nNumber of compartments: ", numcomp, c(", including an absorptive compartment", "")[1 + as.numeric(!absorb)], sep = ""))
    cat(paste("\nPrimary Variables: ", paste(blocks$primVar, collapse = ", "), sep = ""))
    if ((engine$alg == "IT" | engine$alg == "ERR") & length(fixedpos) > 0) {
      cat(paste(" (", paste(blocks$primVar[fixedpos], collapse = ", "), " fixed to be positive)", sep = ""))
    }
    cat(paste("\nCovariates in data file: ", c(paste(engine$covnames, collapse = ", "), "None")[1 + as.numeric(is.na(engine$covnames[1]))]))
    cat(paste("\nCovariates used in model file: ", c(paste(blocks$covar, collapse = ", "), "None")[1 + as.numeric(blocks$covar[1] == "")]))
    cat(paste("\nSecondary Variables: ", paste(secVarNames, collapse = ", "), sep = ""))
    cat(paste("\nModel conditions: ", c("bioavailability term defined, ", "no bioavailability term defined, ")[1 + as.numeric(blocks$f[1] == "")],
    c("initial conditions are not zero, ", "initial conditions are zero, ")[1 + as.numeric(blocks$ini[1] == "")],
    c("lag term defined", "no lag term defined")[1 + as.numeric(blocks$lag[1] == "")],
    sep = ""
  ))
  if (engine$alg != "SIM") cat(paste("\nNumber of cycles to run:", engine$cycles))
  cat("\n\n")
}
# end if quiet
if (getPMoptions("backend") == "rust") {
  model_file <- "main.rs"
} else {
  model_file <- modelFor
}

ret_list <- list(
  status = 1, N = N, ptype = ptype, model = model_file,
  ctype = ctype, nvar = nvar, nofix = nofix, nranfix = nranfix,
  valfix = valfix, ab = ab.df, indpts = indpts,
  asserr = asserr, blocks = blocks
)
return(ret_list)
}
# end makeModel function


###### END NICELY FROM CRASHED NPrun or ITrun

endNicely <- function(message, model = -99, data = -99) {
  files <- Sys.glob("*", T)
  if ("mQRZZZ.txt" %in% files) {
    file.remove(model)
    file.rename("mQRZZZ.txt", model)
    files <- Sys.glob("*", T)
  }
  cleanUp <- Sys.glob(c(
    "fort.*", "XQZPJ*.ZMQ", "extnum", "npag*.*", "CHMAX*.*", "SAVEINST.TMP", "go", "np_prep*", "np_run*", "nplog.txt", "NPcontrol", "npscript*",
    "it2b*.*", "itas*.*", "it_prep*", "it_run*", "itlog.txt", "ITcontrol", "itscript*", "instr.inx",
    "assdriv.f", "err_prep*", "err_run*", "ERRcontrol", "errscript*", "errlog.txt"
  ))
  
  if (length(cleanUp) > 0) file.remove(cleanUp)
  stop(message, call. = F)
}


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

# get the next line when building a file like instructions
getNext <- function(build) {
  return(length(build) + 1)
}

# file name check
FileNameOK <- function(filename) {
  baseName <- basename(filename)
  # strip extension
  baseName2 <- strsplit(baseName, "\\.")[[1]][1]
  if (nchar(baseName2) > 8) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

FileExists <- function(filename) {
  if (!file.exists(filename)) {
    while (!file.exists(filename)) { # oops, filename doesn't exist
      cat(paste0(filename, " does not exist in ", getwd(), ".\n"))
      filename <- tryCatch(readline("Enter another filename or 'ESC' to quit: \n"),
      interrupt = function(e) {
        stop("No filename. Function aborted.\n", call. = F)
      }
    )
  }
}
return(filename)
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

# This might be a solution: https://community.rstudio.com/t/how-to-get-rstudio-ide-to-use-the-correct-terminal-path-in-mac-os-x/131528/3
isM1 <- function() {
  OS <- getOS()
  isM1 <- F
  if (OS == 1 && Sys.info()[5] == "arm64") { # This means it is a M1-chip based apple computer
    isM1 <- T
  }
  isM1
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

# getOSname ------------------------------------------------------------------

# function to retrieve osname in osx,windows,or linux
getOSname <- function() {
  # https://askubuntu.com/questions/46627/how-can-i-make-a-script-that-opens-terminal-windows-and-executes-commands-in-the
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else {
    ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}


# check for installed packages --------------------------------------------

checkRequiredPackages <- function(pkg, repos = "CRAN", quietly = TRUE) {
  managePkgs <- function(thisPkg) {
    if (requireNamespace(thisPkg, quietly = TRUE)) {
      return("ok") # package is installed
    } else { # package is not installed
      cat(paste0("The package ", thisPkg, " is required and will be installed.\n"))
      if (repos == "CRAN") {
        install.packages(thisPkg, dependencies = TRUE, verbose = FALSE, quiet = TRUE) # try to install
      } else {
        tryCatch(remotes::install_github(repos), error = function(e) FALSE)
      }
      if (requireNamespace(thisPkg, quietly = TRUE)) { # check again
        return("ok") # now it is installed and ok
      } else {
        return(thisPkg)
      } # nope, still didn't install
    }
  }
  
  msg <- pkg %>%
  map_chr(managePkgs) %>%
  keep(~ . != "ok")
  
  if (length(msg) > 0) {
    if (!quietly) {
      cat(
        crayon::red("\nError:"), "The following packages needed for this function did not install:",
        paste(msg, collapse = ", ")
      )
    }
    return(invisible(FALSE))
  } else {
    return(invisible(TRUE)) # all packages present or installed
  }
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


# obsStatus -----------------------------------------------------------

# classify observations
# data should be a vector
obsStatus <- function(data) {
  present <- which(data != -99)
  missing <- which(data == -99)
  return(list(present = present, missing = missing))
}

obsStatus <- function(data) {
  status <- ifelse(data == -99, TRUE, FALSE)
  return(status)
}


# import recycled text into documentation
template <- function(name) {
  insert <- readLines(paste0("man-roxygen/", name, ".R")) %>%
  stringr::str_replace("#' ", "") %>%
  stringr::str_replace("<br>", "  \n")
  insert <- c(insert, "  \n")
  insert <- paste(insert, collapse = " ")
  return(insert)
}




# WEIGHTED SUMMARY FUNCTIONS ---------------------------------------------------------------

# modified from Hmisc functions

wtd.table <- function(x, weights = NULL,
  type = c("list", "table"),
  normwt = TRUE,
  na.rm = TRUE) {
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
  
  
  wtd.quantile <- function(x, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1),
  normwt = TRUE,
  na.rm = TRUE) {
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

wtd.var <- function(x, weights = NULL,
  normwt = TRUE,
  na.rm = TRUE,
  method = c("unbiased", "ML")) {
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
  func_to_char <- function(fun){
    deparse(fun, width.cutoff = 500L) %>%
    stringr::str_trim("left") %>%
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
    
    highlight <- attr(df, "highlight") # get columns to highlight minimums from attributes

    # Convert all columns to character for uniform formatting
    df_chr <- df %>% mutate(across(where(is.double), ~round2(.x))) %>%
    mutate(across(everything(), ~as.character(.x, stringsAsFactors = FALSE))) 
 
    
    if (highlight){ # highlight minimums in requested columns
      # first replace minima with special formatting
      # mins <- df %>% summarize(across(c(-run, -nvar, -converged, -pval, -best), ~round2(min(.x, na.rm = TRUE)))) # get minima for each column
      mins <- df %>% summarize(across(c(-run, -nvar, -converged, -pval, -best), ~ which(.x == min(.x, na.rm = TRUE)))) %>% unlist() # get minima for each column

      best <- df %>% summarize(across(best, ~ which(.x == max(.x, na.rm = TRUE)))) %>% unlist() # get best for best column

      # create table to get the spacing
      df_tab <- knitr::kable(df_chr, format = "simple") 

      # rebuild the data frame
      df2 <- map_vec(df_tab, \(x) str_split(x, "(?<=\\s)(?=\\S)")) 
      df2 <- as.data.frame(do.call(rbind, df2))

      # replace minima with highlighted versions
      # first 2 rows are headers and spacers, so need to add 2 to the mins row index
      for (p in 1:length(mins)){
        df2[mins[p]+2, p+3] <- stringr::str_replace_all(df2[mins[p]+2, p+3], "(\\d+(?:\\.\\d+)?)(\\s+)", "{.red \\1}\\2")
      }

      # for(p in 1:length(mins)){
      #   df2[, p+3] <- stringr::str_replace_all(df2[, p+3], as.character(mins[p]), paste0("{.strong ", as.character(mins[p]), "}"))
      # }
      # df2$V18 <- stringr::str_replace(df2$V18, as.character(best), paste0("{.red ", as.character(best), "}"))
      df2$V17[best+2] <- stringr::str_replace(df2$V17[best+2], "(\\d+(?:\\.\\d+)?)(\\s+)", "{.red \\1}\\2")

      # print header
      header <- df2[1,] %>% stringr::str_replace_all(" ", "\u00A0" ) %>% paste(collapse = "")
      cli::cli_text("{.strong {header}}")
      cli::cli_div(theme = list(span.red = list(color = "red", "font-weight" = "bold")))
      
      # replace ≥2 spaces with non-breaking spaces
      for (i in 2:nrow(df2)) {
        # m <- gregexpr("\\s{2,}", df_tab[i], perl = TRUE)
        # regmatches(df_tab[i], m) <- lapply(regmatches(df_tab[i], m), function(ss) {
        #   vapply(ss, function(one) {
        #     paste0(rep("\u00A0", nchar(one)), collapse = "")
        #   }, character(1))
        # })
        # print each row
        cli::cli_text(paste(df2[i,], collapse = "") %>% stringr::str_replace_all(" ", "\u00A0" ) %>% stringr::str_replace_all("strong\u00A0+", "strong ") %>% stringr::str_replace_all("red\u00A0+", "red ")) 
      } 
      cli::cli_end()
    } else { # no highlighting

      # create table
      df_tab <- knitr::kable(df_chr, format = "simple")

      # print header
      header <- df_tab[1] %>% stringr::str_replace_all(" ", "\u00A0" )
      cli::cli_text("{.strong {header}}")

      # print each row
      for (i in 2:length(df_tab)) {
      cli::cli_text(df_tab[i] %>% stringr::str_replace_all(" ", "\u00A0" ))
      }
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
  cor2cov <- function(cor, sd){ 
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
  pos_def <- function(mat, id, source){
    # check to make sure mat (within 15 sig digits, which is in file) is pos-def and fix if necessary
    posdef <- rlang::try_fetch(eigen(signif(mat, 15)),
    error = function(e) {
      return(list(values = 0))
    }
  )
  ans <- NULL
  if (any(posdef$values < 0)) {
    
    mat_names <- dimnames(mat)[[1]] #store for later
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
        eps <- 1e-8  # threshold for small eigenvalues
        eig <- eigen(mat)
        eig$values[eig$values < eps] <- eps  # threshold small eigenvalues
        mat <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
        
        
        posdef <- eigen(signif(mat, 15))
        
        if (all(posdef$values >= 0)) { # success, break out of loop
          break
        }
        if(j == 10) browser()
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
modifyList2 <- function (x, val, keep.null = FALSE) 
{
  stopifnot(is.list(x), is.list(val))
  names(x) <- tolower(names(x))
  names(val) <- tolower(names(val))
  xnames <- names(x)
  vnames <- names(val)
  # handle unnamed lists
  if(is.null(xnames)) xnames <- 1:length(x)
  if(is.null(vnames)) vnames <- 1:length(val)
  # handle unnamed elements
  xnames <- ifelse(xnames == "", as.character(seq_along(xnames)), xnames)
  vnames <- ifelse(vnames == "", as.character(seq_along(vnames)), vnames)
  
  vnames <- vnames[nzchar(vnames)]
  if (keep.null) {
    for (v in vnames) {
      x[v] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
      list(modifyList(x[[v]], val[[v]], keep.null = keep.null))
      else val[v]
    }
  }
  else {
    for (v in vnames) {
      x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && 
      is.list(val[[v]])) 
      modifyList2(x[[v]], val[[v]], keep.null = keep.null)
      else val[[v]]
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
clear_build <- function(){
  fs::dir_delete(system.file("template", package = "Pmetrics"))
}
