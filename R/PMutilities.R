#This file contains internal Pmetrics utility functions

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
#Below is MN's code as of 12/3/18

compareTwoModelERRs <- function(mod1, mod2) {

  getAssErr <- function(model) {
    blocks <- parseBlocks(model)
    if (length(grep(";", blocks$primVar)) > 0) {
      #using ';' as separator
      sep <- ";"
    } else {
      if (length(grep(",", blocks$primVar)) > 0) {
        #using ',' as separator
        sep <- ","
      } else { return(F) }
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
  Ifile = paste("../", old.data, "/etc/instr.inx", sep = "")
  # The header lines for all instruction blocks in old.data (lines that begin w/a " ")
  HLs <- grep(" ", readLines(Ifile))
  lines <- readLines(Ifile)
  FirstChar <- substring(lines, 1, 1)
  HLs <- which(FirstChar == " ")
  # The header line for the requested instruction block
  dataHeaderLine <- (grep(instr, readLines(Ifile), ignore.case = T))
  # The desired block is in instr.inx[dataHeaderLine:HLs[dataHeaderLine+1]]
  instrBlockStart = HLs[which(HLs == dataHeaderLine)] + 1
  instrBlockStop = HLs[which(HLs == dataHeaderLine) + 1] - 1
  if ((instrBlockStop - instrBlockStart) > -1) {
    read1 <- readLines(Ifile, n = instrBlockStop)
    InstrBlock <- read1[instrBlockStart:instrBlockStop]
  }
  else {
    InstrBlock <- -99
  }
  return(InstrBlock)
}
# wmy ########### END of Femke Error Polynomial Quandry

#make pretty log axes
logAxis <- function(side, grid = F, ...) {
  pow <- log10(axTicks(side))
  pow[1] <- ceiling(pow[1])
  pow[length(pow)] <- floor(pow[length(pow)])
  pow <- round(pow, 0)
  pow <- unique(pow)
  ticksat1 <- 10 ^ pow
  ticksat2 <- as.vector(sapply(pow, function(p)(1:9) * 10 ^ p))
  labels <- formatC(ticksat1, digits = 5, format = "g")
  for (i in 1:length(labels)) {
    if (length(grep("e", labels[i])) > 0) labels[i] <- as.expression(substitute(10 ^ x, list(x = log10(ticksat1[i]))))
  }
  axis(side, ticksat1, labels = labels, tcl = -0.5, lwd = 0, lwd.ticks = 1, ...)
  axis(side, ticksat2, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1, ...)

  if (grid & (side == 1 | side == 3)) abline(v = ticksat2, col = "lightgray", lty = 1)
  if (grid & (side == 2 | side == 4)) abline(h = ticksat2, col = "lightgray", lty = 1)

}

#make a density file stub for bootstrapping and priors
makeDen <- function(NPdata, bootstrap = F) {
  f <- file("prior.txt", "w")
  writeLines("DENSITY OCT_15 ... Made by npagranfix6", f)
  if (is.null(NPdata$ndim)) {
    if (bootstrap > 1) return(invisible(-1))
    cat("\nYour NPdata prior object is older and does not contain the number of dimensions in your model.\n")
    cat("Please enter the numerical value of N in your model file.")
    NPdata$ndim <- as.numeric(readline("(-1 for analytic, 0 for calculated output, or the number of differential equations): "))
  }

  #resample, i.e. jitter the population prior if bootstrapping
  if (bootstrap) {
    final <- makeFinal(NPdata)
    corden <- t(apply(final$popPoints[, 1:NPdata$nvar], 1, function(x) rmnorm(n = 1, mean = x, sigma = final$popCov / nrow(final$popCov) / 10)))
    corden <- cbind(corden, NPdata$corden[, (NPdata$nvar + 1)])
    NPdata$corden <- corden
  }
  #write new density file
  write(NPdata$ndim, f)
  write(NPdata$indpts, f)
  write(NPdata$nactve, f)
  write(NPdata$nvar, f)
  writeLines(NPdata$par, f)
  write(NPdata$nofix, f)
  write(NPdata$parfix, f)
  if (length(NPdata$nranfix) > 0) {
    write(NPdata$nranfix, f)
    write(NPdata$parranfix, f)
  } else {
    write("0", f)
    write("", f)
  }
  write.table(NPdata$ab, f, row.names = F, col.names = F)
  write(NPdata$valfix, f)
  if (length(NPdata$nranfix) > 0) {
    write(NPdata$valranfix, f)
  } else {
    write("", f)
  }
  writeLines("100", f)
  write(NPdata$icycst + NPdata$icyctot - 1, f)
  writeLines("0", f)
  write.table(NPdata$corden, f, row.names = F, col.names = F)
  close(f)
  return(invisible(1))
}

#sample from multivariate normal distribution, code modified from mvtnorm package
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


#parse NP_RF file only for final cycle information; used for bootstrapping
#indpts,ab,corden,nvar,nactve,iaddl,icyctot,par  
getFinal <- function(outfile = "NP_RF0001.TXT") {
  #require(utils)
  #get data
  if (!file.exists(outfile)) { stop(paste(outfile, "not found.\n", sep = " ")) }

  setwd(dirname(outfile))

  negflag <- F
  RFver <- readLines(outfile, n = 1)
  vernum <- switch(RFver, " VERSION 1.1 - JAN 2011 " = 1, " VERSION 1.2 - APR 2011 " = 2, " VERSION 1.3 - JUL 2011 " = 3, " VERSION 1.4 - AUG 2011 " = 4, 4)
  dimlines <- switch(vernum, 9, 9, 15, 15)
  NPdims <- scan(outfile, quiet = T, skip = 3, nlines = dimlines,, what = "character", comment.char = "#")
  #number of subjects
  nsub <- as.numeric(NPdims[1])
  #number of active grid points
  nactve <- as.numeric(NPdims[2])
  #number of random parameters
  nvar <- as.numeric(NPdims[3])
  #index of grid points
  indpts <- as.numeric(NPdims[6])

  #if version 1.2 or less
  if (vernum < 3) {
    #final cycle number
    icyctot <- as.numeric(NPdims[7])
    #number of AUC blocks over all subjects
    nauc <- scan(outfile, quiet = T, skip = 13 + 3 * nsub, n = 1, comment.char = "#")
    #get table of contents
    toc <- scan(outfile, quiet = T, skip = 18 + 3 * nsub + nauc, n = 26, comment.char = "#")
  }
  if (vernum == 3) {
    #final cycle number
    icyctot <- as.numeric(NPdims[9])
    #number of AUC blocks over all subjects
    nauc <- scan(outfile, quiet = T, skip = 20 + 3 * nsub, n = 1, comment.char = "#")
    #get table of contents
    toc <- scan(outfile, quiet = T, skip = 25 + 3 * nsub + nauc, n = 26, comment.char = "#")
  }
  if (vernum > 3) {
    #final cycle number
    icyctot <- as.numeric(NPdims[9])
    #number of drugs
    ndrug <- as.numeric(NPdims[15])
    #number of AUC blocks over all subjects
    nauc <- scan(outfile, quiet = T, skip = 19 + ndrug + 3 * nsub, n = 1, comment.char = "#")
    #get table of contents
    toc <- scan(outfile, quiet = T, skip = 24 + ndrug + 3 * nsub + nauc, n = 26, comment.char = "#")
  }
  #get random parameter names
  par <- scan(outfile, what = "character", n = nvar, skip = toc[1], quiet = T)
  #get initial ranges for random parameters
  ab <- matrix(scan(outfile, n = nvar * 2, skip = toc[3], quiet = T), nrow = nvar, ncol = 2, byrow = T)
  #get the density
  corden <- matrix(as.numeric(sub("D", "E", scan(outfile, skip = toc[6], n = nactve * (nvar + 1), quiet = T, what = ""))), nrow = nactve, ncol = nvar + 1, byrow = T)
  #get additional information
  iaddl <- array(data = as.numeric(sub("D", "E", scan(outfile, skip = toc[17], n = icyctot * nvar * 12, quiet = T, what = ""))), dim = c(12, nvar, icyctot))
  #set the number of grid points at the beginning
  gridpts <- switch(indpts, 2129, 5003, 10007, 20011, 40009, 80021)
  if (is.null(gridpts)) {
    gridpts <- (indpts - 100) * 80021
  }
  #summarize weighted corden
  wParVol <- prod(ab[, 2] - ab[, 1]) / gridpts
  if (nrow(corden) > 1) { popMean <- colSums(corden[, 1:nvar] * corden[, nvar + 1]) * wParVol } else {
    popMean <- corden[1:nvar] * corden[nvar + 1] * wParVol
  }

  if (nrow(corden) > 1) {
    popCov <- matrix(NA, ncol = nvar, nrow = nvar)
    for (i in 1:nvar) {
      for (k in 1:nvar) {
        popCov[i, k] <- sum(corden[, i] * corden[, k] * corden[, nvar + 1]) * wParVol - popMean[i] * popMean[k]
      }
    }
    if (any(popCov == 0)) { popCor <- NA } else { popCor <- cov2cor(popCov) }
  } else {
    popCov <- matrix(rep(0, nvar ** 2), nrow = nvar)
    popCor <- matrix(rep(NA, nvar ** 2), nrow = nvar)
    diag(popCor) <- rep(1, nvar)
  }

  popPoints <- data.frame(corden)
  names(popPoints) <- c(par, "prob")
  popPoints$prob <- popPoints$prob * wParVol
  names(popMean) <- par
  dimnames(popCov) <- list(par, par)
  if (all(!is.na(popCor))) dimnames(popCor) <- list(par, par)

  if (icyctot > 0) {
    popMedian <- iaddl[6,, icyctot]
  } else {
    calcWtMed <- function(x, prob) {
      x <- cbind(x, prob)
      if (nrow(x) == 1) { return(x[1]) }
      x <- x[order(x[, 1]),]
      xsum <- cumsum(x[, 2])
      xmedindex <- min(which(xsum > 0.5))
      xmed <- x[xmedindex, 1]
      xnint <- max(c(100, 2 * nsub))
      xint <- diff(range(x[, 1])) / xnint
      xmed <- xmed - (xsum[xmedindex] - 0.5) / xnint * xint
      return(xmed)
    }
    popMedian <- apply(popPoints[, 1:(ncol(popPoints) - 1)], 2, calcWtMed, popPoints$prob)
  }
  names(popMedian) <- par

  popVar <- diag(popCov)
  names(popVar) <- par

  popSD <- sqrt(popVar)
  names(popSD) <- par

  popCV <- abs(100 * (popSD / popMean))
  names(popCV) <- par


  outlist <- list(popPoints = popPoints, popMean = popMean, popSD = popSD, popCV = popCV, popVar = popVar,
                  popCov = popCov, popCor = popCor, popMedian = popMedian, gridpts = gridpts, ab = ab)
  class(outlist) <- c("PMfinal", "NPAG", "list")
  return(outlist)
}



#read and set defaults
# PMreadDefaults <- function() {
#   optFile <- paste(getPMpath(), "/Pmetrics/config/PMopt.Rdata", sep = "")
#   if (file.exists(optFile)) {
#     load(optFile)
#     if (!is.null(PMopt)) {
#       options(PMopt)
#       cat("\nYou have set custom Pmetrics defaults. Use getDefaults() to see them.\n")
#       flush.console()
#     }
#   }
# }

#check whether gfortran is installed
gfortranCheck <- function(gfortran = Sys.which("gfortran")) {
  #figure out which OS
  OS <- getOS()
  if (OS == 1) {
    #Mac
    OSversion <- substr(system("sw_vers -productVersion", intern = T), 1, 4)
    #gfortran is absent
    if (gfortran == "") {
      #cat("\nPmetrics requires gfortran to run. You do not appear to have a working installation of gfortran.  Launching LAPK website...\n")
      if (compareVersion(OSversion, "10.9") >= 0) OSindex <- 6
      if (compareVersion(OSversion, "10.8") >= 0) OSindex <- 0
      if (compareVersion(OSversion, "10.7") == 0) OSindex <- 1
      if (compareVersion(OSversion, "10.7") == -1 & compareVersion(OSversion, "10.6") >= 0 & getBits() == "64") OSindex <- 2
      if (compareVersion(OSversion, "10.7") == -1 & compareVersion(OSversion, "10.6") >= 0 & getBits() == "32") OSindex <- 3
      cat(paste("Opening http://www.lapk.org/gfortran/gfortran.php?OS=", OSindex, sep = ""))
      system(paste("open http://www.lapk.org/gfortran/gfortran.php?OS=", OSindex, sep = ""))

    }
  }
  if (OS == 2) {
    #Windows
    #gfortran is absent
    if (gfortran != "") {
      #cat("\nPmetrics requires gfortran to run. You do not appear to have a working installation of gfortran.  Launching LAPK website...\n")
      if (getBits() == "64") OSindex <- 4
      if (getBits() == "32") OSindex <- 5
      cat(paste("Opening http://www.lapk.org/gfortran/gfortran.php?OS=", OSindex, sep = ""))
      shell(paste("start http://www.lapk.org/gfortran/gfortran.php?OS=", OSindex, sep = ""))
    }
  }
  if (OS == 3) {
    #Linux
    #gfortran is absent
    if (gfortran == "") {
      #cat("\nPmetrics requires gfortran to run. You do not appear to have a working installation of gfortran.  Launching LAPK website...\n")
      cat("Opening http://gcc.gnu.org/wiki/GFortranBinaries64Linux")
      system("open http://gcc.gnu.org/wiki/GFortranBinaries64Linux")

    }
  }
  # -w == no warnings; -Wall == all warnings (except extra ones; use -Wextra for those)
  # return(paste("gfortran -m",get("PmetricsBit",envir=PMenv)," -w -O3 -o <exec> <files>",sep=""))
  return(paste("gfortran -m", getBits(), " -Wall -O3 -o <exec> <files>", sep = ""))
}


#check for numeric id and convert to number if necessary
checkID <- function(id) {
  id <- gsub("^[[:blank:]]+", "", id)
  id <- gsub("[[:blank:]]+$", "", id)
  idNonNum <- suppressWarnings(any(is.na(as.numeric(id))))
  if (!idNonNum) id <- as.numeric(id)
  return(id)
}

#extract pattern from strings
strparse <- function(pattern, x) {
  match <- regexpr(pattern, x)
  start <- match[1]
  stop <- match[1] + attr(match, "match.length") - 1
  return(substr(x, start, stop))
}


#parse blocks in new model template
parseBlocks <- function(model) {
  modelFile <- scan(model, what = "character", sep = "\n", blank.lines.skip = T, quiet = T)
  #remove comment lines
  commLn <- grep("^C ", modelFile)
  if (length(commLn) > 0) modelFile <- modelFile[-commLn]
  if (length(grep("TSTMULT", modelFile)) > 0) { return(list(status = 0, model = model)) }
  #stop, we already have a fortran model file
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
  output <- blockStart[grep("#out", headers)]
  error <- blockStart[grep("#err", headers)]
  extra <- blockStart[grep("#ext", headers)]

  headerPresent <- which(c(length(primVar) > 0, length(covar) > 0, length(secVar) > 0, length(bolus) > 0, length(ini) > 0,
                           length(f) > 0, length(lag) > 0, length(diffeq) > 0, length(output) > 0, length(error) > 0, length(extra) > 0))
  if (any(!c(1, 9, 10) %in% headerPresent)) return(list(status = -1, msg = "You must have #Primary, #Output, and #Error blocks at minimum"))

  headerOrder <- c(primVar, covar, secVar, bolus, ini, f, lag, diffeq, output, error, extra)
  blockStart <- blockStart[rank(headerOrder)]
  blockStop <- blockStop[rank(headerOrder)]

  #remove headers that have no information
  ok <- mapply(function(x, y) x != y, blockStart, blockStop)
  blockStart <- blockStart[ok]
  blockStop <- blockStop[ok]
  headerPresent <- headerPresent[ok]

  #get blocks
  blocks <- list(primVar = NA, covar = NA, secVar = NA, bolus = NA, ini = NA, f = NA, lag = NA, diffeq = NA, output = NA, error = NA, extra = NA)
  for (i in 1:length(headerPresent)) {
    temp <- modelFile[(blockStart[i] + 1):blockStop[i]]
    allblank <- grep("^[[:blank:]]+$", temp)
    if (length(allblank) > 0) temp <- temp[-allblank]
    blocks[[headerPresent[i]]] <- temp
  }
  emptyHeaders <- which(is.na(blocks))
  if (length(emptyHeaders) > 0) blocks[emptyHeaders] <- ""
  return(blocks)
}

#check all blocks statements for more than maxwidth characters and insert line break if necessary
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
}


#convert new model template to model fortran file
#convert new model template to model fortran file
makeModel <- function(model = "model.txt", data = "data.csv", engine, write = T, silent = F) {

  blocks <- parseBlocks(model)

  #check for reserved variable names
  reserved <- c("ndim", "t", "x", "xp", "rpar", "ipar", "p", "r", "b", "npl", "numeqt", "ndrug", "nadd", "rateiv", "cv",
                "n", "nd", "ni", "nup", "nuic", "np", "nbcomp", "psym", "fa", "lag", "tin", "tout")
  conflict <- c(match(tolower(blocks$primVar), reserved, nomatch = -99), match(tolower(blocks$secVar), reserved, nomatch = -99), match(tolower(blocks$covar), reserved, nomatch = -99))
  nconflict <- sum(conflict != -99)
  if (nconflict > 0) {
    msg <- paste("\n", paste(paste("'", reserved[conflict[conflict != -99]], "'", sep = ""), collapse = ", "), " ", c("is a", "are")[1 + as.numeric(nconflict > 1)], " reserved ", c("name", "names")[1 + as.numeric(nconflict > 1)], ", regardless of case.\nPlease choose non-reserved parameter/covariate names.\n", sep = "")
    return(list(status = -1, msg = msg))
  }

  #check all blocks statements for more than maxwidth characters and insert line break if necessary
  maxwidth <- 60
  blocks <- chunks(x = blocks, maxwidth = maxwidth)


  #primary variable definitions
  npvar <- length(blocks$primVar)
  psym <- vector("character", npvar)
  pvardef <- psym
  if (length(grep(";", blocks$primVar)) > 0) {
    #using ';' as separator
    sep <- ";"
  } else {
    if (length(grep(",", blocks$primVar)) > 0) {
      #using ',' as separator
      sep <- ","
    } else { return(list(status = -1, msg = "\nPrimary variables should be defined as 'var,lower_val,upper_val' or 'var,fixed_val'.\n")) }
  }

  #find out if any are fixed to be positive only for IT2B
  fixedpos <- grep("\\+", blocks$primVar)
  if (length(fixedpos) > 0) blocks$primVar <- gsub("\\+", "", blocks$primVar)

  #find out if any are to be fixed (constant) 
  fixcon <- grep("!", blocks$primVar)
  nofix <- length(fixcon)
  if (nofix > 0) blocks$primVar <- gsub("!", "", blocks$primVar)


  #get limits [a,b] on primary variables
  splitprimVar <- strsplit(blocks$primVar, sep)
  a <- as.numeric(unlist(lapply(splitprimVar, function(x) x[2])))
  b <- as.numeric(unlist(lapply(splitprimVar, function(x) x[3])))

  #set parameter type: 1 for random, 0 for constant, -1 for random but pos (IT2B only) and 2 for fixed random
  ptype <- c(1, 2)[1 + as.numeric(is.na(b))]
  #if any fixed constant variables are present, set ptype to 0
  if (nofix > 0) ptype[fixcon] <- 0

  # #for now, make all fixed constants for IT2B and SIM
  # if(engine$alg=="IT" | engine$alg=="SIM"){
  #   ptype[ptype==2] <- 0
  #   nofix <- sum(as.numeric(is.na(b)))
  # }

  #npvar is total number of parameters
  #nvar is number of random (estimated) parameters
  #nranfix is number of fixed (but unknown) parameters
  #nofix is number of constant parameters
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


  #replace a,b with SIM limits argument if it is present
  if (engine$alg == "SIM" & !all(is.na(engine$limits))) {
    if (nrow(engine$limits) == nvar) {
      #make sure same row number
      replA <- engine$limits[, 1]
      replB <- engine$limits[, 2]
      ab.df$a[!is.na(replA)] <- replA[!is.na(replA)]
      ab.df$b[!is.na(replB)] <- replB[!is.na(replB)]
    } else { return(list(status = -1, msg = "Your limit block does not have the same number of parameters as the model file.\n")) }
  }

  if (nofix > 0 & any(is.na(valfix))) return(list(status = -1, msg = "One or more variables did not have any boundaries.\n"))
  if (nranfix > 0 & any(is.na(valranfix))) return(list(status = -1, msg = "One or more variables did not have any boundaries.\n"))

  #set grid point index for NPAG if not supplied
  if (engine$indpts == -99) {
    indpts <- switch(nvar, 1, 1, 3, 4, 6)
    if (is.null(indpts)) indpts <- 100 + nvar - 5
    if (indpts > 108) indpts <- 108
  } else { indpts <- engine$indpts }


  #transform ab
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


  #covariate definitions
  if (blocks$covar[1] != "") {
    ncov <- length(blocks$covar)
    interpol <- grep("!", blocks$covar)
    blocks$covar <- gsub("!", "", blocks$covar)
    covardef <- vector("character", ncov)
    for (i in 1:ncov) {
      covardef[i] <- paste(blocks$covar[i], "=CV(", i, ")", sep = "")
    }
    if (!identical(1:ncov, which(tolower(engine$covnames) %in% tolower(blocks$covar)))) return(list(status = -1, msg = "The covariate set in your model file was not in the same order as in your data file.\n"))
  } else {
    covardef <- ""
    interpol <- grep("!", blocks$covar)
  }
  if (engine$ncov > 0) { ctype = rep(2, engine$ncov) } else { ctype = -99 }
  #set covariate type based on number of covariates in data file, default is 2, interpolated
  if (length(interpol) > 0) ctype[interpol] <- 1 #change those in model file with "!" to constant

  #secondary variable definitions
  svardef <- blocks$secVar

  #get secondary variables and remove continuation lines beginning with "&"
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

  #take out any extra declarations in diffeq to add to declarations in subroutine
  diffdec <- grep("COMMON|EXTERNAL|DIMENSION", blocks$diffeq, ignore.case = T)
  if (length(diffdec) > 0) {
    diffstate <- blocks$diffeq[diffdec]
    blocks$diffeq <- blocks$diffeq[-diffdec]
  } else { diffstate <- "" }

  #detect N
  if (blocks$diffeq[1] == "") {
    if ("KE" %in% toupper(secVarNames) | "KE" %in% toupper(blocks$primVar)) { N <- -1 } else { N <- 0 }
  } else {
    #get number of equations and verify with data file
    #find statements with XP(digit)
    compLines <- grep("XP\\([[:digit:]]+\\)", blocks$diffeq)
    if (length(compLines) == 0) { N <- 0 } else {
      compStatements <- sapply(blocks$diffeq[compLines], function(x) strparse("XP\\([[:digit:]]+\\)", x))
      compNumbers <- sapply(compStatements, function(x) strparse("[[:digit:]]+", x))
      #get max number
      N <- max(as.numeric(compNumbers))
    }
  }

  #figure out model if N = -1 and if so, assign values to required KA,KE,V,KCP,KPC
  reqVars <- c("KA", "KE", "KCP", "KPC", "V")
  matchVars <- match(reqVars, toupper(c(blocks$primVar, secVarNames)))
  if (N == -1) {
    if (any(is.na(matchVars))) {
      missVars <- reqVars[is.na(matchVars)]
      if ("KE" %in% toupper(missVars)) return(list(status = -1, msg = "\nYou have specified an algebraic model, which requires a variable named 'KE'\n"))
      if ("V" %in% toupper(missVars)) return(list(status = -1, msg = "\nYou have specified an algebraic model, which requires a variable named 'V'\n"))
      missVarValues <- paste(missVars, "=0", sep = "")
      if (length(missVarValues) > 0) svardef <- c(svardef, missVarValues)
      svardef <- svardef[svardef != ""]
      #add new secondary variables that won't be estimated
      secVarNames <- c(secVarNames, missVars)
      secVarNames <- secVarNames[secVarNames != ""]
    } else { missVars <- NA }
  } else {
    if (any(is.na(matchVars))) { missVars <- reqVars[is.na(matchVars)] } else { missVars <- NA }
  }

  #get number of equations and verify with data file
  #find statements with Y(digit)
  outputLines <- grep("Y\\([[:digit:]]+\\)", blocks$output)
  if (length(outputLines) == 0) return(list(status = -1, msg = "\nYou must have at least one output equation of the form 'Y(1) = ...'\n"))
  #extract numbers
  outputStatements <- sapply(blocks$output[outputLines], function(x) strparse("Y\\([[:digit:]]+\\)", x))
  outputNumbers <- sapply(outputStatements, function(x) strparse("[[:digit:]]", x))
  #get max number
  modelnumeqt <- max(as.numeric(outputNumbers))
  if (modelnumeqt != engine$numeqt) return(list(status = -1, msg = "\nThe number of output equations in the model file\ndoes not match the maximum value of outeq in your datafile.\n"))

  #remove leading ampersands from getfa, getix, gettlag if present
  oldContLines <- grep("^\\+", c(blocks$f, blocks$ini, blocks$lag))
  if (length(oldContLines > 0)) {
    return(list(status = -1, msg = "\nThe model file format has changed.  Please replace '+' with '&' in all continuation lines.\n"))
  }
  if (length(grep("^&", blocks$f) > 0)) blocks$f <- gsub("^&", "", blocks$f)
  if (length(grep("^&", blocks$ini) > 0)) blocks$ini <- gsub("^&", "", blocks$ini)
  if (length(grep("^&", blocks$lag) > 0)) blocks$lag <- gsub("^&", "", blocks$lag)

  #variable declarations for fortran and make sure not >maxwidth characters
  if (secVarNames[1] != "") {
    vardec <- paste("REAL*8 ", paste(blocks$primVar, collapse = ","), ",", paste(secVarNames, collapse = ","), sep = "")
  } else { vardec <- paste("REAL*8 ", paste(blocks$primVar, collapse = ","), sep = "") }
  if (blocks$covar[1] != "") {
    vardec <- paste(vardec, ",", paste(blocks$covar, collapse = ","), sep = "")
  }
  if (nchar(vardec) > maxwidth) {
    vardec <- paste(unlist(strsplit(vardec, ",")), collapse = ",\n     &  ")
  }

  #error
  blocks$error <- tolower(gsub("[[:space:]]", "", blocks$error))
  #check to make sure coefficient lines are the same number as outputs
  nErrCoeff <- length(blocks$error) - 1
  if (nErrCoeff != modelnumeqt) {
    return(list(status = -1, msg = paste("\nThere ", c("is", "are")[1 + as.numeric(nErrCoeff > 1)], " ",
                                    nErrCoeff, c(" line", " lines")[1 + as.numeric(nErrCoeff > 1)],
                                    " of error coefficients in the model file, but ",
                                    modelnumeqt, " output ", c("equation", "equations")[1 + as.numeric(modelnumeqt > 1)],
                                    ".\nThese must be the same.\n", sep = "")))
  }
  #get assay error only if SIMrun
  if (engine$alg == "SIM") {
    gamlam <- grep("^g|^l", blocks$error)
    if (length(gamlam) > 0) {
      #gamma/lambda is specified in file
      asserr <- unlist(strsplit(blocks$error[-gamlam], sep))
    } else { asserr <- unlist(strsplit(blocks$error, sep)) }
  } else {
    #this is not a SIMrun
    #get gamma or lambda if present and assay error coefficients
    gamlam <- grep("^g|^l", blocks$error)
    if (length(gamlam) > 0) {
      #gamma/lambda is specified in file
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
          #gamma is fixed (error for lambda)
          if (ierrtype == "l") return(list(status = -1, msg = "\nFixed lambda is not currently implemented in NPAG.\nPlease correct the error block in your model file.\n"))
          blocks$error[gamlam[1]] <- gsub("!", "", blocks$error[gamlam[1]])
          ierrmod <- 1 #set to fixed gamma
          gamlam0 <- as.numeric(gsub("!", "", ierr[2])) # but multiply assay error coefficients by fixed amount later
          asserr <- gsub(sep, "  ", blocks$error[-gamlam])
          iass <- rep(1, engine$numeqt)
          assfix <- grep("!", asserr) #check for asserr override
          if (length(assfix) > 0) {
            iass[assfix] <- 2
            asserr <- paste(gsub("!", "", asserr), collapse = "\n")
          }
        } else {
          #gamma/lambda are to be estimated
          ierrmod <- switch(ierrtype, g = 2, l = 3)
          gamlam0 <- as.numeric(ierr[2])
          asserr <- gsub(sep, "  ", blocks$error[-gamlam])
          iass <- rep(1, engine$numeqt)
          assfix <- grep("!", asserr) #check for asserr override
          if (length(assfix) > 0) {
            iass[assfix] <- 2
            asserr <- paste(gsub("!", "", asserr), collapse = "\n")
          }
        }
        asserr <- paste(gsub("!", "", asserr), collapse = "\n") #clean up asserr
        iass <- paste(iass, collapse = "     ") #clean up iass
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
        if (ierrtype == "l") return(list(status = -1, msg = "\nLambda is not currently implemented in IT2B\nPlease correct the error block in your model file.\n"))
        if (length(fixed) > 0) {
          ierrmod <- rep(1, engine$numeqt) #set to fixed gamma, always 1
          iqval <- rep(0, engine$numeqt)
          asserr <- gsub(sep, "  ", blocks$error[-gamlam])
        } else {
          ierrmod <- rep(0, engine$numeqt) #set to estimated gamma, always starting at 1
          iqval <- rep(1, engine$numeqt)
          asserr <- gsub(sep, "  ", blocks$error[-gamlam])
        }
        iass <- rep(1, engine$numeqt) #1 for datafile or general source for asserr
        assfix <- grep("!", asserr) #check for general only (override)
        if (length(assfix) > 0) {
          asserr <- gsub("!", "", asserr)
          iass[assfix] <- 2
        }
        comberr <- paste(ierrmod, asserr, iass, iqval, sep = "\n", collapse = "\n")

      }
      if (engine$alg == "ERR") {
        if (ierrtype == "l") return(list(status = -1, msg = "\nLambda is not currently implemented in IT2B/Error.\nPlease correct the error block in your model file.\n"))
        ierrmod <- rep(2, engine$numeqt) #set to estimated gamma, always starting at 1
        iqval <- rep(0, engine$numeqt)
        asserr <- gsub(sep, "  ", blocks$error[-gamlam])
        iass <- rep(1, engine$numeqt) #1 for datafile or general source for asserr
        assfix <- grep("!", asserr) #check for general only (override)
        if (length(assfix) > 0) {
          iass[assfix] <- 2
          asserr <- gsub("!", "", asserr)
        }
        comberr <- paste(ierrmod, asserr, iass, iqval, sep = "\n", collapse = "\n")

      }

    } else {
      #gamma/lambda was omitted from file
      return(list(status = -1, msg = "Please specify a gamma or lambda error model in your\nmodel file error block."))
    }
  }

  #build the fortran model file

  #function to add blank lines
  blank <- function(n) { x <- rep("", n); return(x) }
  #function to add temporary "@" to block line if spacing to be preserved
  prespace <- function(x) {
    formatStart <- grep("\\[format\\]", x)
    formatEnd <- grep("\\[/format\\]", x)
    if (length(formatStart) != length(formatEnd)) return(list(status = -1, msg = "Ensure that there are matching [format] and [/format] pairs."))
    if (length(formatStart) > 0) {
      for (i in 1:length(formatStart)) {
        x[formatStart[i]:formatEnd[i]] <- paste("@", x[formatStart[i]:formatEnd[i]], sep = "")
      }
    }
    #now erase leading space from lines that don't have "@"
    formatLines <- grep("@", x)
    if (length(formatLines > 0)) {
      x[-formatLines] <- sub("^ +", "", x[-formatLines])
    }
    #finally, erase the format statements
    if (length(formatStart) > 0) {
      x <- x[-c(formatStart, formatEnd)]

    }
    return(x)
  }


  space <- function(n, x) {
    if (length(grep("^@", x)) == 0) {
      #not a preserve format line
      y <- paste(paste(rep(" ", n), collapse = ""), x, collapse = "")
    } else {
      y <- sub("^@", "", x)
    }
    return(y)
  }

  blocks <- lapply(blocks, prespace)

  fmod <- list(header = NA, diffeq = NA, output = NA, symbol = NA, getfa = NA, getix = NA, gettlag = NA, anal3 = NA)
  fmod$header <- c("C  TSTMULTN.FOR                          NOV, 2014", blank(2))
  fmod$diffeq <- c(space(5, "SUBROUTINE DIFFEQ(NDIM,T,X,XP,RPAR,IPAR)"),
                     space(5, "IMPLICIT REAL*8(A-H,O-Z)"),
                     space(5, vardec),
                     space(5, "COMMON /PARAMD/ P"),
                     space(5, "COMMON /INPUT/ R,B"),
                     space(5, "COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG"),
                     space(5, "COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD"),
                     space(5, "DIMENSION X(NDIM),XP(NDIM),P(32),R(37),B(20),CV(26),RATEIV(7)"),
                     "!$omp Threadprivate(/PARAMD/,/INPUT/)  ",
                     blank(1),
                     unlist(lapply(diffstate, function(x) space(5, x))),
                     blank(1),
                     space(8, "DO I = 1,NDRUG"),
                     space(10, "RATEIV(I) = R(2*I - 1)"),
                     space(8, "END DO"),
                     blank(1),
                     space(8, "DO I = 1, NADD"),
                     space(10, "CV(I) = R(2*NDRUG + I)"),
                     space(8, "END DO"),
                     blank(1),
                     unlist(lapply(pvardef, function(x) space(8, x))),
                     unlist(lapply(covardef, function(x) space(8, x))),
                     unlist(lapply(svardef, function(x) space(8, x))),
                     blank(1),
                     unlist(lapply(blocks$diffeq, function(x) space(8, x))),
                     blank(1),
                     space(5, "RETURN"),
                     space(5, "END"),
                     blank(3)
  )
  fmod$output <- c(space(5, "SUBROUTINE OUTPUT(T,Y)"),
                     space(5, "IMPLICIT REAL*8(A-H,O-Z)"),
                     space(5, vardec),
                     space(5, "COMMON /PARAMD/ P"),
                     space(5, "COMMON /STATE/ X"),
                     space(5, "COMMON /INPUT/ R,B"),
                     space(5, "COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG"),
                     space(5, "COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD"),
                     space(5, "PARAMETER(MAXNUMEQ=7)"),
                     space(5, "DIMENSION X(20),P(32),Y(MAXNUMEQ),R(37),B(20),CV(26)"),
                     "!$omp Threadprivate(/PARAMD/,/INPUT/,/STATE/) ",
                     blank(2),
                     space(8, "DO I = 1, NADD"),
                     space(10, "CV(I) = R(2*NDRUG + I)"),
                     space(8, "END DO"),
                     blank(1),
                     unlist(lapply(pvardef, function(x) space(8, x))),
                     unlist(lapply(covardef, function(x) space(8, x))),
                     unlist(lapply(svardef, function(x) space(8, x))),
                     blank(1),
                     unlist(lapply(blocks$output, function(x) space(8, x))),
                     blank(1),
                     space(5, "RETURN"),
                     space(5, "END"),
                     blank(3)
  )
  fmod$symbol <- c(space(5, "SUBROUTINE SYMBOL"),
                     space(5, "IMPLICIT REAL*8(A-H,O-Z)"),
                     space(5, vardec),
                     space(5, "CHARACTER PSYM(32)*11"),
                     space(5, "COMMON /CNST/ N,ND,NI,NUP,NUIC,NP"),
                     space(5, "COMMON/BOLUSCOMP/NBCOMP"),
                     space(5, "DIMENSION NBCOMP(7)"),
                     blank(2),
                     space(8, "DO I = 1,7"),
                     space(10, "NBCOMP(I) = I"),
                     space(8, "END DO"),
                     blank(1),
                     unlist(lapply(blocks$bolus, function(x) space(8, x))),
                     space(6, paste("N=", N, sep = "")),
                     space(6, paste("NP=", npvar, sep = "")),
                     unlist(lapply(psym, function(x) space(6, x))),
                     blank(1),
                     space(5, "RETURN"),
                     space(5, "END"),
                     blank(3)
  )
  fmod$getfa <- c(space(5, "SUBROUTINE GETFA(FA)"),
                    space(5, "IMPLICIT REAL*8(A-H,O-Z)"),
                    space(5, vardec),
                    space(5, "COMMON /PARAMD/ P"),
                    space(5, "COMMON /INPUT/ R,B"),
                    space(5, "COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG"),
                    space(5, "COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD"),
                    space(5, "COMMON /STATE/ X"),
                    space(5, "DIMENSION P(32),R(37),B(20),CV(26),FA(7),X(20)"),
                    "!$omp Threadprivate(/PARAMD/,/INPUT/)  ",
                    blank(2),
                    space(8, "DO I = 1, NADD"),
                    space(10, "CV(I) = R(2*NDRUG + I)"),
                    space(8, "END DO"),
                    blank(1),
                    space(8, "DO I = 1,NDRUG"),
                    space(10, "FA(I) = 1.D0"),
                    space(8, "END DO"),
                    blank(1),
                    unlist(lapply(pvardef, function(x) space(8, x))),
                    unlist(lapply(covardef, function(x) space(8, x))),
                    unlist(lapply(svardef, function(x) space(8, x))),
                    blank(1),
                    unlist(lapply(blocks$f, function(x) space(8, x))),
                    blank(1),
                    space(5, "RETURN"),
                    space(5, "END"),
                    blank(3)
  )
  fmod$getix <- c(space(5, "SUBROUTINE GETIX(N,X)"),
                    space(5, "IMPLICIT REAL*8(A-H,O-Z)"),
                    space(5, vardec),
                    space(5, "COMMON /PARAMD/ P"),
                    space(5, "COMMON /INPUT/ R,B"),
                    space(5, "COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG"),
                    space(5, "COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD"),
                    space(5, "DIMENSION P(32),R(37),B(20),CV(26),X(20)"),
                    "!$omp Threadprivate(/PARAMD/,/INPUT/)  ",
                    blank(2),
                    space(8, "DO I = 1, NADD"),
                    space(10, "CV(I) = R(2*NDRUG + I)"),
                    space(8, "END DO"),
                    blank(1),
                    space(8, "IF(N .GT. 0) THEN"),
                    space(10, "DO I = 1,N"),
                    space(12, "X(I) = 0.D0 "),
                    space(10, "END DO"),
                    space(8, "ENDIF"),
                    blank(1),
                    space(8, "IF(N .EQ. -1) THEN"),
                    space(10, "DO I = 1,3"),
                    space(12, "X(I) = 0.D0 "),
                    space(10, "END DO"),
                    space(8, "ENDIF"),
                    blank(1),
                    unlist(lapply(pvardef, function(x) space(8, x))),
                    unlist(lapply(covardef, function(x) space(8, x))),
                    unlist(lapply(svardef, function(x) space(8, x))),
                    blank(1),
                    unlist(lapply(blocks$ini, function(x) space(8, x))),
                    blank(1),
                    space(5, "RETURN"),
                    space(5, "END"),
                    blank(3)
  )
  fmod$gettlag <- c(space(5, "SUBROUTINE GETTLAG(TLAG)"),
                     space(5, "IMPLICIT REAL*8(A-H,O-Z)"),
                     space(5, vardec),
                     space(5, "COMMON /PARAMD/ P"),
                     space(5, "COMMON /INPUT/ R,B"),
                     space(5, "COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG"),
                     space(5, "COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD"),
                     space(5, "COMMON /STATE/ X"),
                     space(5, "DIMENSION P(32),R(37),B(20),CV(26),TLAG(7),X(20)"),
                     "!$omp Threadprivate(/PARAMD/,/INPUT/)  ",
                     blank(2),
                     space(8, "DO I = 1, NADD"),
                     space(10, "CV(I) = R(2*NDRUG + I)"),
                     space(8, "END DO"),
                     blank(1),
                     space(8, "DO I = 1,NDRUG"),
                     space(10, "TLAG(I) = 0.D0"),
                     space(8, "END DO"),
                     blank(1),
                     unlist(lapply(pvardef, function(x) space(8, x))),
                     unlist(lapply(covardef, function(x) space(8, x))),
                     unlist(lapply(svardef, function(x) space(8, x))),
                     blank(1),
                     unlist(lapply(blocks$lag, function(x) space(8, x))),
                     blank(1),
                     space(5, "RETURN"),
                     space(5, "END"),
                     blank(3)
  )
  fmod$anal3 <- c(space(5, "SUBROUTINE ANAL3(X,TIN,TOUT)"),
                    space(5, "IMPLICIT REAL*8(A-H,O-Z)"),
                    space(5, vardec),
                    c("", space(5, paste("REAL*8 ", paste(missVars, collapse = ","), sep = "")))[1 + as.numeric(N > 0 & !is.na(missVars[1]))],
                    space(5, "COMMON /PARAMD/ P"),
                    space(5, "COMMON /INPUT/ R,B"),
                    space(5, "COMMON /RATESV/ KE,KA,KCP,KPC,V"),
                    space(5, "COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD"),
                    space(5, "DIMENSION X(20),P(32),R(37),B(20),CV(26)"),
                    "!$omp Threadprivate(/PARAMD/,/INPUT/,/RATESV/)   ",
                    blank(2),
                    space(8, "DO I = 1, NADD"),
                    space(10, "CV(I) = R(2*NDRUG + I)"),
                    space(8, "END DO"),
                    blank(1),
                    unlist(lapply(pvardef, function(x) space(8, x))),
                    unlist(lapply(covardef, function(x) space(8, x))),
                    unlist(lapply(svardef, function(x) space(8, x))),
                    blank(1),
                    space(8, "T=TOUT-TIN"),
                    space(8, "IF(KCP.EQ.0.0D0.AND.KPC.EQ.0.0D0) THEN"),
                    space(10, "IF(KA.EQ.0.0D0) ICASE=1"),
                    space(10, "IF(KA.NE.0.0D0) ICASE=2"),
                    space(8, "ELSE"),
                    space(10, "IF(KA.EQ.0.0D0) ICASE=3"),
                    space(10, "IF(KA.NE.0.0D0) ICASE=4"),
                    space(8, "ENDIF"),
                    space(8, "GO TO (100,200,300,400), ICASE"),
                    "C  CASE 1 SOLUTION - 1 COMP. NO 1ST ORDER INPUT",
                    "100     CALL CASE1(X(1),T)",
                    space(8, "TIN=TOUT"),
                    space(8, "RETURN"),
                    "C  CASE 2 SOLUTION - 1 COMP. + 1ST ORDER INPUT",
                    "200     CALL CASE2(X(1),X(2),T)",
                    space(8, "TIN=TOUT"),
                    space(8, "RETURN"),
                    "C  CASE 3 SOLUTION - 2 COMPARTMENT NO 1ST ORDER INPUT",
                    "300     CALL CASE3(X(1),X(2),T)",
                    space(8, "TIN=TOUT"),
                    space(8, "RETURN"),
                    "C CASE 4 SOLUTION - 2 COMP. + 1ST ORDER INPUT",
                    "400     CALL CASE4(X,T)",
                    space(8, "TIN=TOUT"),
                    space(8, "RETURN"),
                    space(8, "END"),
                    blank(1),
                    space(8, "SUBROUTINE CASE1(X2,T)"),
                    space(8, "IMPLICIT REAL*8(A-H,O-Z)"),
                    space(8, "REAL*8 KE,KA,KCP,KPC"),
                    space(8, "DIMENSION R(37),B(20)"),
                    space(8, "COMMON /RATESV/ KE,KA,KCP,KPC,V"),
                    space(8, "COMMON /INPUT/ R,B"),
                    "!$omp Threadprivate(/RATESV/,/INPUT/)  ",
                    space(8, "IF(KE.NE.0.0D0) GO TO 10"),
                    space(8, "X2=T*R(1)+X2"),
                    space(8, "RETURN"),
                    "10      EKET=DEXP(-KE*T)",
                    space(8, "X2=R(1)*(1.0D0-EKET)/KE+X2*EKET"),
                    space(8, "RETURN"),
                    space(8, "END"),
                    blank(1),
                    space(8, "SUBROUTINE CASE2(X1,X2,T)"),
                    space(8, "IMPLICIT REAL*8(A-H,O-Z)"),
                    space(8, "REAL*8 KE,KA,KCP,KPC"),
                    space(8, "COMMON /RATESV/ KE,KA,KCP,KPC,V"),
                    space(8, "COMMON /INPUT/ R,B"),
                    space(8, "DIMENSION R(37),B(20)"),
                    "!$omp Threadprivate(/RATESV/,/INPUT/)  ",
                    space(8, "IF(KA.NE.KE) GO TO 30"),
                    space(8, "EKT=DEXP(-KE*T)"),
                    space(8, "X2=(X2-R(1)/KE)*EKT+R(1)/KE+KE*X1*T*EKT"),
                    space(8, "X1=X1*DEXP(-KA*T)"),
                    space(8, "RETURN"),
                    "30      IF(KE.NE.0.0D0) GO TO 50",
                    space(8, "EKAT=DEXP(-KA*T)"),
                    space(8, "X2=X2+T*R(1)+X1*(1.0D0-EKAT)"),
                    space(8, "X1=X1*EKAT"),
                    space(8, "RETURN"),
                    "50      EKET=DEXP(-KE*T)",
                    space(8, "EKAT=DEXP(-KA*T)"),
                    space(8, "X2=X2*EKET+R(1)*(1.0D0-EKET)/KE+"),
                    space(4, "X    KA*X1*(EKET-EKAT)/(KA-KE)"),
                    space(8, "X1=X1*EKAT"),
                    space(8, "RETURN"),
                    space(8, "END"),
                    blank(1),
                    space(8, "SUBROUTINE CASE3(X2,X3,T)"),
                    space(8, "IMPLICIT REAL*8(A-H,O-Z)"),
                    space(8, "REAL*8 KE,KA,KCP,KPC,L1,L2"),
                    space(8, "COMMON /RATESV/ KE,KA,KCP,KPC,V"),
                    space(8, "COMMON /INPUT/ R,B"),
                    space(8, "DIMENSION EA(2,2),R(37),B(20)"),
                    "!$omp Threadprivate(/RATESV/,/INPUT/)  ",
                    space(8, "T1=KE+KCP+KPC"),
                    space(8, "T2=DSQRT(T1*T1-4.0D0*KE*KPC)"),
                    space(8, "L1=0.5D0*(T1+T2)"),
                    space(8, "L2=0.5D0*(T1-T2)"),
                    space(8, "IF(L2.NE.0.0D0) GO TO 200"),
                    space(8, "EL1T=DEXP(-L1*T)"),
                    space(8, "OEL1T=1.0D0-EL1T"),
                    space(8, "EA(1,1)=(L1-KPC)*EL1T+KPC"),
                    space(8, "EA(1,2)=KPC*OEL1T"),
                    space(8, "EA(2,1)=KCP*OEL1T"),
                    space(8, "EA(2,2)=(L1-KE-KCP)*EL1T+KE+KCP"),
                    space(8, "P1=R(1)*((1.0D0-KPC/L1)*OEL1T+KPC*T)"),
                    space(8, "P2=R(1)*((-KCP/L1)*OEL1T+KCP*T)"),
                    space(8, "C1=EA(1,1)*X2+EA(1,2)*X3+P1"),
                    space(8, "C2=EA(2,1)*X2+EA(2,2)*X3+P2"),
                    space(8, "X2=C1/L1"),
                    space(8, "X3=C2/L1"),
                    space(8, "RETURN"),
                    "200     CONTINUE",
                    space(8, "EL1T=DEXP(-L1*T) "),
                    space(8, "EL2T=DEXP(-L2*T)"),
                    space(8, "OEL1T=1.0D0-EL1T"),
                    space(8, "OEL2T=1.0D0-EL2T"),
                    space(8, "DEL2L1=EL2T-EL1T"),
                    space(8, "EA(1,1)=(L1-KPC)*EL1T+(KPC-L2)*EL2T"),
                    space(8, "EA(1,2)=KPC*DEL2L1"),
                    space(8, "EA(2,1)=KCP*DEL2L1"),
                    space(8, "EA(2,2)=(L1-KE-KCP)*EL1T+(KE+KCP-L2)*EL2T"),
                    space(8, "P1=R(1)*((1.0D0-KPC/L1)*OEL1T+(KPC/L2-1.0D0)*OEL2T)"),
                    space(8, "P2=R(1)*((-KCP/L1)*OEL1T+(KCP/L2)*OEL2T)"),
                    space(8, "D=L1-L2"),
                    space(8, "C1=EA(1,1)*X2+EA(1,2)*X3+P1"),
                    space(8, "C2=EA(2,1)*X2+EA(2,2)*X3+P2"),
                    space(8, "X2=C1/D"),
                    space(8, "X3=C2/D"),
                    space(8, "RETURN"),
                    space(8, "END"),
                    blank(1),
                    space(8, "SUBROUTINE CASE4(X,T)"),
                    space(8, "IMPLICIT REAL*8(A-H,O-Z)"),
                    space(8, "REAL*8 KE,KA,KCP,KPC,L1,L2"),
                    space(8, "COMMON /RATESV/ KE,KA,KCP,KPC,V"),
                    space(8, "COMMON /INPUT/ R,B"),
                    space(8, "DIMENSION EA(2,2),R(37),B(20),X(20)"),
                    "!$omp Threadprivate(/RATESV/,/INPUT/)  ",
                    space(8, "T1=KE+KCP+KPC"),
                    space(8, "T2=DSQRT(T1*T1-4.0D0*KE*KPC)"),
                    space(8, "L1=0.5D0*(T1+T2)"),
                    space(8, "L2=0.5D0*(T1-T2)"),
                    space(8, "IF(L2.NE.0.0D0) GO TO 200"),
                    space(8, "EL1T=DEXP(-L1*T)"),
                    space(8, "OEL1T=1.0D0-EL1T"),
                    space(8, "EA(1,1)=(L1-KPC)*EL1T+KPC"),
                    space(8, "EA(1,2)=KPC*OEL1T"),
                    space(8, "EA(2,1)=KCP*OEL1T"),
                    space(8, "EA(2,2)=(L1-KE-KCP)*EL1T+KE+KCP"),
                    space(8, "P1A=R(1)*((1.0D0-KPC/L1)*OEL1T+KPC*T)"),
                    space(8, "P2A=R(1)*((-KCP/L1)*OEL1T+KCP*T)"),
                    space(8, "EKAT=DEXP(-KA*T)"),
                    space(8, "RL1=(EL1T-EKAT)/(KA-L1)"),
                    space(8, "RKA=(1.0D0-EKAT)/KA"),
                    space(8, "P1B=KA*X(1)*((L1-KPC)*RL1+KPC*RKA)"),
                    space(8, "P2B=KA*X(1)*(-KCP*RL1+KCP*RKA)"),
                    space(8, "C1=EA(1,1)*X(2)+EA(1,2)*X(3)+P1A+P1B"),
                    space(8, "C2=EA(2,1)*X(2)+EA(2,2)*X(3)+P2A+P2B"),
                    space(8, "X(1)=X(1)*EKAT"),
                    space(8, "X(2)=C1/L1"),
                    space(8, "X(3)=C2/L1"),
                    space(8, "RETURN"),
                    "200     CONTINUE",
                    space(8, "EL1T=DEXP(-L1*T)"),
                    space(8, "EL2T=DEXP(-L2*T)"),
                    space(8, "EKAT=DEXP(-KA*T)"),
                    space(8, "OEL1T=1.0D0-EL1T"),
                    space(8, "OEL2T=1.0D0-EL2T"),
                    space(8, "DEL2L1=EL2T-EL1T"),
                    space(8, "EA(1,1)=(L1-KPC)*EL1T+(KPC-L2)*EL2T"),
                    space(8, "EA(1,2)=KPC*DEL2L1"),
                    space(8, "EA(2,1)=KCP*DEL2L1"),
                    space(8, "EA(2,2)=(L1-KE-KCP)*EL1T+(KE+KCP-L2)*EL2T"),
                    space(8, "P1A=R(1)*((1.0D0-KPC/L1)*OEL1T+(KPC/L2-1.0D0)*OEL2T)"),
                    space(8, "P2A=R(1)*((-KCP/L1)*OEL1T+(KCP/L2)*OEL2T)"),
                    space(8, "IF(KA.NE.L1) GO TO 240"),
                    space(8, "RL=DEL2L1/(L1-L2)"),
                    space(8, "P1B=L1*X(1)*(T*(L1-KPC)*EL1T+(KPC-L2)*RL)"),
                    space(8, "P2B=L1*X(1)*(-T*KCP*EL1T+KCP*RL)"),
                    space(8, "GO TO 300"),
                    "240     IF(KA.NE.L2) GO TO 280",
                    space(8, "RL=DEL2L1/(L1-L2)"),
                    space(8, "P1B=L2*X(1)*((L1-KPC)*RL+T*(KPC-L2)*EL2T)"),
                    space(8, "P2B=L2*X(1)*(-KCP*RL+T*KCP*EL2T)"),
                    space(8, "GO TO 300 "),
                    "280     RL1KA=(EL1T-EKAT)/(KA-L1)",
                    space(8, "RL2KA=(EL2T-EKAT)/(KA-L2)"),
                    space(8, "P1B=KA*X(1)*((L1-KPC)*RL1KA+(KPC-L2)*RL2KA)"),
                    space(8, "P2B=KA*X(1)*(-KCP*RL1KA+KCP*RL2KA)"),
                    "300     D=L1-L2",
                    space(8, "C1=EA(1,1)*X(2)+EA(1,2)*X(3)+P1A+P1B"),
                    space(8, "C2=EA(2,1)*X(2)+EA(2,2)*X(3)+P2A+P2B"),
                    space(8, "X(1)=X(1)*EKAT"),
                    space(8, "X(2)=C1/D"),
                    space(8, "X(3)=C2/D"),
                    space(8, "RETURN "),
                    space(8, "END")
  )

  fmod$extra <- c(blank(2), unlist(lapply(blocks$extra, function(x) space(6, x)))
  )

  modelFor <- paste(unlist(strsplit(model, "\\."))[1], "for", sep = ".")
  writeLines(unlist(fmod), modelFor)
  #check model file for errors
  OS <- getOS()
  compiler <- PMFortranConfig()
  if (is.null(compiler)) return(list(status = -1, msg = "Execute PMbuild or PMFortranConfig(reconfig=T) to choose your fortran compiler.\n"))

  #build syntax check statement and check model if possible
  fortran <- strsplit(compiler, " ")[[1]][1]
  syntaxcheck <- NA
  if (length(grep("gfortran", fortran) > 0)) { syntaxcheck <- paste(fortran, "-fsyntax-only", modelFor) }
  #fortran syntax different for Windows and UNIX
  if (length(grep("ifort", fortran) > 0)) {
    if (OS == 1 | OS == 3) {
      syntaxcheck <- paste(fortran, "-fsyntax-only", modelFor)
    } else {
      syntaxcheck <- paste(fortran, "/syntax-only", modelFor)
    }
  }
  if (length(grep("g95", fortran) > 0)) { syntaxcheck <- paste(fortran, "-fsyntax-only", modelFor) }

  if (!is.na(syntaxcheck)) {
    if (OS == 1 | OS == 3) {
      modelErr <- system(syntaxcheck, intern = T)
    } else { modelErr <- shell(syntaxcheck, intern = T) }
    if (length(attr(modelErr, "status")) > 0) return(list(status = -1, msg = "\nYou have Fortran syntax errors in your model statments, as detailed above.\n"))
  } else {
    cat("\nYour fortran compiler does not support model syntax checking.\n")
  }
  #end of model file creation

  #start instruction file creation if not SIM  
  if (engine$alg != "SIM" & write) {
    instr <- vector("character")
    if (engine$alg == "NP") { instr[getNext(instr)] <- "REM_BAK OCT_15" } else { instr[getNext(instr)] <- "REM_FRN JUL_13" }
    if (length(engine$salt > 1)) engine$salt <- paste(engine$salt, collapse = "     ")
    instr[getNext(instr)] <- " IVERIFY: 1 --> YES; 0 --> NO"
    instr[getNext(instr)] <- 0
    instr[getNext(instr)] <- " FORTRAN MODEL FILE"
    instr[getNext(instr)] <- modelFor
    instr[getNext(instr)] <- " NDIM"
    instr[getNext(instr)] <- N
    instr[getNext(instr)] <- " NP"
    instr[getNext(instr)] <- npvar
    instr[getNext(instr)] <- " IRAN INDICES"
    instr[getNext(instr)] <- paste(ptype, collapse = "    ")
    instr[getNext(instr)] <- " NVAR"
    instr[getNext(instr)] <- nvar
    instr[getNext(instr)] <- " PAR(I),I=1,NVAR"
    instr[getNext(instr)] <- paste(blocks$primVar[which(abs(ptype) == 1)], collapse = "\n")
    instr[getNext(instr)] <- " AB ARRAY"
    instr[getNext(instr)] <- ab
    instr[getNext(instr)] <- " NOFIX"
    instr[getNext(instr)] <- nofix
    instr[getNext(instr)] <- " PARFIX(I),I=1,NOFIX, IF NOFIX > 0"
    if (any(ptype == 0)) { instr[getNext(instr)] <- paste(blocks$primVar[which(ptype == 0)], collapse = "\n") }
    instr[getNext(instr)] <- " VALFIX ARRAY IF NOFIX > 0"
    if (length(valfix) > 1) { instr[getNext(instr)] <- paste(valfix, collapse = "    ") } else { instr[getNext(instr)] <- valfix }
    if (engine$alg == "NP") {
      instr[getNext(instr)] <- "NRANFIX"
      instr[getNext(instr)] <- nranfix
      instr[getNext(instr)] <- " PARRANFIX(I),I=1,NRANFIX, IF NRANFIX > 0"
      if (any(ptype == 2)) { instr[getNext(instr)] <- paste(blocks$primVar[which(ptype == 2)], collapse = "\n") }
      instr[getNext(instr)] <- " RANFIXEST ARRAY IF NRANFIX > 0"
      if (length(valranfix) > 1) { instr[getNext(instr)] <- paste(valranfix, collapse = "    ") } else { instr[getNext(instr)] <- valranfix }
    }
    instr[getNext(instr)] <- " O.D.E. TOLERANCE"
    instr[getNext(instr)] <- 10 ** engine$ode
    instr[getNext(instr)] <- " IFORMT"
    if (!engine$wrkFlag) {
      #not using old working copy files
      instr[getNext(instr)] <- 1
      instr[getNext(instr)] <- " BLOCKPAT"
      instr[getNext(instr)] <- data
      instr[getNext(instr)] <- " NCOVA"
      instr[getNext(instr)] <- engine$ncov
      instr[getNext(instr)] <- " COVNAME(I),I=1,NCOVA, IF NCOVA > 0"
      instr[getNext(instr)] <- ifelse(engine$ncov > 0, paste(engine$covnames, collapse = "\n"), NA)
      instr[getNext(instr)] <- " ICOVTYPE ARRAY IF NCOVA > 0"
      instr[getNext(instr)] <- ifelse(engine$ncov > 0, paste(ctype, collapse = "    "), NA)

    } else {
      #using old working copy files
      instr[getNext(instr)] <- 2
      instr[getNext(instr)] <- " PREFIX"
      instr[getNext(instr)] <- "XQZPJ"
      instr[getNext(instr)] <- " EXT"
      instr[getNext(instr)] <- "ZMQ"
    }

    instr[getNext(instr)] <- " NSUBTOT"
    instr[getNext(instr)] <- engine$nsubtot
    instr[getNext(instr)] <- " NSUB"
    instr[getNext(instr)] <- engine$nsub
    instr[getNext(instr)] <- " ACTIVE PATIENT NUMBERS, FOLLOWED BY A LINE WITH 0"
    instr[getNext(instr)] <- paste(engine$activesub, collapse = "\n")


    if (engine$alg == "NP") {
      #create NPAG instruction file
      instr[getNext(instr)] <- " NUMEQT"
      instr[getNext(instr)] <- engine$numeqt
      instr[getNext(instr)] <- " NUMEQT LINES OF ASSAY COEFFICIENTS"
      instr[getNext(instr)] <- asserr
      instr[getNext(instr)] <- " IERRMOD"
      instr[getNext(instr)] <- ierrmod
      instr[getNext(instr)] <- " GAMLAM0"
      instr[getNext(instr)] <- gamlam0
      instr[getNext(instr)] <- " IASS(I),I=1,NUMEQT"
      instr[getNext(instr)] <- iass
      instr[getNext(instr)] <- " NDRUG"
      instr[getNext(instr)] <- engine$ndrug
      instr[getNext(instr)] <- " AF(I),I=1,NDRUG"
      instr[getNext(instr)] <- engine$salt
      instr[getNext(instr)] <- " INDPTS"
      instr[getNext(instr)] <- indpts
      instr[getNext(instr)] <- " MAXCYC"
      instr[getNext(instr)] <- engine$cycles
      instr[getNext(instr)] <- " JSTOP"
      instr[getNext(instr)] <- 3
      instr[getNext(instr)] <- " IF JSTOP .NE. 1, TOLC IS ON NEXT LINE"
      instr[getNext(instr)] <- engine$tol
      instr[getNext(instr)] <- " IDELTA"
      instr[getNext(instr)] <- engine$idelta
      instr[getNext(instr)] <- " XMIC"
      instr[getNext(instr)] <- engine$xmic
      instr[getNext(instr)] <- " ICENT, WHICH IS NOW IRRELEVANT"
      instr[getNext(instr)] <- switch(engine$icen, mean = 1, median = 2, mode = 3, 2)
      instr[getNext(instr)] <- " AUCINT"
      instr[getNext(instr)] <- engine$aucint
      instr[getNext(instr)] <- " INPRI"
      instr[getNext(instr)] <- engine$priorString[1]
      instr[getNext(instr)] <- " NAME OF APRIORI DENSITY FILE IF INPRI = 0"
      instr[getNext(instr)] <- engine$priorString[2]
    }
    if (engine$alg == "IT" | engine$alg == "ERR") {
      #create IT2B/ERR instruction file
      instr[getNext(instr)] <- " XSIG"
      instr[getNext(instr)] <- engine$xsig
      instr[getNext(instr)] <- " NUMEQT"
      instr[getNext(instr)] <- engine$numeqt
      instr[getNext(instr)] <- " NUMEQT SETS OF IGAMMA,Cs,IASS,IQVAL"
      instr[getNext(instr)] <- comberr
      instr[getNext(instr)] <- " NDRUG"
      instr[getNext(instr)] <- engine$ndrug
      instr[getNext(instr)] <- " AF(I),I=1,NDRUG"
      instr[getNext(instr)] <- engine$salt
      instr[getNext(instr)] <- " TOL"
      instr[getNext(instr)] <- engine$tol
      instr[getNext(instr)] <- " MAXIT"
      instr[getNext(instr)] <- engine$cycles
      instr[getNext(instr)] <- " XDEV"
      instr[getNext(instr)] <- engine$xdev

    }
    instr <- instr[!is.na(instr)]
    writeLines(instr, "instr.inx")
  }
  #end instruction file creation

  #write report
  if (!silent) {
    cat(paste("\nModel solver mode: ", switch(letters[N + 2], a = "Algebraic", b = "Exact", "ODE"), sep = ""))
    if (N > 0) { numcomp <- N; absorb <- F }
    if (N == 0) { numcomp <- 1; absorb <- F }
    if (N == -1) {
      if (identical(missVars, c("KA", "KCP", "KPC"))) { numcomp <- 1; absorb <- F }
      if (identical(missVars, c("KCP", "KPC"))) { numcomp <- 2; absorb <- T }
      if (identical(missVars, "KA")) { numcomp <- 2; absorb <- F }
      if (identical(missVars, NA)) { numcomp <- 3; absorb <- T }
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
              sep = ""))
    if (engine$alg != "SIM") cat(paste("\nNumber of cycles to run:", engine$cycles))
    cat("\n\n")

  }
  #end if silent
  return(list(status = 1, modelFor = modelFor, N = N, ptype = ptype, ctype = ctype, nvar = nvar, nofix = nofix, nranfix = nranfix, valfix = valfix, ab = ab.df, indpts = indpts, asserr = asserr, blocks = blocks))
}
#end makeModel function


###### END NICELY FROM CRASHED NPrun or ITrun

endNicely <- function(message, model = -99, data = -99) {
  files <- Sys.glob("*", T)
  if ("mQRZZZ.txt" %in% files) {
    file.remove(model)
    file.rename("mQRZZZ.txt", model)
    files <- Sys.glob("*", T)
  }
  cleanUp <- Sys.glob(c("fort.*", "XQZPJ*.ZMQ", "extnum", "npag*.*", "CHMAX*.*", "SAVEINST.TMP", "go", "np_prep*", "np_run*", "nplog.txt", "NPcontrol", "npscript*",
                        "it2b*.*", "itas*.*", "it_prep*", "it_run*", "itlog.txt", "ITcontrol", "itscript*", "instr.inx",
                        "assdriv.f", "err_prep*", "err_run*", "ERRcontrol", "errscript*", "errlog.txt"))

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
  return((sum(w * x ^ 2) * sum.w - sum(w * x) ^ 2) / (sum.w ^ 2 - sum(w ^ 2)))
}

# weighted t test
weighted.t.test <- function(x, w, mu, conf.level = 0.95, alternative = "two.sided", na.rm = TRUE) {

  if (!missing(conf.level) &
     (length(conf.level) != 1 || !is.finite(conf.level) ||
      conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  # see if x is PMop$pop or PMop$post object
  if (identical(inherits(x, c("PMop", "data.frame"), which = T), as.integer(c(1, 2)))) {
    w <- 1 / x$obsSD ** 2
    x <- x$d
    mu <- 0
  }

  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }

  # to achieve consistent behavior in loops, return NA-structure in case of complete missings
  if (sum(is.na(x)) == length(x)) return(list(estimate = NA, se = NA, conf.int = NA, statistic = NA, df = NA, p.value = NA))

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
  }
  else if (alternative == "greater") {
    pval <- pt(t.value, df, lower.tail = FALSE)
    cint <- c(x.w - se * qt(conf.level, df), Inf)
  }
  else {
    pval <- 2 * pt(-abs(t.value), df)
    alpha <- 1 - conf.level
    cint <- x.w + se * qt(1 - alpha / 2, df) * c(-1, 1)
  }

  names(t.value) <- "t"
  return(list(estimate = x.w, se = se, conf.int = cint, statistic = t.value, df = df, p.value = pval))
}

#get the next line when building a file like instructions
getNext <- function(build) {
  return(length(build) + 1)
}

#file name check
FileNameOK <- function(filename) {
  baseName <- basename(filename)
  #strip extension
  baseName2 <- strsplit(baseName, "\\.")[[1]][1]
  if (nchar(baseName2) > 8) { return(FALSE) } else { return(TRUE) }
}



# Get OS version ----------------------------------------------------------

getOS <- function() {
  OS <- switch(Sys.info()[1], Darwin = 1, Windows = 2, Linux = 3)
  return(OS)
}


# MMopt functions ---------------------------------------------------------

wmmopt1 <- function(Mu, time, pH, cassay, nsamp, nsubs, nout, Cbar) {
  # WMMOPT1.M
  #
  # This routine computes the MMOPT 1,2,3 and 4-sample optimal sample designs
  # taking into account an additional weighting matrix C
  #
  # Note: Created from mmopt1.m in order to incorporate control weighting
  #
  # Written by David S. Bayard, February 22,2015 and Alona Kryshchenko, and Michael Neely
  #
  # INPUTS
  # --------
  # Mu - (nt)x(nsubs), Simulated output responses for all models (no noise)
  #              @ nt=# time points
  #              @ nsubs=# subjects
  #              @ Matrix Structure: Time response down, model index across
  # time - (nt)x1, time axis 
  # pH - (ns)x1, Bayesian Prior probabilities (sum(pH)=1)
  # cassay - 4x1, coefficients in assay polynomial:
  #        1-sigma assay error = c0+c1*y+c2*y^2+c3*y^3
  # nsamp - desired # of samples in experiment design
  # Cbar    - (nsubs)x(nsubs), Matrix of elements [cbar_ij] derived from matrix where c_ij is cost incurred from
  #        mistaking i'th support point (truth) to be j'th support point
  #        (wrong classification). Intuitively, you are giving jth subject's 
  #         dose with response a_j to
  #         ith subject with response a_ij, so that the control cost is
  #              c_ij=w_ij*(a_ij-a_j)^2,  where w_ij can be an arbitrary
  #              additional weighting function of i and j
  #         Key property: c_ii=0 for 1=1,...,nsubs, i.e., there is no cost for 
  #           getting classification correct
  #         cbar_ij is the max(cbar_ij, t(cbar_ij)), or max(cbar_ij,cbar_ji)
  #                      
  # Note: All MMOPT designs are computed having less than or equal to nsamp
  # samples
  #
  # OUTPUTS
  # --------
  # optsamp - 4x4, optimal samples times by column
  #             @ Column i contains the optimal design for i samples
  #             @ "-1" indicates "not applicable"
  # brisk_cob   - 4x1, Bayes risk cost overbound
  #            brisk_cob(i) is the Bayes Risk cost overbound associated with 
  #            using the optimal design having i samples
  #                "-1" indicates "not applicable"
  # optindex - 4x4, indices of optimal sample times from time=(nt)x1

  # 

  # -------------------------------------
  # Initialize all entries with -1 
  optsamp <- matrix(-1, 4, 4);
  optindex <- matrix(-1, 4, 4);
  brisk <- matrix(-1, 4, 1);
  nopt1 <- -1;
  nopt2 <- matrix(-1, 2, 1);
  nopt3 <- matrix(-1, 3, 1);
  nopt4 <- matrix(-1, 4, 1);
  Perror1 <- -1
  Perror2 <- -1
  Perror3 <- -1
  Perror4 <- -1
  Perror1_min <- -1
  Perror2_min <- -1
  Perror3_min <- -1
  Perror4_min <- -1
  # -------------------------------
  # Extract needed quantities

  c0 <- cassay[1];
  # additive noise
  c1 <- cassay[2];
  c2 <- cassay[3];
  c3 <- cassay[4];


  # BEGIN MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
  # FULL SCRIPT VERSION OF MMOPT ALGORITHM
  # -----------------------------------
  # Compute Kall
  kallijn <- kall_ijn(Mu, c0, c1, c2, c3, nsubs, nout);
  Kall <- kallijn$Kall
  skall <- kallijn$skall

  # ------------------------------
  # SINGLE SAMPLE OPTIMIZATION
  if (nsamp == 1) {
    #111111111111111111111111111111111111111111111111111111111111111
    #111111111111111111111111111111111111111111111111111111111111111
    # 1-SAMPLE SEARCH: FULL COMBINATORIAL SEARCH
    # Perror1_stor=zeros(nout); # to save intermediary results
    #
    for (n1 in 1:nout) {
      nvec <- n1;
      Perror1 <- perrorc1(pH, Kall, nvec, Cbar);
      # keep running optimal
      if (n1 == 1) { Perror1_min <- Perror1; nopt1 <- n1; }
      if (Perror1 < Perror1_min) { Perror1_min <- Perror1; nopt1 <- n1; }
      # Store Perror
      # Perror1_stor(n1)=Perror1;
    }
    Perror2 <- Perror2_min
    Perror3 <- Perror3_min
    #111111111111111111111111111111111111111111111111111111111111111
  }
  # endif


  # ------------------------------
  # TWO SAMPLE OPTIMIZATION
  if (nsamp == 2) {
    #222222222222222222222222222222222222222222222222222222222222222
    #222222222222222222222222222222222222222222222222222222222222222
    #222222222222222222222222222222222222222222222222222222222222222
    # 2-SAMPLE SEARCH: FULL COMBINATORIAL SEARCH
    # Perror2_stor=zeros(nout,nout); # to save intermediary results
    #
    for (n1 in 1:nout) {
      for (n2 in n1:nout) {
        nvec <- c(n1, n2);
        Perror2 <- perrorc1(pH, Kall, nvec, Cbar);
        # keep running optimal
        if ((n1 == 1) && (n2 == 1)) { Perror2_min <- Perror2; nopt2 <- c(n1, n2); }
        if (Perror2 < Perror2_min) { Perror2_min <- Perror2; nopt2 <- c(n1, n2); }
        #          # Store Perror
        #          Perror2_stor(n1,n2)=Perror2;
      }
    }
    Perror1 <- Perror1_min
    Perror3 <- Perror3_min
    #222222222222222222222222222222222222222222222222222222222222222
  }
  # endif
  #
  # ------------------------------
  # THREE SAMPLE OPTIMIZATION
  if (nsamp == 3) {
    #333333333333333333333333333333333333333333333333333333333333333
    #333333333333333333333333333333333333333333333333333333333333333
    #333333333333333333333333333333333333333333333333333333333333333
    # 3-SAMPLE SEARCH: FULL COMBINATORIAL SEARCH
    # Perror3_stor=zeros(nout,nout,nout); # to save intermediary results
    nout10 <- floor(nout / 10);
    #
    cat("\nComputing 3-sample design\n")
    pb <- txtProgressBar(min = 0, max = nout, style = 3)
    for (n1 in 1:nout) {
      setTxtProgressBar(pb, n1)
      for (n2 in n1:nout) {
        for (n3 in n2:nout) {
          nvec <- c(n1, n2, n3);
          Perror3 <- perrorc1(pH, Kall, nvec, Cbar);
          # keep running optimal
          if ((n1 == 1) && (n2 == 1) && (n3 == 1)) { Perror3_min <- Perror3; nopt3 <- c(n1, n2, n3); }
          if (Perror3 < Perror3_min) { Perror3_min <- Perror3; nopt3 <- c(n1, n2, n3); }
          # Store Perror
          #             Perror3_stor(n1,n2,n3)=Perror3;
        }
      }
    }
    close(pb)
    Perror1 <- Perror1_min
    Perror2 <- Perror2_min
    #333333333333333333333333333333333333333333333333333333333333333
  }
  # endif
  #
  # ------------------------------
  # FOUR SAMPLE OPTIMIZATION
  if (nsamp == 4) {
    #444444444444444444444444444444444444444444444444444444444444444
    #444444444444444444444444444444444444444444444444444444444444444
    # 4-SAMPLE SEARCH: FULL COMBINATORIAL SEARCH
    # Careful: don't store anything or you will blow memory!!!!
    #Perror4_stor=zeros(nout,nout,nout,nout); # to save intermediary results
    cat("\nComputing 4-sample design\n")
    pb <- txtProgressBar(min = 0, max = nout, style = 3)
    for (n1 in 1:nout) {
      setTxtProgressBar(pb, n1)
      for (n2 in n1:nout) {
        for (n3 in n2:nout) {
          for (n4 in n3:nout) {
            nvec <- c(n1, n2, n3, n4);
            Perror4 <- perrorc1(pH, Kall, nvec, Cbar);
            # keep running optimal
            if ((n1 == 1) && (n2 == 1) && (n3 == 1) && (n4 == 1)) { Perror4_min <- Perror4; nopt4 <- c(n1, n2, n3, n4); }
            if (Perror4 < Perror4_min) { Perror4_min <- Perror4; nopt4 <- c(n1, n2, n3, n4); }
            # Store Perror
            #             Perror4_stor(n1,n2,n3,n4)=Perror4;
          }
        }
      }
    }
    close(pb)
    Perror1 <- Perror1_min
    Perror2 <- Perror2_min
    Perror3 <- Perror3_min
    #444444444444444444444444444444444444444444444444444444444444444
  }
  # endif
  # -----------------------------------
  # Compute Output Values
  if (nsamp == 1) { optsamp[1, 1] <- time[nopt1]; }
  if (nsamp == 2) { optsamp[1:2, 2] <- time[nopt2]; }
  if (nsamp == 3) { optsamp[1:3, 3] <- time[nopt3]; }
  if (nsamp == 4) { optsamp[1:4, 4] <- time[nopt4]; }
  # --
  optindex[1, 1] <- nopt1;
  optindex[1:2, 2] <- nopt2;
  optindex[1:3, 3] <- nopt3;
  optindex[1:4, 4] <- nopt4;
  # --
  brisk_cob <- c(Perror1, Perror2, Perror3, Perror4);
  return(list(optsamp = optsamp, brisk_cob = brisk_cob, optindex = optindex))
}

kall_ijn <- function(Mu, c0, c1, c2, c3, nsubs, nout) {
  # KALL_IJN.R
  #
  # Routine to Create Full K matrix
  # K=Kall(i,j,n) where,
  #
  # Kall(i,j,n) -(nsubs)x(nsubs)x(nt) 3D matrix, Risk coefficient as function of i,j and n
  # i = i'th patient
  # j = j'th patient
  # n = n'th time in horizon, n=1,...,nt
  # nsubs = # subjects
  # nout=nt = # times in time horizon

  # INPUTS
  # -------
  # Mu = (nout)X(nsubs), Mean of Assay for nout=# output values, nsubs=#subjects
  # n - time index to evaluate Kmat on
  # c0,c1,c2,c3 - assay error polynomial coefficients
  #
  # OUTPUTS
  # -------
  # Kall - (nsubs)x(nsubs)x(nt) 3D matrix
  # skall - (nout)x1, Frobenius norm over i,j of Kall(i,j,n) becomes function of 
  #                 n=1:nout (stacked as vector)
  #
  # Written by David S. Bayard, October 14, 2013
  #
  #
  # --------------

  # Make full K matrix
  Kall <- array(0, dim = c(nsubs, nsubs, nout));
  skall <- matrix(0, nout, 1);
  # to store norm as function of n=1:nout
  # fill Kall
  for (n in 1:nout) {
    Kn <- kmat_ijn(Mu, n, c0, c1, c2, c3, nsubs, nout);
    skall[n] <- norm(Kn, "F");
    # Frobenious norm
    Kall[,, n] <- Kn;
  }
  return(list("Kall" = Kall, "skall" = skall))
}
kmat_ijn <- function(Mu, n, c0, c1, c2, c3, nsubs, nout) {
  # KMAT_IJN.R
  #
  # This routine is used to construct 3D matrix K(i,j,n) by routine
  # kall_ijn.m which calls current routine once for each n=1,...,nt
  # --------------------------------------
  # Routine to compute Kn=K(i,j,n)|_n, 
  # i.e., evaluation of  K(i,j,n) on n'th time in horizon
  # where 
  # K(i,j,n) -(nsubs)x(nsubs) matrix, Lars risk coefficient as function of i,j and n
  # i = i'th patient
  # j = j'th patient
  # n = n'th time in horizon, n=1,...,nt
  # nsubs = # subjects
  # nt = # times in time horizon
  # -------------
  #
  # INPUTS
  # -------
  # Mu = (nout)x(ns) (e.g., 96x10), true responses for nout=# output values, ns=#subjects
  # n - time index to evaluate Kn on
  # c0,c1,c2,c3 - assay error polynomial coefficients
  #
  # OUTPUTS
  # -------
  # Kn -(nsubs)x(nsubs), Kn=K(i,j,n)|_n, i.e., evaluation of K(i,j,n) on n'th time in horizon
  #
  # Written by David S. Bayard, October 14, 2013  
  # -----------------------------------
  #
  # Make Sig2 by evaluating noise on Mu using assay polynomial
  yout_n <- Mu[n,] # yout_n= (nsubs)x1
  Sig2 <- (c0 * matrix(1, nsubs, 1) + c1 * yout_n + c2 * yout_n ^ 2 + c3 * yout_n ^ 3) ^ 2;
  # CCCCCCCCCCCCCCCCCCCCCCCCCCCC
  # CHECKPOINT
  # yout_n - (nsubs)x1, contains means of each subject (at n'th assay time)
  # Sig2 = (nsubs)x1, contains covariance Sig2 of noise for each subject
  # CCCCCCCCCCCCCCCCCCCCCCCCCCCC
  # 
  # --------------------
  #@@ SLOW APPROACH
  # Make Kijn
  #   if (1==0){
  #     # Create storage
  #     Kijn<-matrix(0,nsubs,nsubs);
  #     # Stuff Kijn for each i,j
  #     for (i in 1:nsubs){
  #       for (j in 1:nsubs){
  #         Sig2plus<-Sig2[i]+Sig2[j];
  #         Sig2prod<-Sig2[i]*Sig2[j];
  #         Kijn[i,j]<- (1/4)*( yout_n[j]-yout_n[i] )^2/Sig2plus+ (1/2)*log(Sig2plus/2)- (1/4)*log(Sig2prod);
  #       }
  #     }
  #     # --
  #   }
  # -------------
  #@@ FAST VECTORIZED REPLACEMENT (slightly slower)
  # try vectorizing!!
  Sig2plus <- Sig2 %*% matrix(1, 1, nsubs) + matrix(1, nsubs, 1) %*% t(Sig2);
  Sig2prod <- (Sig2 %*% matrix(1, 1, nsubs)) * (matrix(1, nsubs, 1) %*% t(Sig2));
  Mun <- yout_n;
  # column vector
  Mun_minus <- Mun %*% matrix(1, 1, nsubs) - matrix(1, nsubs, 1) %*% t(Mun);
  Kijn <- (1 / 4) * (Mun_minus ^ 2) / Sig2plus + (1 / 2) * log(.5 * Sig2plus) - (1 / 4) * log(Sig2prod);

  # ------------------------
  # Create output variable
  Kn <- Kijn;
  return(Kn)
}

perrorc1 <- function(pH, Kall, nvec, Cbar) {
  # PERRORC1.R
  #
  # Routine to compute Bayes Risk Overbound, evaluated on 
  # vector of candidate sample times specified in "nvec"
  #
  # INPUTS
  # ------
  # pH - (nsub)x1 prior on all hypotheses (i.e., sum(pH)=1))
  # Kall - (nsub)x(nsub)x(nout)
  # nvec - vector of candidate sample times to evaluate Perror on
  #            nvec = [n1
  #                    n2
  #                    :
  #                    nsamp]
  # Cbar - (nsub)x(nsub), matrix of control overbound error weights {cbar_ij}
  #    where cbar_ij=max(cij,cji)
  #
  # OUTPUTS
  # -------
  # Perror - MMopt upper bound on Bayes Risk, evaluated on the 
  #          candidate sample times specified in nvec
  #
  # Written by David S. Bayard, October 14, 2013
  # --------------------
  nsubs <- dim(Kall)[1]
  nxx <- dim(Kall)[2]
  nout <- dim(Kall)[3]
  nsamp <- length(nvec);
  pH5 <- sqrt(pH);
  #
  # Create Kallall
  # Example: For nsamp=4 sampling times [n1,n2,n3,n4] we have
  #         Kallall=Kall(:,:,n1)+Kall(:,:,n2)+Kall(:,:,n3)+Kall(:,:,n4);

  # Replace following statement with ONE statement below
  # ----------------
  #@@ LOOP APPROACH
  Kallsum <- matrix(0, nsubs, nsubs);
  for (n in 1:nsamp) {
    nn <- nvec[n];
    Kallsum <- Kallsum + Kall[,, nn];
  }
  # --
  # ----------------
  #@@  VECTORIZED APPROACH
  #   if (fast){
  #     # Replace with: (extracts nvec indices of Kall(:,:,nvec) and sums
  #     Kallsum<-apply(Kall[ , ,nvec],c(1,2), sum);
  #   }
  # ---------------------------------
  ExpKallsum <- exp(-Kallsum);
  ExpKallsum0 <- ExpKallsum - diag(diag(ExpKallsum));
  # remove diagonal
  # Evaluate Lar upper bound 
  # (note- Lars' formula has sum over the upper half of
  # the symmetric matrix ExpKallsum0, while I am summing over ENTIRE matrix, hence the
  # factor of 1/2 below:
  Perror <- .5 * t(pH5) %*% (ExpKallsum0 * Cbar) %*% pH5;
  return(Perror)
}
cbar_make1 <- function(C) {
  # CBAR_MAKE1.R
  #
  # Routine to make Cbar matrix from C matrix
  # used for MMopt control-relevant expt design
  #
  # INPUTS
  # ------
  # C    - (nsubs)x(nsubs), Matrix of elements (c_ij) where c_ij is cost incurred from
  #        mistaking i'th support point (truth) to be j'th support point
  #        (wrong classification). Specifically, you are giving the jth dose
  #        dose_j to the ith subject, giving response a_ij when the desired
  #        response is a_j. Hence, elements of C are given by the formula:
  #         c_ij=w_ij*(a_ij-a_j)^2
  #         Key property: c_ii=0 for 1=1,...,nsubs
  #
  # OUTPUTS
  # ------
  # Cbar - (nsubs)x(nsubs), Matrix of elements (cbar_ij} where 
  #               cbar_ij=max(c_ij,c_ji) 
  #       and c_ij is an element of the C matrix defined next
  # -------------------
  #
  nsubs <- dim(C)[1];
  Cbar <- apply(array(c(C, t(C)), dim = c(nsubs, nsubs, 2)), c(1, 2), max)
  return(Cbar)
}



# makePMmatrixBlock -------------------------------------------------------

makePMmatrixBlock <- function(mdata) {
  #make event blocks, delimited by evid=4
  mdata$block <- 1
  if (any(mdata$evid == 4)) {
    blocks <- tapply(mdata$time[mdata$evid == 1 | mdata$evid == 4], mdata$id[mdata$evid == 1 | mdata$evid == 4], function(x) sum(x == 0))
    blocks <- blocks[rank(unique(mdata$id))] #sort blocks back into id order in mdata
    blocks2 <- unlist(mapply(function(x) 1:x, blocks))
    time0 <- c(which(mdata$time == 0 & mdata$evid != 0), nrow(mdata))
    blocks3 <- rep(blocks2, times = diff(time0))
    mdata$block <- c(blocks3, tail(blocks3, 1))
  }
  return(mdata)
}


# getCov ------------------------------------------------------------------

#function to get covariate information from PMmatrix object
getCov <- function(mdata) {
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

#function to retrieve osname in osx,windows,or linux
getOSname <- function() {
  # https://askubuntu.com/questions/46627/how-can-i-make-a-script-that-opens-terminal-windows-and-executes-commands-in-the
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {
    ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


# check for installed packages --------------------------------------------

checkRequiredPackages <- function(pkg) {

  managePkgs <- function(thisPkg) {
    # if (length(grep(thisPkg, installed.packages()[, 1])) == 0) {
    #   install.packages(thisPkg, dependencies = T)
    # }
    if (requireNamespace(thisPkg, quietly = T)) {
      return("ok") #package is installed
    } else { #package is not installed
      install.packages(thisPkg, dependencies = T, quiet = T) #try to install
      if (requireNamespace(thisPkg, quietly = T)){ #check again
        return("ok") #now it is installed and ok
      } else {return(thisPkg) } #nope, still didn't install
    }
  }

  pkg %>%
    map_chr(managePkgs) %>%
    keep(~. != "ok") %>%
    map_chr(~if (length(.) > 0) {
      stop(paste("The following required packages did not successfully install: ", ., sep = "", collapse = ", "))
    })
  return(invisible())
}


# Time after dose ---------------------------------------------------------

#calculate time after dose 
calcTAD <- function(rawData) {
  for (i in 1:nrow(rawData)) {
    if (rawData$evid[i] != 0) {
      if(!is.na(rawData$addl[i]) && rawData$addl[i]>0){
        doseTime <- rawData$time[i] + rawData$addl[i] * rawData$ii[i]
      } else {doseTime <- rawData$time[i]}
      prevDose <- rawData$dose[i]
      rawData$tad[i] <- 0
    } else {rawData$tad[i] <- rawData$time[i] - doseTime}
  }
  return(rawData$tad)
}


# obsStatus -----------------------------------------------------------

#classify observations 
#data should be a vector
obsStatus <- function(data) {
  present <- which(data != -99)
  missing <- which(data == -99)
  return(list(present = present, missing = missing))
}

binaries.installed <- function() {
  # checkRequiredPackages("purrr")

  # #library(purrr)
  # exists <- function(name) {
  #   paste(system.file("", package = "Pmetrics"), "compiledFortran", sep = "/") %>%
  #   paste(name, sep = "/") %>%
  #   file.exists()
  # }
  # c("DOprep.exe", "mb2csv.exe", "pNPeng.o",
  #   "sDOeng.o", "sITeng.o", "sITerr.o", "sITprep.o",
  #   "sNPeng.o", "sNPprep.o", "sSIMeng.o") %>%
  # map(exists) %>% unlist() %>% all() %>% return()

  exists <- function(name) {
    file.exists(paste(system.file("", package = "Pmetrics"), "compiledFortran", name, sep = "/"))
  }
  installed = T
  for (binary in c("DOprep.exe", "mb2csv.exe", "pNPeng.o",
                   "sDOeng.o", "sITeng.o", "sITerr.o", "sITprep.o",
                   "sNPeng.o", "sNPprep.o", "sSIMeng.o")) {
    installed = installed && exists(binary)
  }
  return(installed)
}
