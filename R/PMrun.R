
.PMrun <- function(type, model, data, run,
                   include, exclude, ode, tol, salt, cycles,
                   indpts, icen, aucint,
                   idelta, prior, xdev, search,
                   auto, intern, quiet, overwrite, nocheck, parallel, batch,
                   alq, report) {
  currwd <- getwd() # set the current working directory to go back to it at the end
  if (missing(alq)) alq <- F
  if (missing(batch)) batch <- F
  if (Sys.getenv("env") == "docker") intern <- T

  # make new output directory
  if (is.null(run)) {
    olddir <- list.dirs(recursive = F)
    olddir <- olddir[grep("^\\./[[:digit:]]+", olddir)]
    olddir <- sub("^\\./", "", olddir)
    if (length(olddir) > 0) {
      newdir <- as.character(max(as.numeric(olddir)) + 1)
    } else {
      newdir <- "1"
    }
  } else {
    if (!is.numeric(run)) {
      endNicely("'run' must be numeric.\n")
    } else {
      newdir <- as.character(run)
    }
  }

  if (file.exists(newdir)) {
    if (overwrite) {
      unlink(newdir, recursive = T)
    } else {
      endNicely(paste("\n", newdir, " exists already.  Set overwrite=T to overwrite.\n"))
    }
  }
  dir.create(newdir)

  setwd(currwd)
  setwd(newdir) # move to the new directory and do each run there (compatible with batching)

  # check for files
  # copy model file if available
  if (is.numeric(model)) {
    modelfile <- suppressWarnings(tryCatch(scan(paste("../", model, "etc/instr.inx", sep = "/"), what = "character", quiet = T, skip = 4, n = 1), error = function(e) NULL))
    modelfile <- Sys.glob(paste("../", model, "/inputs/", strsplit(modelfile, "\\.")[[1]][1], "*", sep = ""))
    if (length(modelfile) > 0) {
      if (length(modelfile) > 1) {
        for (thisfile in modelfile) {
          firstline <- scan(thisfile, nlines = 1, what = "character", quiet = T)
          if (firstline[1] != "POPDATA") {
            modelfile <- thisfile
            break
          }
        }
      }
      file.copy(from = modelfile, to = getwd())
      model <- basename(modelfile)
    }
  } else {
    # model name was a text file
    # make sure model file name is <=8 characters
    if (!FileNameOK(model)) {
      endNicely(paste("Model file name must be 8 characters or fewer.\n"), model = -99, data)
    }

    while (!file.exists(paste("../", model, sep = ""))) {
      model <- readline(paste("The model file", shQuote(paste(getwd(), model)), "does not exist.\nEnter another filename or 'end' to quit: \n"))
      if (tolower(model) == "end") {
        endNicely(paste("No model file specified.\n"), model = -99, data)
        break
      }
    }
    file.copy(from = paste("../", model, sep = ""), to = getwd()) # copy found file to new folder
    file.remove(paste("../", model, sep = "")) # erase the found file in parent folder
  }


  # copy wrk files if available
  wrkFlag <- F
  if (is.numeric(data)) {
    wrkfiles <- Sys.glob(paste("..", data, "wrkcopy/*.ZMQ", sep = "/"))
    if (length(wrkfiles) > 0) {
      file.copy(from = wrkfiles, to = getwd())
      wrkFlag <- T
    } else {
      endNicely(paste("\nNo working copy files found in ", data, "/wrkcopy folder.\n", sep = ""), model, data = -99)
    }
    RFfile <- suppressWarnings(tryCatch(readLines(Sys.glob(paste("..", data, "outputs/??_RF0001.TXT", sep = "/"))), error = function(e) NULL))
    if (length(RFfile) > 0) {
      datafileName <- tail(RFfile, 1)
      file.copy(from = paste("..", data, "inputs", datafileName, sep = "/"), to = getwd())
      data <- datafileName
    } else {
      endNicely(paste("\nNo RF file found in ", data, "/outputs folder to extract matrix data filename.\n", sep = ""), model, data = -99)
    }
  } else if (is.character(data)) {
    # data name was a file
    # make sure data file name is <=8 characters
    if (!FileNameOK(data)) {
      endNicely(paste("Data file name must be 8 characters or fewer.\n"), model, data = -99)
    }

    # ok look for a data file
    while (!file.exists(paste("..", data, sep = "/"))) {
      data <- readline(paste("The data file", shQuote(paste(getwd(), data)), "does not exist.\nEnter another filename or 'end' to quit: \n"))
      if (tolower(data) == "end") {
        endNicely(paste("No data file specified.\n"), model, data = -99)
        break
      }
    }
    file.copy(from = paste("../", data, sep = ""), to = getwd()) # copy found file to new folder
    file.remove(paste("../", data, sep = "")) # erase the found file in parent folder
  } else if (is.data.frame(data)) {
    dataFile <- data
    data <- "gendata.csv"
    PMwriteMatrix(dataFile, data)
  } else {
    endNicely(sprintf("%s not recognized as a valid data type", typeof(data)))
  }



  # get information from datafile
  if (!is.data.frame(data)) {
    dataFile <- PMreadMatrix(data, quiet = T)
  }


  # check for errors in data if nocheck=F
  if (!nocheck) {
    err <- PMcheck(dataFile, quiet = T)
    if (attr(err, "error") == -1) {
      endNicely("\nThere are errors in your data file.  See errors.xlsx file in working directory.\n", model, data)
    }
  }


  ncov <- getCov(dataFile)$ncov
  covnames <- getCov(dataFile)$covnames
  numeqt <- max(dataFile$outeq, na.rm = T)
  id <- unique(dataFile$id)
  nsubtot <- length(id)

  if (is.null(include)) include <- id
  if (!is.null(exclude)) include <- id[!id %in% exclude]

  nsub <- length(include)
  activesub <- c(which(id %in% include), 0)
  ndrug <- max(dataFile$input, na.rm = T)
  if (is.null(salt)) {
    salt <- rep(1, ndrug)
  } else {
    if (length(salt) != length(ndrug)) endNicely("\nSalt fraction length must equal number of drugs.\n", model, data)
  }

  # AUC interval, index of gridpts, prior, and MIC for NPAG
  if (type == "NPAG") {
    # AUC interval
    if (is.null(aucint)) {
      maxTime <- max(dataFile$time, na.rm = T)
      if (maxTime > 24 * 48) {
        aucint <- ceiling(maxTime / 48)
      } else {
        aucint <- 24
      }
    }

    # gridpts
    if (is.null(indpts)) indpts <- -99

    # check if prior and if so, get name for instruction file
    if (is.null(prior)) {
      # prior not specified
      prior <- -99
      priorString <- 1
    } else {
      # prior specified, so choose how
      if (inherits(prior, "NPAG")) {
        priorString <- c(0, "prior.txt")
      }
      # prior is a file_name
      if (is.character(prior)) {
        priorDEN <- Sys.glob(paste0(currwd,"/",prior))[1]
        if (length(priorDEN) > 0) {
          file.copy(from = priorDEN, to = paste(getwd(), "/prior.txt", sep = ""))
          prior <- "prior.txt"
          priorString <- c(0, prior)
        }
      }
      # prior is the name of a file
      if (is.numeric(prior)) {
        # prior is a run number
        priorDEN <- Sys.glob(paste("..", prior, "outputs/DEN*", sep = "/"))[1]
        if (length(priorDEN) > 0) {
          file.copy(from = priorDEN, to = paste(getwd(), "/prior.txt", sep = ""))
          prior <- "prior.txt"
          priorString <- c(0, prior)
        }
      }
    }
    # MIC
    xmic <- 1
  }

  # set fraction of ab range for initial parameter SD for IT2B and ERR
  if (type == "IT2B" | type == "ERR") {
    xsig <- 0.5
  }


  # attempt to translate model and data files into separate fortran model  and instruction files

  if (type == "NPAG") {
    engine <- list(
      alg = "NP", nsubtot = nsubtot, nsub = nsub, activesub = activesub, ncov = ncov, covnames = covnames, ndrug = ndrug, tol = tol,
      salt = salt, numeqt = numeqt, cycles = cycles, icen = icen, indpts = indpts, aucint = aucint, idelta = idelta, xmic = xmic,
      ode = ode, limits = NA, priorString = priorString, wrkFlag = wrkFlag
    )
  }
  if (type == "IT2B") {
    engine <- list(
      alg = "IT", nsubtot = nsubtot, nsub = nsub, activesub = activesub, ncov = ncov, covnames = covnames, ndrug = ndrug,
      salt = salt, numeqt = numeqt, cycles = cycles, xsig = xsig, tol = tol, xdev = xdev, indpts = -99,
      ode = ode, limits = NA, wrkFlag = wrkFlag
    )
  }
  if (type == "ERR") {
    engine <- list(
      alg = "ERR", nsubtot = nsubtot, nsub = nsub, activesub = activesub, ncov = ncov, covnames = covnames, ndrug = ndrug,
      salt = salt, numeqt = numeqt, cycles = cycles, xsig = xsig, tol = tol, xdev = xdev, indpts = -99,
      ode = ode, limits = NA, wrkFlag = wrkFlag
    )
  }

  trans <- makeModel(model = model, data = data, engine = engine, write = T, quiet = quiet)

  if (trans$status == -1) endNicely(trans$msg, model, data) # error
  if (trans$status == 0) {
    useOldFortran <- T # old fortran file model
    auto <- F # disable automatic running
  }
  if (trans$status == 1) {
    # new model and instruction files made
    useOldFortran <- F
    modelFor <- trans$modelFor
    ptype <- trans$ptype
    if (!wrkFlag) {
      ctype <- trans$ctype
    } else {
      ctype <- 0
    }
    instr <- "instr.inx"
  }

  # if parallel not specified, choose serial for algebraic/exact or parallel for ODE
  if (is.na(parallel)) {
    parallel <- c(T, F)[1 + as.numeric(trans$N <= 0)]
  }

  # if parallel is true and in 32-bit, choose serial and warn
  if (parallel & getBits() == "32") {
    parallel <- F
    cat("\nNote: Parallel processing is not available for 32-bit systems.\n")
  }

  OS <- getOS() # 1 Mac, 2 Windows, 3 Linux

  fortSource <- paste(system.file("", package = "Pmetrics"), "compiledFortran", sep = "/")
  # TODO: change this
  if (!file.exists(fortSource)) {
    PMbuild()
  }
  compiler <- getPMoptions()$compilation_statements
  # check if gfortran and choose serial if not
  if (length(compiler) == 1) {
    parallel <- F
  }
  if (is.null(compiler)) {
    endNicely(
      paste("\nExecute ", type, " run after a fortran compiler is installed.\n", sep = ""),
      model, data
    )
  }

  # substitution string for directory separator according to OS
  rep <- c("/", "\\\\", "/")[OS]

  # change units of ODE tolerance to linear from log
  ode <- c(0, 10**ode)

  # generate the names of the permanent modules
  if (parallel) {
    prefix <- "p"
  } else {
    prefix <- "s"
  }
  # add the correct switch for NPAG engine, always serial for IT2B and ERR
  prepfiles <- shQuote(normalizePath(list.files(fortSource,
    pattern = switch(type,
      NPAG = "sNPprep",
      IT2B = "sITprep",
      ERR = "sITprep"
    ), full.names = T
  )))

# CR Hessian fix required NPAG links to a patch file ... replace this w/block below
#  enginefiles <- shQuote(normalizePath(list.files(fortSource,
#    pattern = switch(type,
#      NPAG = paste(prefix, "NPeng", sep = ""),
#      IT2B = "sITeng",
#      ERR = "sITerr"
#    ), full.names = T
#  )))
# OBFUSCATED CODE:
# enginefiles <- shQuote(normalizePath(list.files(fortSource,
#   pattern = switch(type, NPAG = paste(prefix, "NPeng", sep = ""), IT2B = "sITeng", ERR = "sITerr"), full.names = T)))
# if NPAG, then we need to add NPpatch to enginefiles
  if (type == "NPAG") {
     if (parallel) {
        enginefiles <- paste(fortSource,"/pNPpatch_120.o "
           , fortSource,"/pNPeng.o", sep = "")  
     } else {
        enginefiles <- paste(fortSource,"/sNPpatch_120.o "
           , fortSource,"/sNPeng.o", sep = "")  
     }
  } else if (type == "IT2B") { # same command as before
    enginefiles <- shQuote(normalizePath(list.files(fortSource,
      pattern = switch(type,
        IT2B = "sITeng",
         ERR = "sITerr"
       ), full.names = T
      )))
  } else {
     print(paste("Cannot create enginefiles for run type = ", type, sep = ""))
  }
# ------------------------

  # generate names of files that will be created
  prepFileName <- switch(type,
    NPAG = "np_prep",
    IT2B = "it_prep",
    ERR = "err_prep"
  )
  runFileName <- switch(type,
    NPAG = "np_run",
    IT2B = "it_run",
    ERR = "err_run"
  )
  drivFileName <- switch(type,
    NPAG = "npagdriv.f",
    IT2B = "it2bdriv.f",
    ERR = "assdriv.f"
  )
  scriptFileName <- switch(type,
    NPAG = "npscript",
    IT2B = "itscript",
    ERR = "errscript"
  )
  # list of output files
  if (type == "NPAG") {
    outlist <- c("DEN*", "OUT0*", "OUTT*", "PRTB*", "ILOG*", "NP_RF*")
  }
  if (type == "IT2B") {
    outlist <- c("DENF*", "FROM*", "LAST*", "OUFF*", "OUTF0*", "IT_RF*")
  }
  if (type == "ERR") {
    outlist <- "ASS*"
  }

  # generate the compile statements
  prepcompile <- sub("<exec>", prepFileName, compiler[1])
  prepcompile <- sub("<files>", prepfiles, prepcompile, fixed = T)
  enginecompile <- sub("<exec>", runFileName, compiler[1 + as.numeric(parallel)])
  enginecompile <- sub("<files>", enginefiles, enginecompile, fixed = T)
  # now substitute the file separator for inclusion in the batch file
  prepfiles <- gsub("/", rep, prepfiles)
  enginefiles <- gsub("/", rep, enginefiles)

  # initiation statement
  if (OS == 1 | OS == 3) {
    # Mac and Linux
    system(paste(
      "echo '", type, " run initiated at", format(Sys.time(), "%H:%M on %Y %b %d."),
      "\nPREP FILES:", prepfiles, "\nENGINE FILES:", enginefiles, "\n\n' > log.txt"
    ))
    system(prepcompile, ignore.stderr = F)
  }

  if (OS == 2) {
    # Windows
    shell(paste(
      "echo '", type, " run initiated at", format(Sys.time(), "%H:%M on %Y %b %d."),
      "\nPREP FILES:", prepfiles, "\nENGINE FILES:", enginefiles, "\n\n'"
    ))
    shell(prepcompile, ignore.stderr = F)
  }

  if (!intern | !auto) {
    # run as batch file script
    # build the batch file script
    PMscript <- vector("character")
    # format working directory for batch file
    workdir <- gsub("/", rep, getwd())
    # change working directory
    PMscript[getNext(PMscript)] <- paste("cd", shQuote(workdir))
    # start timer
    PMscript[getNext(PMscript)] <- c("echo Unix>time.txt", "echo Windows>time.txt", "echo Linux>time.txt")[OS]
    PMscript[getNext(PMscript)] <- c("date +%s>>time.txt", "echo %time%>>time.txt", "date +%s>>time.txt")[OS]

    if (!auto) {
      # manual run of prep program
      PMscript[getNext(PMscript)] <- c(
        paste("./", prepFileName, " MacOSX", sep = ""),
        paste(prepFileName, " DOS", sep = ""),
        paste("./", prepFileName, " MacOSX", sep = "")
      )[OS]
    } else {
      if (type == "NPAG") {
        # handle the prior for NPAG runs
        if (inherits(prior, "NPAG")) {
          nvar <- trans$nvar
          prior$ab <- as.matrix(trans$ab)
          prior$popPoints <- makeFinal(prior)$popPoints
          for (i in 1:nvar) {
            if (prior$ab[i, 1] > min(prior$popPoints[, i])) endNicely(paste("You have changed ", prior$par[i], " so that the minimum range of ", prior$ab[i, 1], " is greater than the minimum prior point value of ", min(prior$popPoints[, i]), "\nThis will cause NPAG to crash.\n", sep = ""), model, data)
            if (prior$ab[i, 2] < max(prior$popPoints[, i])) endNicely(paste("You have changed ", prior$par[i], " so that the maximum range of ", prior$ab[i, 2], "is less than the maximum prior point value of ", max(prior$popPoints[, i]), "\nThis will cause NPAG to crash.\n", sep = ""), model, data)
          }
          err <- makeDen(prior, F)
          if (err == -1) stop("\nYour NPdata prior object is older and does not contain the number of dimensions in your model.\nRe-run your NPAG analysis with Pmetrics 0.25 or later before bootstrapping.\n")
          prior <- c(0, "prior.txt")
        } else {
          if (prior != -99) {
            prior <- c(0, prior)
          } else {
            prior <- 1
          }
        }
      }
      # end prior handling block for NPAG runs

      # make the control file to execute run with instructions
      ControlFile <- c(
        "1", # using instruction file
        "instr.inx", # name of instruction file
        "1"
      ) # extra in case of error
      f <- file("PMcontrol", "w")
      writeLines(ControlFile, f)
      close(f)

      # run prep program
      PMscript[getNext(PMscript)] <- c(
        paste("./", prepFileName, " MacOSX < PMcontrol", sep = ""),
        paste(prepFileName, " DOS < PMcontrol", sep = ""),
        paste("./", prepFileName, " MacOSX < PMcontrol", sep = "")
      )[OS]
    }
    PMscript[getNext(PMscript)] <- "echo 1 > extnum"
    PMscript[getNext(PMscript)] <- "echo go > go"
    PMscript[getNext(PMscript)] <- paste(enginecompile, drivFileName, sep = " ")
    PMscript[getNext(PMscript)] <- c(
      paste("./", runFileName, " < go", sep = ""),
      paste(runFileName, " <go", sep = ""),
      paste("./", runFileName, " < go", sep = "")
    )[OS]
    PMscript[getNext(PMscript)] <- c("echo;echo Cleaning up....;echo", "echo. & echo Cleaning up.... & echo.", "echo;echo Cleaning up....;echo")[OS]
    PMscript[getNext(PMscript)] <- c("stty -echo", "echo off", "stty -echo")[OS]
    #    PMscript[getNext(PMscript)] <- paste("mkdir ",newdir,sep="")
    PMscript[getNext(PMscript)] <- "mkdir inputs"
    PMscript[getNext(PMscript)] <- "mkdir outputs"
    PMscript[getNext(PMscript)] <- "mkdir wrkcopy"
    PMscript[getNext(PMscript)] <- "mkdir etc"



    # add the name of the data file to the end of the output for NPAG or IT2B
    if (type == "NPAG" | type == "IT2B") {
      PMscript[getNext(PMscript)] <- paste("echo ", data,
        " >> ", substr(type, 1, 2), "_RF0001.TXT",
        sep = ""
      )
    }

    # check to make sure run completed
    if (type == "NPAG" | type == "IT2B") {
      PMscript[getNext(PMscript)] <- c(
        paste("if [ ! -f ", substr(type, 1, 2), "_RF0001.TXT ]; then error=true; else error=false; fi", sep = ""),
        paste("if not exist ", substr(type, 1, 2), "_RF0001.TXT (set error=1) ELSE (set error=0)", sep = ""),
        paste("if [ ! -f ", substr(type, 1, 2), "_RF0001.TXT ]; then error=true; else error=false; fi", sep = "")
      )[OS]
    }
    if (type == "ERR") {
      PMscript[getNext(PMscript)] <- c(
        "if [ ! -f ASS0001 ]; then error=true; else error=false; fi",
        "if not exist ASS0001 (set error=1) ELSE (set error=0)",
        "if [ ! -f ASS0001 ]; then error=true; else error=false; fi"
      )[OS]
    }

    # move output files
    for (i in 1:6) {
      PMscript[getNext(PMscript)] <- paste(c(
        paste("if [ -f ", outlist[i], " ]; then mv ", sep = ""),
        paste("if exist ", outlist[i], " move ", sep = ""),
        paste("if [ -f ", outlist[i], " ]; then mv ", sep = "")
      )[OS],
      outlist[i], " ", c("outputs; fi", "outputs", "outputs; fi")[OS],
      sep = ""
      )
    }
    # if error file exists
    PMscript[getNext(PMscript)] <- c(
      "if [ -f ERROR* ]; then mv ERROR* outputs; fi",
      "if exist ERROR* move ERROR* outputs",
      "if [ -f ERROR* ]; then mv ERROR* outputs; fi"
    )[OS]

    if (instr != -99) {
      PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], instr, " etc", sep = "")
      if (OS != 2) {
        PMscript[getNext(PMscript)] <- paste(c("mv ", "", "mv ")[OS], "log.txt outputs", sep = "")
      }
      PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], "PMcontrol etc", sep = "")
    }
    if (!useOldFortran) {
      # we are using the new model template
      PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], modelFor, " ", c("etc/", "etc\\", "etc/")[OS], modelFor, sep = "") # move fortran file to etc
      PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], model, " ", c("inputs/", "inputs\\", "inputs/")[OS], model, sep = "") # move template file to inputs
    } else {
      PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], model, " ", c("inputs", "inputs", "inputs")[OS], sep = "")
    }
    # using fortran file directly, so move to inputs

    PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], "XQZPJ*.ZMQ wrkcopy", sep = "")
    PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], "extnum etc", sep = "")
    if (type == "NPAG") {
      PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], "npag*.* etc", sep = "")
      PMscript[getNext(PMscript)] <- paste(c("rm ", "erase ", "rm ")[OS], "CHMAX*.*", sep = "")
      PMscript[getNext(PMscript)] <- c(
        paste("if [ -f FROM0001 ]; then mv FROM0001 ", "inputs; fi", sep = ""),
        paste("if exist FROM0001 move FROM0001 ", "inputs", sep = ""),
        paste("if [ -f FROM0001 ]; then mv FROM0001 ", "inputs; fi", sep = "")
      )[OS]
    }
    if (type == "IT2B" | type == "ERR") {
      PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], "it2b*.* etc", sep = "")
      PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], "itas*.* etc", sep = "")
    }
    if (type == "ERR") {
      PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], "assdriv*.* etc", sep = "")
    }
    PMscript[getNext(PMscript)] <- paste(c("rm ", "erase ", "rm ")[OS], "fort.*", sep = "")
    PMscript[getNext(PMscript)] <- paste(c("rm ", "erase ", "rm ")[OS], "go", sep = "")
    PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], prepFileName, "* etc", sep = "")
    PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], runFileName, "* etc", sep = "")
    PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], data, " inputs", sep = "")
    if (type == "NPAG" && prior[1] == 0) PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], basename(prior[2]), " inputs", sep = "")

    # make report
    reportscript <- paste(normalizePath(getPMpath(), winslash = "/"), "/report/",
      switch(type,
        NPAG = "NP",
        IT2B = "IT",
        ERR = "ERR"
      ), "repScript.R",
      sep = ""
    )
    alquimia_data_script <- paste(normalizePath(getPMpath(), winslash = "/"), "/report/genAlquimiaData.R", sep = "")
    outpath <- c(
      paste(workdir, "/outputs", sep = ""),
      paste(workdir, "\\outputs", sep = ""),
      paste(workdir, "/outputs", sep = "")
    )[OS]

    # end  timer and move file to outputs
    PMscript[getNext(PMscript)] <- c("date +%s >> time.txt", "echo %time% >> time.txt", "date +%s >> time.txt")[OS]
    PMscript[getNext(PMscript)] <- paste(c("mv ", "move ", "mv ")[OS], "time.txt outputs", sep = "")


    # close the error loop
    PMscript[getNext(PMscript)] <- c("if ! $error ; then ", "if %error% == 0 (", "if ! $error ; then ")[OS]


  
    PMscript[getNext(PMscript)] <- c(
          paste(normalizePath(R.home("bin"), winslash = "/"), "/Rscript ", shQuote(reportscript), " ", shQuote(outpath), " ", icen, " ", parallel, sep = ""),
          paste(shQuote(paste(gsub("/", rep, normalizePath(R.home("bin"), winslash = "/")), "\\Rscript", sep = "")), " ", shQuote(reportscript), " ", shQuote(outpath), " ", icen, " ", parallel, sep = ""),
          paste(normalizePath(R.home("bin"), winslash = "/"), "/Rscript ", shQuote(reportscript), " ", shQuote(outpath), " ", icen, " ", parallel, sep = "")
        )[OS]
    if (report) {
      if (alq) {
        PMscript[getNext(PMscript)] <- paste(normalizePath(R.home("bin"), winslash = "/"), "/Rscript ", shQuote(alquimia_data_script), " ", shQuote(outpath), " ; fi", sep = "")
      } else {
        PMscript[getNext(PMscript)] <- c(
          paste(normalizePath(R.home("bin"), winslash = "/"), "/Rscript -e ", shQuote(paste0('pander::openFileInOS(',shQuote(paste0(gsub('/', rep, outpath), '/', type, 'report.html')),')')), " ; fi", sep = ""),
          # paste(shQuote(paste(gsub("/", rep, normalizePath(R.home("bin"), winslash = "/")), "\\Rscript -e ", sep = "")), shQuote(paste0('pander::openFileInOS(',shQuote(paste0(gsub('/', rep, outpath), '/', type, 'report.html')),')')), " ",  ")", sep = ""),
          paste("start ", shQuote(paste(type, "Report")), " ", shQuote(paste(gsub("/", rep, outpath), "\\", type, "report.html", sep = "")), ")", sep = ""),
          paste(normalizePath(R.home("bin"), winslash = "/"), "/Rscript -e ", shQuote(paste0('pander::openFileInOS(',shQuote(paste0(gsub('/', rep, outpath), '/', type, 'report.html')),')')), " ; fi", sep = "")
        )[OS]
      #   PMscript[getNext(PMscript)] <- 
      #   c(
      #     paste("open ", shQuote(paste(gsub("/", rep, outpath), "/", type, "report.html", sep = "")), " ; fi", sep = ""),
      #     paste("start ", shQuote(paste(type, "Report")), " ", shQuote(paste(gsub("/", rep, outpath), "\\", type, "report.html", sep = "")), ")", sep = ""),
      #     paste("xdg-open ", shQuote(paste(gsub("/", rep, outpath), "/", type, "report.html", sep = "")), " ; fi", sep = "")
      #   )[OS]
      }
    } else { #close if statement if report = F
      PMscript[getNext(PMscript)] <- c("fi", "", "fi")[OS]
    }
    
    # final clean up
    if (OS == 1 | OS == 3) {
      # for Mac or Linux
      PMscript[getNext(PMscript)] <- paste("mv ", scriptFileName, " etc", sep = "")
    } else {
      # for Windows
      PMscript[getNext(PMscript)] <- paste("copy ", scriptFileName, ".bat etc", sep = "")
      PMscript[getNext(PMscript)] <- paste("echo. & echo Press any key to complete run and close this window... & echo.")
      PMscript[getNext(PMscript)] <- paste("pause > nul")
      PMscript[getNext(PMscript)] <- paste("erase ", scriptFileName, ".bat ", sep = "")
    }

    PMscript <- PMscript[!is.na(PMscript)]


    f <- file(c(scriptFileName, paste(scriptFileName, ".bat", sep = ""), scriptFileName)[OS], "w")
    writeLines(PMscript, f)
    close(f)

    if (OS == 1) {
      # Mac
      system(paste("chmod +x ", scriptFileName))
      if(compareVersion("22.0.0",Sys.info()[2])<=0){ #running Ventura
        if(!file.exists("/Applications/iTerm.app")){
          message <- paste("As of Ventura, there is a bug that prevents running a script in Terminal.\n",
          "Suggested workarounds: 1) use `intern = T` when running NPAG/IT2B\n",
          "or 2) download iTerm2 from https://iterm2.com, which permits use of `intern = F`.")
          endNicely(message = message)
        } else {
          if (!batch) system(paste("open -a iTerm.app ", shQuote(paste(getwd(), "/", scriptFileName, sep = "")), sep = ""))
        }
      }
    }
    if (OS == 2 & !batch) {
      # Create a wrapper script
      f <- file("win_wrapper.ps1", "w")
      writeLines(paste("Start-Process", scriptFileName), f)
      close(f)
      # Windows
      system2("C:/Windows/System32/WindowsPowerShell/v1.0/powershell", args = c("-ExecutionPolicy", "ByPass", "-file", "win_wrapper.ps1"), wait = T)
      # cat(paste("\n***Manually double-click the file called ", scriptFileName, ".bat in ", currwd, "/", newdir, " to execute the NPAG run.***\n\n", sep = ""))
      outpath <- gsub("\\\\", "/", outpath)
    }
    if (OS == 3) {
      # Linux
      system(paste("chmod +x ", scriptFileName))
      # if (!batch) system(paste("openvt ", shQuote(paste(getwd(), "./", scriptFileName, sep = "")), sep = ""))
      system(paste0("./", scriptFileName, " &"))
    }
    
    setwd(currwd)
    return(outpath)
  } else {
    # run internally, also for servers
    if (!auto) {
      # allow users to see questions
      if (OS == 1 | OS == 3) {
        system(paste("./", prepFileName, " MacOSX", sep = ""))
      }
    } else {
      # we do have instructions for prep
      if (type == "NPAG") {
        # handle prior for NPAG
        if (inherits(prior, "NPAG")) {
          nvar <- trans$nvar
          prior$ab <- as.matrix(trans$ab.df)
          prior$popPoints <- makeFinal(prior)$popPoints
          for (i in 1:nvar) {
            if (prior$ab[i, 1] > min(prior$popPoints[, i])) endNicely(paste("You have changed ", prior$par[i], " so that the minimum range of ", prior$ab[i, 1], " is greater than the minimum prior point value of ", min(prior$popPoints[, i]), "\nThis will cause NPAG to crash.\n", sep = ""), model, data)
            if (prior$ab[i, 2] < max(prior$popPoints[, i])) endNicely(paste("You have changed ", prior$par[i], " so that the maximum range of ", prior$ab[i, 2], "is less than the maximum prior point value of ", max(prior$popPoints[, i]), "\nThis will cause NPAG to crash.\n", sep = ""), model, data)
          }
          err <- makeDen(prior, F)
          if (err == -1) stop("\nYour NPdata prior object is older and does not contain the number of dimensions in your model.\nRe-run your NPAG analysis with Pmetrics 0.25 or later before bootstrapping.\n")
          prior <- c(0, "prior.txt")
        } else {
          if (prior != -99) {
            prior <- c(0, prior)
          } else {
            prior <- 1
          }
        }
      }
      # end prior handling block

      ControlFile <- c(
        "1", # we have an instruction file
        "instr.inx"
      ) # name of the instruction file

      f <- file("PMcontrol", "w")
      writeLines(ControlFile, f)
      close(f)

      if (OS == 1 | OS == 3) {
        system(paste("./", prepFileName, " MacOSX < PMcontrol", sep = ""))
      }
      if (OS == 2) {
        shell(paste(prepFileName, " DOS < PMcontrol", sep = ""))
      }
    }

    # RUN  engine

    # timestamp
    timeFile <- file("time.txt", open = "a")
    writeLines(c("Unix", "Windows", "Linux")[OS], timeFile)
    writeLines(as.character(proc.time()[3]), timeFile)

    if (OS == 1 | OS == 3) {
      system("echo 1 > extnum")
      system("echo go > go")
      system(paste(enginecompile, drivFileName, sep = " "))
      system(paste("./", runFileName, " < go", sep = ""))
    } else {
      shell("echo 1 > extnum")
      shell("echo go > go")
      shell(paste(enginecompile, drivFileName, sep = " "))
      shell(paste(runFileName, " < go", sep = ""))
    }

    # CLEAN UP
    # dir.create(newdir) already done above
    dir.create("inputs")
    dir.create("outputs")
    dir.create("wrkcopy")
    dir.create("etc")

    # write data file name to end of NPAG/IT2B output
    if (type == "NPAG" | type == "IT2B") {
      if (length(Sys.glob("??_RF*")) > 0) {
        pmfile <- file(Sys.glob("??_RF*"), open = "a")
        writeLines(data, pmfile)
        close(pmfile)
      }
    }

    # move output files
    file.copy(from = Sys.glob(outlist), to = "outputs")
    file.remove(Sys.glob(outlist))

    if (auto) {
      file.copy(from = instr, to = "etc")
      if (OS != 2) {
        file.copy(from = "log.txt", to = "outputs")
      }
      file.copy(from = "PMcontrol", to = "etc")
      file.copy(from = data, to = "inputs")
      file.remove(instr)
      file.remove("log.txt")
      file.remove("PMcontrol")
      file.remove(data)
    }
    if (type == "NPAG" && prior[1] == 0) {
      file.copy(from = prior[2], to = "inputs")
      file.remove(prior[2])
    }

    if (!useOldFortran) {
      # we are using the new model template
      file.copy(from = modelFor, to = paste("etc/", modelFor, sep = "")) # move fortran file to etc
      file.copy(from = model, to = paste("inputs/", model, sep = "")) # move template file to inputs
      file.remove(modelFor)
    } else {
      file.copy(from = model, to = "inputs") # using fortran file directly, so move to inputs
      file.remove(model)
    }

    if (length(Sys.glob("CHMAX*.*")) > 0) {
      file.remove(Sys.glob("CHMAX*.*"))
    }

    if (length(Sys.glob("FROM*")) > 0) {
      file.copy(from = Sys.glob("FROM*"), to = "inputs")
      file.remove(Sys.glob("FROM*"))
    }
    if (length(Sys.glob("ERROR*")) > 0) {
      file.copy(from = Sys.glob("ERROR*"), to = "outputs")
      file.remove(Sys.glob("ERROR*"))
    }

    file.remove(Sys.glob("fort.*"))
    file.remove("go")

    if (type == "NPAG") {
      file.copy(from = Sys.glob("npag*"), to = "etc")
      file.remove(Sys.glob("npag*"))
    }

    if (type == "IT2B" | type == "ERR") {
      file.copy(from = Sys.glob("it2b*"), to = "etc")
      file.copy(from = Sys.glob("itas*"), to = "etc")
      file.remove(Sys.glob("it2b*"))
      file.remove(Sys.glob("itas*"))
    }

    if (type == "ERR") {
      file.copy(from = "assdriv.f", to = "etc")
      file.remove("assdriv.f")
    }


    file.copy(from = Sys.glob("XQZPJ*.ZMQ"), to = "wrkcopy")
    file.copy(from = "extnum", to = "etc")
    file.copy(from = Sys.glob("*_prep*"), to = "etc")
    file.copy(from = Sys.glob("*_run*"), to = "etc")
    file.remove(Sys.glob("XQZPJ*.ZMQ"))
    file.remove("extnum")
    file.remove(Sys.glob("*_prep*"))
    file.remove(Sys.glob("*_run*"))

    # end time
    writeLines(as.character(proc.time()[3]), timeFile)
    close(timeFile)
    file.copy(from = "time.txt", to = "outputs")
    file.remove("time.txt")

    # make report
    if (type == "NPAG" || type == "IT2B") {
      reportType <- which(c("NPAG", "IT2B") == type)
      makeRdata(paste(currwd, newdir, "outputs", sep = "/"), F, reportType)
      res <- PM_load(paste(currwd, newdir, "outputs", sep = "/"))
      PM_report(res,outfile = paste(currwd, newdir, "outputs/NPreport.html", sep = "/"))
      setwd(currwd)
      return(res)
      # PMreport(paste(currwd, newdir, "outputs", sep = "/"), icen = icen, type = type, parallel = parallel)
    }
    if (type == "ERR") {
      ERRreport(paste(currwd, newdir, "outputs", sep = "/"), icen = icen, type = type)
    }

    # final clean up
    setwd(currwd)
    # file.copy(from = Sys.glob("*.*"), to = "inputs")
    # file.remove(Sys.glob("*.*"))
    # system("mv *.* inputs/")
    # outpath <- paste(currwd, newdir, "outputs", sep = "/")
    if(report){pander::openFileInOS(paste(gsub("/", rep, outpath), "/", type, "report.html", sep = ""))}
    return(outpath)
  }
}
