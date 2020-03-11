
makePmetrics <- function(fortranChange = F, build = T, pdf = F, check = F, buildData = NULL, parallel = T,
                         ITver, NPver, SIMver, DOPTver, MBver) {
  wd <- paste(getwd(), "/", sep = "")
  setpmwd <- function(path) {
    setwd(paste(wd, path, sep = ""))
  }
  print(wd)
  require(devtools)
  OS <- switch(.Platform$OS.type, unix = 1, windows = 2)
  if (OS == 1) {
    #do this only if fortran files have changed
    if (fortranChange == T) {
      #remove old files
      #system("rm ~/LAPK/PmetricsSource/Pmetrics/inst/code/*.f")
      system(paste(paste("rm ", wd), "/inst/code/*.f"))
      setpmwd("Source")

      #copy source files to inst/code
      system(paste("cat ./IT2B/prep/*.* > ../inst/code/ITprep_", ITver, ".f", sep = ""))
      system(paste("cat ./IT2B/error/*.* > ../inst/code/ITerr_", ITver, ".f", sep = ""))
      system(paste("cat ./IT2B/engine/*.* > ../inst/code/ITeng_", ITver, ".f", sep = ""))

      system(paste("cat ./NPAG/prep/*.* > ../inst/code/NPprep_", NPver, ".f", sep = ""))
      system(paste("cat ./NPAG/engine/*.* > ../inst/code/NPeng_", NPver, ".f", sep = ""))

      file.copy(from = paste("./Simulator/engine/MONT", SIMver, ".FOR", sep = ""), to = paste("../inst/code/SIMeng_", SIMver, ".f", sep = ""), overwrite = T)

      system(paste("cat ./DOPT/prep/*.* > ../inst/code/DOprep_", DOPTver, ".f", sep = ""))
      system(paste("cat ./DOPT/engine/*.* > ../inst/code/DOeng_", DOPTver, ".f", sep = ""))

      system(paste("cat ./MB2CSV/*.* > ../inst/code/mb2csv_", MBver, ".f", sep = ""))


      #remove comment lines and clean up
      rmComm <- function(files) {
        for (i in files) {

          system(paste(paste(wd, "other/Source/win2mac.sa", sep = ""), i))
          file.remove(i)
          file.rename("newfile.txt", i)
          code <- readLines(i)
          commLines <- grep("^C", code, ignore.case = T)
          commLines2 <- grep("^\\*", code)
          #these are for parallel coding
          commLines3 <- grep("^![^\\$]", code)
          if (length(commLines3) > 0 & parallel == F) {
            allcomments <- c(commLines, commLines2, commLines3)
          } else { allcomments <- c(commLines, commLines2) }
          if (length(allcomments) > 0) code <- code[-allcomments]
          code <- code[code != ""]
          writeLines(code, i, sep = "\r\n")
        }
      }
      setpmwd("../inst/code")
      #setwd("~/LAPK/PmetricsSource/Pmetrics/inst/code")
      rmComm(files = list.files())
      setpmwd("../inst/config")
      #setwd("~/LAPK/PmetricsSource/Pmetrics/inst/config")
      writeLines("1", "newFort.txt")




    } else {
      setwd("inst/config")
      writeLines("0", "newFort.txt")
    }
    #do this for all changes    
    setwd(wd)
    print(getwd())
    if (file.exists("src/knn.o")) {
      file.remove("src/knn.o")
    }
    if (file.exists("src/Pmetrics.so")) {
      file.remove("src/Pmetrics.so")
    }
    devtools::load_all("Pmetrics")
    roxygen2::roxygenise("Pmetrics")
    document("Pmetrics")
    if (pdf) {
      file.remove("inst/doc/Pmetrics-manual.pdf")
      system("R CMD Rd2pdf --output=inst/doc/Pmetrics-manual.pdf --no-preview man/")
    }

    #write html changelog file
    chlog <- readLines("inst/NEWS.Rd")
    delLines <- which(chlog == "")
    chlog <- chlog[-delLines]
    chlog <- chlog[-1]
    chlog <- gsub("\\\\subsection\\{NEW FEATURES\\}\\{", "<h3>NEW FEATURES</h3>", chlog)
    chlog <- gsub("\\\\subsection\\{BUG FIXES\\}\\{", "<h3>BUG FIXES</h3>", chlog)
    chlog <- gsub("\\\\itemize\\{", "<ul>", chlog)
    items <- grep("\\\\item", chlog)
    chlog[items] <- lapply(chlog[items], function(x) paste("<li>", substr(x, 7, nchar(x)), "</li>", sep = ""))
    chlog <- gsub("\\\\section\\{Changes in version", "<h2 style='color:#222222'>Changes in version", chlog)
    chlog <- gsub("\\}\\{", "</h2>", chlog)
    chlog <- sub("^\\}", "</ul>", chlog)

    writeLines(chlog, "PMchangelog.html")

    #build example datasets
    #NPAG
    if (buildData$npag) {
      setpmwd("Test/NPAG")
      #setwd("~/LAPK/PmetricsSource/Test/NPAG")
      file.copy(from = c("../src/model.txt", "../src/ex.csv"), to = getwd(), overwrite = T)
      NPrun(data = "ex.csv", cycles = 100, run = 1, overwrite = T, intern = T)
      PMload(1)
      save(NPdata.1, final.1, cycle.1, op.1, cov.1, pop.1, post.1, mdata.1, file = paste(wd, "data/PMex1.rda"))
    }
    if (buildData$it2b) {
      #IT2B
      setpmwd("Test/IT2B")
      #setwd("~/LAPK/PmetricsSource/Test/IT2B")
      file.copy(from = c("../src/model.txt", "../src/ex.csv"), to = getwd(), overwrite = T)
      ITrun(data = "ex.csv", cycles = 100, run = 1, overwrite = T, intern = T)
      PMload(1)
      # save(ITdata.1,final.1,cycle.1,op.1,cov.1,mdata.1,file="~/LAPK/PmetricsSource/Pmetrics/data/PMex2.rda")
      save(ITdata.1, final.1, cycle.1, op.1, cov.1, mdata.1, file = paste(wd, "data/PMex2.rda"))
    }
    if (buildData$baddata) {
      setpmwd("Test")
      #setwd("~/LAPK/PmetricsSource/Test")
      badData <- PMreadMatrix("src/ex_bad.csv")
      save(badData, file = paste(wd, "data/PMex3.rda"))
      # save(badData,file="~/LAPK/PmetricsSource/Pmetrics/data/PMex3.rda")
    }
    setwd(wd)
    # setwd("~/LAPK/PmetricsSource")

    if (check) check("Pmetrics")
    if (build) {
      build("Pmetrics", binary = T)
      build("Pmetrics", binary = F)
    }
    install("Pmetrics")

    #copy to repository
    setwd(wd)
    # setwd("~/LAPK/PmetricsSource")
    Rvers <- paste(version$major, substr(version$minor, 1, 1), sep = ".")

    #tar.gz
    tools::write_PACKAGES(type = "source")
    file.remove(Sys.glob("other/Repos/src/contrib/Pmetrics*.tar.gz"))
    file.copy(from = Sys.glob("Pmetrics*.tar.gz"), to = "other/Repos/src/contrib")
    file.copy(from = Sys.glob("Pmetrics*.tar.gz"), to = "Archived")
    file.copy(from = "PACKAGES", to = "other/Repos/src/contrib", overwrite = T)
    file.remove(Sys.glob("Pmetrics*.tar.gz"))


    #tgz
    tools::write_PACKAGES(type = "mac.binary")
    macBinDir <- paste("other/Repos/bin/macosx/contrib/", Rvers, sep = "")
    if (!file.exists(macBinDir)) {
      dir.create(macBinDir)
    }
    file.remove(Sys.glob(paste(macBinDir, "Pmetrics*.tgz", sep = "/")))
    file.copy(from = Sys.glob("Pmetrics*.tgz"), to = macBinDir)
    file.copy(from = Sys.glob("Pmetrics*.tgz"), to = "Archived")
    file.copy(from = "PACKAGES", to = macBinDir, overwrite = T)


    mavBinDir <- paste("other/Repos/bin/macosx/mavericks/contrib/", Rvers, sep = "")
    if (!file.exists(mavBinDir)) {
      dir.create(mavBinDir)
    }
    file.remove(Sys.glob(paste(mavBinDir, "Pmetrics*.tgz", sep = "/")))
    file.copy(from = Sys.glob("Pmetrics*.tgz"), to = mavBinDir)
    file.copy(from = "PACKAGES", to = mavBinDir, overwrite = T)
    file.remove(Sys.glob("Pmetrics*.tgz"))



  } else {
    #this is for Windows

    #do this for all changes
    print(wd)
    setwd(wd)
    # writeLines(commandArgs(), paste("C:/Users/julia/Desktop", "makePmetrics.txt", sep = "/"))
    if (build) {
      build("Pmetrics", binary = T, args = "--no-multiarch")
      install("Pmetrics")
    }
    #copy to repository
    setwd(wd)
    Rvers <- paste(version$major, substr(version$minor, 1, 1), sep = ".")
    #zip
    tools::write_PACKAGES(type = "win.binary")
    winBinDir <- paste(wd, paste("other/Repos/bin/windows/contrib/", Rvers, sep = ""), sep = "")
    print(winBinDir)
    if (!file.exists(winBinDir)) {
      dir.create(winBinDir)
    }
    file.remove(Sys.glob(paste(winBinDir, "Pmetrics*.zip", sep = "/")))
    file.copy(from = Sys.glob("Pmetrics*.zip"), to = winBinDir)
    # file.copy(from=Sys.glob("Pmetrics*.zip"),to="Y:/LAPK/PmetricsSource/Archived")
    file.copy(from = "PACKAGES", to = winBinDir, overwrite = T)
    file.remove(Sys.glob("Pmetrics*.zip"))

  }
  setwd(wd)

}

makePmetrics(fortranChange = F, build = T, pdf = F, check = F,
             buildData = list(npag = F, it2b = F, baddata = F),
             ITver = 114, NPver = 120, SIMver = "114", DOPTver = 7, MBver = 1)

