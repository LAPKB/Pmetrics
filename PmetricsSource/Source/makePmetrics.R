
makePmetrics <- function(fortranChange=F,build=T,pdf=F,check=F,
                         buildData=NULL,parallel=T,
                         ITver,NPver,SIMver,DOPTver,MBver){
  wd <- getwd()
  require(devtools)
  OS <- switch(.Platform$OS.type,unix=1,windows=2)
  if(OS==1){
    #do this only if fortran files have changed
    if(fortranChange==T){
      npagEngineDir <- c("./NPAG/engine/") # NPAG parent directory
      # create list of files to cat into the distribution source
      { # NPver = 123
        npagEngineDir120 <- paste("./NPAG/engine/","v126/",sep="")
        np120 <-c("")
        npag120_original <- c("NPeng_120_1_5_0.f")
        # MvG's files on 9/4/2017
        npag120_o<- c("0blasnpag.f", "0read23.f", "0shift10.f",
                      "0idm1x18.f", "0idm2x18.f", "0idm3x19.f",
                      "0npagranfix6.f")
        npag120files <- c("npagranfix6.f", "read23.f", "shift10.f",
                          "idm1x18.f", "idm2x18.f", "idm3x19.f",
                          "blasnpag.f") # ,"lapack.f", "emint.f", "vodetot.f")
        # idm1x18_b03.f calls DVSRCO()
        # idm1x18_b04.f removes GO TO statements from logic leading into the
        #    looping call to DVODE in idm1x18_b02.f
        #
        npag120betafiles <- c("0blasnpag.f", "0read23.f", "0shift10.f",
                              "idm1x18_b05.f", "idm2x18_b01.f", "idm3x19_b01.f",
                              "npagranfix6_b09.f", "emint_b01.f")
        # Adding count data capability
        # This only requires changing idm1 to ver 06, and npagranfix6 to ver 10
        # note: in making 06 and 10, I first included all anticipated code changes
        # as comments and then validated the commented code.  I then made the
        # 06 and 10 files and uncommented the code, but validated a completely Normal
        # model
        npag120betafiles <- c("0blasnpag.f", "0read23.f", "0shift10.f90",
                              "idm1x18_b06.f", "idm2x18_b01.f", "idm3x19_b01.f",
                              "npagranfix6_b10.f", "emint_b01.f")
        npagSupplementaryFiles <- c("interface_0SHIFT.txt", # shift is now in npag_utils
                                    "npag_utils.f90",
                                    "0blasnpag.f", "emint_b01.f") # these two used by simulator
        # dvode_file <- c("dvode_v0.f90") # PMbuild() has dvode_v0.f90 hardcoded!
        # dvode_v0.f90 -- minimal changes to original f90 code
        dvode_file <- c("dvode_v1.f90") # PMbuild() has dvode_v1.f90 hardcoded
        # dvode_v1.f90 has RTOL a scalar
        # if (make.beta == T) {
        #     concatenate npag120betafiles
        # } else {
        #     concatenate npag120files
        # }
        # beta declares threadprivate rpar and ipar in idm1x18.f, idm2x and idm3x
        # reserves ipar(1) <- IgIsGoodPoint, if 0 then set pyjgx = 0 and cycle to next x
        # reserves ipar(23) <- First enry to dvode(jsub,ig), to skip initialization code in diffeq
        npag_files <- npag120betafiles
        dvode_ver <- paste(npagEngineDir120,dvode_file,sep="")
        for (ii in seq(1,length(npag_files)))
          {
             newfile <- paste(npagEngineDir120,npag_files[ii],sep="")
             np120 <- paste(np120,newfile,sep = " ")
        }
        # NPver = 121 -- Contains original and hardenned 120 source
        # NPver = 122 -- Contains *.f and *.f90 modules -- will be MPI ready
        # NPver = 123 ... 126 Increasingly harder code
        # NPver = 127 -- Contains MPI directives to calculate pyjgx for ea. y
        #   i.e. OpenMPI over y containing OpenMP over points; roughly speaking,
        #       y over available machines, x over available cores per machine
        #       but allow compiler to make the call what happens where.
        # NPver = 128 -- Contains MPI directives to calculate pyjgx for ea. y and ea. x
        #   i.e. OpenMP is now gone.
      }
      
      #remove old files
      system("rm ~/LAPK/PmetricsSource/Pmetrics/inst/code/*.f")
      setwd("~/LAPK/PmetricsSource/Source")
      
      #copy source files to inst/code
      system(paste("cat ./IT2B/prep/*.* > ../Pmetrics/inst/code/ITprep_",ITver,".f",sep=""))
      system(paste("cat ./IT2B/error/*.* > ../Pmetrics/inst/code/ITerr_",ITver,".f",sep=""))
      system(paste("cat ./IT2B/engine/*.* ./NPAG/engine/v126/0blasnpag.f ./NPAG/engine/v126/emint_b01.f > ../Pmetrics/inst/code/ITeng_",ITver,".f",sep=""))
      system(paste("cat ./NPAG/prep/*.* > ../Pmetrics/inst/code/NPprep_",NPver,".f",sep=""))
      if (NPver == 120)
        {
          system(paste("cat ",np120," > ../Pmetrics/inst/code/NPeng_",NPver,".f",sep=""))
          system(paste("cp", dvode_ver , "../Pmetrics/inst/code/", sep = " " ))
          for (ii in seq(1,length(npagSupplementaryFiles)))
          {
            ftocopy <- paste(npagEngineDir120,npagSupplementaryFiles[ii],sep = "")
            system(paste("cp", ftocopy , "../Pmetrics/inst/code/", sep = " " ))
          }
          list.files(path = "../Pmetrics/inst/code")
        }
      
      file.copy(from=paste("./Simulator/engine/MONT",SIMver,".FOR",sep=""),
                to=paste("../Pmetrics/inst/code/SIMeng_",SIMver,".f",sep=""),
                overwrite=T)
      
      system(paste("cat ./DOPT/prep/*.* > ../Pmetrics/inst/code/DOprep_",DOPTver,".f",sep=""))  
      system(paste("cat ./DOPT/engine/*.* > ../Pmetrics/inst/code/DOeng_",DOPTver,".f",sep=""))
      
      system(paste("cat ./MB2CSV/*.* > ../Pmetrics/inst/code/mb2csv_",MBver,".f",sep=""))  
      
      system("cp ./remoteNPrun.sh ~/LAPK/PmetricsSource/Pmetrics/inst/code/")     
      #system("wc -l ./remoteNPrun.sh")
      #system("wc -l ~/LAPK/PmetricsSource/Pmetrics/inst/code/remoteNPrun.sh")
      system("cp ./win2mac.sa     ~/LAPK/PmetricsSource/Pmetrics/inst/code/")   
      system("chmod 555 ~/LAPK/PmetricsSource/Pmetrics/inst/code/win2mac.sa")
      system("cp ./win2mac.sa     ~/LAPK/PmetricsSource/Pmetrics/inst/code/") 
      
      #remove comment lines and clean up
      rmComm <- function(files){
        for (i in files){
          system(paste("~/LAPK/PmetricsSource/Source/win2mac.sa",i))
          file.remove(i)
          file.rename("newfile.txt",i)
          code <- readLines(i)
          commLines <- grep("^C",code,ignore.case=T)
          commLines2 <- grep("^\\*",code)
          #these are for parallel coding
          commLines3 <- grep("^![^\\$]",code)
          if(length(commLines3)>0 & parallel==F){
            allcomments <- c(commLines,commLines2,commLines3)
          }else{allcomments <- c(commLines,commLines2)}
          if(length(allcomments)>0) code <- code[-allcomments]
          code <- code[code!=""]
          writeLines(code,i,sep="\r\n")
        }
      }
      
      setwd("~/LAPK/PmetricsSource/Pmetrics/inst/code")
      rmComm(files=list.files())
      setwd("~/LAPK/PmetricsSource/Pmetrics/inst/config")
      writeLines("1","newFort.txt")
      
      
      
      
    } else { # FortranChange = F
      setwd("~/LAPK/PmetricsSource/Pmetrics/inst/config")
      writeLines("0","newFort.txt")
    }
    #do this for all changes    
    setwd("~/LAPK/PmetricsSource")
    if (file.exists("./Pmetrics/src/knn.o")) {
      file.remove("./Pmetrics/src/knn.o")
    }
    if (file.exists("./Pmetrics/src/Pmetrics.so")) {
      file.remove("./Pmetrics/src/Pmetrics.so")
    }
    devtools::load_all(path = "./Pmetrics") # 1st half of document(), below
    devtools::document(pkg = "Pmetrics") # will re-create the two files just deleted.
    # roxygen2::roxygenise() # should do second half of document(), above.
    if(pdf){
      file.remove("./Pmetrics/inst/doc/Pmetrics-manual.pdf")
      system("R CMD Rd2pdf --output=./Pmetrics/inst/doc/Pmetrics-manual.pdf --no-preview ./Pmetrics/man")
    }
    
    #write html changelog file
    chlog <- readLines("Pmetrics/inst/NEWS.Rd")
    delLines <- which(chlog=="")
    chlog <- chlog[-delLines]
    chlog <- chlog[-1]
    chlog <- gsub("\\\\subsection\\{NEW FEATURES\\}\\{" ,"<h3>NEW FEATURES</h3>",chlog)
    chlog <- gsub("\\\\subsection\\{BUG FIXES\\}\\{" ,"<h3>BUG FIXES</h3>",chlog)
    chlog <- gsub("\\\\itemize\\{","<ul>",chlog)
    items <- grep("\\\\item",chlog)
    chlog[items] <- lapply(chlog[items],function(x) paste("<li>",substr(x,7,nchar(x)),"</li>",sep=""))
    chlog <- gsub("\\\\section\\{Changes in version" ,"<h2 style='color:#222222'>Changes in version",chlog)
    chlog <- gsub("\\}\\{" ,"</h2>",chlog)
    chlog <- sub("^\\}","</ul>",chlog)
    
    writeLines(chlog,"PMchangelog.html")
    
    #build example datasets
    #NPAG
    if(buildData$npag){
      setwd("~/LAPK/PmetricsSource/Test/NPAG")
      file.copy(from=c("../src/model.txt","../src/ex.csv"),to=getwd(),overwrite=T)
      NPrun(data="ex.csv",cycles=100,run=1,overwrite=T,intern=T)
      PMload(1)
      save(NPdata.1,final.1,cycle.1,op.1,cov.1,pop.1,post.1,mdata.1,file="~/LAPK/PmetricsSource/Pmetrics/data/PMex1.rda")
    }
    if(buildData$it2b){
      #IT2B
      setwd("~/LAPK/PmetricsSource/Test/IT2B")
      file.copy(from=c("../src/model.txt","../src/ex.csv"),to=getwd(),overwrite=T)
      ITrun(data="ex.csv",cycles=100,run=1,overwrite=T,intern=T)
      PMload(1)
      save(ITdata.1,final.1,cycle.1,op.1,cov.1,mdata.1,file="~/LAPK/PmetricsSource/Pmetrics/data/PMex2.rda")
    }
    if(buildData$baddata){
      setwd("~/LAPK/PmetricsSource/Test")
      badData <- PMreadMatrix("src/ex_bad.csv")
      save(badData,file="~/LAPK/PmetricsSource/Pmetrics/data/PMex3.rda")
    }
    
    setwd("~/LAPK/PmetricsSource")
    
    if(check) check("Pmetrics")
    if(build){
      build("Pmetrics",binary=T)
      build("Pmetrics",binary=F)
    } 
    install("Pmetrics")
    
    #copy to repository
    setwd("~/LAPK/PmetricsSource")
    Rvers <- paste(version$major,substr(version$minor,1,1),sep=".")
    
    #tar.gz
    tools::write_PACKAGES(type="source")
    file.remove(Sys.glob("Repos/src/contrib/Pmetrics*.tar.gz"))
    file.copy(from=Sys.glob("Pmetrics*.tar.gz"),to="Repos/src/contrib")
    file.copy(from=Sys.glob("Pmetrics*.tar.gz"),to="Archived")
    file.copy(from="PACKAGES",to="Repos/src/contrib",overwrite=T)
    file.remove(Sys.glob("Pmetrics*.tar.gz"))
    
    
    #tgz
    tools::write_PACKAGES(type="mac.binary")
    macBinDir <- paste("Repos/bin/macosx/contrib/",Rvers,sep="")
    if(!file.exists(macBinDir)){
      dir.create(macBinDir)
    }
    file.remove(Sys.glob(paste(macBinDir,"Pmetrics*.tgz",sep="/")))
    file.copy(from=Sys.glob("Pmetrics*.tgz"),to=macBinDir)
    file.copy(from=Sys.glob("Pmetrics*.tgz"),to="Archived")
    file.copy(from="PACKAGES",to=macBinDir,overwrite=T)
    
    
    mavBinDir <- paste("Repos/bin/macosx/mavericks/contrib/",Rvers,sep="")
    if(!file.exists(mavBinDir)){
      dir.create(mavBinDir)
    }
    file.remove(Sys.glob(paste(mavBinDir,"Pmetrics*.tgz",sep="/")))
    file.copy(from=Sys.glob("Pmetrics*.tgz"),to=mavBinDir)
    file.copy(from="PACKAGES",to=mavBinDir,overwrite=T)
    file.remove(Sys.glob("Pmetrics*.tgz"))
    
    
    # END if (OS==1)
  } else {  #this is for Windows

    #do this for all changes
    setwd("C:/LAPK/PmetricsSource")
    if(build) build("Pmetrics",binary=T)
    #copy to repository
    setwd("C:/LAPK/PmetricsSource")
    Rvers <- paste(version$major,substr(version$minor,1,1),sep=".")
    #zip
    tools::write_PACKAGES(type="win.binary")
    winBinDir <- paste("Y:/LAPK/PmetricsSource/Repos/bin/windows/contrib/",Rvers,sep="")
    if(!file.exists(winBinDir)){
      dir.create(winBinDir)
    }
    file.remove(Sys.glob(paste(winBinDir,"Pmetrics*.zip",sep="/")))
    file.copy(from=Sys.glob("Pmetrics*.zip"),to=winBinDir)
    file.copy(from=Sys.glob("Pmetrics*.zip"),to="Y:/LAPK/PmetricsSource/Archived")
    file.copy(from="PACKAGES",to=winBinDir,overwrite=T)
    file.remove(Sys.glob("Pmetrics*.zip"))
    
  }
  setwd(wd)
  
}

# -------------------------------------------------------------------
#  Note to wmy All you have to do is run the following function ...
#  ... _after_ loading it above.
#
# use fortranChange=T if you edit the fortran, else use F
#
# Standard build does not do much
# makePmetrics(fortranChange=T,build=T,pdf=F,check=F,
#             buildData=list(npag=F,it2b=F,baddata=F),
#             ITver=114,NPver=120,SIMver="114",DOPTver=7,MBver=1)
# Following build is better; only test data is missing;
# but it really is missing! So don't try to install the
# test data or you get an early exit and error

fortranChange=T; build=T; pdf=F; check=T; buildData=NULL 
ITver=114; NPver=120; SIMver="114"; DOPTver=7; MBver=1; parallel=T

makePmetrics(fortranChange=T,build=T,pdf=F,check=F,
             buildData=list(npag=F,it2b=F,baddata=F),
             ITver=114,NPver=120,SIMver="114",DOPTver=7,MBver=1)

# Last build was npag120files -- and running 281
# Last build was npag120_original -- and running 283
# Last build was npag120_o -- and running 285
# Last build was npag120betafiles -- and running 284, 286
getwd()
con <- file("pmbuildout.txt","w") # this does NOT work ! compiler output still to Rconsole
# best bet is to go to terminal and do manual compile
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
PMbuild()
sink() # Remember to do this last sink() or you will lose subsequent work

unlink("pmbuildout.txt")
# Last build was "date"









