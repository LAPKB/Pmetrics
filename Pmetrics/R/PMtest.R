
PMtest <- function(){
  currwd <- getwd()
  tempwd <- tempdir()
  setwd(tempwd)
  data(PMex1)
  PMwriteMatrix(mdata.1[mdata.1$id==1,],"data.csv",override=T)
  msg <- "Congratulations; you have successfully installed all components of Pmetrics.\n"
  modeltxt <- c("#Primary",
                "Ka, 0.1, 0.9",
                "Ke, 0.001, 0.1",
                "V, 30, 120",
                "Tlag1, 0, 4",
                "#Lag",
                "TLAG(1) = Tlag1",
                "#Out",
                "Y(1) = X(2)/V",
                "#Err",
                "G=5",
                "0.02, 0.05, -0.0002, 0")
  
  writeLines(modeltxt,"model.txt")
  
  engine <- list(alg="NP",nsubtot=1,nsub=1,activesub=1,ncov=5,covnames=c("wt","africa","age","gender","height"),
                 ndrug=1,tol=0.01,
                 salt=1,numeqt=1,cycles=1,icen="median",indpts=6,aucint=24,idelta=12,xmic=1,
                 ode=-4,limits=NA,priorString=1,wrkFlag=F)
  trans <- makeModel(model="model.txt",data="data.csv",engine=engine,write=T,silent=T)
  
  OS <- getOS()
  fortSource <- switch(OS,"~/.config/Pmetrics/compiledFortran",
                       paste(Sys.getenv("APPDATA"),"\\Pmetrics\\compiledFortran",sep=""),
                       "~/.config/Pmetrics/compiledFortran")
  
  if(!file.exists(fortSource)){
    msg <- c(msg,"You must run PMbuild().\n")
  }
  
  compiler <- PMFortranConfig()
  if(is.null(compiler)){
    msg <- c(msg,"You need to choose a Fortran compiler.  Run PMFortranConfig().\n")
  }
  
  #substitution string for directory separator according to OS  
  rep <- c("/","\\\\","/")[OS]
  
  #generate the names of the permanent modules  
  prepfiles <- shQuote(normalizePath(list.files(fortSource,pattern="sNPprep",full.names=T)))
  
  #generate names of files that will be created
  prepFileName <- "np_prep"
  runFileName <- "np_run"
  drivFileName <- "npagdriv.f"
  
  #run command
  runPrep <- c(paste("./",prepFileName," MacOSX",sep=""),
               paste(prepFileName," DOS",sep=""),
               paste("./",prepFileName," MacOSX",sep=""))[OS]
  
  #generate the compile statements  
  prepcompile <- sub("<exec>",prepFileName,compiler[1])
  prepcompile <- sub("<files>",prepfiles,prepcompile,fixed=T)
  
  #compile prep program as test
  if (OS==1 | OS==3){  #Mac and Linux
    error <- system(paste(prepcompile,"2>error.txt"))
    if(error!=0){
      errormsg <- readLines("error.txt")
      if(length(grep("command not found",errormsg[1]))>0){
        msg <- c(msg,"You have not installed a fortran compiler or have chosen the wrong compiler.\nRe-install, or tell Pmetrics the correct compiler with PMFortranConfig().\n")
      }
    } else { #ok np_prep compiled
      #error=system(paste(runPrep,"2>error.txt"))
    }
  } 
  
  if (OS==2){  #Windows
    error <- suppressWarnings(shell(paste(prepcompile,"2>error.txt")))
    if(error!=0){
      errormsg <- readLines("error.txt")
      if(length(grep("is not recognized",errormsg[1]))>0){
        msg <- c(msg,"You have not installed a fortran compiler or have chosen the wrong compiler.\nRe-install, or tell Pmetrics the correct compiler with PMFortranConfig().\n")
      }
    } else { #ok np_prep compiled
      #error=shell(paste(runPrep,"2>error.txt"))
    }       
  }
  

  
  
  if(length(msg)>1) {cat(msg[-1])} else {cat(msg)}
  
  setwd(currwd)
  unlink(tempwd)
  
  
} #end function
