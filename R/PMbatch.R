


PMbatch <- function(type,datafolder,modelfolder,batchname="batch",...){
  data <- list.files(datafolder)
  model <- list.files(modelfolder)
  ndata <- length(data)
  nmodel <- length(model)
  
  OS <- getOS()
  currwd <- getwd()
  
  #pad data or model as necessary to match longer
  if(nmodel>ndata){
    nrep <- nmodel
    data <- rep(data,nrep)
  } else {
    nrep <- ndata
    model <- rep(model,nrep)
  }
  typef <- sapply(type,function(x) tolower(substr(x,1,2)))
  PMfunc <- sapply(typef,function(x) switch(x,np="NPrun",it="ITrun",NA))
  if(any(is.na(PMfunc))) stop("Please ensure type is either NPAG or IT2B")
  if(length(PMfunc)!=nrep){PMfunc <- rep(PMfunc,nrep)}
  
  batchdir <- vector("character")
  
  for(i in 1:nrep){
    file.copy(from=paste(datafolder,data[i],sep="/"),to=getwd())
    file.copy(from=paste(modelfolder,model[i],sep="/"),to=getwd())
    outdir <- do.call(PMfunc[i],args=list(data=data[i],model=model[i],batch=T,...))
    scriptFileName <- switch(PMfunc[i],NPrun="npscript",ITrun="itscript")
    batchdir[getNext(batchdir)] <- paste(c("open ","start ","open ")[OS],dirname(outdir),"/",scriptFileName,sep="")
  }
  
  
  #final clean up
  if(!file.exists(paste(dirname(currwd),"/Batches",sep=""))) dir.create(paste(dirname(currwd),"/Batches",sep=""))
  if(OS==1 | OS==3){ #for Mac or Linux
    batchdir[getNext(batchdir)] <- paste("mv ",currwd,"/",batchname," ",dirname(currwd),"/Batches",sep="")
    
  } else {  #for Windows
    batchdir[getNext(batchdir)] <- paste("copy ",currwd,"/",batchname,".bat ",dirname(currwd),"/Batches",sep="")
    batchdir[getNext(batchdir)] <- paste("echo. & echo Press any key to complete run and close this window... & echo.")
    batchdir[getNext(batchdir)] <- paste("pause > nul")
    batchdir[getNext(batchdir)] <- paste("erase ",currwd,"/",batchname,".bat ",sep="")
  }
  
  #write the batching script
  
  f <- file(c(batchname,paste(batchname,".bat",sep=""),batchname)[OS],"w")
  writeLines(batchdir,f)
  close(f)
  
  
  if (OS==1){ #Mac
    system(paste("chmod +x ",batchname))
    system(paste("open -a Terminal.app ",shQuote(paste(getwd(),"/",batchname,sep="")),sep=""))
  } 
  if (OS==2){ #Windows
    cat(paste("Launch ",batchname,".bat in your working directory to execute the batched runs.\n",sep=""))
  }
  if (OS==3){ #Linux
    system(paste("chmod +x ",batchname))
    system(paste("openvt ",shQuote(paste(getwd(),"./",batchname,sep="")),sep=""))
  } 
  
}

