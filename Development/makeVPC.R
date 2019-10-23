

makeValid <- function(run,outeq=1,input=1,icen="median",binCov,clusters,
                    lower=0.025,upper=0.975,...){
  
  if(length(grep("mclust",installed.packages()[,1]))==0){
    install.packages("mclust",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  mclust.installed <- require(mclust,quietly=T,warn.conflicts=F)
  if(!mclust.installed) stop("Package mclust not installed.")
  
  #save current wd
  currwd <- getwd()
  
  #get the run
  if(missing(run)) run <- readline("Enter the run number: ")
  PMload(run)
  
  getName <- function(x){
    return(get(paste(x,run,sep=".")))
  }
  
  #parse dots
  arglist <- list(...)
  namesSIM <- names(formals(SIMrun))
  #namesNPDE <- names(formals(autonpde))
  argsSIM <- arglist[which(names(arglist) %in% namesSIM)]
  
  # Cluster raw data --------------------------------------------------------

  #grab raw data file
  mdata <- getName("mdata")
  maxInput <- max(mdata$input,na.rm=T)
  maxOuteq <- max(mdata$outeq,na.rm=T)
  if(outeq > maxOuteq){
    stop("You entered an output equation number greater than the number of output equations.")
  }
  if(input > maxInput){
    stop("You entered a drug input number greater than the number of drug inputs.")
  }
  #filter mdata to appropriate input and outeq
  mdata <- mdata[(mdata$evid>0 & mdata$input==input) | (mdata$evid==0 & mdata$outeq==outeq),]
  
  #filter to include/exclude subjects
  if("include" %in% names(argsSIM)){
    mdata <- mdata[mdata$id %in% argsSIM$include,]
    argsSIM[[which(names(argsSIM)=="include")]] <- NULL
  }
  if("exclude" %in% names(argsSIM)){
    mdata <- mdata[!mdata$id %in% argsSIM$exclude,]
    argsSIM[[which(names(argsSIM)=="exclude")]] <- NULL
  }
  
  nsub <- length(unique(mdata$id))

  if(missing(binCov)){
    cat(paste("Covariates in your data file: ",paste(names(mdata)[-c(1:get("nfixed",envir=PMenv))],collapse=", ")))
    binCov <- readline("Enter any covariates to be binned, separated by commas (<Return> for none): ")
    binCov <- unlist(strsplit(binCov,","))
    #remove leading/trailing spaces
    binCov <- gsub("^[[:space:]]|[[:space:]]$","",binCov)
  }
  
  if(!all(binCov %in% names(mdata))){
    stop("You have entered covariates which are not valid covariates in your data.")
  }
  #set up data for clustering
  #fill in gaps for cluster analysis only for binning variables (always dose and time)
  mdata2 <- mdata[,c("id","evid","time","dose",binCov)]
  #set zero doses (covariate changes) as missing
  mdata2$dose[mdata2$dose==0] <- NA
  for(i in 1:nrow(mdata2)){
    missingVal <- which(is.na(mdata2[i,]))
    if(4 %in% missingVal){ #dose is missing
      if(i==1 | (mdata2$id[i-1]!=mdata2$id[i])){ #first record for patient has zero dose
        j <- 0
        while(is.na(mdata2$dose[i+j])){ #increment until non-zero dose is found
          j <- j+1
        }
        mdata2$dose[i] <- mdata2$dose[i+j] #set dose equal to first non-zero dose
        missingVal <- missingVal[-which(missingVal==3)] #take out missing flag for dose as it has been dealt with
      }
    }
    mdata2[i,missingVal] <- mdata2[i-1,missingVal]
  }
  
  #restrict to doses for dose/covariate clustering (since covariates applied on doses)
  mdata2a <- mdata2[mdata2$evid>0,-c(1:3)]
  #restrict to observations for time clustering
  mdata2b <- mdata2$time[mdata2$evid==0]
  
  if(missing(clusters)){
    clusters <- c(NA,NA)
    #ELBOW PLOT
    elbow <- function(x){
      set.seed(123)
      # Compute and plot wss for k = 2 to k = 15.
      # set k.max
      if(is.null(dim(x))){
        k.max <- min(length(unique(x)),15)
      }else{
        k.max <- min(nrow(unique(x)),15)
      }
                                  
      wss <- sapply(2:k.max, 
                    function(k){val <- kmeans(x, k, nstart=50,iter.max = 15 );
                    val$tot.withinss})
      wss
      plot(2:k.max, wss,
           type="b", pch = 19, frame = FALSE, 
           xlab="Number of clusters",
           ylab="Total within-clusters sum of squares (WSS)") 
    }
    
    #DOSE/COVARIATES
    cat("Now optimizing clusters for dose/covariates.\n")
    cat("First step is a Gaussian mixture model analysis, followed by an elbow plot.\n")
    readline(paste("Press <Return> to start cluster analysis for ",
                   paste(c("dose",binCov),collapse=", ",sep=""),": ",sep=""))
    cat("Now performing Gaussian mixture model analysis.")
    mod1 <- Mclust(mdata2a)
    cat(paste("Most likely number of clusters is ",mod1$G,".",sep=""))
    readline("Press <Return> to see classification plot: ")
    plot(mod1,"classification")   
    readline("Press <Return> to see elbow plot: ")
    elbow(mdata2a)
    clusters[1] <- as.numeric(readline(paste("Specify your dose/covariate cluster number, <Return> for ",mod1$G,": ",sep="")))
    if(is.na(clusters[1])) clusters[1] <- mod1$G
    
    #TIME
    readline("Press <Return> to start cluster analysis for sample times: ")
    mod2 <- Mclust(mdata2b)
    cat(paste("Most likely number of clusters is ",mod2$G,".\n",sep=""))
    readline("Press <Return> to see classification plot: ")
    plot(mod2,"classification")  
    readline("Press <Return> to see cluster plot: ")
    plot(out~time,mdata[mdata$out>-99,],xlab="Time",ylab="Observation",xlim=c(min(mdata$time[mdata$evid==0]),
                                                              max(mdata$time[mdata$evid==0])))
    abline(v=mod2$parameters$mean,col="red")
    readline("Press <Return> to see elbow plot: ")
    elbow(mdata2b)
    clusters[2] <- as.numeric(readline(paste("Specify your sample time cluster number, <Return> for ",mod2$G,": ",sep="")))
    if(is.na(clusters[2])) clusters[2] <- mod2$G
  } #end if missing clusters
  
  #now set the cluster bins
  mod3 <- kmeans(mdata2a,centers=clusters[1],nstart=50)
  mdata2$mbin[mdata2$evid==1] <- mod3$cluster  #m=dose,covariate bins
  
  mod4 <- kmeans(mdata2b,centers=clusters[2],nstart=50)
  mdata2$nbin[mdata2$evid==0] <- sapply(mod4$cluster,function(x) which(order(mod4$centers)==x))  #n=ordered time bins
  
  
  # Simulations -------------------------------------------------------------
  
  #create /vpc
  if(!file.exists(paste(run,"/vpc",sep=""))) dir.create(paste(run,"/vpc",sep=""))
  
  #get model file
  instrfile <- suppressWarnings(tryCatch(readLines(paste(run,"etc/instr.inx",sep="/")),error=function(e) NULL))
  if(length(grep("IVERIFY",instrfile))==0){ #not updated instruction file
    modelfile <- readline("Your run used an old instruction file. Enter model name: ")
  } else { #ok we are using updated instruction file
    if(length(instrfile)>0){  #ok we got one
      #model.for file name
      modelfile <- instrfile[5]
      #convert to original name    
      modelfile <- basename(Sys.glob(paste(run,"/inputs/",strsplit(modelfile,"\\.")[[1]][1],"*",sep="")))
      if(length(modelfile)>1){modelfile <- modelfile[grep(".txt",modelfile)]}
      
    } else {stop("Model file not found.\n")}
  }
  
  #copy this modelfile to new /vpc folder
  invisible(file.copy(from=paste(run,"/inputs/",modelfile,sep=""),to=paste(run,"/vpc",sep="")))
  
  #now get the data file  
  RFfile <- suppressWarnings(tryCatch(readLines(Sys.glob(paste(run,"outputs/??_RF0001.TXT",sep="/"))),error=function(e) NULL))
  if(length(RFfile)>0){
    datafileName <- tail(RFfile,1)
    file.copy(from=paste(run,"inputs",datafileName,sep="/"),to=paste(run,"/vpc",sep=""))
    datafile <- datafileName
  } else {stop("Data file not found\n")}
  
  #change wd to new /vpc folder which now contains data and model files
  setwd(paste(run,"/vpc",sep=""))
  
  #simulate PRED_bin from pop icen parameter values and median of each bin for each subject
  #first, calculate median of each bin
  med1 <- aggregate(mdata2[,4:(ncol(mdata2)-2)],by=list(mdata2$mbin),FUN=median)
  names(med1) <- c("bin","dose",binCov)
  med2 <- aggregate(mdata2$time,by=list(mdata2$nbin),FUN=median)
  names(med2) <- c("bin","time")
  
  #create  datafile based on mdata, but with covariates and doses replaced by medians
  #and sample times by bin times
  mdataMedian <- mdata
  mdataMedian$mbin <- mdata2$mbin
  mdataMedian$nbin <- mdata2$nbin
  mdataMedian$dose <- med1$dose[match(mdataMedian$mbin,med1$bin)]
  mdataMedian$time[mdataMedian$evid==0] <- med2$time[match(mdataMedian$nbin[mdataMedian$evid==0],med2$bin)]
  covCols <- which(names(mdataMedian) %in% binCov)
  if(length(covCols)>0) mdataMedian[,covCols] <- med1[match(mdataMedian$mbin,med1$bin),-c(1:2)]
  
  #write median file
  MedianDataFileName <- paste(substr(paste("m_",datafileName,sep=""),0,8),".csv",sep="")
  PMwriteMatrix(mdataMedian[,1:(ncol(mdataMedian)-2)],MedianDataFileName,override=T)
  
  #remove old files
  invisible(file.remove(Sys.glob("sim*.txt")))
  
  #get poppar and make one with zero covariance
  poppar=getName("final")
  popparZero <- poppar
  popparZero$popCov[popparZero$popCov!=0] <- 0
  #do the simulation for each subject using the median dose, median covariates and pop parameters
  if("seed" %in% names(argsSIM)){
    seed.start <- argsSIM$seed
    argsSIM[[which(names(argsSIM)=="seed")]] <- NULL
  } else {seed.start <- -17}
  set.seed(seed.start)
  argsSIM1 <- c(list(poppar=popparZero,data=MedianDataFileName,model=modelfile,nsim=1,
                    seed=runif(nsub,-100,100),obsNoise=rep(0,4),outname="simMed"),argsSIM)
  cat("Simulating outputs for each subject using population means...\n")
  flush.console()
  do.call("SIMrun",argsSIM1)
  
  #read and format the results of the simulation
  PRED_bin <- SIMparse("simMed*",combine=T,silent=T)
  
  #make tempDF subset of PMop for subject, time, obs, outeq, pop, icen predictions (PREDij)
  tempDF <- getName("op")
  tempDF <- tempDF[tempDF$pred.type=="pop" & tempDF$icen==icen,]
  
  #add PRED_bin to tempDF
  tempDF$PRED_bin <- PRED_bin$obs$out
  
  #add pcYij column to tempDF as obs * PREDbin/PREDij
  tempDF$pcObs <- tempDF$obs * tempDF$PRED_bin/tempDF$pred
  
  #bin pcYij by time and add to tempDF
  tempDF$nbin <- mdata2$nbin[mdata2$evid==0] 
  tempDF$bintime <- med2$time[match(tempDF$nbin,med2$bin)]

  
  #calculate 5th, 50th and 95th percentiles for pcYij by time bin
  quant_pcObs <- tapply(tempDF$pcObs,tempDF$nbin,quantile,probs=c(lower,0.5,upper),na.rm=T)
  #calculate 5th, 50th and 95th percentiles for Yij by time bin
  quant_Obs <- tapply(tempDF$obs,tempDF$nbin,quantile,probs=c(lower,0.5,upper),na.rm=T)
  
  #Now, simulate using full pop model
  set.seed(seed.start)
  argsSIM2 <- c(list(poppar=poppar,data=datafileName,model=modelfile,nsim=1000,
                    seed=runif(nsub,-100,100),obsNoise=rep(0,4),outname="full"),argsSIM)
  do.call("SIMrun",argsSIM2)
  #read and format the results of the simulation
  simFull <- SIMparse("full*",combine=T,silent=T)
  #filter outeq
  simFull$obs <- simFull$obs[simFull$obs$outeq==outeq,]
  #pull in time bins from tempDF
  simFull$obs$nbin <- unlist(tapply(tempDF$nbin,tempDF$id,function(x) rep(x,1000)))

  
  #make simulation number 1:nsim
  simFull$obs$simnum <- as.numeric(sapply(strsplit(simFull$obs$id,"\\."), function(x) x[1]))
  #find lower, median, upper percentiles by sim and bin
  simMed <- tapply(simFull$obs$out,list(simFull$obs$simnum,simFull$obs$nbin),FUN=median,na.rm=T) #nsim row, nbin col
  simLower <- tapply(simFull$obs$out,list(simFull$obs$simnum,simFull$obs$nbin),FUN=quantile,na.rm=T,lower) #nsim row, nbin col
  simUpper <- tapply(simFull$obs$out,list(simFull$obs$simnum,simFull$obs$nbin),FUN=quantile,na.rm=T,upper) #nsim row, nbin col

  #calculate median and CI for upper, median, and lower for each bin
  upperLower <- apply(simUpper,2,quantile,0.05,na.rm=T)[order(med2$time)]
  upperUpper <- apply(simUpper,2,quantile,0.95,na.rm=T)[order(med2$time)]
  medianLower <- apply(simMed,2,quantile,0.05,na.rm=T)[order(med2$time)]
  medianUpper <- apply(simMed,2,quantile,0.95,na.rm=T)[order(med2$time)]
  lowerLower <- apply(simLower,2,quantile,0.05,na.rm=T)[order(med2$time)]
  lowerUpper <- apply(simLower,2,quantile,0.95,na.rm=T)[order(med2$time)]
  
  #calculate time boundaries for each bin
  minBin <- tapply(tempDF$time,tempDF$bin,min)
  maxBin <- tapply(tempDF$time,tempDF$bin,max)
  nbin <- length(minBin)
  
  polytime <- c(minBin[1],rep(sapply(1:(nbin-1),function(x) mean(c(minBin[x+1],maxBin[x]))),each=2),maxBin[nbin])
  
  upperDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(upperUpper,each=2),rev(rep(upperLower,each=2))))
  medDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(medianUpper,each=2),rev(rep(medianLower,each=2))))
  lowerDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(lowerUpper,each=2),rev(rep(lowerLower,each=2))))
  

# NPDE --------------------------------------------------------------------

  #prepare data for npde
  obs <- tempDF[,c("id","time","obs")]
  #remove missing obs
  obs <- obs[obs$obs!=-99,]
  names(obs)[3] <- "out"
  
  simobs <- simFull$obs
  #remove missing simulations
  simobs <- simobs[simobs$out!=-99,]
  simobs$id <- rep(obs$id,each=1000)
  
  #get NPDE
  assign("thisobs",obs,pos=1)
  assign("thissim",simobs,pos=1)
  npdeRes <- tryCatch(autonpde(namobs=thisobs,namsim=thissim,1,2,3,verbose=T),error=function(e) e)
  
  class(simFull) <- c("PMsim","list")
  class(npdeRes) <- c("PMnpde","list")
  
  NPAGout <- list(NPdata=getName("NPdata"),
                  pop=getName("pop"),
                  post=getName("post"),
                  final=getName("final"),
                  cycle=getName("cycle"),
                  op=getName("op"),
                  cov=getName("cov"),
                  mdata=getName("mdata"),
                  npde=npdeRes,
                  sim=simFull)
  save(NPAGout,file="../outputs/NPAGout.Rdata")
  
  #put sim in global environment
  assign(paste("sim",as.character(run),sep="."),simFull,pos=1)
  

# Clean Up ----------------------------------------------------------------

  setwd(currwd)
  
  return(list(upperDF=upperDF,medDF=medDF,lowerDF=lowerDF,quant_Obs=quant_Obs,
              quant_pcObs=quant_pcObs,med2=med2,tempDF=tempDF,npdeRes=npdeRes))
  
  

} #end function

setwd("~/LAPK/Pmetrics/Examples/Runs")
setwd("~/LAPK/Pmetrics/Busulfan/Runs")
setwd("~/LAPK/Pmetrics/Voriconazole/Runs")
setwd("~/LAPK/Pmetrics/VancoBD/Runs")
setwd("~/LAPK/Pmetrics/Cefepime/Runs")



VPC <- makeVPC(run=5,limits=c(0,3),clusters=c(7,11),
               binCov=c("sex","wtkg","ageyr","ave_scr"))

p <- with(VPC,ggplot(mapping=aes(x=med2$time,
                        y=unlist(lapply(quant_pcObs,function(x) x[3])))) +
  geom_polygon(aes(x=time,y=value),data=upperDF,fill="dodgerblue",alpha=0.25) +
  geom_polygon(aes(x=time,y=value),data=medDF,fill="lightpink",alpha=0.25) +
  geom_polygon(aes(x=time,y=value),data=lowerDF,fill="dodgerblue",alpha=0.25) +
  geom_line(col="blue",lty=2,lwd=1) +
  geom_line(aes(x=med2$time,
                y=unlist(lapply(quant_pcObs,function(x) x[2]))),col="red",lty=1,lwd=1) +
  geom_line(aes(x=med2$time,
                y=unlist(lapply(quant_pcObs,function(x) x[1]))),col="blue",lty=2,lwd=1) + 
  geom_point(aes(x=time,y=pcObs),data=tempDF,col="black",pch=1) + scale_y_log10() +
   xlab("Time (h)") + ylab("Concentration mg/L") 
)
p



p2 <- with(VPC,ggplot(mapping=aes(x=med2$time,
                                 y=unlist(lapply(quant_Obs,function(x) x[3])))) +
            geom_line(col="blue",lty=2,lwd=1) + scale_x_continuous(breaks=floor(med2$time)) +
            geom_polygon(aes(x=time,y=value),data=upperDF,fill="dodgerblue",alpha=0.25) +
            geom_polygon(aes(x=time,y=value),data=medDF,fill="lightpink",alpha=0.25) +
            geom_polygon(aes(x=time,y=value),data=lowerDF,fill="dodgerblue",alpha=0.25) +
           
            geom_line(aes(x=med2$time,
                          y=unlist(lapply(quant_Obs,function(x) x[2]))),col="red",lty=1,lwd=1) +
            geom_line(aes(x=med2$time,
                          y=unlist(lapply(quant_Obs,function(x) x[1]))),col="blue",lty=2,lwd=1) + 
            geom_point(aes(x=time,y=obs),data=tempDF,col="black",pch=1) + scale_y_log10() +
            xlab("Time (h)") + ylab("Concentration mg/L") 
)
p2
