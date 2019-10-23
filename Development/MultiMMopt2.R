MMopt <- function(mdata,nsamp=1,weight=c("none","AUC")){
  
  #transform into format for MMopt
  #nsubs is the number of subjects
  nsubs <- nrow(mdata)
  #time is the simulated times
  time <- as.numeric(names(mdata))
  #nout is the number of simulated times (outputs)
  nout <- length(time)
  #Mu is a matrix of nout rows x nsubs columns containing the outputs at each time
  Mu <- t(mdata)
  
  #pH is the vector of probabilities of each population point
  pH <- 1/nrow(mdata)
  #replicate pH and normalize based on number of simulation templates
  pH <- rep(pH,nsubs)

  numeqt <- 1
  
  mdata2 <- data.frame(id=rep(1:nsubs,each=nout),time=rep(time,nsubs),out=unlist(c(mdata)))
  auc <- makeAUC(mdata2,out~time)$tau
  #set the assay error 
  cassay <- c(0.5*min(Mu[Mu>0],na.rm=T),0.1,0,0)
  
  #make the weighting Matrix
  #default is no penalites (diag=0, off-diag=1)
  C <- matrix(1,nrow=nsubs,ncol=nsubs)
  diag(C) <- 0
  
  weight <- tolower(match.arg(weight))
  if(weight=="auc"){
    sqdiff <- sapply(1:nsubs,function(x) (auc[x] - auc)^2 )
    C <- matrix(sqdiff,nrow=nsubs,ncol=nsubs)
  }
  
  # Call MMMOPT1 routine to compute optimal sampling times
  mmopt1 <- wmmopt1(Mu,time,pH,cassay,nsamp,nsubs,nout,C);
  optsamp <- mmopt1$optsamp
  brisk <- mmopt1$brisk_cob
  optindex <- mmopt1$optindex
  Cbar <- mmopt1$Cbar
  
  
  # ---------------------------
  
  
  
  mmopt <- list(sampleTime=optsamp[1:nsamp,nsamp],
                bayesRisk=brisk[nsamp],Cbar=Cbar,
                simdata=simdata)
  class(mmopt) <- c("MMopt","list")
  return(mmopt)
  
  #   # display Results
  #   nout<-dim(Mu)[1];
  #   nsubs<-dim(Mu)[2];
  #   print('---------------------------')
  #   print('Background')
  #   print('Assay Polynomial sigma= c0 + c1*y + c2*y^2 + c3*y^3')
  #   print('[c0,c1,c2,c3]')
  #   print(cassay)
  #   #print('Bayesian Prior: Uniform')
  #   print('---------------------------')
  #   print('Optimal Designs')
  #   print('One Sample Design (hr)')
  #   print(optsamp[1,1])
  #   print('Two Sample Design (hr)')
  #   print(optsamp[1:2,2])
  #   print('Three Sample Design (hr)')
  #   print(optsamp[1:3,3])
  #   print('Four Sample Design (hr)')
  #   print(optsamp[1:4,4])
  #   print('Bayes Risk Overbound')
  #   #print(brisk)
  #   print(paste('1-Sample:   ', as.character(brisk[1])))
  #   print(paste('2-Sample:   ', as.character(brisk[2])))
  #   print(paste('3-Sample:   ', as.character(brisk[3])))
  #   print(paste('4-Sample:   ', as.character(brisk[4])))
  #   # --------------------------------------
  #   # Plot All Responses
  #   timediv=time%*%matrix(1,1,nsubs);
  #   dev.set(1)
  #   matplot(timediv,Mu,type = 'l',main='All Model Responses',xlab='hr',ylab='Conc',lty=1)
  #   grid()
  #   # --------------------------------------
  #   # plot Bayesian Prior Weights
  #   dev.set(2)
  #   matplot(pH,pch=4,type="b",main='Bayesian Prior Weights',xlab= 'Model Index',ylab='Prob',lty=1)
  #   grid()
  #   # --------------------------------------
  #   # Plot Results with 3-sample MMopt design
  #   timediv=time%*%matrix(1,1,nsubs);
  #   dev.set(3)
  #   matplot(timediv,Mu,type = 'l',main='All Model Responses with 3-Sample Design=(triangle,square,o)',xlab='hr',ylab='Conc',lty=1)
  #   vtime1<-matrix(time[optindex[1,3]],1,nsubs)
  #   points(vtime1,Mu[optindex[1,3], ],pch=6)
  #   vtime2<-matrix(time[optindex[2,3]],1,nsubs)
  #   points(vtime2,Mu[optindex[2,3],],pch=5)
  #   vtime3<-matrix(time[optindex[3,3]],1,nsubs)
  #   points(vtime3,Mu[optindex[3,3],],pch=1)
  #   grid()
  
}

setwd("~/LAPK/Pmetrics/Isoniazid/src")
library(openxlsx)
mdata.orig <- read.xlsx("INHBUFF3 071306.xlsx")
mdata <- mdata.orig[,16:32] #all
mdata <- mdata.orig[,16:28] #first part
mdata <- mdata.orig[,25:32] #second part
mdata <- mdata.orig[,18:29] #overlap part

#mdata[,1] <- 0

miss <- which(is.na(mdata),arr.ind=T)
for(i in 1:nrow(miss)){
  thisX <- miss[i,1]
  thisY <- miss[i,2]
  mdata[thisX,thisY] <- approx(mdata[(thisX-1):(thisX+1),thisY],n=3)$y[2]
}

mmopt <- MMopt(mdata,nsamp=5,weight="AUC")
mmopt$sampleTime

plot(x=rep(as.numeric(names(mdata)),each=31),y=unlist(c(mdata)))

library(Power2Stage)

p <- power.2stage(method="B",n1=12,GMR=0.95,CV=0.16)
p

TRTA <- c(3.3,5.3,6.2,2.0,3.1,0.8,1.6,11.8,4.1,1.7,4.4,3.1)
TRTB <- c(2.9,6.1,7.1,1.2,3.2,0.8,1.5,6.9,3.2,1.2,3.5,2.4)
sd(log(TRTA/TRTB))/mean(log(TRTA/TRTB))
