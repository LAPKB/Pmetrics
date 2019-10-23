

plot.PMvalid <- function(x,type="vpc",lower=0.025,upper=0.975,
                         log=F,pch.obs = 1,col.obs="black",cex.obs=1,theme="color",
                         col.obs.ci="blue",col.obs.med="red",col.sim.ci="dodgerblue",col.sim.med="lightpink"
                         
){
  
  if(length(grep("ggplot2",installed.packages()[,1]))==0){
    install.packages("ggplot2",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  ggplot2.installed <- require(ggplot2,quietly=T,warn.conflicts=F)
  if(!ggplot2.installed) stop("Package ggplot2 not installed.")
  
  #calculate lower, 50th and upper percentiles for pcYij by time bins
  quant_pcObs <- tapply(x$opDF$pcObs,x$opDF$nbin,quantile,probs=c(lower,0.5,upper),na.rm=T)
  #calculate lower, 50th and upper percentiles for Yij by time bin
  quant_Obs <- tapply(x$opDF$obs,x$opDF$nbin,quantile,probs=c(lower,0.5,upper),na.rm=T)
  
  #find lower, median, upper percentiles by sim and bin
  simMed <- tapply(x$simdata$obs$out,list(x$simdata$obs$simnum,x$simdata$obs$nbin),FUN=median,na.rm=T) #nsim row, nbin col
  simLower <- tapply(x$simdata$obs$out,list(x$simdata$obs$simnum,x$simdata$obs$nbin),FUN=quantile,na.rm=T,lower) #nsim row, nbin col
  simUpper <- tapply(x$simdata$obs$out,list(x$simdata$obs$simnum,x$simdata$obs$nbin),FUN=quantile,na.rm=T,upper) #nsim row, nbin col
  
  #calculate median and CI for upper, median, and lower for each bin
  upperLower <- apply(simUpper,2,quantile,lower,na.rm=T)[order(x$bintimes$time)]
  upperUpper <- apply(simUpper,2,quantile,upper,na.rm=T)[order(x$bintimes$time)]
  medianLower <- apply(simMed,2,quantile,lower,na.rm=T)[order(x$bintimes$time)]
  medianUpper <- apply(simMed,2,quantile,upper,na.rm=T)[order(x$bintimes$time)]
  lowerLower <- apply(simLower,2,quantile,lower,na.rm=T)[order(x$bintimes$time)]
  lowerUpper <- apply(simLower,2,quantile,upper,na.rm=T)[order(x$bintimes$time)]
  
  #calculate time boundaries for each bin
  minBin <- tapply(x$opDF$time,x$opDF$bin,min)
  maxBin <- tapply(x$opDF$time,x$opDF$bin,max)
  nbin <- length(minBin)
  
  #polytime <- c(minBin[1],rep(sapply(1:(nbin-1),function(x) mean(c(minBin[x+1],maxBin[x]))),each=2),maxBin[nbin])
  polytime <- x$bintimes$time
  
  # upperDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(upperUpper,each=2),rev(rep(upperLower,each=2))))
  # medDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(medianUpper,each=2),rev(rep(medianLower,each=2))))
  # lowerDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(lowerUpper,each=2),rev(rep(lowerLower,each=2))))
  upperDF <- data.frame(time=c(polytime,rev(polytime)),value=c(upperUpper,rev(upperLower)))
  medDF <- data.frame(time=c(polytime,rev(polytime)),value=c(medianUpper,rev(medianLower)))
  lowerDF <- data.frame(time=c(polytime,rev(polytime)),value=c(lowerUpper,rev(lowerLower)))
  
  
  if(type=="vpc"){ plotData <- list(obsQuant=quant_Obs,obs=x$opDF$obs,binTime=x$bintimes$time,
                                 obsTime=x$opDF$time,upperDF=upperDF,lowerDF=lowerDF,
                                 medDF=medDF)
  }
  if(type=="pcvpc"){ plotData <- list(obsQuant=quant_pcObs,obs=x$opDF$pcObs,binTime=x$bintimes$time,
                                   obsTime=x$opDF$bintime,upperDF=upperDF,lowerDF=lowerDF,
                                   medDF=medDF)
  }
  #set the scale for the y-axis
  if(log){scaleY <- scale_y_log10()} else (scaleY <- scale_y_continuous())
  #override colors to make greyscale
  if(theme=="grey"|theme=="gray"){ #set to grayscale
    col.obs <- "black"
    col.obs.ci <- "grey20"
    col.obs.med <- "grey20"
    col.sim.ci <- "grey75"
    col.sim.med <- "grey50"
  }
  
  #GENERATE THE PLOT
  p <- with(plotData,
            ggplot(mapping=aes(x=binTime,y=unlist(lapply(obsQuant,function(x) x[3])))) +
              geom_line(col=col.obs.ci,lty=2,lwd=1) + scale_x_continuous(breaks=floor(binTime)) +
              geom_polygon(aes(x=time,y=value),data=upperDF,fill=col.sim.ci,alpha=0.25) +
              geom_polygon(aes(x=time,y=value),data=medDF,fill=col.sim.med,alpha=0.25) +
              geom_polygon(aes(x=time,y=value),data=lowerDF,fill=col.sim.ci,alpha=0.25) +
              geom_line(aes(x=binTime,
                            y=unlist(lapply(obsQuant,function(x) x[2]))),col=col.obs.med,lty=1,lwd=1) +
              geom_line(aes(x=binTime,
                            y=unlist(lapply(obsQuant,function(x) x[1]))),col=col.obs.ci,lty=2,lwd=1) + 
              geom_point(aes(x=obsTime,y=obs),col=col.obs,pch=pch.obs,cex=cex.obs) + scaleY +
              xlab("Time (h)") + ylab("Concentration mg/L") 
  )
  #SEND TO CONSOLE
  print(p)
  
  
  if(type=="npde"){
    plot(x$npde)
    par(mfrow=c(1,1))
  }
  
}