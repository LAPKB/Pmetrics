
#get midpoint of each bin

binUp <- function(x,target.bins,minpts=10,start=0,end){
  if(missing(end)) end <- max(x$time)
  if(inherits(x,"PMmatrix")){
    binnedTimes <- bins(x$time[x$time>=start & x$time<=end & x$evid==0],target.bins=target.bins,minpts=minpts)
  }
  
  if(inherits(x,"PMsim")){
    binnedTimes <- bins(x$time[x$time>=start & x$time<=end],target.bins=target.bins,minpts=minpts)
  }
  
  binvals <- bins.getvals(binnedTimes)
  valDF <- data.frame(lo=attr(binvals,"binlo"),hi=attr(binvals,"binhi"))
  binNumber <- sapply(x$time,function(y) which(valDF$lo<=y & valDF$hi>=y))
  binNumber <-  sapply(binNumber,function(z) if(length(z)==0){z <- NA} else {z <- z})
  binMeans <- rep(NA,max(binNumber,na.rm=T))
  for(i in 1:max(binNumber,na.rm=T)){
    binMeans[i] <- mean(x$time[which(binNumber==i)],na.rm=T)
  }
  x$binTime <- binMeans[binNumber]
  return(x)
}



mdata.1b <- binUp(mdata.1,7,minpts=5,start=120)
mdata.1b$time[mdata.1b$evid==0] <- mdata.1b$binTime[mdata.1b$evid==0]
plot(mdata.1b,join=F,pch=3,xlim=c(120,144),main="Binned")
plot(mdata.1,join=F,pch=3,xlim=c(120,144),main="Original")

mdata.34b <- binUp(mdata.34,9,end=24)
mdata.34b$time[mdata.34b$evid==0] <- mdata.34b$binTime[mdata.34b$evid==0]
plot(mdata.34b,join=F,pch=3,xlim=c(0,24),main="Binned")
plot(mdata.34,join=F,pch=3,xlim=c(0,24),main="Original")
