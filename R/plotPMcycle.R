
library(ggplot2)
library(gridExtra)
library(purrr)

plot.PMcycle <- function(x,x.leg=0,y.leg=1,cex.leg=1.2,omit,col,out=NA,...){
  
  #choose output
  if(inherits(out,"list")){
    if(out$type=="eps") {setEPS();out$type <- "postscript"}
    if(length(out)>1) {do.call(out$type,args=out[-1])} else {do.call(out$type,list())}
  }
  
  data <- x
  numcycles <- nrow(data$mean)
  if (missing(omit)) {omit <- floor(0.2*nrow(data$mean))} else {omit <- floor(omit*nrow(data$mean))} 
  if(omit==0) omit <- 1
  if(missing(col)) {col <- rep(c("red","blue","green","black","purple","pink","orange","brown","gold","grey"),3)
  } else { col <- rep(col,10)}
  lnty <- rep(1:3,each=10)
  x <- omit:numcycles
  if(length(data$cycnum)==0) {cycnum <- x} else {cycnum <- data$cycnum[x]}
  nvar <- ncol(data$mean)
  nout <- ncol(data$gamlam)
  #establish windows
  #par(mfrow=c(3,2))
  # This is an example
  
  # -2 x log-likelihood
  #plot(y=data$ll[omit:numcycles],x=x,type="l",xlab="Cycle",ylab="",main="-2 x Log likelihood",xaxt="n")
  graph_data <- data.frame(
    x = x,
    m_two_ll = data$ll[omit:numcycles]
  )
  p1 <- ggplot(data = graph_data, aes(x=x, y=m_two_ll)) + geom_line() + geom_point() + ggtitle("-2 x Log likelihood") + xlab("Cycle") + ylab("")
    #qplot(y=data$ll[omit:numcycles], x=x, geom=c("point", "line"), xlab = "Cycle", ylab = "") + ggtitle("-2 x Log likelihood")
  
  #axis(1,at=x,labels=cycnum)
  #AIC and BIC
  #aicbic <- c(data$aic[omit:numcycles],data$bic[omit:numcycles])
  #plot(y=aicbic,x=c(x,x),type="n",xlab="Cycle",ylab="",main="AIC/BIC",xaxt="n")
  #axis(1,at=x,labels=cycnum)
  #lines(y=data$bic[omit:numcycles],x=x,type="l",col=col[1])
  #lines(y=data$aic[omit:numcycles],x=x,type="l",col=col[2])
  #legend(x=x.leg*max(x)+omit,y=min(aicbic)+y.leg*(max(aicbic)-min(aicbic)),legend=c("BIC","AIC"),
  #       col=col[1:2],lty=lnty[1:2],lwd=2,bg="white",cex=cex.leg,
  #       x.intersp=0.8,y.intersp=0.8)   
  graph_data$aic <- data$aic[omit:numcycles]
  graph_data$bic <- data$bic[omit:numcycles]
  p2 <- ggplot(data = graph_data) + geom_line(aes(x = x, y = aic, colour = "aic")) + geom_line(aes(x = x, y = bic, colour = "bic")) + theme(legend.title = element_blank()) + ggtitle("AIC/BIC") + xlab("Cycle") + ylab("")
  
  
  #gamma/lambda
  if(is.null(nout)){
    plot(y=data$gamlam[omit:numcycles],x=x,type="l",xlab="Cycle",ylab="",main="Gamma/Lambda",xaxt="n",...)
    axis(1,at=x,labels=cycnum)
  } else {
    #plot(y=max(data$gamlam[omit:numcycles,]),x=max(x),type="n",xlab="Cycle",ylab="",main="Gamma/Lambda",
    #     xlim=range(x),ylim=range(data$gamlam[omit:numcycles,]),xaxt="n")
    #axis(1,at=x,labels=cycnum)
    #for(i in 1:nout){
    #  lines(y=data$gamlam[omit:numcycles,i],x=x,col=col[i])
    #}
    #if(nout>1){
    #  legend(x=x.leg*max(x)+omit,y=min(data$gamlam[omit:numcycles,])+y.leg*(max(data$gamlam[omit:numcycles,])-min(data$gamlam[omit:numcycles,])),
    #         legend=paste("Output",1:nout),col=col[1:nvar],lty=lnty[1:nvar],lwd=2,bg="white",cex=cex.leg,
    #         x.intersp=0.8,y.intersp=0.8)
    #}
    graph_data$gamma_lambda <- data$gamlam[omit:numcycles,]
    p3 <- ggplot(data = graph_data) + geom_line(aes(x = x, y = gamma_lambda))  + ggtitle("Gamma/Lambda") + xlab("Cycle") + ylab("")
  }
  
  
  #standardized means
  #plot(y=data$mean[omit:numcycles,],x=rep(x,nvar),xlab="Cycle",ylab="",main="Normalized Mean",type="n",xaxt="n")
  #axis(1,at=x,labels=cycnum)
  
  graph_data$mean <- data$mean[omit:numcycles,]
  graph_data$names <- data$names
  #for(i in 1:nvar){
    #lines(y=data$mean[omit:numcycles,i],x=x,col=col[i],lty=lnty[i])
  #}
  
  p4 <- reduce(1:nvar, ~.x + geom_line(aes(x=x, y= mean[,.y], colour = data$names[.y])), .init=ggplot(data = graph_data) + theme(legend.title = element_blank()) + ggtitle("Normalized Mean") + xlab("Cycle") + ylab(""))
  #legend(x=x.leg*max(x)+omit,y=min(data$mean[omit:numcycles,])+y.leg*(max(data$mean[omit:numcycles,])-min(data$mean[omit:numcycles,])),
  #       legend=data$names,col=col[1:nvar],lty=lnty[1:nvar],lwd=2,bg="white",cex=cex.leg,
  #       x.intersp=0.8,y.intersp=0.8)
  
  
  
  
  #standardized SD
  if(!all(is.na(data$sd))){
    plot(y=data$sd[omit:numcycles,],x=rep(x,nvar),xlab="Cycle",ylab="",main="Normalized SD",type="n",xaxt="n",...)
    axis(1,at=x,labels=cycnum)
    for(i in 1:nvar){
      lines(y=data$sd[omit:numcycles,i],x=x,col=col[i],lty=lnty[i])
    }
    legend(x=x.leg*max(x)+omit,y=min(data$sd[omit:numcycles,])+y.leg*(max(data$sd[omit:numcycles,])-min(data$sd[omit:numcycles,])),
           legend=data$names,col=col[1:nvar],lty=lnty[1:nvar],lwd=2,bg="white",cex=cex.leg,
           x.intersp=0.8,y.intersp=0.8)
  } else {
    plot(y=data$median[omit:numcycles,],x=rep(x,nvar),xlab="Cycle",ylab="",main="Normalized SD",type="n",xaxt="n",...)
    axis(1,at=x,labels=cycnum)
    text("Initial standard deviation = 0\nAssay error may be too large.",x=median(c(omit,numcycles)),y=median(data$median[omit:numcycles,]),col="gray50",cex=2)
  }
  #standardized median
  plot(y=data$median[omit:numcycles,],x=rep(x,nvar),xlab="Cycle",ylab="",main="Normalized Median",type="n",xaxt="n",...)
  axis(1,at=x,labels=cycnum)
  for(i in 1:nvar){
    lines(y=data$median[omit:numcycles,i],x=x,col=col[i],lty=lnty[i])
  }
  legend(x=x.leg*max(x)+omit,y=min(data$median[omit:numcycles,])+y.leg*(max(data$median[omit:numcycles,])-min(data$median[omit:numcycles,])),
         legend=data$names,col=col[1:nvar],lty=lnty[1:nvar],lwd=2,bg="white",cex=cex.leg,
         x.intersp=0.8,y.intersp=0.8)
  
  par(mfrow=c(1,1))
  
  #close device if necessary
  if(inherits(out,"list")) dev.off()
  
  grid.arrange(p1, p2, p3, p4, nrow=2)
  
  return(invisible(1))
}

