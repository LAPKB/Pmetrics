#' Extract CDC pediatric growth charts
#'
#' Will extract height and weight for boys, girls or both for a given range of ages in months and percentile. This can be useful for 
#' simulations in Pmetrics.
#'
#' @param sex A single quoted character: \dQuote{M} for males, \dQuote{F} for females, or \dQuote{B} for both, in which case an average
#' of the two sexes will be returned.  Default is \dQuote{M}.
#' @param percentile An integer of the percentile for each age/sex to return.  Default is 5.
#' @param agemos The ages in months to return.  The default is \code{seq(0,18)*12}, i.e. 1 to 18 years.
#' @return A dataframe with columns
#'  \item{age }{Age in months}
#'  \item{wt }{Weight in kilograms}
#'  \item{ht }{Height or length in centimeters}
#'  \item{sex }{The selected \code{sex}}
#'  \item{percentile }{The selected \code{percentile}}
#' @author Michael Neely
qgrowth <- function(sex=c("M","F","B"),percentile=c("5","10","25","50","75","90","95"),agemos=(seq(0,18)*12)){
  data(growth)
  percentile <- as.numeric(match.arg(percentile))
  sex <- match.arg(sex)
  if(sex=="b" | sex=="B") {sex <- c("M","F")} else{sex <- toupper(sex)}
  final <- data.frame(age=NA,wt=NA,ht=NA,sex=NA,percentile=NA)
  for(j in sex){
    sub1 <- growth[which(growth$SEX==j & (growth$CHART=="wt  x age" | growth$CHART=="length x age"
                                          | growth$CHART=="ht x age") & growth$PERCENTILE==percentile),]
    sub2 <- data.frame(age=agemos)
    wt <- vector("numeric")
    for (i in 1:length(sub2$age)){
      temp <- sub1[which(sub1$CHART=="wt  x age" & sub1$AGE==c("0-36 mos", "2-18 years")[1+as.numeric(agemos[i]>36)]),]
      temp2 <- max(which(temp$KNOT<=agemos[i]))
      wt[i] <- temp$A[temp2] + temp$B1[temp2]*(agemos[i]-temp$KNOT[temp2]) + temp$B2[temp2]*(agemos[i]-temp$KNOT[temp2])**2 + temp$B3[temp2]*(agemos[i]-temp$KNOT[temp2])**3
    }
    
    ht <- vector("numeric")
    for (i in 1:length(sub2$age)){
      temp <- sub1[which(sub1$CHART==c("length x age","ht x age")[1+as.numeric(agemos[i]>36)] & sub1$AGE==c("0-36 mos", "2-18 years")[1+as.numeric(agemos[i]>36)]),]
      temp2 <- max(which(temp$KNOT<=agemos[i]))
      ht[i] <- temp$A[temp2] + temp$B1[temp2]*(agemos[i]-temp$KNOT[temp2]) + temp$B2[temp2]*(agemos[i]-temp$KNOT[temp2])**2 + temp$B3[temp2]*(agemos[i]-temp$KNOT[temp2])**3
    }
    sub2 <- cbind(sub2,wt,ht)
    sub2$sex <- j
    sub2$percentile <- percentile
    final <- rbind(final,sub2)
  }
  final <- final[-1,]
  if(length(sex)==2){
    final.nrow <- nrow(final)
    final.1 <- final[1:(final.nrow/2),]
    final.2 <- final[(1+final.nrow/2):final.nrow,]
    final.3 <- data.frame(age=final.1$age,wt=(final.1$wt+final.2$wt)/2,ht=(final.1$ht+final.2$ht)/2,sex="B",percentile=percentile)
    final <- final.3
  }

  return(final)  
}
