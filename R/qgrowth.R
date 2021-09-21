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
#' @export

qgrowth <- function(sex=c("M","F","B"),percentile=c("5","10","25","50","75","90","95"),agemos=(seq(0,18)*12)){
  
  growth <- NULL
  data(growth,envir = environment())
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

#' Extract CDC pediatric BMI z-scores
#'
#' Will extract BMI z-scores based on age in months and sex. Overweight is 
#' a z-score of >1.04, and obese is a z-score > 1.64. Calculations are based on
#' CDC data/formulae as found here: \link{https://www.cdc.gov/nccdphp/dnpa/growthcharts/resources/biv-cutoffs.pdf}
#'
#' @param agemos The age in months.  Should be between 24 and 240.5.
#' @param sex A single quoted character: \dQuote{M} for males, \dQuote{F} for females.  Default is \dQuote{M}.
#' @param bmi The individual's BMI.
#' @return A list with objects calculated for \code{agemos} and \code{sex}.
#'  \item{z }{Z-score}
#'  \item{mod_z }{Modified Z-score for extreme BMI}
#'  \item{per }{BMI percentile}
#'  \item{mod_per }{Modifed BMI percentile}}
#' @author Michael Neely
#' @export

zBMI <- function(agemos, sex, bmi){
  if(agemos < 24 | agemos > 240.5){
    stop("Agemos should be >=24 or <=240.5.")
  }
  
  if(agemos >= 24.5){
    agemos <- floor(agemos) + 0.5
  } else {agemos <- floor(agemos)}
  
  #all_bmi <- NULL
  #data(all_bmi,envir = environment())
  if(is.character(sex)){
    sex_var <- 1 + as.numeric(tolower(sex) == "f")
  } else {stop("Use 'M' for male and 'F' for female (case insensitive).")}
  
  this_bmi <- all_bmi %>% filter(Agemos == agemos & Sex == sex_var)
  z_bmi <- ((bmi/this_bmi$M)^this_bmi$L -1) / (this_bmi$L * this_bmi$S) #z score
  
  #find values for modified z score to correct for extremes
  z0 <- this_bmi$M #median
  z2 <- this_bmi$M * (1 + this_bmi$L * this_bmi$S * 2)**(1 / this_bmi$L)
  z_dist <- (z2 - z0)/2
  mod_z_bmi <- (bmi - z0) / z_dist
  
  #return
  return(list(z = z_bmi, mod_z = mod_z_bmi, 
              per = pnorm(z_bmi,0,1), mod_per = pnorm(mod_z_bmi,0,1)))
  
 
}

