#' Gets creatinine clearance as estimated by the Jelliffe equation.
#'
#' The equation depends on age, sex, weight, and serum creatinine.
#' ESS = wt * (29.3 - (0.203 * age)) for males
#' ESS = wt * (25.1 - (0.175 * age)) for females
#' scrAve = (Scr1 + Scr2) / 2
#' ESS_cor = ESS * (1.035 - (0.0337 * scrAve))
#' E = ESS_cor - 4 * wt * (Scr2 - Scr1) / (time2 - time1)
#' CRCL = E / (14.4 * scrAve) in ml/min/1.73m^2
#'
#' @title Add Jelliffe Creatinine Clearance
#' @param mdata A Pmetrics matrix data object
#' @param idCol A character vector with the name
#'  of the id column in \code{mdata}. The default is \dQuote{id}.
#' @param wtCol A character vector with the name of the weight
#'  column in \code{data}.  The default is \dQuote{wt}.
#' @param maleCol A character vector with the name of the gender column in \code{mdata}.  
#' Male should be 1 and female should be 0. The default is \dQuote{male}.
#' @param ageCol A character vector with the name of the age column in \code{mdata}.
#' The default is \dQuote{age}.
#' @param scrCol A character vector with the name of the serum creatinine column in \code{mdata}.  
#' Default units are mg/dL, and the the default name is \dQuote{scr}.
#' @param SI Boolean value, if true, will expect serum creatinine to be in micromol/L.
#' Default is \code{FALSE}.
#' @return A vector of length \code{nrow(mdata)} with Jelliffe CRCL values for every dose in \code{mdata}. Vector values
#' for observation events in \code{mdata} are NA.
#' @author Michael Neely
#' @export

PMgetCRCL <- function(mdata,idCol="id",wtCol="wt",maleCol="male",ageCol="age",scrCol="scr",SI=F){
  dataCols <- names(mdata)  #check to make sure names ok
  if(!all(c(idCol,wtCol,maleCol,ageCol,scrCol) %in% dataCols)){stop("Please provide column names for id, wt, male, age and scr\n")}
  if(SI) mdata[,scrCol] <- mdata[,scrCol]/88.4 #convert
  calcCRCL <- function(temp){
    #first sub temp for dose rows
    doseRows <- which(temp$evid!=0)
    tempDose <- temp[doseRows,]
    ndose <- nrow(tempDose)
    #sex doesn't change
    tempDose[,maleCol] <- tempDose[1,maleCol]
    #find last covariate entry for each of the others and carry forward
    last <- sapply(c(scrCol,ageCol,wtCol),function(x) max(which(!is.na(tempDose[,x]))))
    tempDose[last[1]:ndose,scrCol] <-  tempDose[last[1],scrCol]
    tempDose[last[2]:ndose,ageCol] <-  tempDose[last[2],ageCol]
    tempDose[last[3]:ndose,wtCol] <-  tempDose[last[3],wtCol]
    #find missing and interpolate
    missing <- lapply(c(scrCol,ageCol,wtCol),function(x) which(is.na(tempDose[,x])))
    interp <- function(x,y){
      interpVal <- approx(tempDose$time[!is.na(tempDose[,y])],tempDose[,y][!is.na(tempDose[,y])],xout=tempDose$time[x])
      return(interpVal$y)
    }
    if(length(missing[[1]])>0) tempDose[missing[[1]],scrCol] <- sapply(missing[[1]],function(x) suppressWarnings(interp(x,y=scrCol)))
    if(length(missing[[2]])>0) tempDose[missing[[2]],ageCol] <- sapply(missing[[2]],function(x) suppressWarnings(interp(x,y=ageCol)))
    if(length(missing[[3]])>0) tempDose[missing[[3]],wtCol] <- sapply(missing[[3]],function(x) suppressWarnings(interp(x,y=wtCol)))
    
    #calculate crcl
    
    if(tempDose[1,maleCol]==1){ #male
      ess <- tempDose[,wtCol]*(29.3-(0.203*tempDose[,ageCol]))
      
    } else { #female
      ess <- tempDose[,wtCol]*(25.1-(0.175*tempDose[,ageCol]))
    }
    if(ndose>1){
      scrAve <- c(sapply(1:(ndose-1),function(x) mean(tempDose[c(x,x+1),scrCol])),tempDose[ndose,scrCol])
    } else {scrAve <- tempDose[,scrCol]}
    essCorr <- ess*(1.035-(0.0337*scrAve))
    
    E <- essCorr - 4*tempDose[,wtCol]*c(0,diff(tempDose[,scrCol])/(diff(tempDose$time)/24))
    E[E==Inf | E==-Inf] <- NA #remove -Inf due to double EVID=4
    temp$crcl <- NA
    temp$crcl[doseRows] <- E/(14.4*scrAve) 
    return(temp$crcl) 
  }
  crcl <- unlist(by(mdata,mdata$id,calcCRCL))
  return(crcl)
}

