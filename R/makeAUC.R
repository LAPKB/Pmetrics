#' Calculates AUC from a variety of inputs
#'
#' \code{makeAUC} will calculate the area under the time concentration curve using the
#' trapezoidal approximation from a variety of inputs.  If a PMpost, PMop, or PMsim object is specified, 
#' \code{formula} is not required.  AUCs from PMop objects are based on observations.
#' For AUCs based on predictions, use a PMpost object.
#'
#' @title Calculation of AUCs
#' @param formula A formula of the form \code{obs ~ time}.  This is only required with data that is not of class PMpop, PMpost, PMop or PMsim.
#' @param data A suitable data object of the \emph{PMpop} class (see \code{\link{makePop}}),
#' \emph{PMpost} class (see \code{\link{makePost}}),
#' \emph{PMop} class (see \code{\link{makeOP}}),
#' the \emph{PMsim} class (see \code{\link{SIMparse}}), or some other suitable dataframe
#' with at least time/observation columns referred to by \code{formula}, with an \dQuote{id} column (so named) if necessary.
#' @param include A vector of subject IDs to include in the AUC calculations, e.g. c(1:3,5,15)
#' @param exclude A vector of subject IDs to exclude in the AUC calculations, e.g. c(4,6:14,16:20)
#' @param start Specify the time to begin AUC calculations. Default is 0.
#' @param end Specify the time to end AUC calculations so that AUC is calculated
#' from \code{start} to \code{end}.  Default for end is the maximum observation
#' time for each subject.  Subjects with insufficient data for a specified interval will have
#' AUC calculated for the available data, not to exceed the specified interval.
#' @param icen Only relevant for PMpost or PMpop objects which have predictions based on median or mean of each
#' subject's Bayesian posterior parameter distribution.  Default is "median", but could be "mean".
#' @param outeq Specify which output equation is to be used.  Default is 1.
#' @param block Specify which observation block (separated by EVID=4) is to be used for each subject.  Default is 1.
#' @param method Default is "linear" for AUC trapezoidal calculation.  Any other value will result in
#' linear up, log down.
#' @param addZero Boolean to add a zero concentration at time 0. Default is \code{FALSE}.
#' @return The output of \code{makeAUC} is a dataframe of class \emph{PMauc},
#' which has 2 columns:
#' \item{id }{subject identification}
#' \item{tau }{AUC from \code{start} to \code{end}}
#' @author Michael Neely
#' @seealso \code{\link{makeOP}}, \code{\link{SIMparse}}
#' @examples
#' data(NPdata.1)
#' op <- makeOP(NPdata.1)
#' makeAUC(op)
#' @export

makeAUC <- function(data,formula,include,exclude,start=0,end=Inf,icen="median",
                    outeq=1,block=1,method="linear",addZero=F){
  
  #auc function
  get_auc <- function(df, addZero, method) {
    if (addZero & !any(df$time == 0)) df <- rbind(data.frame(time=0, out=0), df)
    N <- nrow(df)
    if(N <= 1) return(data.frame(NA, NA))
    #(t_i - t_i-1)
    diffTimes <- diff(df$time)
    #(C_i + C_i-1)
    sumConc <- df$out[-1] + df$out[-N]
    #(C_i - C_i-1)
    diffConc <- diff(df$out)
    #log(C_i/C_i-1)
    logConc <- log(df$out[-1]/df$out[-N])
    #tmax
    tmax <- which(df$out==max(df$out))
    
    #auc
    if(method=="linear"){
      auc <- 0.5 * sum(diffTimes * sumConc)
    } else {
      auc <- sum(
        #linear up
        0.5 * sum ( diffTimes[1:tmax] * sumConc[1:tmax]),
        #log down
        sum(diffTimes[(tmax+1):(N-1)] * diffConc[(tmax+1):(N-1)]/logConc[(tmax+1):(N-1)])
      )
    }
    
    # sumScaleConc <- df$out[-1] * df$time[-1] + df$out[-N] * df$time[-N]
    # aumc <- 0.5 * sum( diffTimes * sumScaleConc)
    return(auc)
  }
  
  #handle objects
  if(missing(data)) stop("Please supply a data object.\n")
  
  if (inherits(data,"PMsim")){
    data2 <- data$obs
  }
  
  if(inherits(data,"PMop")){
    if(inherits(data,"list")) {data2 <- data[[2*outeq-1]]} else {data2 <- data}
    data2 <- data2 %>% mutate(out = obs)
  }
  
  if (inherits(data,c("PMpost","PMpop"))){
    data2 <- data %>% mutate(out = pred)
  }
  
  if(!inherits(data,c("PMsim","PMpost","PMpop","PMop"))){ #needs formula
    if (missing(formula)) stop("\nPlease supply a formula of form 'out~time' for objects other than class PMsim, PMpost or PMop.")
    y.name <- as.character(attr(terms(formula),"variables")[2])
    x.name <- as.character(attr(terms(formula),"variables")[3])
    if (length(grep(y.name,names(data)))==0) {stop(paste("\n'",y.name,"' is not a variable in the data.",sep=""))}
    if (length(grep(x.name,names(data)))==0) {stop(paste("\n'",x.name,"' is not a variable in the data.",sep=""))}
    data2 <- data %>% mutate(time = get(x.name), out = get(y.name))
  }
  
  if(!"id" %in% names(data2)) data2$id <- 1 #add id if missing
  if(!"outeq" %in% names(data2)) data2$outeq <- 1 #add outeq if missing
  if(!"block" %in% names(data2)) data2$block <- 1 #add block if missing
  if(!"icen" %in% names(data2)) data2$icen <- "median" #add icen if missing
  if(missing(include)) include <- unique(data2$id)
  if(missing(exclude)) exclude <- NA

  #make object to pass to auc calculation
  data3 <- tibble::tibble(data2) %>%
    filter(outeq==outeq,
           block==block,
           id %in% include,
           !id %in% exclude,
           time>=start & time<=end,
           icen==icen,
           !is.na(out)
    ) %>%
    select(id,time,out) %>%
    group_by(id)

  #calculate AUC
  AUCdf <- tibble(id=unique(data3$id),
                  tau = group_map(data3, ~get_auc(.x,addZero,method)) %>% unlist())
  class(AUCdf) <- c("PMauc",class(AUCdf))
  return(AUCdf)
  
}

