#' Calculates AUC from a variety of inputs
#'
#' Calculates the area under the time concentration curve using the
#' trapezoidal approximation from a variety of inputs.  
#' If a PM_pop, PM_post, PM_op, or PMsim object is specified, 
#' `formula` is not required.
#'
#' @title Calculation of AUCs
#' @param formula A formula of the form `obs ~ time`.  
#' This is only required with data that is not of class PM_pop, PM_post, PM_op or PM_sim.
#' @param data A suitable data object, i.e. [PM_pop], [PM_post], [PM_op],
#' [PM_sim], or some other suitable dataframe
#' with at least time/observation columns referred to by `formula`, with an
#'  "id" column (so named) if necessary.
#' @param include `r template("include")`
#' @param exclude `r template("exclude")`
#' @param start Specify the time to begin AUC calculations. Default is 0.
#' @param end Specify the time to end AUC calculations so that AUC is calculated
#' from `start` to `end`.  Default for end is the maximum observation
#' time for each subject.  Subjects with insufficient data for a specified interval will have
#' AUC calculated for the available data, not to exceed the specified interval.
#' @param icen `r template("icen")` Only relevant for PMpost or PMpop objects.
#' @param outeq `r template("outeq")`
#' @param block `r template("block")`
#' @param method Default is "linear" for AUC trapezoidal calculation.  Any other value will result in
#' linear up, log down.
#' @param addZero Boolean to add a zero concentration at time 0. Default is \code{FALSE}.
#' @return A dataframe of class *PMauc*, which has 2 columns:
#' * id - Subject identification
#' * tau - AUC from `start` to `end`
#' @author Michael Neely
#' @seealso [PM_result], [PM_sim], [PM_op]
#' @examples
#' NPex$op$auc()
#' Indometh %>% mutate(id = Subject) %>% makeAUC(conc ~ time)
#' @export

makeAUC <- function(data,
                    formula,
                    include, exclude,
                    start = 0, end = Inf,
                    icen = "median",
                    outeq = 1, block = 1,
                    method = "linear",
                    addZero = F){
  

  #handle objects
  if(missing(data)) stop("Please supply a data object.\n")
  
  data_class <- which(
    inherits(data, c("PMsim", "PM_sim", 
                     "PMop", "PM_op", 
                     "PMpop", "PM_pop",
                     "PMpost", "PM_post",
                     "PM_data", "PMmatrix"), which = T) > 0
  ) #will be all zeros except matching class, undefined if none
  
  if(length(data_class)>0){ #there was a match
    data2 <- switch(data_class,
                    data$obs, #PMsim
                    data$obs, #PM_sim
                    data %>% mutate(out = obs), #PMop
                    data$data %>% mutate(out = obs), #PM_op
                    data %>% mutate(out = pred), #PMpop
                    data$data %>% mutate(out = pred), #PM_pop
                    data %>% mutate(out = pred), #PMpost
                    data$data %>% mutate(out = pred), #PM_post
                    data$standard_data %>% 
                      makePMmatrixBlock() %>%
                      filter(!is.na(out)), #PM_data
                    data %>% 
                      makePMmatrixBlock() %>%
                      filter(!is.na(out)), #PMmatrix
                    )
  } else { #class not matched, so needs formula
    if (missing(formula)) stop("\nPlease supply a formula of form 'out~time' for objects other than class PMsim, PMpost or PMop.")
    y.name <- as.character(attr(terms(formula),"variables")[2])
    x.name <- as.character(attr(terms(formula),"variables")[3])
    if (length(grep(y.name,names(data)))==0) {stop(paste("\n'",y.name,"' is not a variable in the data.",sep=""))}
    if (length(grep(x.name,names(data)))==0) {stop(paste("\n'",x.name,"' is not a variable in the data.",sep=""))}
    data2 <- data %>% dplyr::mutate(time = get(x.name), out = get(y.name))
  }

  
  #create dummy variables if missing
  if(!"id" %in% names(data2)) data2$id <- 1 #add id if missing
  if(!"outeq" %in% names(data2)) data2$outeq <- 1 #add outeq if missing
  if(!"block" %in% names(data2)) data2$block <- 1 #add block if missing
  if(!"icen" %in% names(data2)) data2$icen <- "median" #add icen if missing
  if(missing(include)) include <- unique(data2$id)
  if(missing(exclude)) exclude <- NA

  #filter to create object to pass to auc calculation
  data3 <- data2 %>%
    dplyr::filter(outeq == !!outeq,
           block == !!block,
           id %in% include,
           !id %in% exclude,
           time>=start & time<=end,
           icen == !!icen,
           !is.na(out)
    ) %>%
    dplyr::select(id,time,out) %>%
    dplyr::group_by(id)
  
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
    
    return(auc)
  }
  
  #calculate AUC
  AUCdf <- tidyr::nest(data3, pk = !id) %>% 
    dplyr::mutate(tau = sapply(pk, get_auc, addZero, method)) %>%
    dplyr::select(id, tau)
  
  class(AUCdf) <- c("PMauc",class(AUCdf))
  
  #reorder according to original
  AUCdf <- AUCdf[match(unique(data3$id),AUCdf$id),]
  
  return(AUCdf)
}

