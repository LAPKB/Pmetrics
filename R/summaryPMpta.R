#' @title Summarize Percent Target Attainment
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Summarize a Pmetrics Percent Target Attainment Object
#' @details
#' Summarize Pharmacodynamic Index (PDI) statistics and success proportions in a *PMpta* object made by [makePTA]
#' and usually found in a [PM_pta] object. The PDI is the metric calculated by the target type and target, e.g. AUC/Target,
#' or %time>target. Since a PDI cannot be calculated for intersections, summarizing the intersection object only 
#' provides the success proportion per simulation/target.
#'
#' @method summary PMpta
#' @param object A PMpta object made by [makePTA].
#' @param at Which object in the *PM_pta* result list to summarize. By default "intersect" if
#' an intersection is present due to creation of the object with multiple target types, or
#' 1 if no intersection is present, which means only 1 target type was selected. If
#' "intersect" is present in the object, the default can be overridden with a number to 
#' summarize one of the individual PTAs, e.g. `at = 2` to summarize the second PTA rather than the
#' intersection of all the PTAs. 
#' @param ci Width of the interval for pharmacodynamic index reporting.  Default is 0.95, i.e. 2.5th to 97.5th percentile.
#' @param ... Not used.
#' @return A tibble with the following columns (only the first three if `at = "intersect"`):
#' 
#' * **sim_num** is the number of the simulation
#' * **target** is the target for the row, if targets are discrete, not used for simulated targets
#' * **label** is the simulation label, for reference
#' * **prop_success** is the proportion of simulated profiles that met the success definition
#' * **median** is the median parmacodynamic index (PDI), i.e. the proportion or ratio depending on the target type
#' * **lower** is the lower bound of the interval defined by `ci`
#' * **upper** is the upper bound of the interval defined by `ci`
#' * **mean** is the mean of the PDI
#' * **sd** is the standard deviation of the PDI
#' * **min** is the minimum PDI
#' * **max** is the maximum PDI
#' 
#' @author Michael Neely
#' @seealso [makePTA], [PM_pta]
#' @export

summary.PMpta <- function(object, at = "intersect", ci=0.95, ...){
  
  pta <- object$clone()$results
  
  if(at == "intersect"){
    if(length(pta$intersect)>1){
      pta <- pta$intersect %>%
        mutate(target = as.numeric(stringr::str_extract(target, "\\d+\\.*\\d*")))
    } else {
      pta <- pta[[1]] #no intersect, summarize first
    }
  } else {
    at <- suppressWarnings(tryCatch(as.numeric(at), error = function(e) NA))
    if(!is.na(at)){
      if(at > length(pta)){
        stop("'at' is greater than the number of PTAs.")
      } else {
        pta <- pta[[at]]
      }
    } else {
      stop("'at' should be either \"intersect\" or the number of one of the objects to summarize.")
    }
    
  }
  
  simTarg <- 1+as.numeric(inherits(pta$target[[1]],"tbl_df")) #1 if missing or set, 2 if random
  if(length(simTarg)==0) simTarg <- 1
  
  if(simTarg==1){ #set targets
    
    pdi <- pta %>% group_by(.data$sim_num, .data$target) 
    
  } else { #random targets
    pdi <- pta %>% group_by(.data$sim_num)
    
  }
  
  if(at != "intersect"){
    pdi <- pdi %>% transmute(
      label = .data$label, prop_success = .data$prop_success,
      median = median(unlist(pdi), na.rm = TRUE),
      lower = quantile(unlist(pdi), probs = 0.5 - ci/2, na.rm = TRUE),
      upper = quantile(unlist(pdi), probs = 0.5 + ci/2, na.rm = TRUE),
      mean = mean(unlist(pdi), na.rm = TRUE),
      sd = sd(unlist(pdi), na.rm = TRUE),
      min = min(unlist(pdi), na.rm = TRUE),
      max = max(unlist(pdi), na.rm = TRUE)
    )
  } else {
    pdi <- pdi %>% transmute(label = label, prop_success = prop_success)
  }
  
  pdi <- pdi %>% ungroup()
  
  return(pdi)
}
