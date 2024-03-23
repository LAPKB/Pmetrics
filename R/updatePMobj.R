update_PM_obj <- function(x){
  if(inherits(x, "PM_result")){
    
    msg <- NULL
    #CYCLE
    if(!is.null(x$cycle$data)){
      dat <- x$cycle$data
      if(
        is.matrix(dat$gamlam) #version prior to 2.2, add next update via or join
      ){ 
        #start conversion
        n_cyc <- nrow(dat$mean)
        dat$gamlam <- c(dat$gamlam)
        dat$mean <- tibble::tibble(cycle = 1:n_cyc) %>% 
          dplyr::bind_cols(tidyr::as_tibble(dat$mean))
        dat$median <- tibble::tibble(cycle = 1:n_cyc) %>% 
          dplyr::bind_cols(tidyr::as_tibble(dat$median))
        dat$sd <- tibble::tibble(cycle = 1:n_cyc) %>% 
          dplyr::bind_cols(tidyr::as_tibble(dat$sd))
        msg <- c(msg, "cycle")
      }
    }
    if(!is.null(msg)){
      cat(crayon::blue("NOTE: "), 
          "The", 
          crayon::green(dplyr::case_when(
            length(msg)==1 ~ msg,
            length(msg)==2 ~ paste(msg, collapse = " and "),
            length(msg)>2 ~ paste(msg, collapse = ", ")
          )[1]),
          ifelse(length(msg)>1, "fields", "field"), 
          "in your PM_result object",
          ifelse(length(msg)>1, "have", "has"),
          "been updated",
          "to the most current format.",
          "If the PM_result is named run2 and was loaded from Runs/2 folder for example,", 
          "save with `run2$save(2)`.")
      flush.console()
      
    }
  } else {
    return(x)
  }
  
}