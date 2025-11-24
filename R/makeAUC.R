#' @title Calculation of AUCs
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Calculates AUC from a variety of inputs
#' @details
#' Calculates the area under the time concentration curve using the
#' trapezoidal approximation from a variety of inputs.
#' If a PM_pop, PM_post, PM_op, or PMsim object is specified,
#' `formula` is not required.
#'
#' @param formula A formula of the form `obs ~ time | group`. Default value for 
#' `group` is "id", so when if the data contain an "id" column, the formula can simply
#' be `obs ~ time`.  If the data contain a grouping variable, it can be specified
#' as `obs ~ time | group`, where `group` is the name of the grouping variable.
#' This is only required with data that is not of class PM_pop, PM_post, PM_op or PM_sim.
#' @param data A suitable data object, i.e. [PM_pop], [PM_post], [PM_op],
#' [PM_sim], or some other suitable dataframe
#' with at least time/observation columns referred to by `formula`.
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
#' * `group` - Subject identification, usually "id"
#' * tau - AUC from `start` to `end`
#' @author Michael Neely
#' @seealso [PM_result], [PM_sim], [PM_op]
#' @examples
#' \dontrun{
#' NPex$cov$plot(V ~ wt)
#' NPex$cov$plot(Ke ~ wt, line = list(lm = TRUE, ref = FALSE, loess = FALSE))
#' NPex$cov$plot(Ke ~ wt, line = list(loess = list(ci = 0.9, color = "green")))
#' NPex$cov$plot(V ~ time, marker = list(color = "blue"))
#' NPex$cov$plot(V ~ wt,
#'   line = list(lm = TRUE, loess = FALSE),
#'   stats = list(x = 0.5, y = 0.2, font = list(size = 7, color = "blue"))
#' )
#' }
#' @export

makeAUC <- function(data,
  formula,
  include, exclude,
  start = 0, end = Inf,
  icen = "median",
  outeq = 1, block = 1,
  method = "linear",
  addZero = F) {
    # handle objects
    if (missing(data)) {
      cli::cli_abort("Please supply a data object to calculate AUC.")
    }
    
    data_class <- which(
      inherits(data, c(
        "PM_sim_data", "PM_sim",
        "PM_op_data", "PM_op",
        "PM_pop_data", "PM_pop",
        "PM_post_data", "PM_post",
        "PM_data_data", "PM_data"
      ), which = TRUE) > 0
    ) # will be all zeros except matching class, undefined if none
    
    if (length(data_class) > 0) { # there was a match
      data2 <- switch(data_class,
        data$obs, # PM_sim_data
        data$obs, # PM_sim
        data %>% mutate(out = obs), # PM_op_data
        data$data %>% mutate(out = obs), # PM_op
        data %>% mutate(out = pred), # PM_pop_data
        data$data %>% mutate(out = pred), # PM_pop
        data %>% mutate(out = pred), # PM_post_data
        data$data %>% mutate(out = pred), # PM_post
        data %>%
        makePMmatrixBlock() %>%
        filter(!is.na(out)), # PM_data_data
        data$standard_data %>%
        makePMmatrixBlock() %>%
        filter(!is.na(out)) # PM_data
      )
      group <- "id"
    } else { # class not matched, so needs formula
      if (missing(formula)) {
        cli::cli_abort(c("x" = "Please supply a formula of form {.code out~time} for objects other than class {.cls PM_sim}, {.cls PM_op}, {.cls PM_pop}, {.cls PM_post}, or {.cls PM_data}."))
      }
    
      x.name <- all.vars(formula[[3]])[1] 
      y.name <- all.vars(formula[[2]])  
      group <- all.vars(formula[[3]])[2]  
      if(is.na(group)) group <- "id" # default group is id
      
      # y.name <- as.character(attr(terms(formula), "variables")[2])
      # x.name <- as.character(attr(terms(formula), "variables")[3])
      if (length(grep(y.name, names(data))) == 0) {
        cli::cli_abort("{.var y.name} is not a variable in the data.")
      }
      if (length(grep(x.name, names(data))) == 0) {
        cli::cli_abort("{.var x.name} is not a variable in the data.")
      }
      data2 <- data %>% dplyr::mutate(time = get(x.name), out = get(y.name))
    }
    
    
    # create dummy variables if missing
    if (!group %in% names(data2)) data2[[group]] <- 1 # add id if missing
    if (!"outeq" %in% names(data2)) data2$outeq <- 1 # add outeq if missing
    if (!"block" %in% names(data2)) data2$block <- 1 # add block if missing
    if (!"icen" %in% names(data2)) data2$icen <- "median" # add icen if missing
    if (missing(include)) include <- unique(data2[[group]])
    if (missing(exclude)) exclude <- NA
    
    group_sym <- rlang::sym(group)
    # filter to create object to pass to auc calculation
    data3 <- data2 %>%
    dplyr::filter(
      outeq == !!outeq,
      block == !!block,
      !!group_sym %in% include,
      !(!!group_sym) %in% exclude,
      time >= start & time <= end,
      icen == !!icen,
      !is.na(out)
    ) %>%
    dplyr::select(!!group_sym, time, out) %>%
    dplyr::group_by(!!group_sym)
    
    if (nrow(data3) < 2) {
      cli::cli_warn(c("!" = "You have selected fewer than 2 rows in your data.", "i" = "Check the values of {.code include}, {.code exclude}, {.code outeq}, {.code block}, {.code start}, and {.code end}."))
    }
    
    
    # auc function
    get_auc <- function(df, addZero, method) {
      if (addZero & !any(df$time == 0)) df <- rbind(data.frame(time = 0, out = 0), df)
      N <- nrow(df)
      if (N <= 1) {
        return(data.frame(NA, NA))
      }
      # (t_i - t_i-1)
      diffTimes <- diff(df$time)
      # (C_i + C_i-1)
      sumConc <- df$out[-1] + df$out[-N]
      # (C_i - C_i-1)
      diffConc <- diff(df$out)
      # log(C_i/C_i-1)
      logConc <- log(df$out[-1] / df$out[-N])
      # tmax
      tmax <- which(df$out == max(df$out))
      
      # auc
      if (method == "linear") {
        auc <- 0.5 * sum(diffTimes * sumConc)
      } else {
        auc <- sum(
          # linear up
          0.5 * sum(diffTimes[1:tmax] * sumConc[1:tmax]),
          # log down
          sum(diffTimes[(tmax + 1):(N - 1)] * diffConc[(tmax + 1):(N - 1)] / logConc[(tmax + 1):(N - 1)])
        )
      }
      
      return(auc)
    }
    
    # calculate AUC
    AUCdf <- tidyr::nest(data3, pk = !(!!group_sym)) %>%
    dplyr::mutate(tau = sapply(pk, get_auc, addZero, method)) %>%
    dplyr::select(!!group_sym, tau)
    
    class(AUCdf) <- c("PMauc", class(AUCdf))
    
    # reorder according to original
    AUCdf <- AUCdf[match(unique(data3[[group]]), AUCdf[[group]]), ]
    
    return(AUCdf)
  }
  