#' A flexible parser for Pmetrics output
#'
#' Currently written for the Rust implementation of NPAG
#' @title Parse Pmetrics output
#' @param wd The directory containing the output from the Rust-implementation of NPAG
#' @return The output of \code{PM_parse} is a list containing the following elements
#' \item{op }{Written to the standard of PM_op}
#' \item{pop }{Written to the standard of PM_pop}
#' \item{post }{Written to the standard of PM_post}
#' \item{cycles }{Written to the standard of PM_cycle}
#' 
#' @seealso \code{\link{NPparse}}
#' @importFrom data.table fread
#' @export

PM_parse = function(wd = getwd(), write = TRUE) {
  pred_file = "pred.csv"
  obs_file = "obs.csv"
  meta_r_file = "meta_r.csv"
  meta_rust_file = "meta_rust.csv"
  cycle_file = "cycles.csv"
  
  op = make_OP(pred_file = pred_file, obs_file = obs_file)
  post = make_Post(pred_file = pred_file)
  pop = make_Pop(pred_file = pred_file)
  cycle = make_Cycle(
    cycle_file = cycle_file,
    meta_r_file = meta_r,
    meta_rust_file = meta_rust_file
  )
  
  NPcore = list(
    op = op,
    post = post,
    pop = pop,
    backend = "rust",
    algorithm = "NPAG"
  )
  
  if (write) {
    save(NPcore, file = "NPcore.Rdata")
    return()
  }
  
  return(NPcore)
  
}

# DATA
make_OP = function(pred_file = "pred.csv", obs_file = "obs.csv", version) {
  pred_raw = data.table::fread(
    input = pred_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )
  
  obs_raw = data.table::fread(
    input = obs_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )
  
  obs_raw = obs_raw %>%
    rename(id = sub_num)
  
  op = obs_raw %>%
    left_join(pred_raw, by = c("id", "time", "outeq")) %>%
    pivot_longer(cols = c(popMean, popMedian, postMean, postMedian)) %>%
    mutate(
      icen = case_when(
        name == "popMean" ~ "mean",
        name == "popMedian" ~ "median",
        name == "postMean" ~ "mean",
        name == "postMedian" ~ "median",
      )
    ) %>%
    mutate(
      pred.type = case_when(
        name == "popMean" ~ "pop",
        name == "popMedian" ~ "pop",
        name == "postMean" ~ "post",
        name == "postMedian" ~ "post",
      )
    ) %>%
    select(-name) %>%
    rename(pred = value) %>%
    mutate(d = pred - obs) %>%
    mutate(ds = d * d) %>%
    # Hardcoded for now
    mutate(block = 1)
  
  class(op) = c("PMop", "data.frame")
  return(op)
  
}

# POST
make_Post = function(pred_file = "pred.csv", version) {
  raw = data.table::fread(
    input = pred_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )
  
  post = raw %>%
    select(-popMedian, -popMean) %>%
    pivot_longer(cols = c(postMedian, postMean),
                 values_to = "pred") %>%
    rename(icen = name) %>%
    mutate(icen = case_when(icen == "postMedian" ~ "median",
                            icen == "postMean" ~ "mean")) %>%
    # Hardcoded for now
    mutate(block = 1) %>%
    relocate(id, time, icen, outeq, pred, block)
  
  class(post) = c("PMpost", "data.frame")
  return(post)
  
}

# POP
make_Pop = function(pred_file = "pred.csv", version) {
  raw = data.table::fread(
    input = pred_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )
  
  pop = raw %>%
    select(-postMedian, -postMean) %>%
    pivot_longer(cols = c(popMedian, popMean), values_to = "pred") %>%
    rename(icen = name) %>%
    mutate(icen = case_when(icen == "popMedian" ~ "median",
                            icen == "popMean" ~ "mean")) %>%
    # Hardcoded for now
    mutate(block = 1) %>%
    relocate(id, time, icen, outeq, pred, block)
  
  class(pop) = c("PMpop", "data.frame")
  return(pop)
  
}

# FINAL

# CYCLES
make_Cycle = function(cycle_file = "cycles.csv", meta_r_file = "meta_r.csv", version) {
  
  raw = data.table::fread(
    input = cycle_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )
  
  meta = data.table::fread(
    input = meta_r_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )
  
  par_names = meta %>% 
    select(ends_with(".name")) %>% 
    pivot_longer(cols = everything(), values_to = "parameter", names_to = "param_number") %>% 
    mutate(param_number = gsub(pattern = ".name", replacement = "", x = param_number))
  
  # Fix parameter names
  cycle_data = raw %>% 
    pivot_longer(cols = starts_with("param")) %>% 
    separate_wider_delim(name, delim = ".", names = c("param_number", "statistic")) %>% 
    left_join(par_names, by = "param_number") %>% 
    select(-param_number)
  
  # Calculate AIC and BIC
  # TO-DO: Do not include fixed (but not random) parameters!
  num_fixed = meta %>% 
    select(ends_with(".fixed")) %>% 
    pivot_longer(cols = everything()) %>% 
    pull(value) %>% 
    sum()
  
  num_params = nrow(par_names) - num_fixed
  
  aic = 2*num_params - cycle_data$neg2ll
  names(aic) = cycle_data$cycle
  bic = num_params * log(meta$nsub) - cycle_data$neg2ll
  names(bic) = cycle_data$cycle
  
  mean = cycle_data %>% 
    filter(statistic == "mean") %>% 
    select(cycle, value, parameter) %>% 
    pivot_wider(names_from = parameter, values_from = value) %>% 
    arrange(cycle) %>% 
    mutate(across(.cols = -cycle, .fns = function(x) {
      x / first(x)
    }))
  
  sd = cycle_data %>% 
    filter(statistic == "sd") %>% 
    select(cycle, value, parameter) %>% 
    pivot_wider(names_from = parameter, values_from = value) %>% 
    arrange(cycle) %>% 
    mutate(across(.cols = -cycle, .fns = function(x) {
      x / first(x)
    }))
  
  median = cycle_data %>% 
    filter(statistic == "median") %>% 
    select(cycle, value, parameter) %>% 
    pivot_wider(names_from = parameter, values_from = value) %>% 
    arrange(cycle) %>% 
    mutate(across(.cols = -cycle, .fns = function(x) {
      x / first(x)
    }))
  
  res = list(
    names = par_names$parameter,
    cycnum = cycle_data$cycle,
    ll = cycle_data$neg2ll,
    gamlam = 1, # Not implemented
    mean = mean,
    sd = sd,
    median = median,
    aic = aic,
    bic = bic
  )
  
  return(res)
  
}
