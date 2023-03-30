#' A flexible parser for Pmetrics output
#'
#' Currently written for the Rust implementation of NPAG
#' @title Parse Pmetrics output
#' @param wd The directory containing the output from the Rust-implementation of NPAG
#' @param write A logical value indicating if the results should be returned (`FALSE`, default) or written to disk (`TRUE`)
#' @return The output of \code{PM_parse} is a list containing the following elements
#' \item{op }{Written to the standard of PM_op}
#' \item{pop }{Written to the standard of PM_pop}
#' \item{post }{Written to the standard of PM_post}
#' \item{cycles }{Written to the standard of PM_cycle}
#'
#' @seealso \code{\link{NPparse}}
#' @importFrom data.table fread
#' @importFrom dplyr select rename mutate relocate left_join case_when
#' @importFrom tidyr pivot_longer
#' @importFrom RcppTOML parseTOML
#' @importFrom matrixStats weightedMedian
#' @export

PM_parse <- function(wd = getwd(), write = TRUE) {
  pred_file <- paste(wd, "pred.csv", sep = "/")
  obs_file <- paste(wd, "obs.csv", sep = "/")
  # meta_r_file <- paste(wd, "meta_r.csv", sep = "/")
  meta_rust_file <- paste(wd, "meta_rust.csv", sep = "/")
  toml_config_file <- paste(wd, "config.toml", sep = "/")
  cycle_file <- paste(wd, "cycles.csv", sep = "/")
  theta_file <- paste(wd, "theta.csv", sep = "/")
  post_file <- paste(wd, "posterior.csv", sep = "/")

  op <- make_OP(pred_file = pred_file, obs_file = obs_file)
  post <- make_Post(pred_file = pred_file)
  pop <- make_Pop(pred_file = pred_file)
  final <- make_Final(theta_file = theta_file, toml_config_file = toml_config_file, post_file = post_file)
  cycle <- make_Cycle(cycle_file = cycle_file, toml_config_file = toml_config_file)

  NPcore <- list(
    op = op,
    post = post,
    pop = pop,
    cycle = cycle,
    final = final,
    backend = "rust",
    algorithm = "NPAG",
    numeqt = 1 # Fixed
  )

  if (write) {
    save(NPcore, file = "NPcore.Rdata")
    return()
  }

  return(NPcore)
}

# DATA
make_OP <- function(pred_file = "pred.csv", obs_file = "obs.csv", version) {
  pred_raw <- data.table::fread(
    input = pred_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )

  obs_raw <- data.table::fread(
    input = obs_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )

  obs_raw <- obs_raw %>%
    rename(id = sub_num)

  op <- obs_raw %>%
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

  class(op) <- c("PMop", "data.frame")
  return(op)
}

# POST
make_Post <- function(pred_file = "pred.csv", version) {
  raw <- data.table::fread(
    input = pred_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )

  post <- raw %>%
    select(-popMedian, -popMean) %>%
    pivot_longer(
      cols = c(postMedian, postMean),
      values_to = "pred"
    ) %>%
    rename(icen = name) %>%
    mutate(icen = case_when(
      icen == "postMedian" ~ "median",
      icen == "postMean" ~ "mean"
    )) %>%
    # Hardcoded for now
    mutate(block = 1) %>%
    relocate(id, time, icen, outeq, pred, block)

  class(post) <- c("PMpost", "data.frame")
  return(post)
}

# POP
make_Pop <- function(pred_file = "pred.csv", version) {
  raw <- data.table::fread(
    input = pred_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )

  pop <- raw %>%
    select(-postMedian, -postMean) %>%
    pivot_longer(cols = c(popMedian, popMean), values_to = "pred") %>%
    rename(icen = name) %>%
    mutate(icen = case_when(
      icen == "popMedian" ~ "median",
      icen == "popMean" ~ "mean"
    )) %>%
    # Hardcoded for now
    mutate(block = 1) %>%
    relocate(id, time, icen, outeq, pred, block)

  class(pop) <- c("PMpop", "data.frame")
  return(pop)
}

# FINAL
make_Final <- function(theta_file = "theta.csv", toml_config_file = "config.toml", post_file = "posterior.csv") {
  theta <- data.table::fread(
    input = theta_file,
    sep = ",",
    header = FALSE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )

  # meta <- data.table::fread(
  #   input = toml_config_file,
  #   sep = ",",
  #   header = TRUE,
  #   data.table = FALSE,
  #   dec = ".",
  #   showProgress = TRUE
  # )

  post <- data.table::fread(
    input = post_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )

  # par_names <- meta %>%
  #   select(ends_with(".name")) %>%
  #   pivot_longer(cols = everything(), values_to = "parameter", names_to = "param_number") %>%
  #   mutate(param_number = gsub(pattern = ".name", replacement = "", x = param_number)) %>%
  #   pull(parameter)

  par_names <- parseTOML(toml_config_file)$random %>% names()

  par_names <- c(par_names, "prob")

  names(theta) <- par_names

  popMean <- theta %>%
    summarise(across(.cols = -prob, .fns = function(x) {
      mean(x)
    }))

  popSD <- theta %>%
    summarise(across(.cols = -prob, .fns = function(x) {
      sd(x)
    }))

  popCov <- theta %>%
    select(-prob) %>%
    cov()

  popCor <- theta %>%
    select(-prob) %>%
    cor()

  popMedian <- theta %>%
    summarise(across(.cols = -prob, .fns = function(x) {
      median(x)
    }))

  # Posterior
  post_names <- c("id", "point", par_names)
  names(post) <- post_names

  postMean <- post %>%
    group_by(id) %>%
    summarise(across(.cols = -c(point, prob), .fns = function(x) {
      weighted.mean(x = x, w = prob)
    }))

  postSD <- post %>%
    group_by(id) %>%
    summarise(across(.cols = -c(point, prob), .fns = function(x) {
      sd(x)
    }))

  postVar <- post %>%
    group_by(id) %>%
    summarise(across(.cols = -c(point, prob), .fns = function(x) {
      sd(x)**2
    }))

  # TO-DO: Add postCov, postCor, Post

  postMed <- post %>%
    group_by(id) %>%
    summarise(across(.cols = -c(point, prob), .fns = function(x) {
      weightedMedian(x = x, w = prob, interpolate = TRUE)
    }))


  shrinkage <- 1
  # varEBD <- apply(postVar[,-1],2,mean) # Mean postVar / popVar
  # sh <- varEBD/popVar

  # ab <- meta %>%
  #   select(ends_with(c(".min", ".max"))) %>%
  #   pivot_longer(cols = everything()) %>%
  #   separate_wider_delim(cols = name, delim = ".", names = c("param_num", "type")) %>%
  #   pivot_wider(names_from = type, values_from = value) %>%
  #   arrange(param_num) %>%
  #   select(min, max) %>%
  #   as.matrix()

  # ab <- unname(ab)
  ab <- parseTOML(toml_config_file)$random %>%
    unlist() %>%
    matrix(ncol = 2, byrow = T)

  gridpts <- parseTOML(toml_config_file)$config$init_points

  final <- list(
    popPoints = theta,
    popMean = popMean,
    popSD = popSD,
    popCV = popSD / popMean,
    popVar = popSD**2,
    popCov = popCov,
    popCor = popCor,
    popMedian = popMedian,
    #
    postMean = postMean,
    postMed = postMed,
    gridpts = gridpts,
    nsub = 51, # TODO: Hardcoded for now
    ab = ab
  )

  return(final)
}

# CYCLES
make_Cycle <- function(cycle_file = "cycles.csv", toml_config_file = "config.toml", version) {
  raw <- data.table::fread(
    input = cycle_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )

  # meta <- data.table::fread(
  #   input = toml_config_file,
  #   sep = ",",
  #   header = TRUE,
  #   data.table = FALSE,
  #   dec = ".",
  #   showProgress = TRUE
  # )

  # par_names <- meta %>%
  #   select(ends_with(".name")) %>%
  #   pivot_longer(cols = everything(), values_to = "parameter", names_to = "param_number") %>%
  #   mutate(param_number = gsub(pattern = ".name", replacement = "", x = param_number))

  par_names <- parseTOML(toml_config_file)$random %>% names()

  # Fix parameter names
  # cycle_data <- raw %>%
  #   pivot_longer(cols = starts_with("param")) %>%
  #   separate_wider_delim(name, delim = ".", names = c("param_number", "statistic")) %>%
  #   left_join(par_names, by = "param_number") %>%
  #   select(-param_number)
  cycle_data <- raw %>%
    pivot_longer(cols = ends_with(c("mean", "median", "sd"))) %>%
    separate_wider_delim(name, delim = ".", names = c("parameter", "statistic"))

  # Calculate AIC and BIC
  # TO-DO: Do not include fixed (but not random) parameters!
  # num_fixed <- meta %>%
  #   select(ends_with(".fixed")) %>%
  #   pivot_longer(cols = everything()) %>%
  #   pull(value) %>%
  #   sum()

  num_fixed <- 0 # TODO: Hardcoded for now

  # num_params <- nrow(par_names) - num_fixed
  num_params <- length(par_names) - num_fixed

  aic <- 2 * num_params - cycle_data$neg2ll
  names(aic) <- cycle_data$cycle
  # bic <- num_params * log(meta$nsub) - cycle_data$neg2ll
  bic <- num_params * log(51) - cycle_data$neg2ll # TODO: Hardcoded for now
  names(bic) <- cycle_data$cycle

  mean <- cycle_data %>%
    filter(statistic == "mean") %>%
    select(cycle, value, parameter) %>%
    pivot_wider(names_from = parameter, values_from = value) %>%
    arrange(cycle) %>%
    mutate(across(.cols = -cycle, .fns = function(x) {
      x / first(x)
    }))

  sd <- cycle_data %>%
    filter(statistic == "sd") %>%
    select(cycle, value, parameter) %>%
    pivot_wider(names_from = parameter, values_from = value) %>%
    arrange(cycle) %>%
    mutate(across(.cols = -cycle, .fns = function(x) {
      x / first(x)
    }))

  median <- cycle_data %>%
    filter(statistic == "median") %>%
    select(cycle, value, parameter) %>%
    pivot_wider(names_from = parameter, values_from = value) %>%
    arrange(cycle) %>%
    mutate(across(.cols = -cycle, .fns = function(x) {
      x / first(x)
    }))

  res <- list(
    names = par_names, # TODO: changed
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
