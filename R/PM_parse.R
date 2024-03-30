#' @title Parse Pmetrics output
#' @description
#' `r lifecycle::badge("stable")`
#'
#' A flexible parser for Pmetrics output
#' @details
#' Currently written for the Rust implementation of NPAG
#' @param wd The directory containing the output from the Rust-implementation of NPAG
#' @param write A logical value indicating if the results should be returned (`FALSE`, default) or written to disk (`TRUE`)
#' @return The output of \code{PM_parse} is a list containing the following elements
#' \item{op }{Written to the standard of PM_op}
#' \item{pop }{Written to the standard of PM_pop}
#' \item{post }{Written to the standard of PM_post}
#' \item{cycles }{Written to the standard of PM_cycle}
#' \item{final }{Written to the standard of PM_final}
#'
#' @seealso \code{\link{NPparse}}
#' @import dplyr
#' @import tidyr
#' @importFrom dplyr select rename mutate relocate left_join case_when first across
#' @export

PM_parse <- function(wd = getwd(), write = TRUE) {
  # Default paths
  pred_file <- paste(wd, "pred.csv", sep = "/")
  obs_file <- paste(wd, "obs.csv", sep = "/")
  meta_rust_file <- paste(wd, "meta_rust.csv", sep = "/")
  config_file <- paste(wd, "config.toml", sep = "/")
  cycle_file <- paste(wd, "cycles.csv", sep = "/")
  theta_file <- paste(wd, "theta.csv", sep = "/")
  post_file <- paste(wd, "posterior.csv", sep = "/")
  fit <- get(load("fit.Rdata"))
  #need to load fit.Rdata to get model and data

  op <- make_OP(pred_file = pred_file, obs_file = obs_file, config_file = config_file)
  post <- make_Post(pred_file = pred_file)
  pop <- make_Pop(pred_file = pred_file)
  final <- make_Final(theta_file = theta_file, config_file = config_file, post_file = post_file)
  cycle <- make_Cycle(cycle_file = cycle_file, config_file = config_file)
  cov <- make_Cov(final = final, data = fit$data)

  meta_rust <- data.table::fread(
    input = meta_rust_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )
  NPcore <- list(
    data = fit$data,
    model = fit$model,
    op = op,
    post = post,
    pop = pop,
    cycle = cycle,
    final = final,
    backend = "rust",
    algorithm = "NPAG",
    numeqt = 1,
    converge = meta_rust$converged
  )

  class(NPcore) <- "PM_result"

  if (write) {
    system("mkdir outputs")
    save(NPcore, file = "outputs/PMout.Rdata")
    return(invisible(NPcore))
  }

  return(NPcore)
}

# DATA
make_OP <- function(pred_file = "pred.csv", obs_file = "obs.csv", config_file = "config.toml", version) {
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

  config <- RcppTOML::parseTOML(config_file)
  poly <- config$error$poly

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
    dplyr::rename(pred = value) %>%
    mutate(d = pred - obs) %>%
    mutate(ds = d * d) %>%
    mutate(obsSD = poly[1] + poly[2] * obs + poly[3] * (obs^2) + poly[4] * (obs^3)) %>%
    mutate(wd = d / obsSD) %>%
    mutate(wds = wd * wd) %>%
    # HARDCODED
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
    dplyr::rename(icen = name) %>%
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
    dplyr::rename(icen = name) %>%
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
make_Final <- function(theta_file = "theta.csv", config_file = "config.toml", post_file = "posterior.csv") {
  theta <- data.table::fread(
    input = theta_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )

  post <- data.table::fread(
    input = post_file,
    sep = ",",
    header = TRUE,
    data.table = FALSE,
    dec = ".",
    showProgress = TRUE
  )

  par_names <- names(theta)[names(theta) != "prob"]

  # Pop
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
    summarise(across(-prob, \(x) median(x)))

  # Posterior

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

  # TODO: Add postCov, postCor, Post

  postMed <- post %>%
    #group_by(id) %>% 
    filter(id==6) %>%
    reframe(across(-c(point, prob), \(x) weighted_median(x, prob))) # in PMutilities

  # TODO: Calculate shrinkage
  shrinkage <- 1
  # varEBD <- apply(postVar[,-1],2,mean) # Mean postVar / popVar
  # sh <- varEBD/popVar

  ab <- RcppTOML::parseTOML(config_file)$random %>%
    unlist() %>%
    matrix(ncol = 2, byrow = T)

  gridpts <- RcppTOML::parseTOML(config_file)$config$init_points

  final <- list(
    popPoints = theta,
    popMean = popMean,
    popSD = popSD,
    popCV = popSD / popMean,
    popVar = popSD**2,
    popCov = popCov,
    popCor = popCor,
    popMedian = popMedian,
    postMean = postMean,
    postSD = postSD,
    postMed = postMed,
    gridpts = gridpts,
    nsub = length(unique(post$id)),
    ab = ab
  )
  class(final) <- c("PMfinal", "NPAG", "list")
  
  return(final)
}

# CYCLES
make_Cycle <- function(cycle_file = "cycles.csv", obs_file = "obs.csv", config_file = "config.toml", version) {
  raw <- data.table::fread(
    input = cycle_file,
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


  config <- RcppTOML::parseTOML(config_file)

  cycle_data <- raw %>%
    pivot_longer(cols = ends_with(c("mean", "median", "sd"))) %>%
    separate_wider_delim(name, delim = ".", names = c("parameter", "statistic"))

  num_params <- length(names(config$random)) + length(names(config$fixed))

  aic <- 2 * num_params + raw$neg2ll
  names(aic) <- raw$cycle
  bic <- num_params * log(length(unique(obs_raw$id))) + raw$neg2ll
  names(bic) <- raw$cycle

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
  
  n_out <- max(obs_raw$outeq)
  n_cyc <- max(cycle_data$cycle)
  gamlam <- tibble::as_tibble(raw$gamlam, .name_repair = "minimal") 
  if(ncol(gamlam) == 1 & n_out > 1){gamlam <- cbind(gamlam, replicate((n_out-1),gamlam[,1]))} 
  names(gamlam) <- as.character(1:ncol(gamlam))
  gamlam <- gamlam %>% pivot_longer(cols = everything(), 
                                                values_to = "value", names_to = "outeq") %>%
    mutate(cycle = rep(1:n_cyc, each = n_out)) %>%
    select(cycle, value, outeq)
  
  

  res <- list(
    names = c(names(config$random), names(config$fixed), names(config$constant)),
    cycnum = raw$cycle,
    ll = raw$neg2ll,
    gamlam = gamlam,
    mean = mean,
    sd = sd,
    median = median,
    aic = aic,
    bic = bic
  )
  class(res) <- c("PMcycle", "list")
  return(res)
}

#COV

make_Cov <- function(final = final, data = fit$data){
  data1 <- data$data %>% filter(!is.na(dose)) %>% 
    select(id, time, !!getCov(.)$covnames) %>% #in PMutitlities
    tidyr::fill(-id, -time) %>%
    dplyr::left_join(final$postMean, by = "id") %>% 
    mutate(icen = "mean") 
  data2 <- data$data %>% filter(!is.na(dose)) %>% 
    select(id, time, !!getCov(.)$covnames) %>% 
    tidyr::fill(-id, -time) %>%
    dplyr::left_join(final$postMed, by = "id") %>% 
    mutate(icen = "median") 
  
  res <- bind_rows(data1, data2)
  class(res) <- c("PMcov", "data.frame")
  
  return(res)
  
}
