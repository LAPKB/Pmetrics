#' @title Estimate renal function using various equations
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Adds columns to [PM_data] object for creatinine clearance or estimated glomerular filtration rate (eGFR).
#' 
#' Creatinine clearance can be estimated by:
#' * The Jelliffe equation for paired creatinines 
#' * The Cockcroft-Gault equation using a single creatinine.
#' 
#' eGFR can be estimated by:
#' * The MDRD equation
#' * The CKD-EPI equation
#' * The Schwartz equation for children
#' 
#' 
#' @details
#' **Note**: In all the equations below, age is in years, weight is in kilograms, height is in meters,
#' and cystatin C (cysC) is in mg/L. Serum creatinine (Scr) is in mg/dL and blood urea nitrogen (BUN)
#' is in mg/L. However, if `SI = TRUE`, then
#' serum creatinine is in micromol/L and BUN is in micromol/L of urea.
#' 
#' Missing covariate values are interpolated using linear interpolation within each subject.
#' 
#' 
#' * The **Jelliffe** equation depends on age, sex, weight, and serum creatinine.
#' It uniquely uses two serum creatinine values separated by time to estimate changing
#' renal function.
#'   - \eqn{ESS = wt * (29.3 - (0.203 * age))} for males
#'   - \eqn{ESS = wt * (25.1 - (0.175 * age))} for females
#'   - \eqn{scrAve = (Scr1 + Scr2) / 2}
#'   - \eqn{ESS_cor = ESS * (1.035 - (0.0337 * scrAve))}
#'   - \eqn{E = ESS_cor - 4 * wt * (Scr2 - Scr1) / (time2 - time1)}
#'   - \eqn{CRCL = E / (14.4 * scrAve)} in ml/min/1.73m^2
#' * The **Cockroft-Gault** equation is a point estimate of creatinine clearance. It
#' depends on the same variables as Jelliffe except it does not require paired creatinines.
#'   - \eqn{CRCL = ((140 - age) * weight * K)/(72 * Scr)}
#'   - \eqn{K} is 1 for males and 0.85 for females
#' * The **MDRD** equation estimates GFR. It depends on age, sex, black race, and serum creatinine.
#'   - \eqn{eGFR = 175 * (Scr)^{-1.154} * (age)^{-0.203}}
#'   - Multiply by 1.212 if black.
#' * The **CKD-EPI** equation estimates GFR. It depends on the same variables as MDRD, except `black` is
#' no longer required in the 2021 version used here.
#'   - \eqn{eGFR = 141 * min(Scr/k, 1)^a * max(Scr/k, 1)^{-1.209} * 0.993^{age}}
#'   - Multiply by 1.1018 if female.
#' * The **Schwartz** equation estimates GFR in children. The equation used depends on which covariates
#' are included in the data. 
#'   - height and serum creatinine: \eqn{eGFR = 41.3 * height/scr} (updated Scwartz equation)
#'   - height, serum creatinine, and cystatin C: \eqn{eGFR = 41.6 * (height/scr)^{0.599} * (1.8/cysC)^{0.317}} (Equation 1a)
#'   - height, serum creatinine, and BUN: \eqn{eGFR = 40.7 * (height/scr)^{0.640} * (30/BUN)^{0.202}} (Equation 1b)
#'   - height, serum creatinine, cystatin C, and BUN: \eqn{eGFR = 41.1 * (height/scr)^{0.510} * (1.8/cysC)^{0.272} * (30/BUN)^{0.171}} (Equation II)
#'   - height, serum creatinine, cystatin C, BUN, and male: \eqn{eGFR = 39.1 * (height/scr)^{0.516} * (1.8/cysC)^{0.294} * (30/BUN)^{0.169} * 1.099^{male} * (height/1.4)^{1.88}} (Equation III)
#'   
#' Discussion of the strengths and weaknesses of these equations are beyond the scope of this documentation.
#' 
#' @param x A [PM_data] data object
#' @param method A character vector defining the method to use. Options are
#' * "jelliffe" or "jel"
#' * "cockcroft-gault" or "cg"
#' * "mdrd"
#' * "ckd-epi" or "ckd" 
#' * "schwartz"
#' For the fuller versions, Only the first 4 letters are required
#' @param id A character vector with the name
#'  of the id column in `x`. The default is "id".
#' @param wt A character vector with the name of the weight
#'  column in `x`.  The default is "wt". Only required for "jelliffe" and "cockcroft-gault" methods.
#' Values must be in **kilograms**.
#' @param ht A character vector with the name of the height column in `x`.
#' The default is "ht". Only required for the "schwartz" method. Values must be in **meters**
#' @param male A character vector with the name of the column defining sex in `x`.
#' Male should be 1 and female should be 0. The default is "male".
#' @param age A character vector with the name of the age column in `x`.
#' The default is "age". Values must be in **years**.
#' @param black A character vector with the name of a column defining black race in `x`.
#' Male should be 1 and female should be 0. The default is "male".
#' @param scr A character vector with the name of the serum creatinine column in `x`.
#' Default units are **mg/dL**, unless `SI = TRUE` below. The the default name is "scr".
#' @param bun A character vector with the name of the blood urea nitrogen column in `x`.
#' Default units are **mg/dL**, unless `SI = TRUE` below. Default name is "bun". Optional for the Schwartz method. 
#' @param cysC A character vector with the name of the cystatin C column in `x`.
#' Units are always **mg/L**. Default is "cysC". Optional for the Schwartz method.
#' @param SI Boolean value, if true, will expect serum creatinine to be in **micromol/L** and BUN reported
#' as **micromol/L** of urea.
#' Default is `FALSE`.
#' @return A column added to the `x` data object with the name of the method used and calcuated values.
#' @author Michael Neely
#' @export

add_renal <- function(x, method, id = "id", wt = "wt", ht = "ht", male = "male", age = "age", black = "black", scr = "scr", bun = "bun", cysC = "cysC", SI = F) {
  if (!inherits(x, "PM_data")){
    cli::cli_abort(c("x" = "Please supply a PM_data object."))
  } 
  mdata <- x$standard_data
  dataCols <- names(mdata) # check to make sure names ok
  
  # Get required columns
  method <- tolower(method)
  
  method_key <- dplyr::case_when(
    startsWith(method, "jel") ~ "jel",
    method == "cg" | startsWith(method, "coc") ~ "cg",
    startsWith(method, "mdr") ~ "mdr",
    startsWith(method, "ckd") ~ "ckd",
    startsWith(method, "sch") ~ "sch",
    TRUE ~ "unknown"
  )
  
  if (method_key == "unknown") {
    cli::cli_abort(c("x" = "Please provide a valid method.",
    " " = "Valid methods are at least the first three letters of: jelliffe, cockcroft-gault (or just cg), mdrd, ckd-epi, or schwartz."))
  }
  
  req_cols <- switch(
    method_key,
    jel = c("id", "wt", "male", "age", "scr"),
    cg = c("id", "wt", "male", "age", "scr"),
    mdrd = c("id", "male", "age", "scr", "black"),
    ckd = c("id", "male", "age", "scr"),
    sch = c("id", "ht", "scr"),
  )
  
  matched_names <- purrr::map_chr(req_cols, \(x){
    get(x)
  })
  
  
  if (!all(matched_names %in% dataCols)) {
    cli::cli_abort(c("x" = "Please provide column names for {req_cols} for the '{method}' method."))
  }
  
  
  
  # Interpolate missing values for required columns
  mdata <- mdata %>%
  group_by(!!id) %>%
  mutate(across(
    all_of(matched_names),
    ~ {
      idx <- which(!is.na(.x))
      if (length(idx) == 0) return(.x)  # all values are NA
      approx(
        x = idx,
        y = .x[idx],
        xout = seq_along(.x),
        method = "linear",
        rule = 2  # extrapolate using last/first value
      )$y
    }
  )) %>%
  ungroup() %>%
  select(-last_col())
  
  # Define necessary symbols
  
  # all need these two
  id_sym   <- sym(id)
  scr_sym  <- sym(scr)
  
  if(method_key %in% c("jel", "cg", "mdr", "ckd")){
    male_sym <- sym(male)
    age_sym  <- sym(age)
  } 
  if(method_key %in% c("jel", "cg")){
    wt_sym   <- sym(wt)
  }
  if(method_key == "mdr"){
    black_sym <- sym(black)
  }
  if(method_key == "sch"){
    ht_sym   <- sym(ht)
  }
  
  if (SI) {
    mdata <- mdata %>% mutate(
      !!scr_sym := !!scr_sym / 88.4,
      !!bun_sym := !!bun_sym * 2.8) # convert
  }
    
    
    # Jelliffe method
    if(method_key == "jel"){
      mdata <- mdata %>%
      arrange(!!id_sym, time) %>%
      group_by(!!id_sym) %>%
      mutate(
        Scr1 = lag(!!scr_sym, default = first(!!scr_sym)),
        Scr2 = !!scr_sym,
        scrAve = (Scr1 + Scr2) / 2,
        
        ESS = dplyr::case_when(
          !!male_sym == 1 ~ !!wt_sym * (29.3 - 0.203 * !!age_sym),
          !!male_sym == 0 ~ !!wt_sym * (25.1 - 0.175 * !!age_sym),
          TRUE ~ NA_real_
        ),
        ESS_cor = ESS * (1.035 - 0.0337 * scrAve),
        
        time1 = lag(time, default = first(time)),
        time2 = time,
        
        E = ESS_cor - 4 * !!wt_sym * (Scr2 - Scr1) / (time2 - time1),
        crcl_jelliffe = E / (14.4 * scrAve)
      ) %>%
      select(-Scr1, -Scr2, -scrAve, -ESS, -ESS_cor, -time1, -time2, -E) %>%      
      fill(crcl_jelliffe, .direction = "up") %>%
      ungroup()
    }
    
    # Cockcroft-Gault method
    if(method_key == "cg"){
      mdata <- mdata %>%
      mutate(crcl_cg = ((140 - !!age_sym) * !!wt_sym) / (72 * !!scr_sym)) %>%
      mutate(crcl_cg = ifelse(!!male_sym, crcl_cg , crcl_cg * 0.85)) # females are 85%
    }
    
    # MDRD method
    if(method_key == "mdr"){
      mdata <- mdata %>%
      mutate(gfr_mdrd = 175 * (!!scr_sym)^-1.154 * (!!age_sym)^-0.203) %>%
      mutate(gfr_mdrd = ifelse(!!black_sym), gfr_mdrd * 1.212, crcl)
    }
    
    # CKD-EPI method
    if(method_key == "ckd"){
      mdata <- mdata %>%
      mutate(gfr_ckd = ifelse(
        !!male_sym, 
        141 * min(!!scr_sym / 0.9, 1)^-0.411 * max(!!scr_sym / 0.9, 1)^-1.209 * 0.993^!!age_sym, # males
        144 * min(!!scr_sym / 0.7, 1)^-0.329 * max(!!scr_sym / 0.7, 1)^-1.209 * 0.993^!!age_sym # females
      )
    ) 
  }
  
  # Schwartz method
  if(method_key == "sch"){
    # optional parameters, based on Schwartz et al. New Equations to Estimate GFR in Children with CKD. J Am Soc Nephrol 20: 629-637, 2009
    optional_names <- purrr::map_chr(c("male", "bun", "cysC"), \(x){
      get(x)
    })
    
    if (optional_names[1] %in% dataCols) {
      male_sym <- sym(male)
    } else { male_sym <- NULL}
    if (optional_names[2] %in% dataCols) {
      bun_sym <- sym(bun)
    } else { bun_sym <- NULL }
    if (optional_names[3] %in% dataCols) {
      cysC_sym <- sym(cysC)
    } else { cysC_sym <- NULL }

    #browser()
    
    # choose the Schwartz equation based on the available data (scr and ht are always required)
    if (all(purrr::map_lgl(list(male_sym, bun_sym, cysC_sym), \(x) !is.null(x)))) { # male, bun, cysC all present, so equation III
      mdata <- mdata %>%
      mutate(gfr_schwartz = 39.1 * (!!ht_sym / !!scr_sym)^0.516 * (1.8 / !!cysC_sym)^0.294 * (30 / !!bun_sym)^0.169 * 1.099^!!male_sym * (!!ht_sym / 1.4)^1.88) # Schwartz with cysC and bun
      cli::cli_inform("Using Equation III with {.arg {c(matched_names, optional_names)}} from Schwartz et al. New Equations to Estimate GFR in Children with CKD. J Am Soc Nephrol 2009; 20: 629-637.")
    }

    else if (!is.null(cysC_sym) & !is.null(bun_sym) & is.null(male_sym)) { # bun, cysC present but male absent, so equation II
      mdata <- mdata %>%
      mutate(gfr_schwartz = 41.1 * (!!ht_sym / !!scr_sym)^0.510 * (1.8 / !!cysC_sym)^0.272 * (30 / !!bun_sym)^0.171) # bun, cysC present but male absent, so equation II
      cli::cli_inform("Using Equation II with {.arg {c(matched_names, optional_names)}} from Schwartz et al. New Equations to Estimate GFR in Children with CKD. J Am Soc Nephrol 2009; 20: 629-637.")
    }
    
    else if (is.null(cysC_sym) & !is.null(bun_sym))   { # only bun present, so equation Ib
      mdata <- mdata %>%
      mutate(gfr_schwartz = 40.7 * (!!ht_sym / !!scr_sym)^0.640 * (30 / !!bun_sym)^0.202)
      cli::cli_inform("Using Equation Ib with {.arg {c(matched_names, optional_names[2])}} from Schwartz et al. New Equations to Estimate GFR in Children with CKD. J Am Soc Nephrol 2009; 20: 629-637.")

    }
    
    else if (is.null(bun_sym) & !is.null(cysC_sym)) { # only cysC present, so equation Ia
      mdata <- mdata %>%
      mutate(gfr_schwartz = 41.6 * (!!ht_sym / !!scr_sym)^0.599 * (1.8 / !!cysC_sym)^0.317)
      cli::cli_inform("Using Equation Ia with {.arg {c(matched_names, optional_names[3])}} from Schwartz et al. New Equations to Estimate GFR in Children with CKD. J Am Soc Nephrol 2009; 20: 629-637.")

    }
    else {
      mdata <- mdata %>%
      mutate(gfr_schwartz = 41.3 * !!ht_sym / !!scr_sym) # updated Schwartz equation
      cli::cli_inform("Using updated Schwartz equation with {.arg {matched_names}} from Schwartz et al. New Equations to Estimate GFR in Children with CKD. J Am Soc Nephrol 2009; 20: 629-637.")

    }

  }
  
  return(mdata)
  
}
