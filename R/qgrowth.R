#' @title
#' Extract CDC pediatric growth charts
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Will extract height, weight, and BMI for boys, girls or both for a given
#' range of ages in months and percentiles. This can be useful for
#' simulations in Pmetrics.
#'
#' @param sex A single quoted character: "M" for males, "F" for
#' females, or "B" for both.  Default is "B".
#' @param agemos A vector of ages in months to return.  The default is
#' `seq(0,18)*12`, i.e. 0 to 216 months in increments of 12, which is
#' 1 to 18 years. Values do not have to be multiples of 12.
#' @param percentile An integer vector of the percentile for each age/sex to return.
#' Default is 50, but could be, for example `c(5, 10, 25, 50, 75, 90, 95)`.
#' @return A dataframe with columns
#'  * agemos Age in months
#'  * ageyrs Age in years
#'  * wt Weight in kilograms
#'  * ht Height or length in centimeters
#'  * bmi Body mass index $kg/m^2$
#'  * sex The selected sex(es)
#'  * percentile The selected percentile(s)
#' @author Michael Neely
#' @export

qgrowth <- function(sex = "B", agemos = (seq(0, 18) * 12), percentile = 50) {
  sex <- match.arg(sex)
  if (sex == "b" | sex == "B") {
    sex <- c("M", "F")
  } else {
    sex <- toupper(sex)
  }

  sub1 <- purrr::map2_df(sex, percentile, function(x, y) {
    growth %>%
      filter(SEX == x, (CHART == "wt  x age" | CHART == "length x age" |
        CHART == "ht x age"), PERCENTILE == y) %>%
      rename(agecat = AGE, percentile = PERCENTILE, sex = SEX)
  })

  sub2 <- tidyr::crossing(agemos, sex, percentile) %>% # all combinations
    mutate(agecat = ifelse(agemos <= 36, "0-36 mos", "2-18 years")) %>% # categorize age
    inner_join(., sub1, by = c("agecat", "sex", "percentile")) %>% # lookup combinations in CDC table
    group_by(agemos, sex, percentile, CHART) %>%
    filter(KNOT <= agemos) %>%
    slice_tail(n = 1) %>% # choose the maximum KNOT which is < agemos for each combination
    ungroup() %>%
    group_by(CHART) %>%
    rowwise() %>%
    mutate(corr_age = agemos - KNOT, measure = A + B1 * corr_age + B2 * corr_age**2 + B3 * corr_age**3) %>% # calculate appropriate measure
    ungroup() %>%
    mutate(across(CHART, stringr::str_replace, "length", "ht")) %>% # tidy labels
    mutate(across(CHART, stringr::str_replace, regex("\\s+x age"), "")) %>%
    select(agemos, sex, percentile, CHART, measure) %>%
    pivot_wider(id_cols = 1:3, names_from = CHART, values_from = measure) %>% # reform the data frame
    mutate(ageyrs = agemos / 12, bmi = wt / (ht / 100)**2) %>% # add age in years and BMI
    select(agemos, ageyrs, wt, ht, bmi, sex, percentile) # final form

  return(sub2)
}

#' @title Extract CDC pediatric BMI z-scores
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Will extract BMI z-scores based on a single age in months and sex. Overweight is
#' a z-score of >1.04, and obese is a z-score > 1.64. Calculations are based on
#' [CDC formulae](https://www.cdc.gov/nccdphp/dnpa/growthcharts/resources/biv-cutoffs.pdf)
#'
#' For a z-score > 3, which indicates an extreme BMI, consider using the modified
#' z-score and percentile.
#'
#' @param agemos The age in months.  Should be between 24 and 240.5.
#' @param sex A single quoted character: "M" for males, "F" for females.  Default is "M".
#' @param bmi The individual's BMI. If specified, `wt` and `ht` are not necessary
#' and will be ignored.
#' @param wt  The individual's weight in kg as an alternative to specifying `BMI`.
#' Will be ignored if `BMI` is specified.
#' @param ht  The individual's height in centimeters. Required if `wt` is specified
#' and `BMI` is not. Ignored if `BMI` is specified.
#' @param data Source data for calculations. Default is "CDC" which uses the [cdc_bmi]
#' dataset. The alternative is "NHANES", which uses the [ger_bmi] dataset.
#' @return A list with objects calculated for `agemos` and `sex`.
#'  * z Z-score
#'  * mod_z Modified Z-score for extreme BMI
#'  * per BMI percentile
#'  * mod_per Modified BMI percentile
#' @author Michael Neely
#' @examples
#' \dontrun{
#' zBMI(agemos = 36, bmi = 15)
#' }
#' @export

zBMI <- function(agemos, sex, bmi, wt, ht, data = "CDC") {
  if (agemos < 24 | agemos > 240) {
    stop("Agemos should be >=24 or <=240")
  }

  if (agemos >= 24.5 & tolower(data) == "cdc") {
    agemos <- floor(agemos) + 0.5
  } else {
    agemos <- floor(agemos)
  }

  all_bmi <- NULL # avoid CRAN check
  if (tolower(data) == "cdc") {
    all_bmi <- cdc_bmi
  } else {
    all_bmi <- ger_bmi
  }

  if (missing(sex)) {
    sex_var <- 1 # male
  } else {
    if (is.character(sex)) {
      sex_var <- 1 + as.numeric(tolower(sex) == "f")
    } else {
      stop("Use 'M' for male and 'F' for female (case insensitive).")
    }
  }

  if (missing(bmi)) {
    if (missing(wt) | missing(ht)) stop("If BMI not specified, supply both weight and height.")
    bmi <- wt / (ht / 100)**2
  }

  this_bmi <- all_bmi %>% dplyr::filter(Agemos == agemos & Sex == sex_var)
  z_bmi <- ((bmi / this_bmi$M)^this_bmi$L - 1) / (this_bmi$L * this_bmi$S) # z score

  # find values for modified z score to correct for extremes
  z0 <- this_bmi$M # median
  z2 <- this_bmi$M * (1 + this_bmi$L * this_bmi$S * 2)**(1 / this_bmi$L)
  z_dist <- (z2 - z0) / 2
  mod_z_bmi <- (bmi - z0) / z_dist

  # return
  return(list(
    z = z_bmi, mod_z = mod_z_bmi,
    per = pnorm(z_bmi, 0, 1), mod_per = pnorm(mod_z_bmi, 0, 1)
  ))
}
