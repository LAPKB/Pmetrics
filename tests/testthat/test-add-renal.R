library(Pmetrics)

make_renal_standard_data <- function() {
  base_data <- dataEx$clone(deep = TRUE)$standard_data
  subject_ids <- sort(unique(base_data$id))
  n_subjects <- length(subject_ids)
  subject_heights <- base_data[!duplicated(base_data$id), c("id", "height")] |>
    dplyr::arrange(id) |>
    dplyr::pull("height")

  make_group <- function(group_name, id_offset, scr_range, cys_range, bun_range, black_pattern) {
    scr_values <- seq(scr_range[1], scr_range[2], length.out = n_subjects)
    cys_values <- seq(cys_range[1], cys_range[2], length.out = n_subjects)
    bun_values <- seq(bun_range[1], bun_range[2], length.out = n_subjects)

    subject_profile <- tibble::tibble(
      id = subject_ids + id_offset,
      renal_group = group_name,
      scr = scr_values,
      scr_si = scr_values * 88.4,
      height_m = subject_heights / 100,
      cysC = cys_values,
      bun = bun_values,
      bun_si = bun_values / 2.8,
      black = rep(black_pattern, length.out = n_subjects)
    )

    base_data |>
      dplyr::mutate(id = id + id_offset) |>
      dplyr::left_join(subject_profile, by = "id")
  }

  dplyr::bind_rows(
    make_group("normal", 0L, c(0.7, 1.0), c(0.7, 1.1), c(10, 18), c(0L, 1L)),
    make_group("mild", 100L, c(1.3, 1.9), c(1.0, 1.8), c(18, 30), c(1L, 0L)),
    make_group("moderate", 200L, c(2.0, 3.0), c(1.6, 2.6), c(30, 50), c(0L, 1L))
  )
}

make_renal_pm_data <- function(standard_data) {
  renal_data <- dataEx$clone(deep = TRUE)
  renal_data$standard_data <- standard_data
  renal_data
}

subject_rows <- function(data) {
  data |>
    dplyr::arrange(id, time) |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::arrange(.data$renal_group, id)
}

expected_jelliffe <- function(age, wt, scr, male) {
  ess <- ifelse(
    male == 1,
    wt * (29.3 - 0.203 * age),
    wt * (25.1 - 0.175 * age)
  )

  ess_cor <- ess * (1.035 - 0.0337 * scr)
  ess_cor / (14.4 * scr)
}

expected_cg <- function(age, wt, scr, male) {
  base <- ((140 - age) * wt) / (72 * scr)
  ifelse(male == 1, base, base * 0.85)
}

expected_mdrd <- function(age, scr, black) {
  175 * scr^-1.154 * age^-0.203 * ifelse(black == 1, 1.212, 1)
}

expected_ckd <- function(age, scr, male) {
  male_value <- 141 * pmin(scr / 0.9, 1)^-0.411 * pmax(scr / 0.9, 1)^-1.209 * 0.993^age
  female_value <- 144 * pmin(scr / 0.7, 1)^-0.329 * pmax(scr / 0.7, 1)^-1.209 * 0.993^age

  ifelse(male == 1, male_value, female_value)
}

expected_schwartz_iii <- function(height, scr, cysC, bun, male) {
  39.1 * (height / scr)^0.516 * (1.8 / cysC)^0.294 * (30 / bun)^0.169 * 1.099^male * (height / 1.4)^0.2
}

expected_schwartz_ii <- function(height, scr, cysC, bun) {
  41.1 * (height / scr)^0.510 * (1.8 / cysC)^0.272 * (30 / bun)^0.171
}

expected_schwartz_ib <- function(height, scr, bun) {
  40.7 * (height / scr)^0.640 * (30 / bun)^0.202
}

expected_schwartz_ia <- function(height, scr, cysC) {
  41.6 * (height / scr)^0.599 * (1.8 / cysC)^0.317
}

expected_schwartz_updated <- function(height, scr) {
  41.3 * height / scr
}

test_that("add_renal adds expected creatinine clearance and eGFR columns", {
  renal_std <- make_renal_standard_data()
  renal_data <- make_renal_pm_data(renal_std)

  expect_true(any(renal_std$gender == 0))
  expect_true(any(renal_std$gender == 1))
  expect_true(any(renal_std$black == 0))
  expect_true(any(renal_std$black == 1))

  jelliffe <- add_renal(renal_data, method = "jelliffe", male = "gender")
  jelliffe_rows <- subject_rows(jelliffe)
  expect_equal(
    jelliffe_rows$crcl_jelliffe,
    expected_jelliffe(jelliffe_rows$age, jelliffe_rows$wt, jelliffe_rows$scr, jelliffe_rows$gender),
    tolerance = 1e-8
  )

  cockcroft_gault <- add_renal(renal_data, method = "cockcroft-gault", male = "gender")
  cg_rows <- subject_rows(cockcroft_gault)
  expect_equal(
    cg_rows$crcl_cg,
    expected_cg(cg_rows$age, cg_rows$wt, cg_rows$scr, cg_rows$gender),
    tolerance = 1e-8
  )

  mdrd <- add_renal(renal_data, method = "mdrd", male = "gender", black = "black")
  mdrd_rows <- subject_rows(mdrd)
  expect_equal(
    mdrd_rows$gfr_mdrd,
    expected_mdrd(mdrd_rows$age, mdrd_rows$scr, mdrd_rows$black),
    tolerance = 1e-8
  )

  ckd <- add_renal(renal_data, method = "ckd-epi", male = "gender")
  ckd_rows <- subject_rows(ckd)
  expect_equal(
    ckd_rows$gfr_ckd,
    expected_ckd(ckd_rows$age, ckd_rows$scr, ckd_rows$gender),
    tolerance = 1e-8
  )
})

test_that("add_renal supports SI creatinine inputs and Schwartz variants", {
  renal_std <- make_renal_standard_data()
  renal_data <- make_renal_pm_data(renal_std)

  cockcroft_gault <- add_renal(renal_data, method = "cg", male = "gender")
  cockcroft_gault_si <- add_renal(renal_data, method = "cg", male = "gender", scr = "scr_si", SI = TRUE)
  expect_equal(cockcroft_gault_si$crcl_cg, cockcroft_gault$crcl_cg, tolerance = 1e-8)

  mdrd <- add_renal(renal_data, method = "mdrd", male = "gender", black = "black")
  mdrd_si <- add_renal(renal_data, method = "mdrd", male = "gender", black = "black", scr = "scr_si", SI = TRUE)
  expect_equal(mdrd_si$gfr_mdrd, mdrd$gfr_mdrd, tolerance = 1e-8)

  ckd <- add_renal(renal_data, method = "ckd-epi", male = "gender")
  ckd_si <- add_renal(renal_data, method = "ckd-epi", male = "gender", scr = "scr_si", SI = TRUE)
  expect_equal(ckd_si$gfr_ckd, ckd$gfr_ckd, tolerance = 1e-8)

  schwartz_iii <- add_renal(
    renal_data,
    method = "schwartz",
    male = "gender",
    ht = "height_m",
    scr = "scr",
    bun = "bun",
    cysC = "cysC"
  )
  schwartz_iii_rows <- subject_rows(schwartz_iii)
  expect_equal(nrow(schwartz_iii_rows), dplyr::n_distinct(schwartz_iii_rows$id))
  expect_true(all(is.finite(schwartz_iii_rows$gfr_schwartz)))
  expect_true(all(schwartz_iii_rows$gfr_schwartz > 0))

  schwartz_si <- add_renal(
    renal_data,
    method = "schwartz",
    male = "gender",
    ht = "height_m",
    scr = "scr_si",
    bun = "bun_si",
    cysC = "cysC",
    SI = TRUE
  )
  expect_true(all(is.finite(schwartz_si$gfr_schwartz)))
  expect_equal(nrow(schwartz_si), nrow(renal_data$standard_data))

  schwartz_no_gender_data <- make_renal_pm_data(dplyr::select(renal_std, -gender))
  schwartz_ii <- add_renal(
    schwartz_no_gender_data,
    method = "schwartz",
    ht = "height_m",
    scr = "scr",
    bun = "bun",
    cysC = "cysC"
  )
  schwartz_ii_rows <- subject_rows(schwartz_ii)
  expect_equal(
    schwartz_ii_rows$gfr_schwartz,
    expected_schwartz_ii(schwartz_ii_rows$height_m, schwartz_ii_rows$scr, schwartz_ii_rows$cysC, schwartz_ii_rows$bun),
    tolerance = 1e-8
  )

  schwartz_bun_only_data <- make_renal_pm_data(dplyr::select(renal_std, -gender, -cysC))
  schwartz_ib <- add_renal(
    schwartz_bun_only_data,
    method = "schwartz",
    ht = "height_m",
    scr = "scr",
    bun = "bun"
  )
  schwartz_ib_rows <- subject_rows(schwartz_ib)
  expect_equal(
    schwartz_ib_rows$gfr_schwartz,
    expected_schwartz_ib(schwartz_ib_rows$height_m, schwartz_ib_rows$scr, schwartz_ib_rows$bun),
    tolerance = 1e-8
  )

  schwartz_cysc_only_data <- make_renal_pm_data(dplyr::select(renal_std, -gender, -bun))
  schwartz_ia <- add_renal(
    schwartz_cysc_only_data,
    method = "schwartz",
    ht = "height_m",
    scr = "scr",
    cysC = "cysC"
  )
  schwartz_ia_rows <- subject_rows(schwartz_ia)
  expect_equal(
    schwartz_ia_rows$gfr_schwartz,
    expected_schwartz_ia(schwartz_ia_rows$height_m, schwartz_ia_rows$scr, schwartz_ia_rows$cysC),
    tolerance = 1e-8
  )

  schwartz_minimal_data <- make_renal_pm_data(dplyr::select(renal_std, -gender, -bun, -cysC))
  schwartz_updated <- add_renal(
    schwartz_minimal_data,
    method = "schwartz",
    ht = "height_m",
    scr = "scr"
  )
  schwartz_updated_rows <- subject_rows(schwartz_updated)
  expect_equal(
    schwartz_updated_rows$gfr_schwartz,
    expected_schwartz_updated(schwartz_updated_rows$height_m, schwartz_updated_rows$scr),
    tolerance = 1e-8
  )
})

test_that("add_renal requires a PM_data object", {
  expect_error(add_renal(list(), method = "cg"), "PM_data")
})
