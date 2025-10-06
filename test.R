library(tidyverse)
library(openxlsx)
devtools::load_all()

wd <- OneDrive("Documents/LAPK/Pmetrics/Drusano/TB_2024/checkerboards")

mod <- PM_model$new(

  pri = list(
    kg = ab(0, 0.6),
    kk = ab(0.6, 1.5),
    h1 = ab(0, 3),
    h2 = ab(0, 3),
    e50_1 = ab(0, 2),
    e50_2 = ab(0, 2),
    alpha = ab(-2, 2)
    #popmax = ab(9, 10)
  ),

  cov = list(
    TBAJ = interp(),
    BDQ = interp()
  ),

  eqn = function(){
    v <- 1 # fix volumes to 1
    #eg <- 1.0 - x[1] / 10**popmax # growth suppression term
    eg <- 1.0 # no growth suppression term
    a <- TBAJ / (v * e50_1)
    b <- BDQ / (v * e50_2)
    w <- alpha * a * b / (e50_1 * e50_2)
    e2 <- get_e2(a, b, w, 1.0 / h1, 1.0 / h2, alpha) # 2-drug effect model
    dx[1] = x[1] * (kg * eg - kk * e2) # CFU counts
  },

  out = function(){
    y[1] = log10(x[1]) # x[1] is linear scale, but observations are in log10 scale
  },
  
  err = list(
    additive(1, c(0.1, 0, 0, 0), fixed = TRUE)

  )
)

dat <- read.xlsx(file.path(wd, "RO1 Checkerboards 5 Tebi + TBAJ876 vs J37Rv-LPG.xlsx"), sheet = 6,
rows = 7:16, cols = 15:23, rowNames = TRUE)

row_name <- "TBAJ"
col_name <- "TEBI_CLV"
df <- dat %>%
  tibble::rownames_to_column(row_name) %>%
  pivot_longer(
    cols = -!!row_name,
    names_to = col_name,
    values_to = "OUT" #CFU
  ) %>%
  arrange(!!row_name, !!col_name) %>%
  # IDs 1..64 in row-major order (row, then column)
  mutate(ID = row_number(), TIME = 14, DOSE = NA) %>%
  select(ID, TIME, DOSE, OUT, !!row_name, !!col_name)

df2 <- df %>% mutate(TIME = 0, OUT = NA, DOSE = 3.15E+06) %>% 
  bind_rows(df) %>%
  arrange(ID, TIME) %>%
  mutate(across(c(TBAJ, TEBI_CLV), as.numeric))

names(df2) <- tolower(names(df2))

cfu <- PM_data$new(df2)

run3 <- mod$fit(cycles = 10, data = cfu, run = 3, path = wd, overwrite = TRUE, idelta = 1, points = 3000)