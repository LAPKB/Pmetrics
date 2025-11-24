cdc_bmi <- read_csv("data-raw/bmiagerev.csv")
ger_bmi <- read_csv("data-raw/bmiGerhart.csv")

usethis::use_data(cdc_bmi,overwrite=T)
usethis::use_data(ger_bmi,overwrite=T)
