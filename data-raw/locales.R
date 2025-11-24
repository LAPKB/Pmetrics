locales <- readr::read_csv("data-raw/locales.csv")

usethis::use_data(locales,overwrite=T)
