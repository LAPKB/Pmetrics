## code to prepare `model` dataset goes here
model <- readLines("data-raw/model.txt")

usethis::use_data(model)
