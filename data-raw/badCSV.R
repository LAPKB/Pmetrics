## code to prepare `badCSV` dataset goes here

library(Pmetrics)
badCSV <- PMreadMatrix("data-raw/bad.csv")

usethis::use_data(badCSV)
