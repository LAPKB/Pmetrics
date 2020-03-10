## code to prepare `growth` dataset goes here
growth <- read.table(url("https://www.cdc.gov/growthcharts/data/1977coefficients2.txt"))
write.csv(growth,"data-raw/growth.csv",row.names=F)

growth <- read.csv("data-raw/growth.csv")

usethis::use_data(growth,overwrite=T)
