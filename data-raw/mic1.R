
#copied table from http://mic.eucast.org/Eucast2/regShow.jsp?Id=1214 to csv file
mic <- read.csv("data-raw/mic1.csv")
names(mic) <- c("mic","n")

usethis::use_data("mic1")
