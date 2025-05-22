library(readr)
library(Pmetrics)

wd <- getwd()
setwd(OneDrive("/Documents/LAPK/Development/Pmetrics_rust/data-raw"))

# BMI data -------------------------------------------------------------

cdc_bmi <- read_csv("bmiagerev.csv")
ger_bmi <- read_csv("bmiGerhart.csv")

usethis::use_data(cdc_bmi, overwrite = TRUE)
usethis::use_data(ger_bmi, overwrite = TRUE)


# Growth ------------------------------------------------------------------

growth <- read.csv("growth.csv")
usethis::use_data(growth, overwrite = TRUE)


# Locales -----------------------------------------------------------------

locales <- read_csv("locales.csv")
usethis::use_data(locales, overwrite = TRUE)


# MIC ---------------------------------------------------------------------

#copied table from http://mic.eucast.org/Eucast2/regShow.jsp?Id=1214 to csv file
mic1 <- read_csv("mic1.csv")
names(mic1) <- c("mic","n")
usethis::use_data(mic1, overwrite = TRUE)



# Run Files -------------------------------------------------------------------

model <- readLines("model.txt")
usethis::use_data(model, overwrite = TRUE)

# model file
modEx <- PM_model$new("model.txt")
usethis::use_data(modEx, overwrite = TRUE)

# data
dataEx <- PM_data$new("ex.csv")
usethis::use_data(dataEx, overwrite = T)

# fit object
fitEx <- PM_fit$new(model = modEx, data = dataEx)
usethis::use_data(dataEx, overwrite = T)

# bad data
badData <- PM_data$new("bad.csv")
usethis::use_data(badData, overwrite = T)

#do the run

setwd("Runs")
# NPAG
fitEx$run(run = 1, overwrite = TRUE)

NPex <- PM_load(1)
# NPex$validate(limits = NA)
# usethis::use_data(NPex, overwrite = T)

# IT2B
# fitEx$run(run = 2, engine = "IT2B", overwrite = TRUE, intern = TRUE)

# ITex <- PM_load(2)
# usethis::use_data(ITex, overwrite = TRUE)

#setwd("..")

# simulator example
simEx <- NPex$sim(
  limits = c(0, 3), data = PM_data$new("ptaex1.csv"),
  predInt = c(120, 144, 0.5), seed = rep(-17, 4)
)
usethis::use_data(simEx, overwrite = TRUE)


setwd(wd)


