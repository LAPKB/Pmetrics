require(Pmetrics)
wd <- commandArgs()[6]
setwd(wd)
Pmetrics:::GenAlData(wd)
