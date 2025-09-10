require(Pmetrics)
wd <- commandArgs()[6]
icen <- commandArgs()[7]
setwd(wd)
unused <- Pmetrics:::makeRdata(wd, 2)
res <- PM_load(file = "PMout.Rdata")
Pmetrics:::PM_report(res, outfile = "IT2Breport.html")


