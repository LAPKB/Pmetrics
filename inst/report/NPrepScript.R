require(Pmetrics)
wd <- commandArgs()[6]
icen <- commandArgs()[7]
parallel <- as.logical(commandArgs()[8])
setwd(wd)
unused <- Pmetrics:::makeRdata(wd, 1)
res <- PM_load(file = "PMout.Rdata")
Pmetrics:::PM_report(res, outfile = "NPAGreport.html")
