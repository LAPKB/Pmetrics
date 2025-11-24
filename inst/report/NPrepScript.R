require(Pmetrics)
wd <- commandArgs()[6]
icen <- commandArgs()[7]
parallel <- as.logical(commandArgs()[8])
setwd(wd)
# PMreport(wd,icen=icen,type="NPAG",parallel=parallel)
unused <- Pmetrics:::makeRdata(wd, F, 1)
res <- PM_load(file = "PMout.Rdata")
Pmetrics:::PM_report(res, outfile = "NPAGreport.html")
