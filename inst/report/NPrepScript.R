require(Pmetrics)
wd <- commandArgs()[6]
icen <- commandArgs()[7]
parallel <- as.logical(commandArgs()[8])
setwd(wd)
#PMreport(wd,icen=icen,type="NPAG",parallel=parallel)
Pmetrics:::makeRdata(wd, F, 1)
res <- Pmetrics:::PM_load(wd)
Pmetrics:::PM_report(res,outfile = paste0(wd,"/NPAGreport.html"))



