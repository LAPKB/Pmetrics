require(Pmetrics)
wd <- commandArgs()[6]
icen <- commandArgs()[7]
parallel <- as.logical(commandArgs()[8])
setwd(wd)
PMreport(wd,icen=icen,type="NPAG",parallel=parallel)



