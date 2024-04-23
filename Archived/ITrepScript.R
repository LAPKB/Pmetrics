require(Pmetrics)
wd <- commandArgs()[6]
icen <- commandArgs()[7]
setwd(wd)
PMreport(wd,icen=icen,type="IT2B",parallel=F)


