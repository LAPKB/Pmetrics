require(Pmetrics)
args <- commandArgs(trailingOnly=TRUE)
wd <- args[1]
icen <- args[2]
parallel <- as.logical(args[3])
setwd(wd)
PMreport(wd,icen=icen,type="NPAG",parallel=parallel)



