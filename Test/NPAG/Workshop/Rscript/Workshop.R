#Use PMmanual() for help
library(Pmetrics)

#Run 1 - add your run description here

setwd("/Users/Neely/LAPK/PmetricsSource/Test/NPAG/Workshop/Runs")
NPrun(model="[replace with your model file name]",data="[replace with your data file name]")
PMload(1)

#Plots

plot(op.1,pred.type="pop")
plot(op.1)
plot(final.1)
plot(cycle.1)
