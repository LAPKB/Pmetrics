library(RSclient)
rsc <- RSconnect(port = 6311)
RSshutdown(rsc)


remove.packages("Pmetrics")
rm -rf /Library/Frameworks/R.framework/Versions/3.6/Resources/library/Pmetrics
Rscript Source/MNmakePmetrics.R

library(Rserve)
Rserve()

