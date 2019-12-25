Rscript -e 'library(RSclient)
rsc <- RSconnect(port = 6311)
RSshutdown(rsc)'

Rscript -e 'remove.packages("Pmetrics")'
rm -rf /Library/Frameworks/R.framework/Versions/3.6/Resources/library/Pmetrics
Rscript Source/MNmakePmetrics.R

Rscript -e '
library(Rserve)
Rserve()'

