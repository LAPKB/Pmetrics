Rscript Source/shutdownRserve.R
Rscript Source/uninstallPmetrics.R
rm -rf /Library/Frameworks/R.framework/Versions/3.6/Resources/library/Pmetrics
Rscript Source/MNmakePmetrics.R
Rscript Source/startRserve.R
