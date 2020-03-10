'Pause...' <- function(...) invisible(readline())

#Lines that start with "#" are comments and ignored by R.  Follow the directions in them.
#Execute each non-comment line in this script by putting your cursor on it and sending it to the R console.
#You can do this in several ways:
#     Windows
#        R-studio
#           1) The Run button at the top
#           2) Ctrl-Enter
#        R GUI - when the script window is active
#           1) The Run line or selection button at the top 
#           2) Ctrl-R
#     Mac
#        R-studio
#           1) The Run button at the top
#           2) Command-Enter
#        R GUI 
#           1) Command-Enter 

#Load Pmetrics into memory.  You must include this line at the beginning of every script.

library(Pmetrics)

'Pause...'('Press return')

#EXAMPLE NPAG RUN - tlag, ka, kel, vol 
#It is useful to annotate your runs so that you can remember what you did later!
#Tell R where your working directory is going to be.
#Windows users:  Make sure that you separate directories with a forward slash "/".  
#Unfortunately, Windows is the only OS that uses backslashes "\", so R conforms to
#Unix/Linux style.

#The first thing to do is set your working directory.
#This tells R where your model and data files are located.
#Normally you would type the specific path, e.g.
#setwd("~/Pmetrics")
#but here will choose it interactively.
#Choose the model.txt file that you downloaded.
'Pause...'('Press return')

setwd(dirname(file.choose()))

#Run NPAG - type ?NPrun in the R console for help on NPrun and arguments you can specify. 
#You must specify a model and data file.
#In this case, we are using the default model name of "model.txt", so we don't have to explicitly specify it.
#Other values are set to their defaults based on the model and data files.

NPrun(data="ex.csv")

#Windows users: launch the npscript.bat file in your working directory
#Mac users: a terminal window will open and run; don't worry about pauses; the program has not crashed"

#After the run is complete you need get the extracted information back into R.
#They will be sequentially numbered as /1, /2, /3,... in you working directory.
'Pause...'('Press return')

PMload(1)

#Load all the extracted information.  The "1" in the parentheses tells Pmetrics to
#look in the /1 folder.  You can load multiple runs with PMload(2), PMload(3),...,
#and you can compare them with PMcompare().
#Type ?PMload in the R console for help on PMload.


#Your original data file is also loaded with PMload.
'Pause...'('Press return')

#Plot the raw data with various options.  Type ?plot.PMmatrix in the R console for help.
plot(mdata.1,overlay=T,xlim=c(120,144))
plot(mdata.1,pred=post.1,overlay=T,group="gender",pch=NA,cex=1,lwd=2,xlim=c(120,144),join=F,doses=F,legend=list(legend=c("Female","Male")),col=c("red","black"),log=T)

#Plot some observed vs. predicted data.  Type ?plot.PMop in the R console for help.
par(mfrow=c(1,2))
plot(op.1$pop1,type="pop",main="Population",square=T,ref=T,reg=T,col="blue")
plot(op.1$post1,main="Posterior",log=F,square=T,ref=T,reg=F,lowess=T)
par(mfrow=c(1,1))


#you can also specify op plots like this
plot(op.1,type="post",outeq=1,reg=F,lowess=T,col="black")
plot(op.1,type="post",resid=T)

summary(op.1)
summary(op.1$post1)


#Plot final population joint density information.  Type ?plot.PMfinal in the R console for help.
plot(final.1)
plot(final.1,density=T)
plot(final.1,Ke~V)

#Plot cycle information.  Type ?plot.PMcycle in the R console for help.
plot(cycle.1)

#Plot covariate information.  Type ?plot.PMcov in the R console for help.
plot(cov.1,Ke~age,lowess=F,reg=T,pch=3)
plot(cov.1,V~wt)
#when time is the x variable, the y variable is aggregated by subject
plot(cov.1,I(V*wt)~time)
#Look at all possible covariate-parameter relationships by multiple linear regression with forward
#and backward elimination - type ?PMstep in the R console for help.
PMstep(cov.1)








