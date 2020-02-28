pkgname <- "Pmetrics"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "Pmetrics-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('Pmetrics')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("NpdeData-class")
### * NpdeData-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NpdeData-class
### Title: Class "NpdeData" representing the structure of the longitudinal
###   data
### Aliases: NpdeData-class NpdeData show,NpdeData-method
###   print,NpdeData-method summary,NpdeData-method [,NpdeData-method
###   [<-,NpdeData-method
### Keywords: classes

### ** Examples


methods(class="NpdeData")

showClass("NpdeData")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NpdeData-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NpdeObject-class")
### * NpdeObject-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NpdeObject-class
### Title: Class "NpdeObject"
### Aliases: NpdeObject-class NpdeObject NpdeObject-class,
###   show,NpdeObject-method print,NpdeObject-method
###   showall,NpdeObject-method summary,NpdeObject-method
###   test,NpdeObject-method [,NpdeObject-method [<-,NpdeObject-method
###   npde.main,NpdeObject npde.save,NpdeObject npde.graphs,NpdeObject
###   plot,NpdeObject
### Keywords: classes

### ** Examples


methods(class="NpdeObject")

showClass("NpdeObject")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NpdeObject-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NpdeRes-class")
### * NpdeRes-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NpdeRes-class
### Title: Class "NpdeRes"
### Aliases: NpdeRes-class NpdeRes NpdeRes-class, show,NpdeRes-method
###   print,NpdeRes-method showall,NpdeRes-method summary,NpdeRes-method
###   test,NpdeRes-method [,NpdeRes-method [<-,NpdeRes-method
### Keywords: classes internal

### ** Examples


data(theopp)

methods(class="NpdeRes")

showClass("NpdeRes")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NpdeRes-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NpdeSimData-class")
### * NpdeSimData-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NpdeSimData-class
### Title: Class "NpdeSimData" representing the structure of the
###   longitudinal data
### Aliases: NpdeSimData-class NpdeSimData show,NpdeSimData-method
###   [,NpdeSimData-method [<-,NpdeSimData-method
### Keywords: classes

### ** Examples


showClass("NpdeSimData")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NpdeSimData-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PMcheck")
### * PMcheck

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PMcheck
### Title: Check Pmetrics Inputs for Errors
### Aliases: PMcheck

### ** Examples

## Not run: 
##D data(PMex3)
##D err <- PMcheck(badData)
##D #look at the errors.xlsx file in the working directory
##D #try to automatically fix what can be fixed
##D goodData <- PMcheck(badData,fix=T)
##D PMcheck(goodData)
##D #you have to fix manually problems which require data entry
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PMcheck", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PMnews")
### * PMnews

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PMnews
### Title: Pmetrics changelog
### Aliases: PMnews

### ** Examples

PMnews()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PMnews", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PMtree")
### * PMtree

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PMtree
### Title: Create a new Pmetrics folder tree
### Aliases: PMtree

### ** Examples

PMtree("DrugX")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PMtree", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PMwriteMatrix")
### * PMwriteMatrix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PMwriteMatrix
### Title: Write a Pmetrics .csv Matrix File
### Aliases: PMwriteMatrix

### ** Examples

## Not run: 
##D data <- PMreadMatrix(paste(.libPaths(),"/Pmetrics/example/NPAG/PMex1.csv",sep=""))
##D data
##D #write to the current directory
##D PMwriteMatrix(data,"PMex1.csv")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PMwriteMatrix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SIMrun")
### * SIMrun

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SIMrun
### Title: Run the Pmetrics Simulator
### Aliases: SIMrun

### ** Examples

## Not run: 
##D wd <- getwd()
##D #make 1 lognormal distribution for each parameter
##D weights <- 1
##D mean <- log(c(0.7,0.05,100))
##D cov <- matrix(rep(0,length(mean)**2),ncol=length(mean))
##D diag(cov) <- (c(0.15,0.15,0.15)*mean)**2
##D #make the prior for the simulation
##D poppar <- list(weights,mean,cov)
##D setwd(paste(normalizePath(get("PmetricsPath",envir=PMenv),winslash="/"),"/Pmetrics/example/Sim",sep=""))
##D #run simulation
##D SIMrun(poppar,"temp1.csv",nsim=15,model="model1.for",obsNoise=c(0.02,0.1,0,0),makecsv="PMex1.csv",outname="example",clean=T)
##D #extract results of simulation
##D simout <- SIMparse("example1.txt")
##D file.remove("example1.txt")
##D #plot simulated profiles (use help(plot.PMsim) for more information)
##D plot(simout,ci=0,probs=NA,x.qlab=0.75,log=T,col="red",lwd=2,pch=NA,join=T)
##D setwd(wd)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SIMrun", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dist.pred.sim")
### * dist.pred.sim

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dist.pred.sim
### Title: Compute distribution of pd/npde using simulations
### Aliases: dist.pred.sim calcnpde.sim

### ** Examples


data(theopp)
data(simtheopp)
x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
# Use random samples from N(0,1) to obtain a prediction interval on the empirical cdf of the npde
plot(x,plot.type="ecdf",bands=TRUE,approx.pi=TRUE)
# defaults to computing the pd and npde for 100 simulated datasets (in the theophylline example, this uses all the simulated datasets)
x<-dist.pred.sim(x)
# Use the npde from the simulated datasets to obtain a prediction interval on the empirical cdf
plot(x,plot.type="ecdf",bands=TRUE,approx.pi=FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dist.pred.sim", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gof.test")
### * gof.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gof.test
### Title: Test on npde or pd
### Aliases: gof.test gof.test.NpdeObject gof.test.numeric gof.test.NpdeRes
###   print.gof.test
### Keywords: test

### ** Examples


data(theopp)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gof.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("kurtosis")
### * kurtosis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: kurtosis
### Title: Kurtosis
### Aliases: kurtosis
### Keywords: univar

### ** Examples


x <- rnorm(100)
kurtosis(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("kurtosis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("makeAUC")
### * makeAUC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: makeAUC
### Title: Calculation of AUCs
### Aliases: makeAUC

### ** Examples

data(PMex1)
op <- makeOP(NPdata.1)
makeAUC(op)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("makeAUC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("makeCov")
### * makeCov

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: makeCov
### Title: Extract covariate data
### Aliases: makeCov

### ** Examples

data(PMex1)
cov <- makeCov(NPdata.1)
cov
names(cov)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("makeCov", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("makeCycle")
### * makeCycle

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: makeCycle
### Title: Summarize Pmetrics Run Cycle Information
### Aliases: makeCycle

### ** Examples

data(PMex1)
cycle <- makeCycle(NPdata.1)
cycle
names(cycle)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("makeCycle", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("makeErrorPoly")
### * makeErrorPoly

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: makeErrorPoly
### Title: Assay error polynomial coefficients
### Aliases: makeErrorPoly

### ** Examples

makeErrorPoly(obs=c(0,5,50,100,250,500,1000),sd=c(1,0.4,4.5,12,34,60,190))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("makeErrorPoly", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("makeFinal")
### * makeFinal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: makeFinal
### Title: Summarize NPAG or IT2B Final Cycle Population Values
### Aliases: makeFinal

### ** Examples

data(PMex1)
final <- makeFinal(NPdata.1)
final
names(final)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("makeFinal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("makeOP")
### * makeOP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: makeOP
### Title: Generated observed vs. predicted data
### Aliases: makeOP

### ** Examples

data(PMex1)
op <- makeOP(NPdata.1)
op
names(op)
summary(op)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("makeOP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("npde")
### * npde

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: npde
### Title: Compute normalised prediction distribution errors
### Aliases: npde autonpde
### Keywords: models

### ** Examples


data(theopp)
data(simtheopp)

# Calling autonpde with dataframes

x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
x

# Calling autonpde with names of files to be read from disk

write.table(theopp,"theopp.tab",quote=FALSE,row.names=FALSE)
write.table(simtheopp,"simtheopp.tab",quote=FALSE,row.names=FALSE)
x<-autonpde(namobs="theopp.tab", namsim="simtheopp.tab", iid = 1,
ix = 3, iy = 4, imdv=0, boolsave = FALSE)

head(x["results"]["res"])




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("npde", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("npdeData")
### * npdeData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: npdeData
### Title: Creates a NpdeData object
### Aliases: npdeData
### Keywords: models

### ** Examples


data(theopp)

x<-npdeData(theopp) # Automatic detection
print(x)
x<-npdeData(theopp,name.group="ID",name.predictor="Time",name.response="Conc", 
name.covariates=c("Wt"),units=list(x="hr",y="mg/L",covariates="kg")) # Explicit
print(x)
plot(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("npdeData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.NpdeData")
### * plot.NpdeData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.NpdeData
### Title: Plots a NpdeData object
### Aliases: plot.NpdeData
### Keywords: plot

### ** Examples


data(theopp)

x<-npdeData(theopp,name.group="ID",name.predictor="Time",name.response="Conc", 
name.covariates=c("Wt"),units=list(x="hr",y="mg/L",covariates="kg"))
plot(x)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.NpdeData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.NpdeObject")
### * plot.NpdeObject

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.NpdeObject
### Title: Plots a NpdeObject object
### Aliases: plot.NpdeObject
### Keywords: plot

### ** Examples


data(theopp)
data(simtheopp)

x<-autonpde(theopp,simtheopp,iid="ID",ix="Time", iy="Conc", boolsave=FALSE)
plot(x)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.NpdeObject", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.NpdeRes")
### * plot.NpdeRes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.NpdeRes
### Title: Plots a NpdeRes object
### Aliases: plot.NpdeRes
### Keywords: internal plot

### ** Examples


data(theopp)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.NpdeRes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.PMcov")
### * plot.PMcov

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.PMcov
### Title: Plot Pmetrics Covariate objects
### Aliases: plot.PMcov

### ** Examples

data(PMex1)
plot(cov.1,V~wt)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.PMcov", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.PMcycle")
### * plot.PMcycle

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.PMcycle
### Title: Plot NPAG Cycle Information
### Aliases: plot.PMcycle

### ** Examples

data(PMex1)
plot(cycle.1)
plot(cycle.1,omit=0)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.PMcycle", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.PMfinal")
### * plot.PMfinal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.PMfinal
### Title: Plot Pmetrics Final Cycle Parameter Value Distributions
### Aliases: plot.PMfinal

### ** Examples

data(PMex1)
plot(final.1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.PMfinal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.PMmatrix")
### * plot.PMmatrix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.PMmatrix
### Title: Plot PMmatrix Time-Output Data
### Aliases: plot.PMmatrix

### ** Examples

data(PMex1)
plot(mdata.1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.PMmatrix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.PMop")
### * plot.PMop

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.PMop
### Title: Plot Pmetrics Observed vs. Predicted Objects
### Aliases: plot.PMop

### ** Examples

data(PMex1)
plot(op.1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.PMop", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("skewness")
### * skewness

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: skewness
### Title: Skewness
### Aliases: skewness
### Keywords: univar

### ** Examples


x <- rnorm(100)
skewness(x)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("skewness", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.PMfinal")
### * summary.PMfinal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.PMfinal
### Title: Summary Statistics for PMfinal Objects
### Aliases: summary.PMfinal

### ** Examples

data(PMex1)
final <- makeFinal(NPdata.1)
summary(final)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.PMfinal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
