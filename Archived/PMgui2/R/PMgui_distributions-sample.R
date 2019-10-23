# Distributions menu dialogs for selecting samples

# last modified 2012-01-26 by J. Fox
# modified by Miroslav M. Ristic (15 January 2011)


normalDistributionSamples <- function(){distributionSamples("normal")}
tDistributionSamples <- function(){distributionSamples("t")}
chisqDistributionSamples <- function(){distributionSamples("chisq")}
FDistributionSamples <- function(){distributionSamples("F")}
exponentialDistributionSamples <- function(){distributionSamples("exponential")}
uniformDistributionSamples <- function(){distributionSamples("uniform")}
betaDistributionSamples <- function(){distributionSamples("beta")}
CauchyDistributionSamples <- function(){distributionSamples("Cauchy")}
logisticDistributionSamples <- function(){distributionSamples("logistic")}
lognormalDistributionSamples <- function(){distributionSamples("lognormal")}
gammaDistributionSamples <- function(){distributionSamples("gamma")}
WeibullDistributionSamples <- function(){distributionSamples("Weibull")}
GumbelDistributionSamples <- function(){distributionSamples("Gumbel")}
binomialDistributionSamples <- function(){distributionSamples("binomial")}
PoissonDistributionSamples <- function(){distributionSamples("Poisson")}
geomDistributionSamples <- function(){distributionSamples("geom")}
hyperDistributionSamples <- function(){distributionSamples("hyper")}
negbinomialDistributionSamples <- function(){distributionSamples("negbinomial")}

distributionSamples <- function(nameVar) {
	fVar<-get(paste(nameVar,"Distribution",sep=""))
	nnVar<-length(fVar$params)
	dialogName <- paste(nameVar,"DistributionSamples", sep="")
	defaults <- list(initialValues=fVar$initialValues, dsname=gettextPMgui(paste(fVar$titleName,"Samples",sep="")),
			nobs="100", nsamples="1", mean="1", sum="0", sd="0")
	initial <- getDialog(dialogName, defaults=defaults)
	initializeDialog(title=gettextPMgui(paste("Sample from ",fVar$titleName," Distribution")))
	dsname <- tclVar(initial$dsname)
	dsFrame <- tkframe(top)
	entryDsname <- ttkentry(dsFrame, width="20", textvariable=dsname)
	paramsVar<-paste(fVar$params,"Var",sep="")
	paramsEntry<-paste(fVar$params,"Entry",sep="")
	for (i in 1:nnVar) {
		eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
		eval(parse(text=paste(paramsEntry[i],"<-ttkentry(top, width='6', textvariable=",paramsVar[i],")",sep="")))
	}
	obserVar <- tclVar(initial$nobs)
	obserEntry <- ttkentry(top, width="6", textvariable=obserVar)
	samplesVar <- tclVar(initial$nsamples)
	samplesEntry <- ttkentry(top, width="6", textvariable=samplesVar)
	checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
			initialValues=c(initial$mean, initial$sum, initial$sd), 
			labels=gettextPMgui(c("Sample means", "Sample sums",
							"Sample standard deviations")))
	onOK <- function(){
		nameVarF<-get(paste(nameVar,"DistributionSamples",sep=""),mode="function")
		closeDialog()
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if (dsnameValue == "") {
			errorCondition(recall=nameVarF, 
					message=gettextPMgui("You must enter the name of a data set."))  
			return()
		}  
		if (!is.valid.name(dsnameValue)) {
			errorCondition(recall=nameVarF,
					message=paste('"', dsnameValue, '" ',
							gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
				nameVarF()
				return()
			}
		}
		warn <- options(warn=-1)
		vars<-real(nnVar)
		for (i in 1:nnVar) {
			vars[i]<-as.numeric(tclvalue(get(paramsVar[i])))
		}
		if (length(fVar$paramsRound)>0) {
			for (j in fVar$paramsRound) {
				vars[j]<-round(vars[j])
			}
		}
		options(warn)
		for (i in 1:length(fVar$errorConds)) {
			if (eval(parse(text=fVar$errorConds[i]))) {
				errorCondition(recall=nameVarF, message=gettextPMgui(fVar$errorTexts[i]))
				return()
			}
		}
		obser <- as.numeric(tclvalue(obserVar))
		samples <- as.numeric(tclvalue(samplesVar))
		if (is.na(obser) || obser <= 0) {
			errorCondition(recall=nameVarF, 
					message=gettextPMgui("Sample size must be positive."))
			return()
		}
		if (is.na(samples) || samples <= 0) {
			errorCondition(recall=nameVarF, 
					message=gettextPMgui("Number of samples must be positive."))
			return()
		}
		pasteVar<-""
		for (i in 1:nnVar) {
			pasteVar<-paste(pasteVar,", ",fVar$params[i],"=",vars[i],sep="")
		}
		if (nameVar=="Gumbel") {
			command <- paste(dsnameValue, " <- as.data.frame(matrix(log(rweibull(", samples, "*", obser, ", shape=", vars[1], ", scale=", vars[2], ")), ncol=", obser, "))", sep="")
		} else {
			command <- paste(dsnameValue, " <- as.data.frame(matrix(r",fVar$funName,"(", samples, "*", obser, pasteVar, "), ncol=", obser, "))", sep="")   
		}
		justDoIt(command)
		logger(command)
		command <- if (samples == 1) 
					paste("rownames(", dsnameValue, ') <- "sample"', sep="")
				else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
							', sep="")', sep="")
		justDoIt(command)
		logger(command)
		command <- if (obser == 1) 
					paste("colnames(", dsnameValue, ') <- "obs"', sep="")
				else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', obser,
							', sep="")', sep="")
		justDoIt(command)
		logger(command)
		if (tclvalue(meanVariable) == "1") {
			command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
					"[,1:", obser, "])", sep="")
			justDoIt(command)
			logger(command)
		}
		if (tclvalue(sumVariable) == "1") {
			command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
					"[,1:", obser, "])", sep="")
			justDoIt(command)
			logger(command)
		}
		if (tclvalue(sdVariable) == "1") {
			command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
					"[,1:", obser, "], 1, sd)", sep="")
			justDoIt(command)
			logger(command)
		}
		activeDataSet(dsnameValue)
		tkfocus(PmetricsWindow())
		putDialog(dialogName, list(initialValues=vars, dsname=dsnameValue,
						nobs=obser, nsamples=samples, 
						mean=tclvalue(meanVariable), sum=tclvalue(sumVariable), sd=tclvalue(sdVariable)),
				resettable=FALSE)
	}
	OKCancelHelp(helpSubject=paste("r",fVar$funName,sep=""), reset=dialogName)
	tkgrid(labelPMgui(dsFrame, text=gettextPMgui("Enter name for data set:")), entryDsname, 
			sticky="w")
	tkgrid(dsFrame, columnspan=2, sticky="w")
	tkgrid(labelPMgui(top, text=""))
	for (i in 1:nnVar) {
		tkgrid(labelPMgui(top, text=gettextPMgui(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w")
	}
	tkgrid(labelPMgui(top, text=gettextPMgui("Number of samples (rows) ")), samplesEntry, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Number of observations (columns) ")), obserEntry, sticky="w")
	tkgrid(labelPMgui(top, text=""))
	tkgrid(labelPMgui(top, text=gettextPMgui("Add to Data Set:"), fg="blue"), sticky="w")
	tkgrid(checkBoxFrame, columnspan=2, sticky="w")
	tkgrid(labelPMgui(top, text=""))
	tkgrid(buttonsFrame, columnspan=2, sticky="w")
	dialogSuffix(rows=10, columns=2, focus=get(paramsEntry[1]))
}
