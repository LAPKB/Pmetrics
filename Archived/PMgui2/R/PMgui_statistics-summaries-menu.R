# Statistics Menu dialogs

# last modified 2011-12-27 by J. Fox

# Summaries menu

summarizeDataSet <- function(){
	nvar <- length(Variables())
	.activeDataSet <- ActiveDataSet()
	if (nvar > 10){
		response <- PMguiTkmessageBox(message=sprintf(gettextPMgui("There are %d variables in the data set %s.\nDo you want to proceed?"), nvar, .activeDataSet),
				icon="question", type="okcancel", default="cancel")
		if ("cancel" == tclvalue(response)) {
			tkfocus(PmetricsWindow())
			return()
		}
	}
	doItAndPrint(paste("summary(", .activeDataSet, ")", sep=""))
}

numericalSummaries <- function(){ # dialog memory 2011-06-27  J. Fox
	Library("abind")
	Library("e1071")
	defaults <- list(initial.x=NULL, initial.mean="1", initial.sd="1", initial.cv="0",
			initial.quantiles.variable="1", 
			initial.quantiles="0, .25, .5, .75, 1", 
			initial.skewness="0", initial.kurtosis="0", initial.type="2",
			initial.group=NULL)
	dialog.values <- getDialog("numericalSummaries", defaults)
	initial.group <- dialog.values$initial.group
	initializeDialog(title=gettextPMgui("Numerical Summaries"))
	xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextPMgui("Variables (pick one or more)"),
			initialSelection=varPosn(dialog.values$initial.x, "numeric"))
	selectFrame <- tkframe(top)
	checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sd", "cv"), 
			initialValues=c(dialog.values$initial.mean, dialog.values$initial.sd, dialog.values$initial.cv), 
			labels=gettextPMgui(c("Mean", "Standard Deviation", "Coefficient of Variation")))
	checkBoxes(window=selectFrame, frame="skCheckBoxFrame", boxes=c("skewness", "kurtosis"), 
			initialValues=c(dialog.values$initial.skewness, dialog.values$initial.kurtosis), 
			labels=gettextPMgui(c("Skewness", "Kurtosis")))
	radioButtons(window=selectFrame, name="typeButtons", buttons=c("b1", "b2", "b3"), values=c("1", "2", "3"), 
			initialValue=dialog.values$initial.type,
			labels=gettextPMgui(c("Type 1", "Type 2", "Type 3")))
	quantilesVariable <- tclVar(dialog.values$initial.quantiles.variable)
	quantilesFrame <- tkframe(top)
	quantilesCheckBox <- tkcheckbutton(quantilesFrame, variable=quantilesVariable)
	quantiles <- tclVar(dialog.values$initial.quantiles)
	quantilesEntry <- ttkentry(quantilesFrame, width="20", textvariable=quantiles)
	groupsBox(recall=numericalSummaries, label=gettextPMgui("Summarize by:"), 
			initialLabel=if (is.null(initial.group)) gettextPMgui("Summarize by groups") 
					else paste(gettextPMgui("Summarize by:"), initial.group), 
			initialGroup=initial.group)
	onOK <- function(){
		x <- getSelection(xBox)
		quants <- tclvalue(quantiles)
		meanVar <- tclvalue(meanVariable)
		sdVar <- tclvalue(sdVariable)
		cvVar <- tclvalue(cvVariable)
		quantsVar <- tclvalue(quantilesVariable)
		skewnessVar <- tclvalue(skewnessVariable)
		kurtosisVar <- tclvalue(kurtosisVariable)
		typeVar <- tclvalue(typeButtonsVariable)
		putDialog("numericalSummaries", list(
						initial.x=x, initial.mean=meanVar, initial.sd=sdVar, initial.cv=cvVar,
						initial.quantiles.variable=quantsVar, initial.quantiles=quants,
						initial.skewness=skewnessVar, initial.kurtosis=kurtosisVar, initial.type=typeVar,
						initial.group=if (.groups != FALSE) .groups else NULL
				))		
		if (length(x) == 0){
			errorCondition(recall=numericalSummaries, message=gettextPMgui("You must select a variable."))
			return()
		}
		closeDialog()
		quants <- paste("c(", gsub(",+", ",", gsub(" ", ",", quants)), ")", sep="")
		.activeDataSet <- ActiveDataSet()
		vars <- if (length(x) == 1) paste('"', x, '"', sep="") 
				else paste("c(", paste('"', x, '"', collapse=", ", sep=""), ")", sep="")
		vars <- paste(.activeDataSet, "[,", vars, "]", sep="")
		stats <- paste("c(",
				paste(c('"mean"', '"sd"', '"quantiles"', '"cv"', '"skewness"', '"kurtosis"')
								[c(meanVar, sdVar, quantsVar, cvVar, skewnessVar, kurtosisVar) == 1], 
						collapse=", "), ")", sep="")
		if (stats == "c()"){
			errorCondition(recall=numericalSummaries, message=gettextPMgui("No statistics selected."))
			return()
		}
		type.text <- if (skewnessVar == 1 || kurtosisVar == 1) paste(', type="', typeVar, '"', sep="") else ""
		command <- if (.groups != FALSE) {
					grps <- paste(.activeDataSet, "$", .groups, sep="")
					paste("numSummary(", vars, ", groups=", grps, ", statistics=", stats, 
							", quantiles=", quants, type.text, ")", sep="")
				}
				else  paste("numSummary(", vars, ", statistics=", stats, 
							", quantiles=", quants, type.text, ")", sep="")
		doItAndPrint(command) 
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="numSummary", reset="numericalSummaries")
	tkgrid(getFrame(xBox), sticky="nw")    
	tkgrid(checkBoxFrame, sticky="w")
	tkgrid(skCheckBoxFrame, typeButtonsFrame, sticky="nw")
	tkgrid(selectFrame, sticky="w")
	tkgrid(labelPMgui(quantilesFrame, text=gettextPMgui("Quantiles")), quantilesCheckBox,
			labelPMgui(quantilesFrame, text=gettextPMgui(" quantiles:")), quantilesEntry, sticky="w")
	tkgrid(quantilesFrame, sticky="w")
	tkgrid(groupsFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=7, columns=1)
}

frequencyDistribution <- function () {
	defaults <- list (initial.x = NULL, initial.goodnessOfFit = "0")
	dialog.values <- getDialog ("frequencyDistribution", defaults)
	initializeDialog(title = gettextPMgui("Frequency Distributions"))
	xBox <- variableListBox(top, Factors(), selectmode = "multiple", 
			title = gettextPMgui("Variables (pick one or more)"),
			initialSelection = varPosn (dialog.values$initial.x, "factor"))
	optionsFrame <- tkframe(top)
	goodnessOfFitVariable <- tclVar(dialog.values$initial.goodnessOfFit)
	goodnessOfFitCheckBox <- tkcheckbutton(optionsFrame, variable = goodnessOfFitVariable)
	onOK <- function() {
		x <- getSelection(xBox)
		if (length(x) == 0) {
			errorCondition(recall = frequencyDistribution, message = gettextPMgui("You must select a variable."))
			return()
		}
		goodnessOfFit <- tclvalue(goodnessOfFitVariable)
		putDialog ("frequencyDistribution", list (initial.x = x, initial.goodnessOfFit = goodnessOfFit))
		if (length(x) > 1 && goodnessOfFit == "1") {
			errorCondition(recall = frequencyDistribution, message = gettextPMgui("Goodness-of-fit test not available when more than one variable is selected."))
			return()
		}
		closeDialog()
		.activeDataSet <- ActiveDataSet()
		for (variable in x) {
			command <- paste("table(", .activeDataSet, "$", variable, 
					")", sep = "")
			logger(paste(".Table <-", command))
			assign(".Table", justDoIt(command), envir = .GlobalEnv)
			doItAndPrint(paste(".Table  # counts for", variable))
			doItAndPrint(paste("round(100*.Table/sum(.Table), 2)  # percentages for", 
							variable))
		}
		env <- environment()
		if (goodnessOfFit == 1) {
			initializeDialog(subwin, title = gettextPMgui("Goodness-of-Fit Test"))
			hypothesisFrame <- tkframe(subwin)
			levs <- eval(parse(text = paste("levels(", .activeDataSet, 
									"$", x, ")", sep = "")))
			n.levs <- length(levs)
			assign(".entry.1", tclVar(paste("1/", n.levs, sep = "")), 
					envir = env)
			make.entries <- "labelPMgui(hypothesisFrame, text='Hypothesized probabilities:   ')"
			make.lev.names <- "labelPMgui(hypothesisFrame, text='Factor levels:')"
			for (i in 1:n.levs) {
				entry.varname <- paste(".entry.", i, sep = "")
				assign(entry.varname, tclVar(paste("1/", n.levs, 
										sep = "")), envir = env)
				make.entries <- paste(make.entries, ", ", "ttkentry(hypothesisFrame, width='5', textvariable=", 
						entry.varname, ")", sep = "")
				make.lev.names <- paste(make.lev.names, ", labelPMgui(hypothesisFrame, text='", 
						levs[i], "')", sep = "")
			}
			eval(parse(text = paste("tkgrid(", make.lev.names, 
									", sticky='w')", sep = "")), envir = env)
			eval(parse(text = paste("tkgrid(", make.entries, 
									", stick='w')", sep = "")), envir = env)
			tkgrid(hypothesisFrame, sticky = "w")
			onOKsub <- function() {
				probs <- rep(NA, n.levs)
				for (i in 1:n.levs) {
					entry.varname <- paste(".entry.", i, sep = "")
					res <- try(entry <- eval(parse(text = eval(parse(text = paste("tclvalue(", 
																	entry.varname, ")", sep = "")), envir = env))), 
							silent = TRUE)
					if (class(res) == "try-error") {
						errorCondition(subwin, message = gettextPMgui("Invalid entry."))
						return()
					}
					if (length(entry) == 0) {
						errorCondition(subwin, message = gettextPMgui("Missing entry."))
						return()
					}
					opts <- options(warn = -1)
					probs[i] <- as.numeric(entry)
					options(opts)
				}
				probs <- na.omit(probs)
				if (length(probs) != n.levs) {
					errorCondition(subwin, message = sprintf(gettextPMgui("Number of valid entries (%d)\nnot equal to number levels (%d)."), 
									length(probs), n.levs))
					return()
				}
				if (any(probs < 0)) {
					errorCondition(subwin, message = gettextPMgui("Negative probabilities not allowed."))
					return()
				}
				if (abs(sum(probs) - 1) > 0.001) {
					Message(message = gettextPMgui("Probabilities rescaled to sum to 1."), 
							type = "warning")
					probs <- probs/sum(probs)
				}
				closeDialog(subwin)
				command <- paste("c(", paste(probs, collapse = ","), 
						")", sep = "")
				logger(paste(".Probs <-", command))
				assign(".Probs", justDoIt(command), envir = .GlobalEnv)
				doItAndPrint("chisq.test(.Table, p=.Probs)")
				logger("remove(.Probs)")
				remove(.Probs, envir = .GlobalEnv)
			}
			subOKCancelHelp(subwin)
			tkgrid(subButtonsFrame, sticky = "w")
			dialogSuffix(subwin, rows = 2, columns = 1, onOK = onOKsub, 
					focus = subwin)
		}
		logger("remove(.Table)")
		remove(.Table, envir = .GlobalEnv)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "table", reset = "frequencyDistribution")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Chi-square goodness-of-fit test (for one variable only)")), 
			goodnessOfFitCheckBox, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 2)
}

statisticsTable <- function () {
	defaults <- list (initial.group=NULL, initial.response=NULL, initial.statistic="mean", initial.other = "")
	dialog.values <- getDialog ("statisticsTable", defaults)
	initializeDialog(title = gettextPMgui("Table of Statistics"))
	variablesFrame <- tkframe(top)
	groupBox <- variableListBox(variablesFrame, Factors(), selectmode = "multiple", 
			title = gettextPMgui("Factors (pick one or more)"), 
			initialSelection = varPosn(dialog.values$initial.group,"factor"))
	responseBox <- variableListBox(variablesFrame, Numeric(), selectmode = "multiple", 
			initialSelection = varPosn(dialog.values$initial.response, "numeric"),
			title = gettextPMgui("Response variables (pick one or more)"))
	statFrame <- tkframe(top)
	radioButtons(statFrame, name = "statistic", buttons = c("mean", "median", 
					"sd", "other"), labels = gettextPMgui(c("Mean", "Median", "Standard deviation", "Other (specify)")), 
			initialValue = dialog.values$initial.statistic, 
			title = gettextPMgui("Statistic"))
	otherVariable <- tclVar(dialog.values$initial.other)
	otherEntry <- ttkentry(statFrame, width = "20", textvariable = otherVariable)
	tkgrid(statisticFrame, labelPMgui(statFrame, text ="  "), otherEntry, sticky = "sw")
	onOK <- function() {
		groups <- getSelection(groupBox)
		if (0 == length(groups)) {
			errorCondition(recall = statisticsTable, message = gettextPMgui("No factors selected."))
			return()
		}
		responses <- getSelection(responseBox)
		if (0 == length(responses)) {
			errorCondition(recall = statisticsTable, message = gettextPMgui("You must select a response variable."))
			return()
		}
		stat <- statistic <- tclvalue(statisticVariable)
		if (statistic == "other") 
			statistic <- tclvalue(otherVariable)
		putDialog ("statisticsTable", list(initial.group=groups, initial.response=responses, 
						initial.statistic=stat, initial.other = if(stat == "other") statistic else ""))  
		closeDialog()
		.activeDataSet <- ActiveDataSet()
		groups.list <- paste(paste(groups, "=", .activeDataSet, 
						"$", groups, sep = ""), collapse = ", ")
		for (response in responses) {
			if (length(responses) > 1) 
				doItAndPrint(paste("# Table for ", response, 
								":", sep = ""))
			doItAndPrint(paste("tapply(", .activeDataSet, "$", 
							response, ", list(", groups.list, "), ", statistic, 
							", na.rm=TRUE)", sep = ""))
		}
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "tapply", reset="statisticsTable")
	tkgrid(getFrame(groupBox), labelPMgui(variablesFrame, text = "    "), 
			getFrame(responseBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "w")
	tkgrid(statFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1, focus = otherEntry)
}

correlationMatrix <- function (){
	defaults <- list (initial.x = NULL, initial.correlations = "Pearson", initial.pvaluesVar="0")  
	dialog.values <- getDialog ("correlationMatrix", defaults)
	initializeDialog(title = gettextPMgui("Correlation Matrix"))
	xBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
			title = gettextPMgui("Variables (pick two or more)"),
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	radioButtons(name = "correlations", buttons = c("pearson", 
					"spearman", "partial"), values = c("Pearson", "Spearman", 
					"partial"), labels = gettextPMgui(c("Pearson product-moment", 
							"Spearman rank-order", "Partial")), title = gettextPMgui("Type of Correlations"),
			initialValue = dialog.values$initial.correlations)
	pvaluesFrame <- tkframe(top)
	pvaluesVar <- tclVar(dialog.values$initial.pvaluesVar)
	pvaluesCheckbox <- tkcheckbutton(pvaluesFrame, variable = pvaluesVar)
	onOK <- function() {
		correlations <- tclvalue(correlationsVariable)
		x <- getSelection(xBox)
		pvalues <- tclvalue(pvaluesVar)
		if (2 > length(x)) {
			errorCondition(recall = correlationMatrix, message = gettextPMgui("Fewer than 2 variables selected."))
			return()
		}
		if ((correlations == "partial") && (3 > length(x))) {
			errorCondition(recall = correlationMatrix, message = gettextPMgui("Fewer than 3 variables selected\nfor partial correlations."))
			return()
		}
		closeDialog()
		putDialog ("correlationMatrix", list (initial.x=x, initial.correlations=correlations, 
						initial.pvaluesVar=pvalues))
		x <- paste("\"", x, "\"", sep = "")
		.activeDataSet <- ActiveDataSet()
		if (correlations == "Pearson") {
			if (pvalues == 0) {
				doItAndPrint(paste("cor(", .activeDataSet, "[,c(", 
								paste(x, collapse = ","), ")], use=\"complete.obs\")", 
								sep = ""))
			}
			else {
				Library("Hmisc")
				doItAndPrint(paste("rcorr.adjust(", .activeDataSet, 
								"[,c(", paste(x, collapse = ","), ")], type=\"pearson\")", 
								sep = ""))
			}
		}
		else if (correlations == "Spearman") {
			logger("# Spearman rank-order correlations")
			if (pvalues == 0) {
				doItAndPrint(paste("cor(", .activeDataSet, "[,c(", 
								paste(x, collapse = ","), ")], use=\"complete.obs\", method=\"spearman\")", 
								sep = ""))
			}
			else {
				Library("Hmisc")
				doItAndPrint(paste("rcorr.adjust(", .activeDataSet, 
								"[,c(", paste(x, collapse = ","), ")], type=\"spearman\")", 
								sep = ""))
			}
		}
		else doItAndPrint(paste("partial.cor(", .activeDataSet, 
							"[,c(", paste(x, collapse = ","), ")], use=\"complete.obs\")", 
							sep = ""))
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "rcorr.adjust", reset="correlationMatrix")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(correlationsFrame, sticky = "w")
	tkgrid(labelPMgui(pvaluesFrame, text = gettextPMgui("Pairwise p-values\nfor Pearson or Spearman correlations")), 
			pvaluesCheckbox, sticky = "w")
	tkgrid(pvaluesFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 4, columns = 1)
}

# the following dialog contributed by Stefano Calza, modified by J. Fox

correlationTest <- function(){
	defaults <- list(initial.x=NULL,initial.correlations="pearson",initial.alternative ="two.sided")
	dialog.values <- getDialog("correlationTest", defaults)
	initializeDialog(title=gettextPMgui("Correlation Test"))
	xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextPMgui("Variables (pick two)"),initialSelection=varPosn(dialog.values$initial.x, "numeric"))
	radioButtons(name="correlations", buttons=c("pearson", "spearman", "kendall"),
			labels=gettextPMgui(c("Pearson product-moment", "Spearman rank-order", "Kendall's tau")),
			initialValue=dialog.values$initial.correlations, 
			title=gettextPMgui("Type of Correlation"))
	radioButtons(name="alternative", buttons=c("two.sided", "less", "greater"), 
			values=c("two.sided", "less", "greater"),
			initialValue=dialog.values$initial.alternative, 
			labels=gettextPMgui(c("Two-sided", "Correlation < 0", "Correlation > 0")), 
			title=gettextPMgui("Alternative Hypothesis"))  
	onOK <- function(){
		alternative <- as.character(tclvalue(alternativeVariable))
		correlations <- as.character(tclvalue(correlationsVariable))
		x <- getSelection(xBox)
		putDialog("correlationTest", list(initial.alternative=alternative, initial.correlations=correlations, initial.x=x))
		if (2 > length(x)) {
			errorCondition(recall=correlationTest,
					message=gettextPMgui("Fewer than 2 variables selected."))
			return()
		}
		if(2 < length(x)) {
			errorCondition(recall=correlationTest,
					message=gettextPMgui("More than 2 variables selected."))
			return()
		}
		closeDialog()
		.activeDataSet <- ActiveDataSet()
		command <- paste("cor.test(", .activeDataSet, "$", x[1], ", ", .activeDataSet, "$", x[2],
				', alternative="', alternative, '", method="', correlations, '")', sep="")
		doItAndPrint(command)  
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="cor.test", reset="correlationTest")
	tkgrid(getFrame(xBox), sticky="nw")
	tkgrid(labelPMgui(top, text=""))
	tkgrid(correlationsFrame,alternativeFrame, sticky="w")
	tkgrid(buttonsFrame,columnspan=2,sticky="w")
	dialogSuffix(rows=4, columns=1)
}

countMissing <- function(){
	command <- paste("sapply(", activeDataSet(), 
			", function(x)(sum(is.na(x)))) # NA counts", sep="")
	doItAndPrint(command)
	invisible(NULL)
}

ShapiroTest <- function () {
	defaults <- list (initial.var = NULL)
	dialog.values <- getDialog ("ShapiroTest", defaults)
	initializeDialog(title = gettextPMgui("Shapiro-Wilk Test for Normality"))
	variableBox <- variableListBox(top, Numeric(), title = gettextPMgui("Variable (pick one)"),
			initialSelection = varPosn (dialog.values$initial.var, "numeric"))
	onOK <- function() {
		var <- getSelection(variableBox)
		putDialog ("ShapiroTest", list (initial.var = var))
		if (length(var) == 0) {
			errorCondition(recall = ShapiroTest, message = gettextPMgui("You must select a variable."))
			return()
		}
		closeDialog()
		doItAndPrint(paste("shapiro.test(", ActiveDataSet(), 
						"$", var, ")", sep = ""))
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "shapiro.test", reset = "ShapiroTest")
	tkgrid(getFrame(variableBox), sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}
