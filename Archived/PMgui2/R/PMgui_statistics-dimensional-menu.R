# Statistics Menu dialogs

# last modified 2012-01-27 by J. Fox

# Dimensional-analysis menu

Reliability <- function () {
	defaults <- list(initial.x = NULL)
	dialog.values <- getDialog("Reliability", defaults)
	initializeDialog(title = gettextPMgui("Scale Reliability"))
	xBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
			initialSelection = varPosn(dialog.values$initial.x, "numeric"),
			title = gettextPMgui("Variables (pick three or more)"))
	onOK <- function() {
		x <- getSelection(xBox)
		closeDialog()
		putDialog("Reliability", list (initial.x = x))
		if (3 > length(x)) {
			errorCondition(recall = Reliability, message = gettextPMgui("Fewer than 3 variables selected."))
			return()
		}
		x <- paste("\"", x, "\"", sep = "")
		doItAndPrint(paste("reliability(cov(", ActiveDataSet(), 
						"[,c(", paste(x, collapse = ","), ")], use=\"complete.obs\"))", 
						sep = ""))
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "reliability", reset = "Reliability")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

principalComponents <- function () {
	defaults <- list(initial.x = NULL, initial.correlations = 1, 
			initial.subset = gettextPMgui("<all valid cases>"), initial.screeplot = 0, initial.addPC = 0)
	dialog.values <- getDialog("principalComponents", defaults)
	initializeDialog(title = gettextPMgui("Principal Components Analysis"))
	xBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
			initialSelection = varPosn(dialog.values$initial.x, "numeric"), 
			title = gettextPMgui("Variables (pick two or more)"))
	subsetBox(subset.expression = dialog.values$initial.subset)
	checkBoxes(frame = "optionsFrame", boxes = c("correlations", 
					"screeplot", "addPC"), initialValues = c(dialog.values$initial.correlations, 
					dialog.values$initial.screeplot, dialog.values$initial.addPC), 
			labels = gettextPMgui(c("Analyze correlation matrix", 
							"Screeplot", "Add principal components to data set")))
	onOK <- function() {
		putPMgui("ncomponents", 0)
		x <- getSelection(xBox)
		nvar <- length(x)#?
		correlations <- tclvalue(correlationsVariable)
		subset <- tclvalue(subsetVariable)
		screeplot <- tclvalue(screeplotVariable)
		addPC <- tclvalue(addPCVariable)
		closeDialog()
		putDialog("principalComponents", list(initial.x = x, initial.correlations = correlations, 
						initial.subset = subset, initial.screeplot = screeplot, initial.addPC = addPC))
		if (2 > length(x)) {
			errorCondition(recall = principalComponents, message = gettextPMgui("Fewer than 2 variables selected."))
			return()
		}
		subset <- if (trim.blanks(subset) == "" || trim.blanks(subset) == gettextPMgui("<all valid cases>")) 
					""
				else paste(", subset=", subset, sep = "")
		correlations <- if (correlations == "1") 
					"TRUE"
				else "FALSE"
		.activeDataSet <- ActiveDataSet()
		command <- paste("princomp(~", paste(x, collapse = "+"), 
				", cor=", correlations, ", data=", .activeDataSet, 
				subset, ")", sep = "")
		assign(".PC", justDoIt(command), envir = .GlobalEnv)
		logger(paste(".PC <- ", command, sep = ""))
		doItAndPrint("unclass(loadings(.PC))  # component loadings")
		doItAndPrint(".PC$sd^2  # component variances")
		doItAndPrint("summary(.PC) # proportions of variance")
		if (screeplot == "1") {
			justDoIt("screeplot(.PC)")
			logger("screeplot(.PC)")
		}
		if (addPC == "1") {
			if (trim.blanks(subset) != ""){
				errorCondition(recall=principalComponents,
						message=gettextPMgui("Component scores are not available when subset is specified."))
				return()
			}
			initializeDialog(subdialog, title = gettextPMgui("Number of Components"))
			tkgrid(labelPMgui(subdialog, text = gettextPMgui("Number of components to retain:"), 
							fg = "blue"), sticky = "w")
			sliderFrame <- tkframe(subdialog)
			sliderValue <- tclVar("1")
			componentsSlider <- tkscale(sliderFrame, from = 1, 
					to = nvar, showvalue = FALSE, variable = sliderValue, 
					resolution = 1, orient = "horizontal")
			componentsShow <- labelPMgui(sliderFrame, textvariable = sliderValue, 
					width = 2, justify = "right")
			onOKsub <- function() {
				closeDialog(subdialog)
				putPMgui("ncomponents", as.numeric(tclvalue(sliderValue)))
			}
			subOKCancelHelp()
			tkgrid(componentsSlider, componentsShow, sticky = "nw")
			tkgrid(sliderFrame, sticky = "w")
			tkgrid(subButtonsFrame, sticky = "w")
			dialogSuffix(subdialog, onOK = onOKsub, rows = 2, 
					columns = 1, focus = subdialog)
			if ((ncomponents <- getPMgui("ncomponents")) > 0) {
				for (i in 1:ncomponents) {
					var <- paste("PC", i, sep = "")
					if (is.element(var, Variables())) {
						if ("no" == tclvalue(checkReplace(var))) 
							next
					}
					justDoIt(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", 
									i, "]", sep = ""))
					logger(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", 
									i, "]", sep = ""))
				}
				activeDataSet(.activeDataSet, flushDialogMemory=FALSE)
			}
		}
		remove(.PC, envir = .GlobalEnv)
		logger("remove(.PC)")
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "princomp", reset = "principalComponents")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 4, columns = 1)
}

factorAnalysis <- function () {
	defaults <- list(initial.x = NULL, initial.subset = gettextPMgui ("<all valid cases>"), 
			initial.rotation = "varimax", initial.scores = "none")
	dialog.values <- getDialog("factorAnalysis", defaults)
	initializeDialog(title = gettextPMgui("Factor Analysis"))
	xBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
			initialSelection = varPosn(dialog.values$initial.x, "numeric"),
			title = gettextPMgui("Variables (pick three or more)"))
	subsetBox(subset.expression = dialog.values$initial.subset)
	optionsFrame <- tkframe(top)
	checkFrame <- tkframe(top)
	radioButtons(checkFrame, name = "rotation", buttons = c("noRotate", 
					"varimax", "promax"), values = c("none", "varimax", "promax"), 
			initialValue = dialog.values$initial.rotation, labels = gettextPMgui(c("None", 
							"Varimax", "Promax")), title = gettextPMgui("Factor Rotation"))
	radioButtons(checkFrame, name = "scores", buttons = c("noScores", 
					"bartlett", "regression"), values = c("none", "Bartlett", 
					"regression"), initialValue = dialog.values$initial.scores,
			labels = gettextPMgui(c("None", "Bartlett's method", 
							"Regression method")), title = gettextPMgui("Factor Scores"))
	onOK <- function() {
		x <- getSelection(xBox)
		nvar <- length(x)
		subset <- tclvalue(subsetVariable)
		rotation <- tclvalue(rotationVariable)
		scores <- tclvalue(scoresVariable)
		closeDialog()
		putDialog ("factorAnalysis", list (initial.x = x, initial.subset = subset, 
						initial.scores = scores, initial.rotation = rotation))
		if (3 > length(x)) {
			errorCondition(recall = factorAnalysis, message = gettextPMgui("Fewer than 3 variables selected."))
			return()
		}
		f <- function(k, p) ((p - k)^2 - p - k)^2
		max.factors <- floor(optimize(f, c(0, nvar), tol = 1e-04, 
						p = nvar)$minimum)
		if (max.factors == 1) {
			putPMgui("nfactors", 1)
		}
		else {
			initializeDialog(subdialog, title = gettextPMgui("Number of Factors"))
			tkgrid(labelPMgui(subdialog, text = gettextPMgui("Number of factors to extract:"), 
							fg = "blue"), sticky = "w")
			sliderFrame <- tkframe(subdialog)
			sliderValue <- tclVar("1")
			componentsSlider <- tkscale(sliderFrame, from = 1, 
					to = max.factors, showvalue = FALSE, variable = sliderValue, 
					resolution = 1, orient = "horizontal")
			componentsShow <- labelPMgui(sliderFrame, textvariable = sliderValue, 
					width = 2, justify = "right")
			onOKsub <- function() {
				closeDialog(subdialog)
				putPMgui("nfactors", as.numeric(tclvalue(sliderValue)))
			}
			subOKCancelHelp()
			tkgrid(componentsSlider, componentsShow, sticky = "nw")
			tkgrid(sliderFrame, sticky = "w")
			tkgrid(subButtonsFrame, sticky = "w")
			dialogSuffix(subdialog, onOK = onOKsub, rows = 2, 
					columns = 1, focus = subdialog)
		}
		subset <- if (trim.blanks(subset) == "" || trim.blanks(subset) == gettextPMgui("<all valid cases>")) 
					""
				else paste(", subset=", subset, sep = "")
		if (scores != "none" && subset != ""){
			errorCondition(recall=factorAnalysis,
					message=gettextPMgui("Factor scores are not available when subset is specified."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		command <- paste("factanal(~", paste(x, collapse = "+"), 
				", factors=", getPMgui("nfactors"), ", rotation=\"", 
				rotation, "\", scores=\"", scores, "\", data=", .activeDataSet, 
				subset, ")", sep = "")
		assign(".FA", justDoIt(command), envir = .GlobalEnv)
		logger(paste(".FA <- ", command, sep = ""))
		doItAndPrint(".FA")
		if (scores != "none") {
			for (i in 1:getPMgui("nfactors")) {
				var <- paste("F", i, sep = "")
				if (is.element(var, Variables())) {
					if ("no" == tclvalue(checkReplace(var))) 
						next
				}
				justDoIt(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", 
								i, "]", sep = ""))
				logger(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", 
								i, "]", sep = ""))
			}
			activeDataSet(.activeDataSet, flushDialogMemory = FALSE)
		}
		logger("remove(.FA)")
		remove(.FA, envir = .GlobalEnv)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "factanal", reset = "factorAnalysis")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(rotationFrame, labelPMgui(checkFrame, text = "    "), 
			scoresFrame, sticky = "w")
	tkgrid(checkFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 5, columns = 2)
}

CFA <- function(){
	Library("sem")
	defaults <- list(initial.matrix="covariance", initial.factorCor="correlated", initial.identify="factors", initial.robust=0)
	dialog.values <- getDialog("CFA", defaults)
	initializeDialog(title=gettextPMgui("Confirmatory Factor Analysis"))
	onFactor <- function(){
		vars <- getSelection(xBox)
		if (length(vars) < 2) {
			errorCondition(recall=CFA,  message=gettextPMgui("Fewer than 2 variables selected to load on factor."))
			return()
		}
		fac.name <- tclvalue(factorName)
		if (!is.valid.name(fac.name)) {
			errorCondition(recall=CFA,  message=paste(fac.name, gettextPMgui("is not a valid name.")))
			return()
		}
		variables[[getPMgui("factorNumber")]] <<- vars
		factors <- factors[getPMgui("factorNumber")] <<- fac.name
		putPMgui("factorNumber", getPMgui("factorNumber") + 1)
		tclvalue(factorName) <- paste("Factor.", getPMgui("factorNumber"), sep = "")
		tkselection.clear(xBox$listbox, "0", "end")
		tclvalue(buttonText) <- paste(gettextPMgui("Define factor"), getPMgui("factorNumber"))
	}
	xBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
			title = gettextPMgui("Select variables\nloading on factor"))
	optionsFrame <- tkframe(top)
	radioButtons(optionsFrame, name = "matrix", buttons = c("covariance", "correlation"),
			initialValue = dialog.values$initial.matrix, 
			labels = gettextPMgui(c("Covariance", "Correlation")), title = gettextPMgui("Matrix to Analyze"))
	radioButtons(optionsFrame, name = "factorCor", buttons = c("correlated", "orthogonal"),
			initialValue = dialog.values$initial.factorCor, 
			labels = gettextPMgui(c("Correlated", "Orthogonal")), title = gettextPMgui("Factor Correlations"))
	radioButtons(optionsFrame, name = "identify", buttons = c("factors", "loadings"),
			initialValue = dialog.values$initial.identify, 
			labels = gettextPMgui(c("Factor variances set to 1", "First loading on each factor set to 1")), 
			title = gettextPMgui("Identifying Constraints"))
	checkBoxes(window=optionsFrame, frame = "robustFrame", boxes = "robust", initialValues = dialog.values$initial.robust, 
			labels = gettextPMgui("Robust standard errors"), title=" ")
	putPMgui("factorNumber", 1)
	buttonText <- tclVar(paste(gettextPMgui("Define factor"), getPMgui("factorNumber")))
	factorFrame <- tkframe(top)
	factorButton <- buttonPMgui(factorFrame, textvariable=buttonText, width="15", 
			command=onFactor, default="active", borderwidth=3)
	factorName <- tclVar(paste("Factor.", getPMgui("factorNumber"), sep = ""))
	factorEntry <- ttkentry(factorFrame, width="20", textvariable=factorName)
	variables <- list()
	factors <- vector()
	onOK <- function(){
		matrix <- tclvalue(matrixVariable)
		correlations <- tclvalue(factorCorVariable)
		identify <- tclvalue(identifyVariable)
		robust <- tclvalue(robustVariable)
		closeDialog()
		putDialog("CFA", list(initial.matrix=matrix, initial.factorCor=correlations, 
						initial.identify=identify, initial.robust=robust))
		if (length(factors) == 0) {
			errorCondition(recall=CFA, message=gettextPMgui("No factors defined."))
			return()
		}
		putPMgui("factorNumber", NULL)
		modelText <- vector(length(factors), mode="character")
		for (i in 1:length(factors)){
			modelText[i] <- paste(factors[i], ": ", paste(variables[[i]], collapse=", "), sep="")
		}
		allvars <- unique(unlist(variables))
		if ((length(allvars)/length(factors)) < 2) {
			errorCondition(recall=CFA,  
					message=gettextPMgui("There are too many factors."))
			return()
		}
		doItAndPrint(paste(".model <- c(", paste(paste("'", modelText, "'", sep=""), collapse=", "), ")", sep=""))
		doItAndPrint(paste(".model <- cfa(file=textConnection(.model)", if(correlations == "correlated") ", " else ", covs=NULL, ",
						"reference.indicators=", if (identify == "factors") "FALSE" else "TRUE", ")", sep=""))
		doItAndPrint(paste(".Data <- ", activeDataSet(),
						"[, c(", paste(paste("'", allvars, "'", sep=""), collapse=", "),  ")]", sep=""))
		if (matrix == "correlation") doItAndPrint(".Data <- as.data.frame(scale(.Data))")
		doItAndPrint(paste("summary(sem(.model, data=.Data), robust=", if (robust == 1) "TRUE" else "FALSE", 
						")", sep=""))
		justDoIt("remove('.model', '.Data', envir=.GlobalEnv)")
		logger("remove('.model', '.Data')")
	}
	OKCancelHelp(helpSubject="CFA", reset="CFA")
	tkgrid(matrixFrame, labelPMgui(optionsFrame, text="    "), factorCorFrame, sticky="nw")
	tkgrid(identifyFrame, labelPMgui(optionsFrame, text="    "),  robustFrame, sticky="w")
	tkgrid(optionsFrame, sticky="w")
	tkgrid(labelPMgui(top, text=""))
	tkgrid(getFrame(xBox), sticky="w")
	tkgrid(labelPMgui(top, text=""))
	tkgrid(factorButton, labelPMgui(factorFrame, text=paste("   ", gettextPMgui("Name for factor:"))), factorEntry, sticky="nw")
	tkgrid(factorFrame, sticky="w")
	tkgrid(labelPMgui(top, text=""))
	tkgrid(buttonsFrame)
	dialogSuffix(rows=7, columns=1)
}

