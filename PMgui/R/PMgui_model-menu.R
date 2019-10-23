# Model menu dialogs

# last modified 2011-12-22 by J. Fox

selectActiveModel <- function(){
	models <- listAllModels()
	.activeModel <- ActiveModel()
	if ((length(models) == 1) && !is.null(.activeModel)) {
		Message(message=gettextPMgui("There is only one model in memory."),
				type="warning")
		tkfocus(PmetricsWindow())
		return()
	}
	if (length(models) == 0){
		Message(message=gettextPMgui("There are no models from which to choose."),
				type="error")
		tkfocus(PmetricsWindow())
		return()
	}
	initializeDialog(title=gettextPMgui("Select Model"))
	.activeDataSet <- ActiveDataSet()
	initial <- if (is.null(.activeModel)) NULL else which(.activeModel == models) - 1
	modelsBox <- variableListBox(top, models, title=gettextPMgui("Models (pick one)"), 
			initialSelection=initial)
	onOK <- function(){
		model <- getSelection(modelsBox)
		closeDialog()
		if (length(model) == 0) {
			tkfocus(PmetricsWindow())
			return()
		}
		dataSet <- as.character(get(model)$call$data)
		if (length(dataSet) == 0){
			errorCondition(message=gettextPMgui("There is no dataset associated with this model."))
			return()
		}
		dataSets <- listDataSets()
		if (!is.element(dataSet, dataSets)){
			errorCondition(message=sprintf(gettextPMgui("The dataset associated with this model, %s, is not in memory."), dataSet))
			return()
		}
		if (is.null(.activeDataSet) || (dataSet != .activeDataSet)) activeDataSet(dataSet)
		putPMgui("modelWithSubset", "subset" %in% names(get(model)$call))
		activeModel(model)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp()
	nameFrame <- tkframe(top)
	tkgrid(labelPMgui(nameFrame, fg="blue", text=gettextPMgui("Current Model: ")), 
			labelPMgui(nameFrame, text=tclvalue(getPMgui("modelName"))), sticky="w")
	tkgrid(nameFrame, sticky="w", columnspan="2")
	tkgrid(getFrame(modelsBox), columnspan="2", sticky="w")
	tkgrid(buttonsFrame, columnspan=2, sticky="w")
	dialogSuffix(rows=3, columns=2)
}

summarizeModel <- function(){
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("summary", .activeModel)) return()
	doItAndPrint(paste("summary(", .activeModel, ", cor=FALSE)", sep=""))
}

plotModel <- function(){
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("plot", .activeModel)) return()
	command <- "oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))"
	justDoIt(command)
	logger(command)
	doItAndPrint(paste("plot(", .activeModel, ")", sep=""))
	command <- "par(oldpar)"
	justDoIt(command)
	logger(command)
}

CRPlots <- function(){
	Library("car")
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("crPlot", .activeModel)) return()
	doItAndPrint(paste("crPlots(", .activeModel, ")", sep=""))
	activateMenus()
}

AVPlots <- function(){
	Library("car")
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("avPlot", .activeModel)) return()
	response <- tclvalue(PMguiTkmessageBox(
					message=paste(gettextPMgui("Identify points with mouse?\n"),
							gettextPMgui(if (MacOSXP()) "esc key to exit." else "right button to exit."), sep=""),
					icon="question", type="yesno", default="no"))
	idmethod <- if (response == "yes") ', id.method="identify"' else ""
	doItAndPrint(paste("avPlots(", .activeModel, idmethod, ")", sep=""))
	activateMenus()
}

anovaTable <- function () {
	Library("car")
	.activeModel <- ActiveModel()
	if (is.null(.activeModel)) 
		return()
	defaults <- list (initial.type = "II")
	dialog.values <- getDialog ("anovaTable", defaults)
	initializeDialog(title = gettextPMgui("ANOVA Table"))
	radioButtons(name = "type", buttons = c("I", "II", "III"), 
			values = c("I", "II", "III"), labels = gettextPMgui(c("Sequential (\"Type I\")", 
							"Partial, obeying marginality (\"Type II\")", "Partial, ignoring marginality (\"Type III\")")), 
			title = gettextPMgui("Type of Tests"), initialValue = dialog.values$initial.type)
	onOK <- function() {
		type <- as.character(tclvalue(typeVariable))
		putDialog ("anovaTable", list (initial.type = type))
		closeDialog()
		if (is.glm <- glmP()) {
			family <- eval(parse(text = paste(.activeModel, "$family$family", 
									sep = "")))
		}
		if (type == "I") {
			if (!checkMethod("anova", .activeModel)) {
				errorCondition(message = gettextPMgui("There is no appropriate anova method for a model of this class."))
				return()
			}
			if (is.glm) {
				test <- if (family %in% c("binomial", "poisson")) 
							"Chisq"
						else "F"
				doItAndPrint(paste("anova(", .activeModel, ", test=\"", 
								test, "\")", sep = ""))
			}
			else doItAndPrint(paste("anova(", .activeModel, ")", 
								sep = ""))
		}
		else {
			if (!checkMethod("Anova", .activeModel)) {
				errorCondition(message = gettextPMgui("There is no appropriate Anova method for a model of this class."))
				return()
			}
			if (is.glm) {
				test <- if (family %in% c("binomial", "poisson")) 
							"LR"
						else "F"
				doItAndPrint(paste("Anova(", .activeModel, ", type=\"", 
								type, "\", test=\"", test, "\")", sep = ""))
			}
			else doItAndPrint(paste("Anova(", .activeModel, ", type=\"", 
								type, "\")", sep = ""))
			if (type == "III") 
				Message(message = gettextPMgui("Type III tests require careful attention to contrast coding."), 
						type = "warning")
		}
	}
	OKCancelHelp(helpSubject = "Anova", reset = "anovaTable")
	tkgrid(typeFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

VIF <- function(){
	Library("car")
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("vif", .activeModel)) return()
	doItAndPrint(paste("vif(", .activeModel, ")", sep=""))
}

InfluencePlot <- function(){
	Library("car")
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("influencePlot", .activeModel)) return()
	response <- tclvalue(PMguiTkmessageBox(
					message=paste(gettextPMgui("Identify points with mouse?\n"),
							gettextPMgui(if (MacOSXP()) "esc key to exit." else "right button to exit."), sep=""),
					icon="question", type="yesno", default="no"))
	idmethod <- if (response == "yes") ', id.method="identify"' else ""
	doItAndPrint(paste("influencePlot(", .activeModel, idmethod, ")", sep=""))
}  

effectPlots <- function(){
	Library("effects")
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("effect", .activeModel)) return()
	doItAndPrint('trellis.device(theme="col.whitebg")')
	command <- paste("plot(allEffects(", .activeModel, "), ask=FALSE)", sep="")
	justDoIt(command)
	logger(command)
	activateMenus()
	NULL
}

addObservationStatistics <- function () {
	if (is.null(.activeModel)) 
		return()
	addVariable <- function(name) {
		variable <- paste(name, ".", .activeModel, sep = "")
		if (is.element(variable, .variables)) {
			ans <- checkReplace(variable)
			if (tclvalue(ans) == "no") 
				return()
		}
		command <- paste(name, "(", .activeModel, ")", sep = "")
		justDoIt(paste(.activeDataSet, "$", variable, " <- ", 
						command, sep = ""))
		logger(paste(.activeDataSet, "$", variable, " <- ", command, 
						sep = ""))
	}
	if (getPMgui("modelWithSubset")) {
		Message(message = gettextPMgui("Observation statistics not available\nfor a model fit to a subset of the data."), 
				type = "error")
		tkfocus(PmetricsWindow())
		return()
	}
	defaults <- list (initial.fitted = 1, initial.residuals = 1, initial.rstudent = 1, 
			initial.hatvalues = 1, initial.cookd = 1, initial.obsNumbers = 1)
	dialog.values <- getDialog ("addObservationStatistics", defaults)
	initializeDialog(title = gettextPMgui("Add Observation Statistics to Data"))
	.activeModel <- ActiveModel()
	.activeDataSet <- ActiveDataSet()
	.variables <- Variables()
	obsNumberExists <- is.element("obsNumber", .variables)
	activate <- c(checkMethod("fitted", .activeModel, default = TRUE, 
					reportError = FALSE), checkMethod("residuals", .activeModel, 
					default = TRUE, reportError = FALSE), checkMethod("rstudent", 
					.activeModel, reportError = FALSE), checkMethod("hatvalues", 
					.activeModel, reportError = FALSE), checkMethod("cooks.distance", 
					.activeModel, reportError = FALSE))
	checkBoxes(frame = "selectFrame", boxes = c(c("fitted", "residuals", 
							"rstudent", "hatvalues", "cookd")[activate], "obsNumbers"), 
			labels = c(gettextPMgui(c("Fitted values", "Residuals", 
									"Studentized residuals", "Hat-values", "Cook's distances"))[activate], 
					gettextPMgui("Observation indices")), initialValues = c(dialog.values$initial.fitted, 
					dialog.values$initial.residuals, dialog.values$initial.rstudent, 
					dialog.values$initial.hatvalues, dialog.values$initial.cookd, dialog.values$initial.obsNumbers))
	onOK <- function() {
		closeDialog()
		if (activate[1] && tclvalue(fittedVariable) == 1) 
			addVariable("fitted")
		if (activate[2] && tclvalue(residualsVariable) == 1) 
			addVariable("residuals")
		if (activate[3] && tclvalue(rstudentVariable) == 1) 
			addVariable("rstudent")
		if (activate[4] && tclvalue(hatvaluesVariable) == 1) 
			addVariable("hatvalues")
		if (activate[5] && tclvalue(cookdVariable) == 1) 
			addVariable("cooks.distance")
		obsNumbers <- tclvalue(obsNumbersVariable)
		putDialog ("addObservationStatistics", list (initial.fitted = tclvalue (fittedVariable),
						initial.residuals = tclvalue (residualsVariable), initial.rstudent = tclvalue(rstudentVariable), 
						initial.hatvalues = tclvalue (hatvaluesVariable), initial.cookd = tclvalue (cookdVariable), 
						initial.obsNumbers = obsNumbers))
		if (tclvalue(obsNumbersVariable) == 1) {
			proceed <- if (obsNumberExists) 
						tclvalue(checkReplace("obsNumber"))
					else "yes"  
			if (proceed == "yes") {
				command <- paste(.activeDataSet, "$obsNumber <- 1:nrow(", 
						.activeDataSet, ")", sep = "")
				justDoIt(command)
				logger(command)
			}
		}
		activeDataSet(.activeDataSet, flushModel = FALSE, flushDialogMemory = FALSE)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "influence.measures", reset = "addObservationStatistics")
	tkgrid(selectFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 5, columns = 1)
}

residualQQPlot <- function () {
	Library("car")
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("qqPlot", .activeModel)) 
		return()
	defaults <- list (initial.simulate = 1, initial.identify = 0)
	dialog.values <- getDialog ("residualQQPlot", defaults)
	initializeDialog(title = gettextPMgui("Residual Quantile-Comparison Plot"))
	selectFrame <- tkframe(top)
	simulateVar <- tclVar(dialog.values$initial.simulate)
	identifyVar <- tclVar(dialog.values$initial.identify)
	simulateCheckBox <- tkcheckbutton(selectFrame, variable = simulateVar)
	identifyCheckBox <- tkcheckbutton(selectFrame, variable = identifyVar)
	onOK <- function() {
		simulate <- tclvalue (simulateVar)  
		identify <- tclvalue (identifyVar)
		putDialog ("residualQQPlot", list (initial.identify = identify, initial.simulate = simulate))
		closeDialog()
		simulate <- tclvalue(simulateVar) == 1
		if (tclvalue(identifyVar) == 1) {
			identify <- ", id.method=\"identify\""
			PMguiTkmessageBox(title = "Identify Points", message = paste(gettextPMgui("Use left mouse button to identify points,\n"), 
							gettextPMgui(if (MacOSXP()) 
												"esc key to exit."
											else "right button to exit."), sep = ""), icon = "info", 
					type = "ok")
		}
		else identify <- ""
		command <- paste("qqPlot(", .activeModel, ", simulate=", 
				simulate, identify, ")", sep = "")
		doItAndPrint(command)
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "qq.plot.lm", reset = "residualQQPlot")
	tkgrid(labelPMgui(selectFrame, text = gettextPMgui("Simulated confidence envelope")), 
			simulateCheckBox, sticky = "w")
	tkgrid(labelPMgui(selectFrame, text = gettextPMgui("Identify points with mouse")), 
			identifyCheckBox, sticky = "w")
	tkgrid(selectFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

testLinearHypothesis <- function(){
	coef.multinom <- car:::coef.multinom
	defaults <- list(previous.model=NULL, nrows=1, table.values=0, rhs.values=0)
	dialog.values <- getDialog("testLinearHypothesis", defaults=defaults)
	Library("car")
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("linearHypothesis", .activeModel, default=TRUE)) return()
	if (!is.null(dialog.values$previous.model)){
		if (dialog.values$previous.model != .activeModel){
			dialog.values <- defaults
		}
	}
	table.values <- dialog.values$table.values
	rhs.values <- dialog.values$rhs.values
	env <- environment()
	initializeDialog(title=gettextPMgui("Test Linear Hypothesis"))
	outerTableFrame <- tkframe(top)
	assign(".tableFrame", tkframe(outerTableFrame), envir=env)
	setUpTable <- function(...){
		tkdestroy(get(".tableFrame", envir=env))
		assign(".tableFrame", tkframe(outerTableFrame), envir=env)
		nrows <- as.numeric(tclvalue(rowsValue))
		if (length(table.values) == 1 && table.values == 0) {
			table.values <- matrix(0, nrows, ncols)
			rhs.values <- rep(0, nrows)
		}
		if (nrow(table.values) < nrows){
			add.rows <- nrows - nrow(table.values)
			table.values <- rbind(table.values, matrix(0, add.rows, ncols))
			rhs.values <- c(rhs.values, rep(0, add.rows))
		}
		col.names <- names(coef(get(.activeModel)))
		col.names <- substring(paste(abbreviate(col.names, 12), "            "), 1, 12)
		make.col.names <- "labelPMgui(.tableFrame, text='')"
		for (j in 1:ncols) {
			make.col.names <- paste(make.col.names, ", ", 
					"labelPMgui(.tableFrame, text='", col.names[j], "')", sep="")
		}
		rhsText <- gettextPMgui("Right-hand side")
		make.col.names <- paste(make.col.names, ", labelPMgui(.tableFrame, text='          ')",
				", labelPMgui(.tableFrame, text='", rhsText, "')", sep="")
		eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
		for (i in 1:nrows){   
			varname <- paste(".tab.", i, ".1", sep="") 
			rhs.name <- paste(".rhs.", i, sep="")
			assign(varname, tclVar(table.values[i, 1]) , envir=env)
			assign(rhs.name, tclVar(rhs.values[i]), envir=env)
			make.row <- paste("labelPMgui(.tableFrame, text=", i, ")")
			make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=", 
					varname, ")", sep="")
			for (j in 2:ncols){
				varname <- paste(".tab.", i, ".", j, sep="")
				assign(varname, tclVar(table.values[i, j]), envir=env)
				make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=", 
						varname, ")", sep="")
			}
			make.row <- paste(make.row, ", labelPMgui(.tableFrame, text='     '),",
					"ttkentry(.tableFrame, width='5', textvariable=", rhs.name, ")", sep="")
			eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
		}
		tkgrid(get(".tableFrame", envir=env), sticky="w")
	}
	ncols <- length(coef(get(.activeModel)))
	rowsFrame <- tkframe(top)
	rowsValue <- tclVar(dialog.values$nrows)
	rowsSlider <- tkscale(rowsFrame, from=1, to=ncols, showvalue=FALSE, variable=rowsValue,
			resolution=1, orient="horizontal", command=setUpTable)
	rowsShow <- labelPMgui(rowsFrame, textvariable=rowsValue, width=2, justify="right")
	onOK <- function(){
		nrows <- as.numeric(tclvalue(rowsValue))
		cell <- 0
		values <- rep(NA, nrows*ncols)
		rhs <- rep(NA, nrows)
		for (i in 1:nrows){
			rhs.name <- paste(".rhs.", i, sep="")
			rhs[i] <- as.numeric(eval(parse(text=paste("tclvalue(", rhs.name,")", sep=""))))
			for (j in 1:ncols){
				cell <- cell+1
				varname <- paste(".tab.", i, ".", j, sep="")
				values[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
			}
		}
		values <- na.omit(values)
		closeDialog()
		if (length(values) != nrows*ncols){
			Message(message=sprintf(gettextPMgui("Number of valid entries in hypothesis matrix(%d)\nnot equal to number of rows (%d) * number of columns (%d)."), 
							length(values), nrows, ncols), type="error")
			testLinearHypothesis()
			return()
		}
		if (qr(matrix(values, nrows, ncols, byrow=TRUE))$rank < nrows) {
			Message(message=gettextPMgui("Hypothesis matrix is not of full row rank."),
					type="error")
			testLinearHypothesis()
			return()
		}            
		rhs <- na.omit(rhs)
		if (length(rhs) != nrows){
			errorCondition(recall=testLinearHypothesis, message=sprintf(gettextPMgui("Number of valid entries in rhs vector (%d)\nis not equal to number of rows (%d)."), length(rhs), nrows))
			return()
		}
		command <- paste("matrix(c(", paste(values, collapse=","), "), ", nrows, ", ", ncols,
				", byrow=TRUE)", sep="")
		assign(".Hypothesis", justDoIt(command), envir=.GlobalEnv)
		logger(paste(".Hypothesis <- ", command, sep=""))
		command <- paste("c(", paste(rhs, collapse=","), ")", sep="")
		assign(".RHS", justDoIt(command), envir=.GlobalEnv)
		logger(paste(".RHS <- ", command, sep=""))
		rhs.values <- .RHS
		command <- paste("linearHypothesis(", .activeModel, ", .Hypothesis, rhs=.RHS)", sep="")
		doItAndPrint(command)
		justDoIt("remove(.Hypothesis, .RHS, envir=.GlobalEnv)") 
		logger("remove(.Hypothesis, .RHS)")                                              
		tkfocus(PmetricsWindow())
		contrast.table <- matrix(values, nrows, ncols, byrow=TRUE)
		putDialog("testLinearHypothesis", list(previous.model=.activeModel, nrows=nrows, table.values=contrast.table,
						rhs.values=rhs.values))
	}
	OKCancelHelp(helpSubject="linearHypothesis", reset="testLinearHypothesis")
	tkgrid(labelPMgui(rowsFrame, text=gettextPMgui("Number of Rows:")), rowsSlider, rowsShow, sticky="w")
	tkgrid(rowsFrame, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Enter hypothesis matrix and right-hand side vector:"), fg="blue"), sticky="w")
	tkgrid(outerTableFrame, sticky="w")
	tkgrid(labelPMgui(top, text=""))
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=4, columns=1)       
} 

compareModels <- function () {
	modelPosn <- function(model){
		if (is.null(model)) return(NULL)
		if (!(model %in% models)) NULL
		else which(model == models) - 1
	}
	defaults <- list (initial.model1 = NULL, initial.model2 = NULL)
	dialog.values <- getDialog ("compareModels", defaults)  
	models <- listAllModels()
	if (length(models) < 2) {
		Message(message = gettextPMgui("There are fewer than two models."), 
				type = "error")
		tkfocus(PmetricsWindow())
		return()
	}
	initializeDialog(title = gettextPMgui("Compare Models"))
	modelsBox1 <- variableListBox(top, models, title = gettextPMgui("First model (pick one)"),
			initialSelection = modelPosn(dialog.values$initial.model1))
	modelsBox2 <- variableListBox(top, models, title = gettextPMgui("Second model (pick one)"),
			initialSelection = modelPosn(dialog.values$initial.model2))
	onOK <- function() {
		model1 <- getSelection(modelsBox1)
		model2 <- getSelection(modelsBox2)
		closeDialog()
		putDialog ("compareModels", list (initial.model1 = model1, initial.model2 = model2))
		if (length(model1) == 0 || length(model2) == 0) {
			errorCondition(recall = compareModels, message = gettextPMgui("You must select two models."))
			return()
		}
		if (!checkMethod("anova", model1)) {
			return()
		}
		if (!class(get(model1, envir = .GlobalEnv))[1] == class(get(model2, 
						envir = .GlobalEnv))[1]) {
			Message(message = gettextPMgui("Models are not of the same class."), 
					type = "error")
			compareModels()
			return()
		}
		if (glmP()) {
			family1 <- eval(parse(text = paste(model1, "$family$family", 
									sep = "")))
			family2 <- eval(parse(text = paste(model2, "$family$family", 
									sep = "")))
			if (family1 != family2) {
				Message(message = gettextPMgui("Models do not have the same family."), 
						type = "error")
				compareModels()
				return()
			}
			test <- if (family1 %in% c("binomial", "poisson")) 
						"Chisq"
					else "F"
			doItAndPrint(paste("anova(", model1, ", ", model2, 
							", test=\"", test, "\")", sep = ""))
		}
		else doItAndPrint(paste("anova(", model1, ", ", model2, 
							")", sep = ""))
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "anova", reset = "compareModels")
	tkgrid(getFrame(modelsBox1), getFrame(modelsBox2), sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 2, columns = 2)
}

BreuschPaganTest <- function () {
	if (is.null(.activeModel)) 
		return()
	Library("lmtest")
	currentModel <- FALSE
	defaults <- list (initial.var = "fitted", initial.student = 0)
	dialog.values <- getDialog ("BreuschPaganTest", defaults)
	initializeDialog(title = gettextPMgui("Breusch-Pagan Test"))
	tkgrid(labelPMgui(top, text = gettextPMgui("Score Test for Nonconstant Error Variance"), 
					fg = "blue"), sticky = "w")
	optionsFrame <- tkframe(top)
	onOK <- function() {
		.activeModel <- ActiveModel()
		student <- tclvalue(studentVariable)
		var <- tclvalue(varVariable)
		putDialog ("BreuschPaganTest", list (initial.var = var, initial.student = student))
		type <- if (var == "fitted") 
					paste(", varformula = ~ fitted.values(", .activeModel, 
							")", sep = "")
				else if (var == "predictors") 
					""
				else paste(", varformula = ~", tclvalue(rhsVariable), 
							sep = "")
		model.formula <- as.character(formula(get(.activeModel)))
		model.formula <- paste(model.formula[2], "~", model.formula[3])
		closeDialog()
		student <- if (tclvalue(studentVariable) == 1) 
					"TRUE"
				else "FALSE"
		command <- paste("bptest(", model.formula, type, ", studentize=", 
				student, ", data=", ActiveDataSet(), ")", sep = "")
		doItAndPrint(command)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "bptest", reset = "BreuschPaganTest")
	studentVariable <- tclVar(dialog.values$initial.student)
	studentFrame <- tkframe(optionsFrame)
	studentCheckBox <- tkcheckbutton(studentFrame, variable = studentVariable)
	tkgrid(labelPMgui(studentFrame, text = gettextPMgui("Studentized test statistic"), 
					justify = "left"), studentCheckBox, sticky = "w")
	tkgrid(studentFrame, sticky = "w")
	radioButtons(optionsFrame, name = "var", buttons = c("fitted", 
					"predictors", "other"), labels = gettextPMgui(c("Fitted values", 
							"Explanatory variables", "Other (specify)")), title = gettextPMgui("Variance Formula"), 
			initialValue = dialog.values$initial.var)
	tkgrid(varFrame, sticky = "w")
	modelFormula(optionsFrame, hasLhs = FALSE)
	tkgrid(formulaFrame, sticky = "w")
	tkgrid(outerOperatorsFrame)
	tkgrid(getFrame(xBox), sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 4, columns = 1)
}

DurbinWatsonTest <- function () {
	if (is.null(.activeModel)) 
		return()
	Library("lmtest")
	defaults <- list (initial.altHypothesis = "greater")
	dialog.values <- getDialog ("DurbinWatsonTest", defaults)
	initializeDialog(title = gettextPMgui("Durbin-Waton Test"))
	tkgrid(labelPMgui(top, text = gettextPMgui("Test for First-Order Error Autocorrelation"), 
					fg = "blue"), sticky = "w")
	onOK <- function() {
		altHypothesis <- tclvalue(altHypothesisVariable)
		putDialog ("DurbinWatsonTest", list(initial.altHypothesis = altHypothesis))
		closeDialog()
		model.formula <- as.character(formula(get(ActiveModel())))
		model.formula <- paste(model.formula[2], "~", model.formula[3])
		command <- paste("dwtest(", model.formula, ", alternative=\"", 
				altHypothesis, "\", data=", ActiveDataSet(), ")", 
				sep = "")
		doItAndPrint(command)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "dwtest", reset = "DurbinWatsonTest")
	radioButtons(name = "altHypothesis", buttons = c("greater", 
					"notequal", "less"), values = c("greater", "two.sided", 
					"less"), labels = c("rho >  0", "rho != 0", "rho <  0"), 
			title = gettextPMgui("Alternative Hypothesis"), 
			initialValue = dialog.values$initial.altHypothesis)
	tkgrid(altHypothesisFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1)
}

RESETtest <- function () {
	if (is.null(.activeModel)) 
		return()
	Library("lmtest")
	defaults <- list (initial.square = 1, initial.cube = 1, initial.type = "regressor")
	dialog.values <- getDialog ("RESETtest", defaults)
	initializeDialog(title = gettextPMgui("RESET Test"))
	tkgrid(labelPMgui(top, text = gettextPMgui("Test for Nonlinearity"), 
					fg = "blue"), sticky = "w")
	onOK <- function() {
		type <- tclvalue(typeVariable)
		square <- tclvalue(squareVariable)
		cube <- tclvalue(cubeVariable)
		putDialog ("RESETtest", list (initial.square = square, initial.cube = cube, initial.type = type))
		closeDialog()
		model.formula <- as.character(formula(get(ActiveModel())))
		model.formula <- paste(model.formula[2], "~", model.formula[3])
		if (square == "0" && cube == "0") {
			errorCondition(recall = RESETtest, message = gettextPMgui("No powers are checked."))
			return()
		}
		powers <- if (square == "1" && cube == "1") 
					"2:3"
				else if (square == "1" && cube == "0") 
					"2"
				else if (square == "0" && cube == "1") 
					"3"
		command <- paste("resettest(", model.formula, ", power=", 
				powers, ", type=\"", type, "\", data=", ActiveDataSet(), 
				")", sep = "")
		doItAndPrint(command)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "reset", reset = "RESETtest")
	optionsFrame <- tkframe(top)
	squareVariable <- tclVar(dialog.values$initial.square)
	squareCheckBox <- tkcheckbutton(optionsFrame, variable = squareVariable)
	cubeVariable <- tclVar(dialog.values$initial.cube)
	cubeCheckBox <- tkcheckbutton(optionsFrame, variable = cubeVariable)
	typeVariable <- tclVar("regressor")
	radioButtons(optionsFrame, name = "type", buttons = c("regressor", 
					"fitted", "princomp"), labels = gettextPMgui(c("Explanatory variables", 
							"Fitted values", "First principal component")), title = gettextPMgui("Type of Test"), 
			initialValue = dialog.values$initial.type)
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Powers to Include"), 
					fg = "blue"), sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("2 (squares)")), 
			squareCheckBox, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("3 (cubes)   ")), 
			cubeCheckBox, sticky = "w")
	tkgrid(typeFrame, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1)
}

OutlierTest <- function(){
	if (is.null(.activeModel)) return()
	Library("car")
	.activeModel <- ActiveModel()
	if (!checkMethod("outlierTest", .activeModel)) {
		errorCondition(gettextPMgui("There is no appropriate outlierTest method for a model of this class."))
		return()
	}
	doItAndPrint(paste("outlierTest(", .activeModel, ")", sep=""))
}

confidenceIntervals <- function () {
	if (is.null(.activeModel)) 
		return()
	Library("MASS")
	defaults <- list (initial.level = "0.95", initial.statistic="LR")
	dialog.values <- getDialog ("confidenceIntervals", defaults)
	initializeDialog(title = gettextPMgui("Confidence Intervals"))
	tkgrid(labelPMgui(top, text = gettextPMgui("Confidence Intervals for Individual Coefficients"), 
					fg = "blue"), sticky = "w")
	onOK <- function() {
		level <- tclvalue(confidenceLevel)
		putDialog ("confidenceIntervals", list (initial.level = level,
						initial.statistic = if(glm) tclvalue(typeVariable) else "LR"))
		opts <- options(warn = -1)
		lev <- as.numeric(level)
		options(opts)
		closeDialog()
		if ((is.na(lev)) || (lev < 0) || (lev > 1)) {
			Message(gettextPMgui("Confidence level must be a number between 0 and 1."))
			tkfocus(PmetricsWindow())
			return()
		}
		command <- if (glm) 
					paste("Confint(", .activeModel, ", level=", level, 
							", type=\"", tclvalue(typeVariable), "\")", sep = "")
				else paste("Confint(", .activeModel, ", level=", level, 
							")", sep = "")
		doItAndPrint(command)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "Confint", reset = "confidenceIntervals")
	confidenceFrame <- tkframe(top)
	confidenceLevel <- tclVar(dialog.values$initial.level)
	confidenceField <- ttkentry(confidenceFrame, width = "6", 
			textvariable = confidenceLevel)
	radioButtons(top, name = "type", buttons = c("LR", "Wald"), initialValue=dialog.values$initial.statistic,
			labels = gettextPMgui(c("Likelihood-ratio statistic", 
							"Wald statistic")), title = gettextPMgui("Test Based On"))
	tkgrid(labelPMgui(confidenceFrame, text = gettextPMgui("Confidence Level: ")), 
			confidenceField, sticky = "w")
	tkgrid(confidenceFrame, sticky = "w")
	.activeModel <- ActiveModel()
	glm <- class(get(.activeModel))[1] == "glm"
	if (glm) 
		tkgrid(typeFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3 + glm, columns = 1)
}

aic <- function(){
	.activeModel <- ActiveModel()
	if (is.null(.activeModel)) return()
	doItAndPrint(paste("AIC(", .activeModel, ")", sep=""))
}

bic <- function(){
	.activeModel <- ActiveModel()
	if (is.null(.activeModel)) return()
	doItAndPrint(paste("BIC(", .activeModel, ")", sep=""))
}

stepwiseRegression <- function () {
	defaults <- list (initial.direction = "backward/forward", initial.criterion = "BIC")
	dialog.values <- getDialog ("stepwiseRegression", defaults)
	initializeDialog(title = gettextPMgui("Stepwise Model Selection"))
	onOK <- function() {
		direction <- as.character(tclvalue(directionVariable))
		criterion <- as.character(tclvalue(criterionVariable))
		putDialog ("stepwiseRegression", list (initial.direction = tclvalue(directionVariable), 
						initial.criterion = tclvalue(criterionVariable)))
		closeDialog()
		doItAndPrint(paste("stepwise(", ActiveModel(), ", direction='", 
						direction, "', criterion='", criterion, "')", sep = ""))
		tkdestroy(top)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "stepwise", reset = "stepwiseRegression")
	radioButtons(top, name = "direction", buttons = c("bf", "fb", 
					"b", "f"), values = c("backward/forward", "forward/backward", 
					"backward", "forward"), labels = gettextPMgui(c("backward/forward", 
							"forward/backward", "backward", "forward")), title = gettextPMgui("Direction"), 
			initialValue = dialog.values$initial.direction)
	radioButtons(top, name = "criterion", buttons = c("bic", 
					"aic"), values = c("BIC", "AIC"), labels = gettextPMgui(c("BIC", 
							"AIC")), title = gettextPMgui("Criterion"), initialValue = dialog.values$initial.criterion)
	tkgrid(directionFrame, criterionFrame, sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 2, columns = 2)
}

subsetRegression <- function () {
	Library("leaps")
	defaults <- list (initial.criterion = "bic", initial.nbest = 1)
	dialog.values <- getDialog ("subsetRegression", defaults)
	initializeDialog(title = gettextPMgui("Subset Model Selection"))
	onOK <- function() {
		formula <- paste(sub("^[ ]*", "", deparse(formula(get(ActiveModel())))), 
				collapse = "")
		criterion <- as.character(tclvalue(criterionVariable))
		nbest <- as.numeric(tclvalue(nbestValue))
		putDialog ("subsetRegression", list (initial.criterion = criterion, initial.nbest = nbest))
		nvmax <- as.numeric(tclvalue(nvmaxValue))
		really.big <- if (nvmax > 50) 
					"TRUE"
				else "FALSE"
		closeDialog()
		doItAndPrint(paste("plot(regsubsets(", formula, ", data=", 
						ActiveDataSet(), ", nbest=", nbest, ", nvmax=", nvmax, 
						"), scale='", criterion, "')", sep = ""))
		tkdestroy(top)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "regsubsets", reset = "subsetRegression")
	radioButtons(top, name = "criterion", buttons = c("bic", 
					"Cp", "adjr2", "r2"), labels = gettextPMgui(c("BIC", 
							"Mallows Cp", "Adjusted R-sq.", "R-squared")), title = gettextPMgui("Criterion for Model Plot"), 
			initialValue = dialog.values$initial.criterion)
	nvar <- ncol(model.matrix(get(ActiveModel())))
	nbestValue <- tclVar(dialog.values$initial.nbest)
	nvmaxValue <- tclVar(as.character(min(25, nvar)))
	slidersFrame <- tkframe(top)
	nbestSlider <- tkscale(slidersFrame, from = 1, to = 10, showvalue = TRUE, 
			variable = nbestValue, resolution = 1, orient = "horizontal")
	nvmaxSlider <- tkscale(slidersFrame, from = 1, to = nvar, 
			showvalue = TRUE, variable = nvmaxValue, resolution = 1, 
			orient = "horizontal")
	tkgrid(tklabel(slidersFrame, text = "     "), tklabel(slidersFrame, 
					text = gettextPMgui("Number of best models\nof each size:"), 
					fg = "blue"), nbestSlider, sticky = "w")
	tkgrid(tklabel(slidersFrame, text = "     "), tklabel(slidersFrame, 
					text = gettextPMgui("Maximum size:"), fg = "blue"), nvmaxSlider, 
			sticky = "e")
	tkgrid(criterionFrame, slidersFrame, sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 2, columns = 2)
}
