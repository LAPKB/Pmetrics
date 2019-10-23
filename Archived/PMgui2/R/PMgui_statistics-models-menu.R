# Statistics Menu dialogs

# last modified 2011-12-09 by J. Fox

    # Models menu

linearRegressionModel <- function () {
	defaults <- list(initial.x = NULL, initial.y = NULL, 
			initial.subset = gettextPMgui("<all valid cases>"))
	dialog.values <- getDialog("linearRegressionModel", defaults)
	initializeDialog(title = gettextPMgui("Linear Regression"))
	variablesFrame <- tkframe(top)
	.numeric <- Numeric()
	xBox <- variableListBox(variablesFrame, .numeric, selectmode = "multiple", 
			title = gettextPMgui("Explanatory variables (pick one or more)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	yBox <- variableListBox(variablesFrame, .numeric, title = gettextPMgui("Response variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.y, "numeric"))
	UpdateModelNumber()
	modelName <- tclVar(paste("RegModel.", getPMgui("modelNumber"), 
					sep = ""))
	modelFrame <- tkframe(top)
	model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
	subsetBox(subset.expression = dialog.values$initial.subset)
	onOK <- function() {
		x <- getSelection(xBox)
		y <- getSelection(yBox)
		closeDialog()
		if (0 == length(y)) {
			UpdateModelNumber(-1)
			errorCondition(recall = linearRegressionModel, message = gettextPMgui("You must select a response variable."))
			return()
		}
		if (0 == length(x)) {
			UpdateModelNumber(-1)
			errorCondition(recall = linearRegressionModel, message = gettextPMgui("No explanatory variables selected."))
			return()
		}
		if (is.element(y, x)) {
			UpdateModelNumber(-1)
			errorCondition(recall = linearRegressionModel, message = gettextPMgui("Response and explanatory variables must be different."))
			return()
		}
		subset <- tclvalue(subsetVariable)
		putDialog ("linearRegressionModel", list (initial.x = x, initial.y = y, 
						initial.subset = subset))
		if (trim.blanks(subset) == gettextPMgui("<all valid cases>") || 
				trim.blanks(subset) == "") {
			subset <- ""
			putPMgui("modelWithSubset", FALSE)
		}
		else {
			subset <- paste(", subset=", subset, sep = "")
			putPMgui("modelWithSubset", TRUE)
		}
		modelValue <- trim.blanks(tclvalue(modelName))
		if (!is.valid.name(modelValue)) {
			UpdateModelNumber(-1)
			errorCondition(recall = linearRegressionModel, message = sprintf(gettextPMgui("\"%s\" is not a valid name."), 
							modelValue))
			return()
		}
		if (is.element(modelValue, listLinearModels())) {
			if ("no" == tclvalue(checkReplace(modelValue, type = gettextPMgui("Model")))) {
				UpdateModelNumber(-1)
				linearRegressionModel()
				return()
			}
		}
		command <- paste("lm(", y, "~", paste(x, collapse = "+"), 
				", data=", ActiveDataSet(), subset, ")", sep = "")
		logger(paste(modelValue, " <- ", command, sep = ""))
		assign(modelValue, justDoIt(command), envir = .GlobalEnv)
		doItAndPrint(paste("summary(", modelValue, ")", sep = ""))
		activeModel(modelValue)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "lm", model = TRUE, reset = "linearRegressionModel")
	tkgrid(labelPMgui(modelFrame, text = gettextPMgui("Enter name for model:")), 
			model, sticky = "w")
	tkgrid(modelFrame, sticky = "w")
	tkgrid(getFrame(yBox), labelPMgui(variablesFrame, text = "    "), 
			getFrame(xBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "w")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(buttonsFrame, stick = "w")
	tkgrid.configure(helpButton, sticky = "e")
	dialogSuffix(rows = 4, columns = 1)
}

linearModel <- function(){
	initializeDialog(title=gettextPMgui("Linear Model"))
	.activeModel <- ActiveModel()
	currentModel <- if (!is.null(.activeModel))
				class(get(.activeModel, envir=.GlobalEnv))[1] == "lm"
			else FALSE
	if (currentModel) {
		currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
		if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
	}
	if (isTRUE(getPMgui("reset.model"))) {
		currentModel <- FALSE
		putPMgui("reset.model", FALSE)
	}
	UpdateModelNumber()
	modelName <- tclVar(paste("LinearModel.", getPMgui("modelNumber"), sep=""))
	modelFrame <- tkframe(top)
	model <- ttkentry(modelFrame, width="20", textvariable=modelName)
	onOK <- function(){
		modelValue <- trim.blanks(tclvalue(modelName))
		closeDialog()
		if (!is.valid.name(modelValue)){
			errorCondition(recall=linearModel, message=sprintf(gettextPMgui('"%s" is not a valid name.'), modelValue), model=TRUE)
			return()
		}
		subset <- tclvalue(subsetVariable)
		if (trim.blanks(subset) == gettextPMgui("<all valid cases>") || trim.blanks(subset) == ""){
			subset <- ""
			putPMgui("modelWithSubset", FALSE)
		}
		else{
			subset <- paste(", subset=", subset, sep="")
			putPMgui("modelWithSubset", TRUE)
		}
		check.empty <- gsub(" ", "", tclvalue(lhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=linearModel, message=gettextPMgui("Left-hand side of model empty."), model=TRUE)
			return()
		}
		check.empty <- gsub(" ", "", tclvalue(rhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=linearModel, message=gettextPMgui("Right-hand side of model empty."), model=TRUE)
			return()
		}
		if (is.element(modelValue, listLinearModels())) {
			if ("no" == tclvalue(checkReplace(modelValue, type=gettextPMgui("Model")))){
				UpdateModelNumber(-1)
				linearModel()
				return()
			}
		}
		formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
		command <- paste("lm(", formula,
				", data=", ActiveDataSet(), subset, ")", sep="")
		logger(paste(modelValue, " <- ", command, sep=""))
		assign(modelValue, justDoIt(command), envir=.GlobalEnv)
		doItAndPrint(paste("summary(", modelValue, ")", sep=""))
		activeModel(modelValue)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="linearModel", model=TRUE, reset="resetLinearModel")
	tkgrid(labelPMgui(modelFrame, text=gettextPMgui("Enter name for model:")), model, sticky="w")
	tkgrid(modelFrame, sticky="w")
	modelFormula()
	subsetBox(model=TRUE)
	tkgrid(getFrame(xBox), sticky="w")
	tkgrid(outerOperatorsFrame, sticky="w")
	tkgrid(formulaFrame, sticky="w")
	tkgrid(subsetFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=6, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
}

resetLinearModel <- function(){
	putPMgui("reset.model", TRUE)
	linearModel()
}

generalizedLinearModel <- function(){
	families <- c("gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian",
			"quasibinomial", "quasipoisson")
	links <- c("identity", "inverse", "log", "logit", "probit",
			"cloglog", "sqrt", "1/mu^2")
	availableLinks <- matrix(c(
					TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
					FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
					TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE,
					TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
					TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,
					FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
					TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE),
			7, 8, byrow=TRUE)
	rownames(availableLinks) <- families
	colnames(availableLinks) <- links
	canonicalLinks <- c("identity", "logit", "log", "inverse", "1/mu^2", "logit", "log")
	names(canonicalLinks) <- families
	initializeDialog(title=gettextPMgui("Generalized Linear Model"))
	.activeModel <- ActiveModel()
	currentModel <- if (!is.null(.activeModel))
				class(get(.activeModel, envir=.GlobalEnv))[1] == "glm"
			else FALSE
	if (currentModel) {
		currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv), glm=TRUE)
		if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
	}
	if (isTRUE(getPMgui("reset.model"))) {
		currentModel <- FALSE
		putPMgui("reset.model", FALSE)
	}
	modelFormula()
	UpdateModelNumber()
	modelName <- tclVar(paste("GLM.", getPMgui("modelNumber"), sep=""))
	modelFrame <- tkframe(top)
	model <- ttkentry(modelFrame, width="20", textvariable=modelName)
	linkFamilyFrame <- tkframe(top)
	familyFrame <- tkframe(linkFamilyFrame)
	familyBox <- tklistbox(familyFrame, height="4", exportselection="FALSE",
			selectmode="single", background="white")
	familyScroll <- ttkscrollbar(familyFrame,
			command=function(...) tkyview(familyBox, ...))
	tkconfigure(familyBox, yscrollcommand=function(...) tkset(familyScroll, ...))
	for (fam in families) tkinsert(familyBox, "end", fam)
	linkFrame <- tkframe(linkFamilyFrame)
	linkBox <- tklistbox(linkFrame, height="4", exportselection="FALSE",
			selectmode="single", background="white")
	subsetBox(model=TRUE)
	onFamilySelect <- function(){
		family <- families[as.numeric(tkcurselection(familyBox)) + 1]
		availLinks <- links[availableLinks[family,]]
		tkdelete(linkBox, "0", "end")
		for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
		canLink <- canonicalLinks[family]
		tkconfigure(linkBox, height=length(availLinks))
		tkselection.set(linkBox, which(canLink == availLinks) - 1)
	}
	onOK <- function(){
		check.empty <- gsub(" ", "", tclvalue(lhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=generalizedLinearModel, model=TRUE, message=gettextPMgui("Left-hand side of model empty."))
			return()
		}
		check.empty <- gsub(" ", "", tclvalue(rhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=generalizedLinearModel, model=TRUE, message=gettextPMgui("Right-hand side of model empty."))
			return()
		}
		modelValue <- trim.blanks(tclvalue(modelName))
		if (!is.valid.name(modelValue)){
			errorCondition(recall=generalizedLinearModel, model=TRUE, message=sprintf(gettextPMgui('"%s" is not a valid name.'), modelValue))
			return()
		}
		if (is.element(modelValue, listGeneralizedLinearModels())) {
			if ("no" == tclvalue(checkReplace(modelValue, type=gettextPMgui("Model")))){
				UpdateModelNumber(-1)
				closeDialog()
				generalizedLinearModel()
				return()
			}
		}
		formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
		family <- families[as.numeric(tkcurselection(familyBox)) + 1]
		availLinks <- links[availableLinks[family,]]
		link <- availLinks[as.numeric(tkcurselection(linkBox)) + 1]
		subset <- tclvalue(subsetVariable)
		closeDialog()
		if (trim.blanks(subset) == gettextPMgui("<all valid cases>") || trim.blanks(subset) == ""){
			subset <- ""
			putPMgui("modelWithSubset", FALSE)
		}
		else{
			subset <- paste(", subset=", subset, sep="")
			putPMgui("modelWithSubset", TRUE)
		}
		command <- paste("glm(", formula, ", family=", family, "(", link,
				"), data=", ActiveDataSet(), subset, ")", sep="")
		logger(paste(modelValue, " <- ", command, sep=""))
		assign(modelValue, justDoIt(command), envir=.GlobalEnv)
		doItAndPrint(paste("summary(", modelValue, ")", sep=""))
		activeModel(modelValue)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="generalizedLinearModel", model=TRUE, reset="resetGLM")
	helpButton <- buttonPMgui(buttonsFrame, text="Help", width="12", command=onHelp)
	tkgrid(labelPMgui(modelFrame, text=gettextPMgui("Enter name for model:")), model, sticky="w")
	tkgrid(modelFrame, sticky="w")
	tkgrid(getFrame(xBox), sticky="w")
	tkgrid(outerOperatorsFrame, sticky="w")
	tkgrid(formulaFrame, sticky="w")
	tkgrid(subsetFrame, sticky="w")
	tkgrid(labelPMgui(linkFamilyFrame, text=gettextPMgui("Family (double-click to select)"), fg="blue"),
			labelPMgui(linkFamilyFrame, text="   "), labelPMgui(linkFamilyFrame, text=gettextPMgui("Link function"), fg="blue"), sticky="w")
	tkgrid(familyBox, familyScroll, sticky="nw")
	tkgrid(linkBox, sticky="nw")
	tkgrid(familyFrame, labelPMgui(linkFamilyFrame, text="   "), linkFrame, sticky="nw")
	tkgrid(linkFamilyFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	tkgrid.configure(familyScroll, sticky="ns")
	fam <- if (currentModel) which(currentFields$family == families) - 1
			else 1
	tkselection.set(familyBox, fam)
	availLinks <- links[availableLinks[fam + 1,]]
	for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
	tkconfigure(linkBox, height=length(availLinks))
	lnk <- if (currentModel) which(currentFields$link == availLinks) - 1
			else 0
	tkselection.set(linkBox, lnk)
	tkbind(familyBox, "<Double-ButtonPress-1>", onFamilySelect)
	dialogSuffix(rows=7, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
}

resetGLM <- function(){
	putPMgui("reset.model", TRUE)
	generalizedLinearModel()
}

ordinalRegressionModel <- function(){
	defaults <- list(initial.type="logistic")
	dialog.values <- getDialog("ordinalRegressionModel", defaults)
	Library("MASS")
	initializeDialog(title=gettextPMgui("Ordinal Regression Model"))
	.activeModel <- ActiveModel()
	.activeDataSet <- ActiveDataSet()
	currentModel <- if (!is.null(.activeModel))
				class(get(.activeModel, envir=.GlobalEnv))[1] == "polr"
			else FALSE
	if (currentModel) {
		currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
		if (currentFields$data != .activeDataSet) currentModel <- FALSE
	}
	if (isTRUE(getPMgui("reset.model"))) {
		currentModel <- FALSE
		putPMgui("reset.model", FALSE)
	}
	UpdateModelNumber()
	modelName <- tclVar(paste("OrdRegModel.", getPMgui("modelNumber"), sep=""))
	modelFrame <- tkframe(top)
	model <- ttkentry(modelFrame, width="20", textvariable=modelName)
	radioButtons(name="modelType",
			buttons=c("logistic", "probit"), initialValue=dialog.values$initial.type,
			labels=gettextPMgui(c("Proportional-odds logit", "Ordered probit")),
			title=gettextPMgui("Type of Model"))
	onOK <- function(){
		modelValue <- trim.blanks(tclvalue(modelName))
		closeDialog()
		if (!is.valid.name(modelValue)){
			errorCondition(recall=proportionalOddsModel, message=sprintf(gettextPMgui('"%s" is not a valid name.'), modelValue), model=TRUE)
			return()
		}
		subset <- tclvalue(subsetVariable)
		if (trim.blanks(subset) == gettextPMgui("<all valid cases>") || trim.blanks(subset) == ""){
			subset <- ""
			putPMgui("modelWithSubset", FALSE)
		}
		else{
			subset <- paste(", subset=", subset, sep="")
			putPMgui("modelWithSubset", TRUE)
		}
		check.empty <- gsub(" ", "", tclvalue(lhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=proportionalOddsModel, message=gettextPMgui("Left-hand side of model empty."), model=TRUE)
			return()
		}
		check.empty <- gsub(" ", "", tclvalue(rhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=proportionalOddsModel, message=gettextPMgui("Right-hand side of model empty."), model=TRUE)
			return()
		}
		if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=get(.activeDataSet, envir=.GlobalEnv)))){
#        if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=eval(parse(text=.activeDataSet), envir=.GlobalEnv)))){
			errorCondition(recall=proportionalOddsModel, message=gettextPMgui("Response variable must be a factor"))
			return()
		}
		if (is.element(modelValue, listProportionalOddsModels())) {
			if ("no" == tclvalue(checkReplace(modelValue, type=gettextPMgui("Model")))){
				UpdateModelNumber(-1)
				proportionalOddsModel()
				return()
			}
		}
		putDialog("ordinalRegressionModel", list(initial.type = tclvalue(modelTypeVariable)))
		formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
		command <- paste("polr(", formula, ', method="', tclvalue(modelTypeVariable),
				'", data=', .activeDataSet, subset, ", Hess=TRUE)", sep="")
		logger(paste(modelValue, " <- ", command, sep=""))
		assign(modelValue, justDoIt(command), envir=.GlobalEnv)
		doItAndPrint(paste("summary(", modelValue, ")", sep=""))
		activeModel(modelValue)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="polr", model=TRUE, reset = "resetPOLR")
	tkgrid(labelPMgui(modelFrame, text=gettextPMgui("Enter name for model:")), model, sticky="w")
	tkgrid(modelFrame, sticky="w")
	modelFormula()
	subsetBox(model=TRUE)
	tkgrid(getFrame(xBox), sticky="w")
	tkgrid(outerOperatorsFrame, sticky="w")
	tkgrid(formulaFrame, sticky="w")
	tkgrid(subsetFrame, sticky="w")
	tkgrid(modelTypeFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=7, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
}

resetPOLR <- function(){
	putPMgui("reset.model", TRUE)
	putDialog("ordinalRegressionModel", NULL)
	ordinalRegressionModel()
}

multinomialLogitModel <- function(){
	Library("nnet")
	initializeDialog(title=gettextPMgui("Multinomial Logit Model"))
	.activeModel <- ActiveModel()
	.activeDataSet <- ActiveDataSet()
	currentModel <- if (!is.null(.activeModel))
				class(get(.activeModel, envir=.GlobalEnv))[1] == "multinom"
			else FALSE
	if (currentModel) {
		currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
		if (currentFields$data != .activeDataSet) currentModel <- FALSE
	}
	if (isTRUE(getPMgui("reset.model"))) {
		currentModel <- FALSE
		putPMgui("reset.model", FALSE)
	}
	UpdateModelNumber()
	modelName <- tclVar(paste("MLM.", getPMgui("modelNumber"), sep=""))
	modelFrame <- tkframe(top)
	model <- ttkentry(modelFrame, width="20", textvariable=modelName)
	onOK <- function(){
		modelValue <- trim.blanks(tclvalue(modelName))
		closeDialog()
		if (!is.valid.name(modelValue)){
			errorCondition(recall=multinomialLogitModel, message=sprintf(gettextPMgui('"%s" is not a valid name.'), modelValue), model=TRUE)
			return()
		}
		subset <- tclvalue(subsetVariable)
		if (trim.blanks(subset) == gettextPMgui("<all valid cases>") || trim.blanks(subset) == ""){
			subset <- ""
			putPMgui("modelWithSubset", FALSE)
		}
		else{
			subset <- paste(", subset=", subset, sep="")
			putPMgui("modelWithSubset", TRUE)
		}
		check.empty <- gsub(" ", "", tclvalue(lhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=multinomialLogitModel, message=gettextPMgui("Left-hand side of model empty."), model=TRUE)
			return()
		}
		check.empty <- gsub(" ", "", tclvalue(rhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=multinomialLogitModel, message=gettextPMgui("Right-hand side of model empty."), model=TRUE)
			return()
		}
		if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=get(.activeDataSet, envir=.GlobalEnv)))){
			errorCondition(recall=multinomialLogitModel, message=gettextPMgui("Response variable must be a factor"))
			return()
		}
		if (is.element(modelValue, listMultinomialLogitModels())) {
			if ("no" == tclvalue(checkReplace(modelValue, type=gettextPMgui("Model")))){
				UpdateModelNumber(-1)
				multinomialLogitModel()
				return()
			}
		}
		formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
		command <- paste("multinom(", formula,
				", data=", .activeDataSet, subset, ", trace=FALSE)", sep="")
		logger(paste(modelValue, " <- ", command, sep=""))
		assign(modelValue, justDoIt(command), envir=.GlobalEnv)
		doItAndPrint(paste("summary(", modelValue, ", cor=FALSE, Wald=TRUE)", sep=""))
		activeModel(modelValue)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="multinom", model=TRUE, reset="resetMNL")
	tkgrid(labelPMgui(modelFrame, text=gettextPMgui("Enter name for model:")), model, sticky="w")
	tkgrid(modelFrame, sticky="w")
	modelFormula()
	subsetBox(model=TRUE)
	tkgrid(getFrame(xBox), sticky="w")
	tkgrid(outerOperatorsFrame, sticky="w")
	tkgrid(formulaFrame, sticky="w")
	tkgrid(subsetFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=6, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
}

resetMNL <- function(){
	putPMgui("reset.model", TRUE)
	multinomialLogitModel()
}

formulaFields <- function(model, hasLhs=TRUE, glm=FALSE){
	formula <- as.character(model$call$formula)
	if (hasLhs){
		lhs <- formula[2]
		rhs <- formula[3]
	} else {
		lhs <- NULL
		rhs <- formula[2]
	}
	data <- as.character(model$call$data)
	which.subset <- which("subset" == names(model$call))
	subset <- if (0 == length(which.subset)) ""
		else as.character(model$call)[[which.subset]]
	if (glm) {
		fam <- as.character(model$call$family)
		family <- fam[1]
		link <- fam[2]
	}
	else {
		family <- NULL
		link <- NULL
	}
	list(lhs=lhs, rhs=rhs, data=data, subset=subset, family=family, link=link)
}