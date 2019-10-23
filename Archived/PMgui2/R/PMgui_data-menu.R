# last modified 2012-01-27 by J. Fox
#  applied patch to improve window behaviour supplied by Milan Bouchet-Valat 2011-09-22

# Data menu dialogs

newDataSet <- function() {
	initializeDialog(title=gettextPMgui("New Data Set"))
	dsname <- tclVar(gettextPMgui("Dataset"))
	entryDsname <- ttkentry(top, width="20", textvariable=dsname)
	onOK <- function(){
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if (dsnameValue == "") {
			errorCondition(recall=newDataSet,
					message=gettextPMgui("You must enter the name of a data set."))
			return()
		}
		if (!is.valid.name(dsnameValue)) {
			errorCondition(recall=newDataSet,
					message=paste('"', dsnameValue, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
				newDataSet()
				return()
			}
		}
		closeDialog()
		command <- "edit(as.data.frame(NULL))"
		result <- justDoIt(command)
		result <- as.data.frame(lapply(result, function(x) if (is.character(x)) factor(x) else x))
		if (class(result)[1] !=  "try-error"){ 
			assign(dsnameValue, result, envir=.GlobalEnv)
			logger(paste(dsnameValue, "<-", command))
			if (nrow(get(dsnameValue)) == 0){
				#        	if (eval(parse(text=paste("nrow(", dsnameValue, ")"))) == 0){
				errorCondition(recall=newDataSet, message=gettextPMgui("empty data set."))
				return()
			}
			activeDataSet(dsnameValue)
		}
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="edit.data.frame")
	tkgrid(labelPMgui(top, text=gettextPMgui("Enter name for data set:")), entryDsname, sticky="e")
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	tkgrid.configure(entryDsname, sticky="w")
	dialogSuffix(rows=2, columns=2, focus=entryDsname)
}

selectActiveDataSet <- function(){
	dataSets <- listDataSets()
	.activeDataSet <- ActiveDataSet()
	if ((length(dataSets) == 1) && !is.null(.activeDataSet)) {
		Message(message=gettextPMgui("There is only one dataset in memory."),
				type="warning")
		tkfocus(PmetricsWindow())
		return()
	}
	if (length(dataSets) == 0){
		Message(message=gettextPMgui("There are no data sets from which to choose."),
				type="error")
		tkfocus(PmetricsWindow())
		return()
	}
	initializeDialog(title=gettextPMgui("Select Data Set"))
	dataSetsBox <- variableListBox(top, dataSets, title=gettextPMgui("Data Sets (pick one)"),
			initialSelection=if (is.null(.activeDataSet)) NULL else which(.activeDataSet == dataSets) - 1)
	onOK <- function(){
		activeDataSet(getSelection(dataSetsBox))
		closeDialog()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp()
	tkgrid(getFrame(dataSetsBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=2, columns=1)
}

listDataSetsInPackages <- function() doItAndPrint("data()")

Recode <- function () {
	require("car")
	processRecode <- function(recode) {
		parts <- strsplit(recode, "=")[[1]]
		if (length(grep(",", parts[1])) > 0) 
			paste("c(", parts[1], ") = ", parts[2], sep = "")
		else paste(parts, collapse = "=")
	}
	dataSet <- activeDataSet()
	defaults <- list (initial.asFactor = 1, initial.variables = NULL, initial.name = gettextPMgui ("variable"),
			initial.recode.directives="")
	dialog.values <- getDialog ("Recode", defaults)
	initializeDialog(title = gettextPMgui("Recode Variables"))
	variablesBox <- variableListBox(top, Variables(), selectmode = "multiple", 
			title = gettextPMgui("Variables to recode (pick one or more)"),
			initialSelection = varPosn (dialog.values$initial.variables, "all"))
	variablesFrame <- tkframe(top)
	newVariableName <- tclVar(dialog.values$initial.name)
	newVariable <- ttkentry(variablesFrame, width = "20", textvariable = newVariableName)
	recodesFrame <- tkframe(top)
	recodes <- tktext(recodesFrame, bg = "white", font = getPMgui("logFont"), 
			height = "5", width = "40", wrap = "none")
	recodesXscroll <- ttkscrollbar(recodesFrame, orient = "horizontal", 
			command = function(...) tkxview(recodes, ...))
	recodesYscroll <- ttkscrollbar(recodesFrame, command = function(...) tkyview(recodes, 
						...))
	tkconfigure(recodes, xscrollcommand = function(...) tkset(recodesXscroll, 
						...))
	tkconfigure(recodes, yscrollcommand = function(...) tkset(recodesYscroll, 
						...))
	tkinsert(recodes, "1.0", dialog.values$initial.recode.directives)
	asFactorFrame <- tkframe(top)
	asFactorVariable <- tclVar(dialog.values$initial.asFactor)
	asFactorCheckBox <- tkcheckbutton(asFactorFrame, variable = asFactorVariable)
	onOK <- function() {
		asFactor <- tclvalue(asFactorVariable) == "1"
		save.recodes <- trim.blanks(tclvalue(tkget(recodes, "1.0", "end")))
		recode.directives <- gsub("\n", "; ", save.recodes)
		check.empty <- gsub(";", "", gsub(" ", "", recode.directives))
		if ("" == check.empty) {
			errorCondition(recall = Recode, message = gettextPMgui("No recode directives specified."))
			return()
		}
		if (0 != length(grep("'", recode.directives))) {
			errorCondition(recall = Recode, message = gettextPMgui("Use only double-quotes (\" \") in recode directives"))
			return()
		}
		recode.directives <- strsplit(recode.directives, ";")[[1]]
		recode.directives <- paste(sapply(recode.directives, 
						processRecode), collapse = ";")
		recode.directives <- sub(" *; *$", "", recode.directives)
		variables <- getSelection(variablesBox)
		closeDialog()
		if (length(variables) == 0) {
			errorCondition(recall = Recode, message = gettextPMgui("You must select a variable."))
			return()
		}
		multiple <- if (length(variables) > 1) 
					TRUE
				else FALSE
		name <- trim.blanks(tclvalue(newVariableName))
#        save.recodes <- gsub("; ", "\\\n", trim.blanks(recode.directives))  
		putDialog ("Recode", list (initial.asFactor = asFactor, initial.variables = variables,
						initial.name = name, initial.recode.directives=save.recodes))
		for (variable in variables) {
			newVar <- if (multiple) 
						paste(name, variable, sep = "")
					else name
			if (!is.valid.name(newVar)) {
				errorCondition(recall = Recode, message = paste("\"", 
								newVar, "\" ", gettextPMgui("is not a valid name."), 
								sep = ""))
				return()
			}
			if (is.element(newVar, Variables())) {
				if ("no" == tclvalue(checkReplace(newVar))) {
					Recode()
					return()
				}
			}
			cmd <- paste("recode(", dataSet, "$", variable, ", '", 
					recode.directives, "', as.factor.result=", asFactor, 
					")", sep = "")
			logger(paste(dataSet, "$", newVar, " <- ", cmd, sep = ""))
			result <- justDoIt(paste(dataSet, "$", newVar, " <- ", 
							cmd, sep = ""))
			if (class(result)[1] != "try-error") 
				activeDataSet(dataSet, flushModel = FALSE, flushDialogMemory = FALSE)
			tkfocus(PmetricsWindow())
		}
	}
	OKCancelHelp(helpSubject = "Recode", reset = "Recode")
	tkgrid(getFrame(variablesBox), sticky = "nw")
	tkgrid(labelPMgui(variablesFrame, text = ""))
	tkgrid(labelPMgui(variablesFrame, text = gettextPMgui("New variable name or prefix for multiple recodes: ")), 
			newVariable, sticky = "w")
	tkgrid(labelPMgui(asFactorFrame, text = gettextPMgui("Make (each) new variable a factor")), 
			asFactorCheckBox, sticky = "w")
	tkgrid(labelPMgui(asFactorFrame, text = ""))
	tkgrid(labelPMgui(recodesFrame, text = gettextPMgui("Enter recode directives"), 
					fg = "blue"), sticky = "w")
	tkgrid(recodes, recodesYscroll, sticky = "nw")
	tkgrid(recodesXscroll)
	tkgrid(variablesFrame, sticky = "w")
	tkgrid(asFactorFrame, sticky = "w")
	tkgrid(recodesFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
	tkgrid.configure(recodesXscroll, sticky = "ew")
	tkgrid.configure(recodesYscroll, sticky = "ns")
	dialogSuffix(rows = 4, columns = 2, bindReturn = FALSE)
}

Compute <- function(){
	onDoubleClick <-function(){
		var <- trim.blanks(getSelection(variablesBox))
		word <- paste("\\[", gettextPMgui("factor"), "\\]", sep="")
		if (length(grep(word, var)) == 1)
			var <- trim.blanks(sub(word, "",  var))
		tkfocus(compute)
		expr <- tclvalue(computeVar)
		tclvalue(computeVar) <- if (expr == "") var
				else paste(expr, var, sep=if (rev(strsplit(expr, "")[[1]])[1] =="(" ) "" else " ")
		tkicursor(compute, "end")
		tkxview.moveto(compute, "1")
	}
	defaults <- list(initial.name = gettextPMgui("variable"), initial.expression = "")
	dialog.values <- getDialog("Compute", defaults)
	dataSet <- activeDataSet()
	initializeDialog(title=gettextPMgui("Compute New Variable"))
	.variables <- Variables()
	variables <- paste(.variables, ifelse(is.element(.variables, Factors()), gettextPMgui("[factor]"), ""))
	variablesBox <- variableListBox(top, variables, title=gettextPMgui("Current variables (double-click to expression)"))
	tkbind(variablesBox$listbox, "<Double-ButtonPress-1>", onDoubleClick)
	variablesFrame <- tkframe(top)
	newVariableName <- tclVar(dialog.values$initial.name)
	newVariable <- ttkentry(variablesFrame, width="20", textvariable=newVariableName)
	computeFrame <- tkframe(top)
	computeVar <- tclVar(dialog.values$initial.expression)
	compute <- ttkentry(computeFrame, font=getPMgui("logFont"), width="30", textvariable=computeVar)
	computeXscroll <- ttkscrollbar(computeFrame,
			orient="horizontal", command=function(...) tkxview(compute, ...))
	tkconfigure(compute, xscrollcommand=function(...) tkset(computeXscroll, ...))
	onOK <- function(){
		closeDialog()
		newVar <- trim.blanks(tclvalue(newVariableName))
		if (!is.valid.name(newVar)){
			errorCondition(recall=Compute,
					message=paste('"', newVar, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		express <- tclvalue(computeVar)
		check.empty <- gsub(";", "", gsub(" ", "", express))
		if ("" == check.empty) {
			errorCondition(recall=Compute,
					message=gettextPMgui("No expression specified."))
			return()
		}
		putDialog("Compute", list(initial.name=newVar, initial.expression=express))
		if (is.element(newVar, Variables())) {
			if ("no" == tclvalue(checkReplace(newVar, gettextPMgui("Variable")))){
				Compute()
				return()
			}
		}
		command <-  paste(dataSet,"$",newVar, " <- with(", ActiveDataSet(),
				", ", express, ")", sep="")
		logger(command)
		result <- justDoIt(command)
		if (class(result)[1] !=  "try-error") activeDataSet(dataSet, flushModel=FALSE, flushDialogMemory=FALSE)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="Compute", reset = "Compute")
	tkgrid(getFrame(variablesBox), sticky="nw", columnspan=2)
	tkgrid(labelPMgui(variablesFrame, text=gettextPMgui("New variable name")), sticky="w")
	tkgrid(newVariable, labelPMgui(variablesFrame, text="     "), sticky="w")
	tkgrid(labelPMgui(computeFrame, text=gettextPMgui("Expression to compute")), sticky="w")
	tkgrid(compute, sticky="w")
	tkgrid(computeXscroll, sticky="ew")
	tkgrid(variablesFrame, computeFrame, sticky="nw")
	tkgrid(buttonsFrame, sticky="w", columnspan=2)
	dialogSuffix(rows=3, columns=2, focus=compute)
}

deleteVariable <- function(){
	dataSet <- activeDataSet()
	initializeDialog(title=gettextPMgui("Delete Variables"))
	variablesBox <- variableListBox(top, Variables(),
			title=gettextPMgui("Variable(s) to delete (pick one or more)"), selectmode="multiple",
			initialSelection=NULL)
	onOK <- function(){
		variables <- getSelection(variablesBox)
		closeDialog()
		if (length(variables) == 0) {
			errorCondition(recall=deleteVariable, message=gettextPMgui("You must select one or more variables."))
			return()
		}
		if (length(variables) == 1){
			response <- tclvalue(PMguiTkmessageBox(message=sprintf(gettextPMgui("Delete %s?\nPlease confirm."), variables), icon="warning", type="okcancel", default="cancel"))
			if (response == "cancel") {
				onCancel()
				return()
			}
		}
		else{
			response <- tclvalue(PMguiTkmessageBox(message=
									sprintf(gettextPMgui("Delete %d variables?\nPlease confirm."), length(variables)),
							icon="warning", type="okcancel", default="cancel"))
			if (response == "cancel") {
				onCancel()
				return()
			}
		}
		for (variable in variables){
			eval(parse(text=paste(dataSet, "$", variable, "<- NULL", sep="")), envir=.GlobalEnv)
			logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
		}
		activeDataSet(dataSet, flushModel=FALSE, flushDialogMemory=FALSE)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="NULL")
	tkgrid(getFrame(variablesBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=2, columns=1)
}

readDataSet <- function() {
	initializeDialog(title=gettextPMgui("Read Text Data From File, Clipboard, or URL"))
	optionsFrame <- tkframe(top)
	dsname <- tclVar(gettextPMgui("Dataset"))
	entryDsname <- ttkentry(optionsFrame, width="20", textvariable=dsname)
	radioButtons(optionsFrame, "location", buttons=c("local", "clipboard", "url"), 
			labels=gettextPMgui(c("Local file system", "Clipboard", "Internet URL")), title=gettextPMgui("Location of Data File"))
	headerVariable <- tclVar("1")
	headerCheckBox <- tkcheckbutton(optionsFrame, variable=headerVariable)
	radioButtons(optionsFrame, "delimiter", buttons=c("whitespace", "commas", "tabs"),
			labels=gettextPMgui(c("White space", "Commas", "Tabs")), title=gettextPMgui("Field Separator"))
	otherButton <- ttkradiobutton(delimiterFrame, variable=delimiterVariable, value="other")
	otherVariable <- tclVar("")
	otherEntry <- ttkentry(delimiterFrame, width="4", textvariable=otherVariable)
	radioButtons(optionsFrame, "decimal", buttons=c("period", "comma"),
			labels=gettextPMgui(c("Period [.]", "Comma [,]")), title=gettextPMgui("Decimal-Point Character"))
	missingVariable <- tclVar("NA")
	missingEntry <- ttkentry(optionsFrame, width="8", textvariable=missingVariable)
	onOK <- function(){
		closeDialog()
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if (dsnameValue == ""){
			errorCondition(recall=readDataSet,
					message=gettextPMgui("You must enter a name for the data set."))
			return()
		}
		if (!is.valid.name(dsnameValue)){
			errorCondition(recall=readDataSet,
					message=paste('"', dsnameValue, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
				readDataSet()
				return()
			}
		}
		location <- tclvalue(locationVariable)
		file <- if (location == "clipboard") "clipboard" 
				else if (location == "local") tclvalue(tkgetOpenFile(filetypes=
											gettextPMgui('{"All Files" {"*"}} {"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}}')))
				else {
					initializeDialog(subdialog, title=gettextPMgui("Internet URL"))
					onOKsub <- function(){
						closeDialog(subdialog)
					}
					urlFrame <- tkframe(subdialog)
					urlVar <- tclVar("")
					url <- ttkentry(urlFrame, font=getPMgui("logFont"), width="30", textvariable=urlVar)
					urlXscroll <- ttkscrollbar(urlFrame,
							orient="horizontal", command=function(...) tkxview(url, ...))
					tkconfigure(url, xscrollcommand=function(...) tkset(urlXscroll, ...))
					subOKCancelHelp()
					tkgrid(url, sticky="w")
					tkgrid(urlXscroll, sticky="ew")
					tkgrid(urlFrame, sticky="nw")
					tkgrid(subButtonsFrame, sticky="w")
					dialogSuffix(subdialog, rows=2, columns=1, focus=url, onOK=onOKsub)
					tclvalue(urlVar)
				}
		if (file == "") {
			if (getPMgui("grab.focus")) tkgrab.release(top)
			tkdestroy(top)
			return()
		}
		head <- tclvalue(headerVariable) == "1"
		delimiter <- tclvalue(delimiterVariable)
		del <- if (delimiter == "whitespace") ""
				else if (delimiter == "commas") ","
				else if (delimiter == "tabs") "\\t"
				else tclvalue(otherVariable)
		miss <- tclvalue(missingVariable)
		dec <- if (tclvalue(decimalVariable) == "period") "." else ","
		command <- paste('read.table("', file,'", header=', head,
				', sep="', del, '", na.strings="', miss, '", dec="', dec, '", strip.white=TRUE)', sep="")
		logger(paste(dsnameValue, " <- ", command, sep=""))
		result <- justDoIt(command)
		if (class(result)[1] !=  "try-error"){
			assign(dsnameValue, result, envir=.GlobalEnv)
			activeDataSet(dsnameValue)
		}
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="read.table")
	tkgrid(labelPMgui(optionsFrame, text=gettextPMgui("Enter name for data set:")), entryDsname, sticky="w")
	tkgrid(labelPMgui(optionsFrame, text=gettextPMgui("Variable names in file:")), headerCheckBox, sticky="w")
	tkgrid(labelPMgui(optionsFrame, text=gettextPMgui("Missing data indicator:")), missingEntry, sticky="w")
	tkgrid(locationFrame, sticky="w")
	tkgrid(labelPMgui(delimiterFrame, text=gettextPMgui("Other")), otherButton,
			labelPMgui(delimiterFrame, text=gettextPMgui("  Specify:")), otherEntry, sticky="w")
	tkgrid(delimiterFrame, sticky="w", columnspan=2)
	tkgrid(decimalFrame, sticky="w")
	tkgrid(optionsFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=5, columns=1)
}

readDataFromPackage <- function() {
	env <- environment()
	datasets <- NULL
	initializeDialog(title=gettextPMgui("Read Data From Package"))
	dsname <- tclVar("")
	package <- NULL
	enterFrame <- tkframe(top)
	entryDsname <- ttkentry(enterFrame, width="20", textvariable=dsname)
	packages <- sort(.packages())
	packages <- packages[! packages %in% c("base", "stats")]
	packages <- packages[sapply(packages, function(package){
						ds <- data(package=package)$results
						if (nrow(ds) == 0) return(FALSE)
						ds <- ds[, "Item"]
						valid <- sapply(ds, is.valid.name)
						length(ds[valid]) > 0
					})]
	packageDatasetFrame <- tkframe(top)
	packageFrame <- tkframe(packageDatasetFrame)
	packageBox <- tklistbox(packageFrame, height="4", exportselection="FALSE",
			selectmode="single", background="white")
	packageScroll <- ttkscrollbar(packageFrame,
			command=function(...) tkyview(packageBox, ...))
	tkconfigure(packageBox, yscrollcommand=function(...) tkset(packageScroll, ...))
	for (p in packages) tkinsert(packageBox, "end", p)
	datasetFrame <- tkframe(packageDatasetFrame)
	datasetBox <- tklistbox(datasetFrame, height="4", exportselection="FALSE",
			selectmode="single", background="white")
	datasetScroll <- ttkscrollbar(datasetFrame,
			command=function(...) tkyview(datasetBox, ...))
	tkconfigure(datasetBox, yscrollcommand=function(...) tkset(datasetScroll, ...))
	onPackageSelect <- function(){
		assign("package", packages[as.numeric(tkcurselection(packageBox)) + 1], envir=env)
		datasets <<- data(package=package)$results[,3]
		valid <- sapply(datasets, is.valid.name)
		datasets <<- datasets[valid]
		tkdelete(datasetBox, "0", "end")
		for (dataset in datasets) tkinsert(datasetBox, "end", dataset)
		tkconfigure(datasetBox, height=min(4, length(datasets)))
		firstChar <- tolower(substr(datasets, 1, 1))
		len <- length(datasets)
		onLetter <- function(letter){
			letter <- tolower(letter)
			current <- 1 + round(as.numeric(unlist(strsplit(tclvalue(tkyview(datasetBox) ), " "))[1])*len)
			mat <- match(letter, firstChar[-(1:current)])
			if (is.na(mat)) return()
			tkyview.scroll(datasetBox, mat, "units")
		}
		onA <- function() onLetter("a")
		onB <- function() onLetter("b")
		onC <- function() onLetter("c")
		onD <- function() onLetter("d")
		onE <- function() onLetter("e")
		onF <- function() onLetter("f")
		onG <- function() onLetter("g")
		onH <- function() onLetter("h")
		onI <- function() onLetter("i")
		onJ <- function() onLetter("j")
		onK <- function() onLetter("k")
		onL <- function() onLetter("l")
		onM <- function() onLetter("m")
		onN <- function() onLetter("n")
		onO <- function() onLetter("o")
		onP <- function() onLetter("p")
		onQ <- function() onLetter("q")
		onR <- function() onLetter("r")
		onS <- function() onLetter("s")
		onT <- function() onLetter("t")
		onU <- function() onLetter("u")
		onV <- function() onLetter("v")
		onW <- function() onLetter("w")
		onX <- function() onLetter("x")
		onY <- function() onLetter("y")
		onZ <- function() onLetter("z")
		for (letter in c(letters, LETTERS)){
			tkbind(datasetBox, paste("<", letter, ">", sep=""),
					get(paste("on", toupper(letter), sep="")))
		}
		onClick <- function() tkfocus(datasetBox)
		tkbind(datasetBox, "<ButtonPress-1>", onClick)
	}
	onDatasetSelect <- function(){
		tclvalue(dsname) <- datasets[as.numeric(tkcurselection(datasetBox)) + 1]
	}
	firstChar <- tolower(substr(packages, 1, 1))
	len <- length(packages)
	onLetter <- function(letter){
		letter <- tolower(letter)
		current <- 1 + round(as.numeric(unlist(strsplit(tclvalue(tkyview(packageBox) ), " "))[1])*len)
		mat <- match(letter, firstChar[-(1:current)])
		if (is.na(mat)) return()
		tkyview.scroll(packageBox, mat, "units")
	}
	onA <- function() onLetter("a")
	onB <- function() onLetter("b")
	onC <- function() onLetter("c")
	onD <- function() onLetter("d")
	onE <- function() onLetter("e")
	onF <- function() onLetter("f")
	onG <- function() onLetter("g")
	onH <- function() onLetter("h")
	onI <- function() onLetter("i")
	onJ <- function() onLetter("j")
	onK <- function() onLetter("k")
	onL <- function() onLetter("l")
	onM <- function() onLetter("m")
	onN <- function() onLetter("n")
	onO <- function() onLetter("o")
	onP <- function() onLetter("p")
	onQ <- function() onLetter("q")
	onR <- function() onLetter("r")
	onS <- function() onLetter("s")
	onT <- function() onLetter("t")
	onU <- function() onLetter("u")
	onV <- function() onLetter("v")
	onW <- function() onLetter("w")
	onX <- function() onLetter("x")
	onY <- function() onLetter("y")
	onZ <- function() onLetter("z")
	for (letter in c(letters, LETTERS)){
		tkbind(packageBox, paste("<", letter, ">", sep=""),
				get(paste("on", toupper(letter), sep="")))
	}
	onClick <- function() tkfocus(packageBox)
	tkbind(packageBox, "<ButtonPress-1>", onClick)
	onOK <- function(){
		datasetName <- datasets[as.numeric(tkcurselection(datasetBox)) + 1]
		dsnameValue <- tclvalue(dsname)
		if (dsnameValue != "" && is.null(package)){
			closeDialog()
			if (is.element(dsnameValue, listDataSets())) {
				if ("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
					if (GrabFocus()) tkgrab.release(top)
					tkdestroy(top)
					readDataFromPackage()
					return()
				}
			}
			save.options <- options(warn=2)
			check <- try(eval(parse(text=logger(paste("data(", dsnameValue, ")", sep=""))),
							envir=.GlobalEnv), silent=TRUE)
			options(save.options)
			if (class(check) == "try-error"){
				errorCondition(recall=readDataFromPackage,
						message=sprintf(gettextPMgui("Data set %s does not exit"), dsnameValue))
				return()
			}
			activeDataSet(dsnameValue)
			tkfocus(PmetricsWindow())
		}
		else{
			if (is.null(package)) {
				errorCondition(recall=readDataFromPackage, message=gettextPMgui("You must select a package."))
				return()
			}
			if (length(datasetName) == 0) {
				errorCondition(recall=readDataFromPackage, message=gettextPMgui("You must select a data set.")    )
				return()
			}
			if (is.element(datasetName, listDataSets())) {
				if ("no" == tclvalue(checkReplace(datasetName, gettextPMgui("Data set")))){
					if (GrabFocus()) tkgrab.release(top)
					tkdestroy(top)
					readDataFromPackage()
					return()
				}
			}
			closeDialog()
			command <- paste("data(", datasetName, ', package="', package, '")', sep="")
			result <- justDoIt(command)
			logger(command)
			if (class(result)[1] !=  "try-error") activeDataSet(datasetName)
			tkfocus(PmetricsWindow())
		}
	}
	onDataHelp <- function(){
		datasetName <- data(package=package)$results[as.numeric(tkcurselection(datasetBox)) + 1,3]
		dsnameValue <- tclvalue(dsname)
		if (dsnameValue == "") dsnameValue <- datasetName
		if (length(dsnameValue) == 0) Message(gettextPMgui("No data set selected."), type="warning")
		else if (is.null(package)) doItAndPrint(paste('help("', dsnameValue, '")', sep=""))
		else doItAndPrint(paste('help("', dsnameValue, '", package="', package, '")', sep=""))
	}
	OKCancelHelp(helpSubject="data")
	dataHelpButton <- buttonPMgui(top, text=gettextPMgui("Help on selected data set"), command=onDataHelp)
	tkgrid(labelPMgui(packageDatasetFrame, text=gettextPMgui("Package (Double-click to select)"), fg="blue"),
			labelPMgui(packageDatasetFrame, text="   "), labelPMgui(packageDatasetFrame, text=gettextPMgui("Data set (Double-click to select)"),
					fg="blue"), sticky="w")
	tkgrid(packageBox, packageScroll, sticky="nw")
	tkgrid(datasetBox, datasetScroll, sticky="nw")
	tkgrid(packageFrame, labelPMgui(packageDatasetFrame, text="   "), datasetFrame, sticky="nw")
	tkgrid(packageDatasetFrame, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("OR"), fg="red"), sticky="w")
	tkgrid(labelPMgui(enterFrame, text=gettextPMgui("Enter name of data set:  "), fg="blue"), entryDsname, sticky="w")
	tkgrid(enterFrame, sticky="w")
	tkgrid(dataHelpButton, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	tkgrid.configure(packageScroll, sticky="ns")
	tkgrid.configure(datasetScroll, sticky="ns")
	tkbind(packageBox, "<Double-ButtonPress-1>", onPackageSelect)
	tkbind(datasetBox, "<Double-ButtonPress-1>", onDatasetSelect)
	dialogSuffix(rows=5, columns=1, focus=entryDsname)
}

importSPSS <- function() {
	Library("foreign")
	initializeDialog(title=gettextPMgui("Import SPSS Data Set"))
	dsname <- tclVar(gettextPMgui("Dataset"))
	entryDsname <- ttkentry(top, width="20", textvariable=dsname)
	asFactor <- tclVar("1")
	asFactorCheckBox <- tkcheckbutton(top, variable=asFactor)
	maxLevels <- tclVar("Inf")
	entryMaxLevels <- ttkentry(top, width="5", textvariable=maxLevels)
	onOK <- function(){
		closeDialog()
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if (dsnameValue == ""){
			errorCondition(recall=importSPSS,
					message=gettextPMgui("You must enter the name of a data set."))
			return()
		}
		if (!is.valid.name(dsnameValue)){
			errorCondition(recall=importSPSS,
					message=paste('"', dsnameValue, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
				importSPSS()
				return()
			}
		}
		file <- tclvalue(tkgetOpenFile(
						filetypes=gettextPMgui('{"All Files" {"*"}} {"SPSS portable files" {".por" ".POR"}} {"SPSS save files" {".sav" ".SAV"}}')))
		if (file == "") {
			tkfocus(PmetricsWindow())
			return()
		}
		factor <- tclvalue(asFactor) == "1"
		levels <- as.numeric(tclvalue(maxLevels))
		command <- paste('read.spss("', file,'", use.value.labels=', factor,
				", max.value.labels=", levels, ", to.data.frame=TRUE)", sep="")
		logger(paste(dsnameValue, " <- ", command, sep=""))
		result <- justDoIt(command)
		if (class(result)[1] !=  "try-error"){
			assign(dsnameValue, result, envir=.GlobalEnv)
			activeDataSet(dsnameValue)
		}
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="read.spss")
	tkgrid(labelPMgui(top, text=gettextPMgui("Enter name for data set:")), entryDsname, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Convert value labels\nto factor levels"), justify="left"),
			asFactorCheckBox, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Maximum number\nof value labels\nfor factor conversion"), justify="left"),
			entryMaxLevels, sticky="w")
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	tkgrid.configure(entryDsname, sticky="w")
	tkgrid.configure(asFactorCheckBox, sticky="w")
	tkgrid.configure(entryMaxLevels, sticky="w")
	dialogSuffix(rows=4, columns=2, focus=entryDsname)
}

importMinitab <- function() {
	Library("foreign")
	initializeDialog(title=gettextPMgui("Import Minitab Data Set"))
	dsname <- tclVar(gettextPMgui("Dataset"))
	entryDsname <- ttkentry(top, width="20", textvariable=dsname)
	onOK <- function(){
		closeDialog()
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if (dsnameValue == ""){
			errorCondition(recall=importMinitab,
					message=gettextPMgui("You must enter the name of a data set."))
			return()
		}
		if (!is.valid.name(dsnameValue)){
			errorCondition(recall=importMinitab,
					message=paste('"', dsnameValue, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
				importMinitab()
				return()
			}
		}
		file <- tclvalue(tkgetOpenFile(
						filetypes=gettextPMgui('{"All Files" {"*"}} {"Minitab portable files" {".mtp" ".MTP"}}')))
		if (file == "") {
			tkfocus(PmetricsWindow())
			return()
		}
		command <- paste('read.mtp("', file,'")', sep="")
		datalist <- justDoIt(command)
		lengths <- sapply(datalist, length)
		datalist <- datalist[lengths != 0]
		lengths <- lengths[lengths != 0]
		if (!all(lengths == length(datalist[[1]]))){
			Message(message=
							paste(gettextPMgui("Minitab data set contains elements of unequal length.\nData set cannot be converted.")),
					type="error")
			tkdestroy(top)
			tkfocus(PmetricsWindow())
			return()
		}
		assign(dsnameValue, as.data.frame(datalist), envir=.GlobalEnv)
		logger(paste(dsnameValue, " <- as.data.frame(", command, ")", sep=""))
		activeDataSet(dsnameValue)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="read.mtp")
	tkgrid(labelPMgui(top, text=gettextPMgui("Enter name for data set:")), entryDsname, sticky="e")
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	tkgrid.configure(entryDsname, sticky="w")
	dialogSuffix(rows=2, columns=2, focus=entryDsname)
}

# the following function was contributed by Michael Ash (modified by J. Fox 2 Feb 05)

importSTATA <- function() {
	Library("foreign")
	initializeDialog(title=gettextPMgui("Import STATA Data Set"))
	dsname <- tclVar(gettextPMgui("Dataset"))
	entryDsname <- ttkentry(top, width="20", textvariable=dsname)
	asFactor <- tclVar("1")
	asFactorCheckBox <- tkcheckbutton(top, variable=asFactor)
	asDate <- tclVar("1")
	asDateCheckBox <- tkcheckbutton(top, variable=asDate)
	asMissingType <- tclVar("1")
	asMissingTypeCheckBox <- tkcheckbutton(top, variable=asMissingType)
	asConvertUnderscore <- tclVar("1")
	asConvertUnderscoreCheckBox <- tkcheckbutton(top, variable=asConvertUnderscore)
	asWarnMissingLabels <- tclVar("1")
	asWarnMissingLabelsCheckBox <- tkcheckbutton(top, variable=asWarnMissingLabels)
	onOK <- function(){
		closeDialog()
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if (dsnameValue == ""){
			errorCondition(recall=importSTATA,
					message=gettextPMgui("You must enter the name of a data set."))
			return()
		}
		if (!is.valid.name(dsnameValue)){
			errorCondition(recall=importSTATA,
					message=paste('"', dsnameValue, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
				importSTATA()
				return()
			}
		}
		file <- tclvalue(tkgetOpenFile(
						filetypes=gettextPMgui('{"All Files" {"*"}} {"STATA datasets" {".dta" ".DTA"}}')))
		if (file == "") {
			tkfocus(PmetricsWindow())
			return()
		}
		convert.date <- tclvalue(asDate) == "1"
		factor <- tclvalue(asFactor) == "1"
		missingtype <- tclvalue(asMissingType) == "1"
		convertunderscore <- tclvalue(asConvertUnderscore) == "1"
		warnmissinglabels <- tclvalue(asWarnMissingLabels) == "1"
		command <- paste('read.dta("', file,'", convert.dates=', convert.date,
				", convert.factors=", factor, ", missing.type=", missingtype,
				", convert.underscore=", convertunderscore, ", warn.missing.labels=TRUE)", sep="")
		logger(paste(dsnameValue, " <- ", command, sep=""))
		result <- justDoIt(command)
		if (class(result)[1] !=  "try-error"){
			assign(dsnameValue, result, envir=.GlobalEnv)
			activeDataSet(dsnameValue)
		}
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="read.dta")
	tkgrid(labelPMgui(top, text=gettextPMgui("Enter name for data set:")), entryDsname, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Convert value labels\nto factor levels"), justify="left"),
			asFactorCheckBox, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Convert dates to R format"), justify="left"),
			asDateCheckBox, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Multiple missing types (>=Stata 8)"), justify="left"),
			asMissingTypeCheckBox, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Convert underscore to period"), justify="left"),
			asConvertUnderscoreCheckBox, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Warn on missing labels"), justify="left"),
			asWarnMissingLabelsCheckBox, sticky="w")
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	tkgrid.configure(entryDsname, sticky="w")
	tkgrid.configure(asFactorCheckBox, sticky="w")
	tkgrid.configure(asDateCheckBox, sticky="w")
	tkgrid.configure(asMissingTypeCheckBox, sticky="w")
	tkgrid.configure(asWarnMissingLabelsCheckBox, sticky="w")
	dialogSuffix(rows=4, columns=2, focus=entryDsname)
}

# The following function was contributed by Matthieu Lesnoff
#  (added with small changes by J. Fox, 20 July 06 & 30 July 08)

importRODBCtable <- function(){
	# load the RODBC package and stops the program if not available
	if(!require(RODBC))
		stop(gettextPMgui("This function requires the RODBC package.\n"))
	# close all databases in case of error
	on.exit(odbcCloseAll())
# Enter the name of data set, by default : Dataset
	initializeDialog(title = gettextPMgui("Import from Excel, Access or dBase data set"))
	dsname <- tclVar(gettextPMgui("Dataset"))
	entryDsname <- ttkentry(top, width = "35", textvariable = dsname)
	onOK <- function(){
		closeDialog()
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if(dsnameValue == ""){
			errorCondition(recall = importRODBCtable,
					message = gettextPMgui("You must enter the name of a data set."))
			return()
		}
		if(!is.valid.name(dsnameValue)){
			errorCondition(recall = queryimportRODBCtable,
					message = paste('"', dsnameValue, '" ',
							gettextPMgui("is not a valid name."), sep = ""))
			return()
		}
		if(is.element(dsnameValue, listDataSets())){
			if("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
				importRODBCtable()
				return()
			}
		}
		File <- tclvalue(tkgetOpenFile(filetypes = gettextPMgui(
								'{"All Files" {"*"}} {"MS Access database" {".mdb" ".MDB"}} {"MS Access 2007 database" {".accdb" ".ACCDB"}} {"dBase-like file" {".dbf" ".DBF"}} {"MS Excel 2007 file" {".xlsx" ".XLSX"}} {"MS Excel file" {".xls" ".XLS"}}'
						), parent=PmetricsWindow()))
		if(File == ""){
			tkfocus(PmetricsWindow())
			return()
		}
		sop <- match(".", rev(strsplit(File, NULL)[[1]]))[1]
		ext <- tolower(substring(File, nchar(File) - sop + 2, nchar(File)))
		channel <- switch(EXPR = ext,
				xls = odbcConnectExcel(File),
				xlsx = odbcConnectExcel2007(File),
				mdb = odbcConnectAccess(File),
				accdb = odbcConnectAccess2007(File),
				dbf = odbcConnectDbase(File))
		# For Excel and Access cases, need to select a particular sheet or table
		if(ext != "dbf"){
			tabdat <- sqlTables(channel)
			names(tabdat) <- tolower(names(tabdat))
			if(ext == "mdb" || ext == "accdb")
				tabdat <- tabdat[tabdat$table_type == "TABLE", 3]
			if(ext == "xls" || ext == "xlsx"){
				tabname <- tabdat$table_name
				tabdat <- ifelse(tabdat$table_type =="TABLE",
						substring(tabname, 2, nchar(tabname) - 2),
						substring(tabname, 1, nchar(tabname) - 1))
			}
			# if there are several tables
			if(length(tabdat)>1)
				fil <- tk_select.list(sort(tabdat),
						title = gettextPMgui("Select one table"))
			else
				fil <- tabdat
			if(fil == ""){
				errorCondition(message=gettextPMgui("No table selected"))
				return()
			}
			if(ext == "xls" || ext == "xlsx")
				fil <- paste("[", fil, "$]", sep = "")
		}
		# dBase file
		else{
			sop <- match(".", rev(strsplit(File, NULL)[[1]]))[1]
			root <- tolower(substring(File, 1, nchar(File) - sop))
			revstr <- rev(strsplit(root, NULL)[[1]])
			sop <- if(is.na(match(c("/", "\\"), revstr)[1]))
						length(revstr) else match(c("/", "\\"), revstr)[1] - 1
			toor <- revstr[seq(sop)]
			fil <- paste(rev(toor), collapse = "")
		}
		# Retrieve the data
		dat <- sqlQuery(channel = channel, query = paste("select * from", fil))
		names(dat)<- trim.blanks(names(dat))
		dat <- trim.col.na(dat)
		odbcCloseAll()
		assign(dsnameValue, as.data.frame(dat), envir = .GlobalEnv)
		command <- paste("sqlQuery(channel = ",channel,", select * from ", fil,")",
				sep = "")
		logger(paste(dsnameValue, " <- ", command, sep = ""))
		activeDataSet(dsnameValue)
		tkfocus(PmetricsWindow())
	}  ## End of function onOK
	OKCancelHelp(helpSubject="odbcConnect")
	tkgrid(labelPMgui(top, text=gettextPMgui("Enter name of data set:  ")),
			entryDsname, sticky="e")
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	tkgrid.configure(entryDsname, sticky="w")
	dialogSuffix(rows=2, columns=2, focus=entryDsname)
}


numericToFactor <- function(){
	initializeDialog(title=gettextPMgui("Convert Numeric Variables to Factors"))
	variableBox <- variableListBox(top, Numeric(), selectmode="multiple",
			title=gettextPMgui("Variables (pick one or more)"))
	radioButtons(name="levels", buttons=c("names", "numbers"),
			labels=gettextPMgui(c("Supply level names", "Use numbers")), title=gettextPMgui("Factor Levels"))
	factorName <- tclVar(gettextPMgui("<same as variables>"))
	factorNameField <- ttkentry(top, width="20", textvariable=factorName)
	onOK <- function(){
		variables <- getSelection(variableBox)
		closeDialog()
		if (length(variables) == 0) {
			errorCondition(recall=numericToFactor, message=gettextPMgui("You must select a variable."))
			return()
		}
		facname <- trim.blanks(tclvalue(factorName))
		.activeDataSet <- ActiveDataSet()
		cmd <- paste("apply(", .activeDataSet, "[c(", paste(
						paste('"', variables, '"', sep=""),
						collapse=","), ")], 2, function(x) sort(unique(x)))", sep="")
		levs <- eval(parse(text=cmd), envir=.GlobalEnv)
		sameLevels <- (length(variables) == 1) ||
				((is.matrix(levs)) && (all(0 == apply(levs, 1, var))))
		for (name in variables){
			fname <- if (facname == gettextPMgui("<same as variables>")) name
					else if (length(variables) == 1) facname
					else paste(facname, name, sep="")
			if (!is.valid.name(fname)){
				errorCondition(recall=numericToFactor,
						message=paste('"', fname, '" ', gettextPMgui("is not a valid name."), sep=""))
				return()
			}
			if (is.element(fname, Variables())) {
				if ("no" == tclvalue(checkReplace(fname))){
					numericToFactor()
					return()
				}
			}
			levelsType <- tclvalue(levelsVariable)
			env <- environment()
			if (((name == variables[1]) || (!sameLevels)) && (levelsType == "names")){
				values <- sort(unique(eval(parse(text=paste(.activeDataSet, "$", name, sep="")),
										envir=.GlobalEnv)))
				nvalues <- length(values)
				if (nvalues > 30) {
					errorCondition(recall=numericToFactor,
							message=sprintf(gettextPMgui("Number of levels (%d) too large."), nvalues))
					return()
				}
				initializeDialog(subdialog,
						title=paste(gettextPMgui("Level Names for"),
								if(sameLevels && length(variables) > 1) "Factors" else fname))
				names <- rep("", nvalues)
				onOKsub <- function() {
					closeDialog(subdialog)
					for (i in 1:nvalues){
						names[i] <- eval(parse(text=paste("tclvalue(levelName", i, ")", sep="")))
					}
					if (length(unique(names)) != nvalues){
						errorCondition(recall=numericToFactor,
								message=gettextPMgui("Levels names are not unique."))
						return()
					}
					if (any(names == "")){
						errorCondition(recall=numericToFactor,
								message=gettextPMgui("A level name is empty."))
						return()
					}
					assign("labels", paste(paste("'", names, "'", sep=""), collapse=","),
							envir=env)
				}
				subOKCancelHelp()
				tkgrid(labelPMgui(subdialog, text=gettextPMgui("Numeric value")), labelPMgui(subdialog, text=gettextPMgui("Level name")), sticky="w")
				for (i in 1:nvalues){
					valVar <- paste("levelName", i, sep="")
					assign(valVar, tclVar(""))
					assign(paste("entry", i, sep=""), ttkentry(subdialog, width="20",
									textvariable=get(valVar)))
#                        textvariable=eval(parse(text=valVar))))
					tkgrid(labelPMgui(subdialog, text=values[i]), get(paste("entry", i, sep="")), sticky="w")
#                    tkgrid(labelPMgui(subdialog, text=values[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
				}
				tkgrid(subButtonsFrame, sticky="w", columnspan=2)
				dialogSuffix(subdialog, rows=nvalues+2, columns=2, focus=entry1, onOK=onOKsub)
			}
			if (levelsType == "names"){
				if (!exists("labels", mode="character")) return()
				command <- paste("factor(", .activeDataSet, "$", name,
						", labels=c(", labels, "))", sep="")
				result <- justDoIt(paste(.activeDataSet, "$", fname, " <- ", command, sep=""))
				logger(paste(.activeDataSet,"$", fname," <- ", command, sep=""))
				if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet)
				tkfocus(PmetricsWindow())
			}
			else{
				command <- paste("as.factor(", .activeDataSet, "$", name, ")", sep="")
				result <- justDoIt(paste(.activeDataSet, "$", fname, " <- ", command, sep=""))
				logger(paste(.activeDataSet, "$", fname," <- ", command, sep=""))
				if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
				tkfocus(PmetricsWindow())
			}
		}
	}
	OKCancelHelp(helpSubject="factor")
	tkgrid(getFrame(variableBox), levelsFrame, sticky="nw")
	tkgrid(labelPMgui(top,
					text=gettextPMgui("New variable name or prefix for multiple variables:")),
			factorNameField, sticky="w")
	tkgrid(buttonsFrame, sticky="w", columnspan=2)
	tkgrid.configure(numbersButton, sticky="w")
	tkgrid.configure(namesButton, sticky="w")
	dialogSuffix(rows=4, columns=2, preventGrabFocus=TRUE)
}

binVariable <- function () {
# Author: Dan Putler (revision by J. Fox, 2 Feb 05)
	defaults <- list (initial.levels = "specify", initial.bins = "3", initial.varName = NULL, 
			initial.newVar = gettextPMgui("variable"), initial.method = "intervals")
	dialog.values <- getDialog ("binVariable", defaults)
	env <- environment()
	initializeDialog(title = gettextPMgui("Bin a Numeric Variable"))
	variableFrame <- tkframe(top)
	variableBox <- variableListBox(variableFrame, Numeric(), 
			title = gettextPMgui("Variable to bin (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.varName, "numeric"))
	newVariableFrame <- tkframe(variableFrame)
	newVariableName <- tclVar(dialog.values$initial.newVar)
	newVariable <- ttkentry(newVariableFrame, width = "18", textvariable = newVariableName)
	binsFrame <- tkframe(top)
	binsVariable <- tclVar(dialog.values$initial.bins)
	slider <- tkscale(binsFrame, from = 2, to = 20, showvalue = TRUE, 
			variable = binsVariable, resolution = 1, orient = "horizontal")
	optionsFrame <- tkframe(top)
	radioButtons(optionsFrame, name = "levels", buttons = c("specify", 
					"numbers", "ranges"), labels = gettextPMgui(c("Specify names", 
							"Numbers", "Ranges")), title = gettextPMgui("Level Names"),
			initialValue = dialog.values$initial.levels)
	radioButtons(optionsFrame, name = "method", buttons = c("intervals", 
					"proportions", "natural"), labels = gettextPMgui(c("Equal-width bins", 
							"Equal-count bins", "Natural breaks\n(from K-means clustering)")), 
			title = gettextPMgui("Binning Method"), 
			initialValue = dialog.values$initial.method)
	onOK <- function() {
		levels <- tclvalue(levelsVariable)
		bins <- as.numeric(tclvalue(binsVariable))
		varName <- getSelection(variableBox)
		closeDialog()
		if (length(varName) == 0) {
			errorCondition(recall = binVariable, message = gettextPMgui("You must select a variable."))
			return()
		}
		newVar <- tclvalue(newVariableName)
		if (is.element(newVar, Variables())) {
			if ("no" == tclvalue(checkReplace(newVar))) {
				binVariable()
				return()
			}
		}
		if (!is.valid.name(newVar)) {
			errorCondition(message = paste("\"", newVar, "\" ", 
							gettextPMgui("is not a valid name."), sep = ""), 
					recall = binVariable)
			return()
		}
		method <- tclvalue(methodVariable)
		putDialog ("binVariable", list (initial.levels = levels, initial.bins = bins, initial.varName = varName, 
						initial.newVar = newVar, initial.method = method))
		if (levels == "specify") {
			initializeDialog(subdialog, title = gettextPMgui("Bin Names"))
			onOKsub <- function() {
				closeDialog(subdialog)
				level <- character(bins)
				for (i in 1:bins) {
					level[i] <- eval(parse(text = paste("tclvalue(levelName", 
											i, ")", sep = "")))
				}
				if (length(unique(level)) != length(level)) {
					errorCondition(window = subdialog, message = gettextPMgui("Level names must be unique."), 
							recall = onOK)
					return()
				}
				assign("levelNames", level, envir = env)
			}
			subOKCancelHelp()
			tkgrid(labelPMgui(subdialog, text = gettextPMgui("Bin"), 
							fg = "blue"), labelPMgui(subdialog, text = gettextPMgui("Name"), 
							fg = "blue"), sticky = "w")
			for (i in 1:bins) {
				valVar <- paste("levelName", i, sep = "")
				assign(valVar, tclVar(i))
				assign(paste("entry", i, sep = ""), ttkentry(subdialog, 
								width = "20", textvariable = get(valVar)))
				tkgrid(labelPMgui(subdialog, text = as.character(i)), 
						get(paste("entry", i, sep = "")), sticky = "w")
			}
			tkgrid(subButtonsFrame, sticky = "w", columnspan = 2)
			dialogSuffix(subdialog, focus = entry1, rows = bins + 
							1, columns = 2, bindReturn = FALSE)
		}
		labels <- if (levels == "numbers") 
					"FALSE"
				else if (levels == "ranges") 
					"NULL"
				else {
					if (!exists("levelNames")) {
						onCancel()
						binVariable()
						return()
					}
					paste("c('", paste(levelNames, collapse = "','"), 
							"')", sep = "")
				}
		.activeDataSet <- ActiveDataSet()
		command <- paste(.activeDataSet, "$", newVar, " <- ", 
				"bin.var(", .activeDataSet, "$", varName, ", bins=", 
				bins, ", method=", "'", method, "', labels=", labels, 
				")", sep = "")
		logger(command)
		result <- justDoIt(command)
		if (class(result)[1] != "try-error") 
			activeDataSet(.activeDataSet, flushModel = FALSE, 
					flushDialogMemory = FALSE)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "bin.var", reset = "binVariable")
	tkgrid(labelPMgui(newVariableFrame, text = gettextPMgui("New variable name"), 
					fg = "blue"), sticky = "w")
	tkgrid(newVariable, sticky = "w")
	tkgrid(getFrame(variableBox), labelPMgui(variableFrame, text = "    "), 
			newVariableFrame, sticky = "nw")
	tkgrid(variableFrame, sticky = "w")
	tkgrid(labelPMgui(binsFrame, text = gettextPMgui("Number of bins:")), 
			slider, sticky = "s")
	tkgrid(binsFrame, sticky = "w")
	tkgrid(levelsFrame, labelPMgui(optionsFrame, text = "    "), 
			methodFrame, sticky = "nw")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 4, columns = 1)
}

reorderFactor <- function(){
	initializeDialog(title=gettextPMgui("Reorder Factor Levels"))
	variableBox <- variableListBox(top, Factors(), title=gettextPMgui("Factor (pick one)"))
	orderedFrame <- tkframe(top)
	orderedVariable <- tclVar("0")
	orderedCheckBox <- tkcheckbutton(orderedFrame, variable=orderedVariable)
	factorName <- tclVar(gettextPMgui("<same as original>"))
	factorNameField <- ttkentry(top, width="20", textvariable=factorName)
	onOK <- function(){
		variable <- getSelection(variableBox)
		closeDialog()
		if (length(variable) == 0) {
			errorCondition(recall=reorderFactor, message=gettextPMgui("You must select a variable."))
			return()
		}
		name <- trim.blanks(tclvalue(factorName))
		if (name == gettextPMgui("<same as original>")) name <- variable
		if (!is.valid.name(name)){
			errorCondition(recall=reorderFactor,
					message=paste('"', name, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(name, Variables())) {
			if ("no" == tclvalue(checkReplace(name))){
				reorderFactor()
				return()
			}
		}
		.activeDataSet <- ActiveDataSet()
		old.levels <- eval(parse(text=paste("levels(", .activeDataSet, "$", variable, ")",
								sep="")), envir=.GlobalEnv)
		nvalues <- length(old.levels)
		ordered <- tclvalue(orderedVariable)
		if (nvalues > 30) {
			errorCondition(recall=reorderFactor,
					message=sprintf(gettextPMgui("Number of levels (%d) too large."), nvalues))
			return()
		}
		initializeDialog(subdialog, title=gettextPMgui("Reorder Levels"))
		order <- 1:nvalues
		onOKsub <- function() {
			closeDialog(subdialog)
			opt <- options(warn=-1)
			for (i in 1:nvalues){
				order[i] <- as.numeric(eval(parse(text=paste("tclvalue(levelOrder", i, ")", sep=""))))
			}
			options(opt)
			if (any(sort(order) != 1:nvalues) || any(is.na(order))){
				errorCondition(recall=reorderFactor,
						message=paste(gettextPMgui("Order of levels must include all integers from 1 to "), nvalues, sep=""))
				return()
			}
			levels <- old.levels[order(order)]
			ordered <- if (ordered == "1") ", ordered=TRUE" else ""
			command <- paste("factor(", .activeDataSet, "$", variable,
					", levels=c(", paste(paste("'", levels, "'", sep=""), collapse=","), ")",
					ordered, ")", sep="")
			result <- justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
			logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
			if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
		}
		subOKCancelHelp()
		tkgrid(labelPMgui(subdialog, text=gettextPMgui("Old Levels"), fg="blue"),
				labelPMgui(subdialog, text=gettextPMgui("New order"), fg="blue"), sticky="w")
		for (i in 1:nvalues){
			valVar <- paste("levelOrder", i, sep="")
			assign(valVar, tclVar(i))
			assign(paste("entry", i, sep=""), ttkentry(subdialog, width="2",
							textvariable=get(valVar)))
			tkgrid(labelPMgui(subdialog, text=old.levels[i]), get(paste("entry", i, sep="")), sticky="w")
		}
		tkgrid(subButtonsFrame, sticky="w", columnspan=2)
		dialogSuffix(subdialog, focus=entry1, rows=nvalues+1, columns=2)
	}
	OKCancelHelp(helpSubject="factor")
	tkgrid(getFrame(variableBox), sticky="nw")
	tkgrid(labelPMgui(top, text=gettextPMgui("Name for factor")), sticky="w")
	tkgrid(factorNameField, sticky="w")
	tkgrid(labelPMgui(orderedFrame, text=gettextPMgui("Make ordered factor")), orderedCheckBox, sticky="w")
	tkgrid(orderedFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=5, columns=1, preventGrabFocus=TRUE)
}

standardize <- function(X){
	initializeDialog(title=gettextPMgui("Standardize Variables"))
	xBox <- variableListBox(top, Numeric(), title=gettextPMgui("Variables (pick one or more)"),
			selectmode="multiple")
	onOK <- function(){
		x <- getSelection(xBox)
		closeDialog()
		if (length(x) == 0) {
			errorCondition(recall=standardize, message=gettextPMgui("You must select one or more variables."))
			return()
		}
		xx <- paste('"', x, '"', sep="")
		.activeDataSet <- ActiveDataSet()
		command <- paste("scale(", .activeDataSet, "[,c(", paste(xx, collapse=","),
				")])", sep="")
		result <- justDoIt(command)
		assign(".Z", result, envir=.GlobalEnv)
		logger(paste(".Z <- ", command, sep=""))
		for (i in 1:length(x)){
			Z <- paste("Z.", x[i], sep="")
			if (is.element(Z, Variables())) {
				if ("no" == tclvalue(checkReplace(Z))){
					if (GrabFocus()) tkgrab.release(top)
					tkdestroy(top)
					next
				}
			}
			justDoIt(paste(.activeDataSet, "$", Z, " <- .Z[,", i, "]", sep=""))
			logger(paste(.activeDataSet, "$", Z, " <- .Z[,", i, "]", sep=""))
		}
		remove(.Z, envir=.GlobalEnv)
		logger("remove(.Z)")
		if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="scale")
	tkgrid(getFrame(xBox), sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=2, columns=1)
}

helpDataSet <- function(){
	.activeDataSet <- ActiveDataSet()
	if (as.numeric(R.Version()$major) >= 2) doItAndPrint(paste('help("', .activeDataSet, '")', sep=""))
	else {
		justDoIt(paste("help('", .activeDataSet, "')", sep=""))
		logger(paste('help("', .activeDataSet, '")', sep=""))
	}
	NULL
}

variablesDataSet <- function(){
	doItAndPrint(paste("names(", ActiveDataSet(), ")", sep=""))
}

exportDataSet <- function() {
	dsname <- activeDataSet()
	initializeDialog(title=gettextPMgui("Export Active Data Set"))
	checkBoxes(frame="optionsFrame", boxes=c("colnames", "rownames", "quotes"),
			initialValues=rep(1,3), labels=gettextPMgui(c("Write variable names:", "Write row names:", "Quotes around character values:")))
	missingVariable <- tclVar("NA")
	missingEntry <- ttkentry(optionsFrame, width="8", textvariable=missingVariable)
	radioButtons(name="delimiter", buttons=c("spaces", "tabs", "commas"), labels=gettextPMgui(c("Spaces", "Tabs", "Commas")),
			title=gettextPMgui("Field Separator"))
	otherButton <- ttkradiobutton(delimiterFrame, variable=delimiterVariable, value="other")
	otherVariable <- tclVar("")
	otherEntry <- ttkentry(delimiterFrame, width="4", textvariable=otherVariable)
	onOK <- function(){
		closeDialog()
		col <- tclvalue(colnamesVariable) == 1
		row <- tclvalue(rownamesVariable) == 1
		quote <- tclvalue(quotesVariable) == 1
		delim <- tclvalue(delimiterVariable)
		missing <- tclvalue(missingVariable)
		sep <- if (delim == "tabs") "\\t"
				else if (delim == "spaces") " "
				else if (delim == "commas") ","
				else trim.blanks(tclvalue(otherVariable))
		saveFile <- tclvalue(tkgetSaveFile(filetypes=gettextPMgui('{"All Files" {"*"}} {"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}}'),
						defaultextension="txt",
						initialfile=paste(dsname, ".txt", sep=""),
						parent=PmetricsWindow()))
		if (saveFile == "") {
			tkfocus(PmetricsWindow())
			return()
		}
		command <- paste("write.table(", dsname, ', "', saveFile, '", sep="', sep,
				'", col.names=', col, ", row.names=", row, ", quote=", quote,
				', na="', missing, '")', sep="")
		justDoIt(command)
		logger(command)
		Message(paste(gettextPMgui("Active dataset exported to file"), saveFile), type="note")
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="write.table")
	tkgrid(labelPMgui(optionsFrame, text=gettextPMgui("Missing values:")), missingEntry, sticky="w")
	tkgrid(optionsFrame, sticky="w")
	tkgrid(labelPMgui(delimiterFrame, text=gettextPMgui("Other")), otherButton,
			labelPMgui(delimiterFrame, text=gettextPMgui("  Specify:")), otherEntry, sticky="w")
	tkgrid(delimiterFrame, stick="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=3, columns=1)
}

filterNA <- function(){
	dataSet <- activeDataSet()
	initializeDialog(title=gettextPMgui("Remove Missing Data"))
	allVariablesFrame <- tkframe(top)
	allVariables <- tclVar("1")
	allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, variable=allVariables)
	variablesBox <- variableListBox(top, Variables(), selectmode="multiple", initialSelection=NULL,
			title=gettextPMgui("Variables (select one or more)"))
	newDataSetName <- tclVar(gettextPMgui("<same as active data set>"))
	dataSetNameFrame <- tkframe(top)
	dataSetNameEntry <- ttkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
	onOK <- function(){
		x <- getSelection(variablesBox)
		closeDialog()
		newName <- trim.blanks(tclvalue(newDataSetName))
		.activeDataSet <- ActiveDataSet()
		if (newName == gettextPMgui("<same as active data set>")) newName <- .activeDataSet
		if (!is.valid.name(newName)){
			errorCondition(recall=filterNA,
					message=paste('"', newName, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(newName, listDataSets())) {
			if ("no" == tclvalue(checkReplace(newName, gettextPMgui("Data set")))){
				filterNA()
				return()
			}
		}
		if (tclvalue(allVariables) == "1"){
			command <- paste(newName, " <- na.omit(", .activeDataSet, ")", sep="")
			logger(command)
			result <- justDoIt(command)
			if (class(result)[1] !=  "try-error") activeDataSet(newName)
			tkfocus(PmetricsWindow())
		}
		else {
			if (length(x) == 0) {
				errorCondition(recall=filterNA, message=gettextPMgui("No variables were selected."))
				return()
			}
			x <- paste('"', x, '"', sep="")
			command <- paste(newName, " <- na.omit(", .activeDataSet, "[,c(", paste(x, collapse=","),
					')])', sep="")
			logger(command)
			result <- justDoIt(command)
			if (class(result)[1] !=  "try-error") activeDataSet(newName)
			tkfocus(PmetricsWindow())
		}
	}
	OKCancelHelp(helpSubject="na.omit")
	tkgrid(labelPMgui(allVariablesFrame, text=gettextPMgui("Include all variables")),
			allVariablesCheckBox, sticky="w")
	tkgrid(allVariablesFrame, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("   OR"), fg="red"), sticky="w")
	tkgrid(getFrame(variablesBox), sticky="nw")
	tkgrid(labelPMgui(dataSetNameFrame, text=gettextPMgui("Name for new data set")), sticky="w")
	tkgrid(dataSetNameEntry, sticky="w")
	tkgrid(dataSetNameFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=4, columns=1)
}

subsetDataSet <- function(){
	dataSet <- activeDataSet()
	initializeDialog(title=gettextPMgui("Subset Data Set"))
	allVariablesFrame <- tkframe(top)
	allVariables <- tclVar("1")
	allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, variable=allVariables)
	variablesBox <- variableListBox(top, Variables(), selectmode="multiple",
			initialSelection=NULL, title=gettextPMgui("Variables (select one or more)"))
	subsetVariable <- tclVar(gettextPMgui("<all cases>"))
	subsetFrame <- tkframe(top)
	subsetEntry <- ttkentry(subsetFrame, width="20", textvariable=subsetVariable)
	subsetScroll <- ttkscrollbar(subsetFrame, orient="horizontal",
			command=function(...) tkxview(subsetEntry, ...))
	tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
	newDataSetName <- tclVar(gettextPMgui("<same as active data set>"))
	dataSetNameFrame <- tkframe(top)
	dataSetNameEntry <- ttkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
	onOK <- function(){
		newName <- trim.blanks(tclvalue(newDataSetName))
		if (newName == gettextPMgui("<same as active data set>")) newName <- ActiveDataSet()
		if (!is.valid.name(newName)){
			errorCondition(recall=subsetDataSet,
					message=paste('"', newName, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(newName, listDataSets())) {
			if ("no" == tclvalue(checkReplace(newName, type=gettextPMgui("Data set")))){
				closeDialog()
				subsetDataSet()
				return()
			}
		}
		selectVars <- if (tclvalue(allVariables) == "1") ""
				else {
					x <- getSelection(variablesBox)
					if (0 > length(x)) {
						errorCondition(recall=subsetDataSet,
								message=gettextPMgui("No variables were selected."))
						return()
					}
					paste(", select=c(", paste(x, collapse=","), ")", sep="")
				}
		closeDialog()
		cases <- tclvalue(subsetVariable)
		selectCases <- if (cases == gettextPMgui("<all cases>")) ""
				else paste(", subset=", cases, sep="")
		if (selectVars == "" && selectCases ==""){
			errorCondition(recall=subsetDataSet,
					message=gettextPMgui("New data set same as active data set."))
			return()
		}
		command <- paste(newName, " <- subset(", ActiveDataSet(), selectCases, selectVars, ")",
				sep="")
		logger(command)
		result <- justDoIt(command)
		if (class(result)[1] !=  "try-error") activeDataSet(newName)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="subset")
	tkgrid(labelPMgui(allVariablesFrame, text=gettextPMgui("Include all variables")),
			allVariablesCheckBox, sticky="w")
	tkgrid(allVariablesFrame, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("   OR"), fg="red"), sticky="w")
	tkgrid(getFrame(variablesBox), sticky="nw")
	tkgrid(labelPMgui(subsetFrame, text=gettextPMgui("Subset expression")), sticky="w")
	tkgrid(subsetEntry, sticky="w")
	tkgrid(subsetScroll, sticky="ew")
	tkgrid(subsetFrame, sticky="w")
	tkgrid(labelPMgui(dataSetNameFrame, text=gettextPMgui("Name for new data set")), sticky="w")
	tkgrid(dataSetNameEntry, sticky="w")
	tkgrid(dataSetNameFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=6, columns=1)
}

setCaseNames <- function(){
	dataSet <- activeDataSet()
	initializeDialog(title=gettextPMgui("Set Case Names"))
	variablesBox <- variableListBox(top, Variables(), title=gettextPMgui("Select variable containing row names"),
			initialSelection=NULL)
	onOK <- function(){
		variable <- getSelection(variablesBox)
		closeDialog()
		if (length(variable) == 0) {
			errorCondition(recall=setCaseNames, message=gettextPMgui("You must select a variable."))
			return()
		}
		var <- eval(parse(text=paste(dataSet, "$", variable, sep="")), envir=.GlobalEnv)
		if (length(var) != length(unique(var))){
			errorCondition(recall=setCaseNames, message=gettextPMgui("Case names must be unique."))
			return()
		}
		command <- paste("row.names(", dataSet, ") <- as.character(", dataSet, "$", variable, ")", sep="")
		result <- justDoIt(command)
		logger(command)
		eval(parse(text=paste(dataSet, "$", variable, "<- NULL", sep="")), envir=.GlobalEnv)
		logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
		if (class(result)[1] !=  "try-error") activeDataSet(dataSet, flushModel=FALSE, flushDialogMemory=FALSE)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="row.names")
	tkgrid(getFrame(variablesBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=3, columns=1)
}

renameVariables <- function(){
	initializeDialog(title=gettextPMgui("Rename Variables"))
	variableBox <- variableListBox(top, Variables(), title=gettextPMgui("Variables (pick one or more)"),
			selectmode="multiple", initialSelection=NULL)
	onOK <- function(){
		variables <- getSelection(variableBox)
		closeDialog()
		nvariables <- length(variables)
		if (nvariables < 1) {
			errorCondition(recall=renameVariables, message=gettextPMgui("No variables selected."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		unordered.names <- names(get(.activeDataSet))
		which.variables <- match(variables, unordered.names)
		initializeDialog(subdialog, title=gettextPMgui("Variable Names"))
		newnames <- rep("", nvariables)
		onOKsub <- function() {
			closeDialog(subdialog)
			for (i in 1:nvariables){
				newnames[i] <- eval(parse(text=paste("tclvalue(newName", i, ")", sep="")))
			}
			if (any(newnames == "")){
				errorCondition(recall=renameVariables, message=gettextPMgui("A variable name is empty."))
				return()
			}
			test.names <- newnames == make.names(newnames)
			if (!all(test.names)){
				errorCondition(recall=renameVariables,
						message=paste(gettextPMgui("The following variable names are not valid:\n"),
								paste(newnames[!test.names], collapse=", ")))
				return()
			}
			all.names <- names(get(.activeDataSet))
#            all.names <- eval(parse(text=paste("names(", .activeDataSet, ")")))
			all.names[which.variables] <- newnames
			if (length(unique(all.names)) != length(all.names)){
				errorCondition(recall=renameVariables, message=gettextPMgui("Variable names are not unique"))
				return()
			}
			command <- paste("names(", .activeDataSet, ")[c(", paste(which.variables, collapse=","),
					")] <- c(", paste('"', newnames, '"', collapse=",", sep=""), ")", sep="")
			result <- justDoIt(command)
			logger(command)
			if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
			tkfocus(PmetricsWindow())
		}
		subOKCancelHelp()
		tkgrid(labelPMgui(subdialog, text=gettextPMgui("Old Name"), fg="blue"),
				labelPMgui(subdialog, text=gettextPMgui("New name"), fg="blue"), sticky="w")
		for (i in 1:nvariables){
			valVar <- paste("newName", i, sep="")
			assign(valVar, tclVar(""))
			assign(paste("entry", i, sep=""), ttkentry(subdialog, width="20",
							textvariable=get(valVar)))
			tkgrid(labelPMgui(subdialog, text=variables[i]), get(paste("entry", i, sep="")), sticky="w")
		}
		tkgrid(subButtonsFrame, sticky="w", columnspan=2)
		dialogSuffix(subdialog, rows=nvariables+2, columns=2, focus=entry1, onOK=onOKsub)
	}
	OKCancelHelp(helpSubject="names")
	tkgrid(getFrame(variableBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=2, columns=1)
}

setContrasts <- function(){
	initializeDialog(title=gettextPMgui("Set Contrasts for Factor"))
	variableBox <- variableListBox(top, Factors(), title=gettextPMgui("Factor (pick one)"))
	radioButtons(name="contrasts", buttons=c("treatment", "sum", "helmert", "poly", "specify"),
			values=c("contr.Treatment", "contr.Sum", "contr.helmert", "contr.poly", "specify"),
			labels=gettextPMgui(c("Treatment (dummy) contrasts", "Sum (deviation) contrasts", "Helmert contrasts",
							"Polynomial contrasts", "Other (specify)")), title=gettextPMgui("Contrasts"))
	onOK <- function(){
		variable <- getSelection(variableBox)
		closeDialog()
		if (length(variable) == 0) {
			errorCondition(recall=setContrasts, message=gettextPMgui("You must select a variable."))
			return()
		}
		contrasts <- tclvalue(contrastsVariable)
		if (contrasts != "specify"){
			command <- paste("contrasts(", ActiveDataSet(), "$", variable, ') <- "', contrasts, '"', sep="")
			result <- justDoIt(command)
			logger(command)
			if (class(result)[1] !=  "try-error") activeDataSet(ActiveDataSet())
			tkfocus(PmetricsWindow())
		}
		else{
			initializeDialog(subdialog, title=gettextPMgui("Specify Contrasts"))
			tkgrid(labelPMgui(subdialog, text=gettextPMgui("Enter Contrast Coefficients"), fg="blue"), sticky="w")
			env <- environment()
			tableFrame <- tkframe(subdialog)
			row.names <- eval(parse(text=paste("levels(", ActiveDataSet(), "$", variable, ")")))
			row.names <- substring(paste(abbreviate(row.names, 12), "            "), 1, 12)
			nrows <- length(row.names)
			ncols <- nrows - 1
			make.col.names <- paste("labelPMgui(tableFrame, text='", gettextPMgui("Contrast Name:"), "')", sep="")
			for (j in 1:ncols) {
				varname <- paste(".col.", j, sep="")
				assign(varname, tclVar(paste(".", j, sep="")), envir=env)
				make.col.names <- paste(make.col.names, ", ",
						"ttkentry(tableFrame, width='12', textvariable=", varname, ")", sep="")
			}
			eval(parse(text=paste("tkgrid(", make.col.names, ", sticky='w')", sep="")), envir=env)
			for (i in 1:nrows){
				make.row <- paste("labelPMgui(tableFrame, text='", row.names[i], "')")
				for (j in 1:ncols){
					varname <- paste(".tab.", i, ".", j, sep="")
					assign(varname, tclVar("0"), envir=env)
					make.row <- paste(make.row, ", ", "ttkentry(tableFrame, width='5', textvariable=",
							varname, ")", sep="")
				}
				eval(parse(text=paste("tkgrid(", make.row, ", sticky='w')", sep="")), envir=env)
			}
			tkgrid(tableFrame, sticky="w")
			onOKsub <- function(){
				closeDialog(subdialog)
				cell <- 0
				values <- rep(NA, nrows*ncols)
				for (j in 1:ncols){
					for (i in 1:nrows){
						cell <- cell + 1
						varname <- paste(".tab.", i, ".", j, sep="")
						values[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
					}
				}
				values <- na.omit(values)
				if (length(values) != nrows*ncols){
					errorCondition(subdialog, recall=setContrasts,
							message=sprintf(gettextPMgui(
											"Number of valid entries in contrast matrix(%d)\nnot equal to number of levels (%d) * number of contrasts (%d)."), length(values), nrows, ncols))
					return()
				}
				if (qr(matrix(values, nrows, ncols))$rank < ncols) {
					errorCondition(subdialog, recall=setContrasts, message=gettextPMgui("Contrast matrix is not of full column rank"))
					return()
				}
				contrast.names <- rep("", ncols)
				for (j in 1:ncols){
					varname <- paste(".col.", j, sep="")
					contrast.names[j] <- eval(parse(text=paste("tclvalue(", varname,")", sep="")))
				}
				if (length(unique(contrast.names)) < ncols) {
					errorCondition(subdialog, recall=setContrasts, message=gettextPMgui("Contrast names must be unique"))
					return()
				}
				command <- paste("matrix(c(", paste(values, collapse=","), "), ", nrows, ", ", ncols,
						")", sep="")
				assign(".Contrasts", justDoIt(command), envir=.GlobalEnv)
				logger(paste(".Contrasts <- ", command, sep=""))
				command <- paste("colnames(.Contrasts) <- c(",
						paste("'", contrast.names, "'", sep="", collapse=", "), ")", sep="")
				justDoIt(command)
				logger(command)
				command <- paste("contrasts(", ActiveDataSet(), "$", variable, ") <- .Contrasts", sep="")
				result <- justDoIt(command)
				logger(command)
				justDoIt("remove(.Contrasts, envir=.GlobalEnv)")
				logger("remove(.Contrasts)")
				if (class(result)[1] !=  "try-error") activeDataSet(ActiveDataSet(), flushModel=FALSE, flushDialogMemory=FALSE)
				tkfocus(PmetricsWindow())
			}
			subOKCancelHelp(helpSubject="contrasts")
			tkgrid(tableFrame, sticky="w")
			tkgrid(labelPMgui(subdialog, text=""))
			tkgrid(subButtonsFrame, sticky="w")
			dialogSuffix(subdialog, rows=5, columns=1, focus=subdialog)
		}
	}
	OKCancelHelp(helpSubject="contrasts")
	tkgrid(getFrame(variableBox), sticky="nw")
	tkgrid(contrastsFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=4, columns=1)
}

refreshActiveDataSet <- function() activeDataSet(ActiveDataSet())

addObsNumbers <- function(){
	dsname <- ActiveDataSet()
	if (is.element("ObsNumber", listVariables())) {
		if ("no" == tclvalue(checkReplace("ObsNumber", gettextPMgui("Variable")))){
			return()
		}
	}
	nrows <- nrow(get(dsname, envir=.GlobalEnv))
#    nrows <- eval(parse(text=paste("nrow(", dsname, ")", sep="")), envir=.GlobalEnv)
	command <- paste(dsname, "$ObsNumber <- 1:", nrows, sep="")
	logger(command)
	result <- justDoIt(command)
	if (class(result)[1] !=  "try-error") activeDataSet(dsname, flushModel=FALSE, flushDialogMemory=FALSE)
}

Stack <- function(){
	initializeDialog(title=gettextPMgui("Stack Variables"))
	variableBox <- variableListBox(top, Numeric(), selectmode="multiple",
			title=gettextPMgui("Variables (pick two or more)"))
	factorName <- tclVar(gettextPMgui("factor"))
	factorNameField <- ttkentry(top, width="20", textvariable=factorName)
	variableName <- tclVar(gettextPMgui("variable"))
	variableNameField <- ttkentry(top, width="20", textvariable=variableName)
	datasetName <- tclVar(gettextPMgui("StackedData"))
	datasetNameField <- ttkentry(top, width="20", textvariable=datasetName)
	onOK <- function(){
		variables <- getSelection(variableBox)
		facname <- tclvalue(factorName)
		varname <- tclvalue(variableName)
		dsname <- tclvalue(datasetName)
		closeDialog()
		if (length(variables) < 2) {
			errorCondition(recall=Stack,
					message=gettextPMgui("You must select at least two variables."))
			return()
		}
		if (!is.valid.name(facname)){
			errorCondition(recall=Stack,
					message=paste('"', facname, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (!is.valid.name(varname)){
			errorCondition(recall=Stack,
					message=paste('"', varname, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (!is.valid.name(dsname)){
			errorCondition(recall=Stack,
					message=paste('"', dsname, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsname, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsname, gettextPMgui("Data set")))){
				Stack()
				return()
			}
		}
		command <- paste(dsname, " <- stack(", activeDataSet(), "[, c(",
				paste(paste('"', variables, '"', sep=""), collapse=","), ")])", sep="")
		logger(command)
		result <- justDoIt(command)
		command <- paste("names(", dsname, ') <- c("', varname, '", "', facname, '")',
				sep="")
		logger(command)
		justDoIt(command)
		if (class(result)[1] !=  "try-error") activeDataSet(dsname)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="stack")
	tkgrid(getFrame(variableBox), sticky="nw", columnspan=2)
	tkgrid(labelPMgui(top, text=""))
	tkgrid(labelPMgui(top,
					text=gettextPMgui("Name for stacked data set:")), datasetNameField, sticky="w")
	tkgrid(labelPMgui(top,
					text=gettextPMgui("Name for variable:")), variableNameField, sticky="w")
	tkgrid(labelPMgui(top,
					text=gettextPMgui("Name for factor:")), factorNameField, sticky="w")
	tkgrid(buttonsFrame, sticky="w", columnspan=2)
	dialogSuffix(rows=5, columns=2, preventGrabFocus=TRUE)
}

loadDataSet <- function() {
	file <- tclvalue(tkgetOpenFile(filetypes=
							gettextPMgui('{"All Files" {"*"}} {"R Data Files" {".RData" ".rda" ".Rda" ".RDA"}}')))
	if (file == "") return()
	command <- paste('load("', file,'")', sep="")
	dsname <- justDoIt(command)
	logger(command)
	if (class(dsname)[1] !=  "try-error") activeDataSet(dsname)
	tkfocus(PmetricsWindow())
}

saveDataSet <- function() {
	file <- tclvalue(tkgetSaveFile(filetypes=
							gettextPMgui('{"All Files" {"*"}} {"R Data Files" {".RData" ".rda" ".Rda" ".RDA"}}'),
					defaultextension=".RData", initialfile=paste(activeDataSet(), ".RData", sep="")))
	if (file == "") return()
	command <- paste('save("', activeDataSet(), '", file="', file, '")', sep="")
	justDoIt(command)
	logger(command)
}

RemoveRows <- function(){
	dataSet <- activeDataSet()
	initializeDialog(title=gettextPMgui("Remove Rows from Active Data Set"))
	removeVariable <- tclVar(gettextPMgui(""))
	removeFrame <- tkframe(top)
	removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
	removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
			command=function(...) tkxview(removeEntry, ...))
	tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
	newDataSetName <- tclVar(gettextPMgui("<same as active data set>"))
	dataSetNameFrame <- tkframe(top)
	dataSetNameEntry <- ttkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
	onOK <- function(){
		newName <- trim.blanks(tclvalue(newDataSetName))
		if (newName == gettextPMgui("<same as active data set>")) newName <- ActiveDataSet()
		if (!is.valid.name(newName)){
			errorCondition(recall=RemoveRows,
					message=paste('"', newName, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(newName, listDataSets())) {
			if ("no" == tclvalue(checkReplace(newName, type=gettextPMgui("Data set")))){
				closeDialog()
				RemoveRows()
				return()
			}
		}
		remove <- tclvalue(removeVariable)
		if (remove==""){
			errorCondition(recall=RemoveRows,
					message="No rows to remove")
			closeDialog()
			return()
		}
		removeRows <- paste("c(", gsub(" ", ",", remove), ")", sep="")
		remove <- try(eval(parse(text=removeRows)), silent=TRUE)
		if (class(remove) == "try-error"){
			errorCondition(recall=RemoveRows,
					message=remove)
			closeDialog()
			return()
		}
		closeDialog()
		removeRows <- if (is.numeric(remove)) paste("-", removeRows, sep="") 
				else paste("!(rownames(", ActiveDataSet(), ") %in% ", removeRows, ")", sep="")
		command <- paste(newName, " <- ", ActiveDataSet(), "[", removeRows, ",]", sep="")
		logger(command)
		result <- justDoIt(command)
		if (class(result)[1] !=  "try-error") activeDataSet(newName)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="[.data.frame")
	tkgrid(labelPMgui(removeFrame, text=gettextPMgui("Indices or quoted names of row(s) to remove"),
					foreground="blue"), sticky="w")
	tkgrid(removeEntry, sticky="w")
	tkgrid(removeScroll, sticky="ew")
	tkgrid(removeFrame, sticky="w")
	tkgrid(labelPMgui(dataSetNameFrame, text=gettextPMgui("Name for new data set")), sticky="w")
	tkgrid(dataSetNameEntry, sticky="w")
	tkgrid(dataSetNameFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=3, columns=1)
}

mergeDataSets <- function(){
	dataSets <- listDataSets()
	.activeDataSet <- ActiveDataSet()
	initializeDialog(title=gettextPMgui("Merge Data Sets"))
	dsname <- tclVar("MergedDataset")
	dsnameFrame <- tkframe(top)
	entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
	dataSet1Box <- variableListBox(top, dataSets, title=gettextPMgui("First Data Set (pick one)"),
			initialSelection=if (is.null(.activeDataSet)) NULL else which(.activeDataSet == dataSets) - 1)
	dataSet2Box <- variableListBox(top, dataSets, title=gettextPMgui("Second Data Set (pick one)"))
	commonVar <- tclVar("0")
	commonFrame <- tkframe(top)
	commonButton <- tkcheckbutton(commonFrame, variable=commonVar)	
	radioButtons(top, "direction", buttons=c("rows", "columns"), 
			labels=gettextPMgui(c("Merge rows", "Merge columns")), title=gettextPMgui("Direction of Merge"))
	onOK <- function(){
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if (dsnameValue == "") {
			errorCondition(recall=mergeDataSets,
					message=gettextPMgui("You must enter the name of a data set."))
			return()
		}
		if (!is.valid.name(dsnameValue)) {
			errorCondition(recall=mergeDataSets,
					message=paste('"', dsnameValue, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
				closeDialog()
				mergeDataSets()
				return()
			}
		}
		name1 <- getSelection(dataSet1Box)
		name2 <- getSelection(dataSet2Box)
		if (length(name1) == 0){
			errorCondition(recall=mergeDataSets,
					message=gettextPMgui("You must select a data set."))
			return()
		}
		if (length(name2) == 0){
			errorCondition(recall=mergeDataSets,
					message=gettextPMgui("You must select a data set."))
			return()
		}
		if (name1 == name2){
			errorCondition(recall=mergeDataSets,
					message=gettextPMgui("You cannot merge a data set with itself."))
			return()
		}
		common <- if (tclvalue(commonVar) == "1") TRUE else FALSE
		direction <- tclvalue(directionVariable)
		if (direction == "rows"){
			command <- paste(dsnameValue, " <- mergeRows(", name1, ", ", name2,
					", common.only=", common, ")", sep="")
			doItAndPrint(command)	
		}
		else {
			command <- paste(dsnameValue, " <- merge(", name1, ", ", name2,
					", all=", !common, ', by="row.names")', sep="")
			doItAndPrint(command)
			command <- paste("rownames(", dsnameValue, ") <- ", dsnameValue, "$Row.names", sep="")
			doItAndPrint(command)
			command <- paste(dsnameValue, "$Row.names <- NULL", sep="")
			doItAndPrint(command)
		}
		activeDataSet(dsnameValue)
		closeDialog()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="mergeRows")
	tkgrid(labelPMgui(dsnameFrame, text=gettextPMgui("Name for merged data set:  ")), entryDsname)
	tkgrid(dsnameFrame, sticky="w", columnspan=2)
	tkgrid(getFrame(dataSet1Box), getFrame(dataSet2Box), sticky="nw")
	tkgrid(labelPMgui(commonFrame, text=gettextPMgui("Merge only common\nrows or columns")), 
			commonButton, sticky="nw")
	tkgrid(directionFrame, commonFrame, sticky="sw")
	tkgrid(buttonsFrame, sticky="w", columnspan=2)
	dialogSuffix(rows=5, columns=2)
}

Aggregate <- function(){
	.activeDataSet <- ActiveDataSet()
	initializeDialog(title=gettextPMgui("Aggregate Observations"))
	dsname <- tclVar("AggregatedData")
	dsnameFrame <- tkframe(top)
	entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
	variablesBox <- variableListBox(top, Variables(), 
			title=gettextPMgui("Variables to aggregate\n(pick one or more)"),
			selectmode="multiple")
	byBox <- variableListBox(top, Factors(), 
			title=gettextPMgui("Aggregate by\n(pick one or more)"),
			selectmode="multiple")
	radioButtons(name="statistic", buttons=c("mean", "sum"), labels=gettextPMgui(c("Mean", "Sum")), 
			title=gettextPMgui("Statistic"))
	otherVariable <- tclVar("")
	otherButton <- ttkradiobutton(statisticFrame, variable=statisticVariable, value="other")
	otherEntry <- ttkentry(statisticFrame, width="20", textvariable=otherVariable)   
	tkgrid(labelPMgui(statisticFrame, text=gettextPMgui("Other (specify)")), otherButton, otherEntry, sticky="w")
	onOK <- function(){
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if (dsnameValue == "") {
			errorCondition(recall=Aggregate,
					message=gettextPMgui("You must enter the name of a data set."))
			return()
		}
		if (!is.valid.name(dsnameValue)) {
			errorCondition(recall=Aggregate,
					message=paste('"', dsnameValue, '" ', gettextPMgui("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextPMgui("Data set")))){
				Aggregate()
				return()
			}
		}
		variables <- getSelection(variablesBox)
		byVariables <- getSelection(byBox)
		if (length(variables) == 0){
			errorCondition(recall=Aggregate,
					message=gettextPMgui("You must select at least one variable to aggregate."))
			return()
		}
		if (length(byVariables) == 0){
			errorCondition(recall=Aggregate,
					message=gettextPMgui("You must select at least one variable to aggregate by."))
			return()
		}
		if (any(byVariables %in% variables)){
			errorCondition(recall=Aggregate,
					message=gettextPMgui("Variables to aggregate and those to aggregate by must be different."))
			return()
		}
		statistic <- tclvalue(statisticVariable)
		if (statistic == "other") statistic <- tclvalue(otherVariable)
		vars <- paste(paste('"', variables, '"', sep=""), collapse=",")
		by <-paste("list(", paste(paste(byVariables, "=", .activeDataSet, "$", byVariables, sep=""), 
						collapse=", "), ")", sep="")
		command <- paste(dsnameValue, " <- aggregate(", .activeDataSet, "[,c(", vars, "), drop=FALSE], by=", by,
				", FUN=", statistic, ")", sep="")
		doItAndPrint(command)
		activeDataSet(dsnameValue)
		closeDialog()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="aggregate")
	tkgrid(labelPMgui(dsnameFrame, text=gettextPMgui("Name for aggregated data set:  ")), entryDsname)
	tkgrid(dsnameFrame, sticky="w", columnspan=2)
	tkgrid(getFrame(variablesBox), getFrame(byBox), sticky="nw")
	tkgrid(statisticFrame, sticky="w", columnspan=2)
	tkgrid(buttonsFrame, sticky="w", columnspan=2)
	dialogSuffix(rows=5, columns=2)
}
