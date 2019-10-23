# Statistics Menu dialogs

# last modified 2011-12-18 by J. Fox

# Tables menu

twoWayTable <- function(){ # dialog memory 2011-06-27 J. Fox
	Library("abind")
	defaults <- list(initial.row=NULL, initial.column=NULL, 
			initial.percents="none", initial.chisq=1, initial.chisqComp=0, initial.expected=0, 
			initial.fisher=0, initial.subset=gettextPMgui("<all valid cases>"))
	dialog.values <- getDialog("twoWayTable", defaults)
	initializeDialog(title=gettextPMgui("Two-Way Table"))
	variablesFrame <- tkframe(top)
	.factors <- Factors()
	rowBox <- variableListBox(variablesFrame, .factors, title=gettextPMgui("Row variable (pick one)"),
			initialSelection=varPosn(dialog.values$initial.row, "factor"))
	columnBox <- variableListBox(variablesFrame, .factors, title=gettextPMgui("Column variable (pick one)"),
			initialSelection=varPosn(dialog.values$initial.column, "factor"))
	subsetBox(subset.expression=dialog.values$initial.subset)
	onOK <- function(){
		row <- getSelection(rowBox)
		column <- getSelection(columnBox)
		percents <- as.character(tclvalue(percentsVariable))
		chisq <- tclvalue(chisqTestVariable)
		chisqComp <- tclvalue(chisqComponentsVariable)
		expected <- tclvalue(expFreqVariable)
		fisher <- tclvalue(fisherTestVariable)
		initial.subset <- subset <- tclvalue(subsetVariable)
		subset <- if (trim.blanks(subset) == gettextPMgui("<all valid cases>")) ""
				else paste(", subset=", subset, sep="")
		putDialog("twoWayTable", list(
						initial.row=row, 
						initial.column=column, 
						initial.percents=percents, initial.chisq=chisq, initial.chisqComp=chisqComp, 
						initial.expected=expected, initial.fisher=fisher, initial.subset=initial.subset
				))
		if (length(row) == 0 || length(column) == 0){
			errorCondition(recall=twoWayTable, message=gettextPMgui("You must select two variables."))
			return()
		}
		if (row == column) {
			errorCondition(recall=twoWayTable, message=gettextPMgui("Row and column variables are the same."))
			return()
		}
		closeDialog()
		command <- paste("xtabs(~", row, "+", column, ", data=", ActiveDataSet(),
				subset, ")", sep="")
		logger(paste(".Table <- ", command, sep=""))
		assign(".Table", justDoIt(command), envir=.GlobalEnv)
		doItAndPrint(".Table")
		if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
		if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
		if (percents == "total") doItAndPrint("totPercents(.Table) # Percentage of Total")
		if (chisq == 1) {
			command <- "chisq.test(.Table, correct=FALSE)"
			logger(paste(".Test <- ", command, sep=""))
			assign(".Test", justDoIt(command), envir=.GlobalEnv)
			doItAndPrint(".Test")
			if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
			warnText <- NULL
			if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
						gettextPMgui("expected frequencies are less than 1"))
			if (0 < (nlt5 <- sum(.Test$expected < 5))) warnText <- paste(warnText, "\n", nlt5,
						gettextPMgui(" expected frequencies are less than 5"), sep="")
			if (!is.null(warnText)) Message(message=warnText,
						type="warning")
			if (chisqComp == 1) {
				command <- "round(.Test$residuals^2, 2) # Chi-square Components"
				doItAndPrint(command)
			}
			logger("remove(.Test)")
			remove(.Test, envir=.GlobalEnv)
		}
		if (fisher == 1) doItAndPrint("fisher.test(.Table)")
		logger("remove(.Table)")
		remove(.Table, envir=.GlobalEnv)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="xtabs", reset="twoWayTable")
	radioButtons(name="percents",
			buttons=c("rowPercents", "columnPercents", "totalPercents", "nonePercents"),
			values=c("row", "column", "total", "none"), initialValue=dialog.values$initial.percents,
			labels=gettextPMgui(c("Row percentages", "Column percentages", "Percentages of total", "No percentages")), title=gettextPMgui("Compute Percentages"))
	checkBoxes(frame="testsFrame", boxes=c("chisqTest", "chisqComponents", "expFreq", "fisherTest"), 
			initialValues=c(dialog.values$initial.chisq, dialog.values$initial.chisqComp, 
					dialog.values$initial.expected, dialog.values$initial.fisher),
			labels=gettextPMgui(c("Chi-square test of independence", "Components of chi-square statistic",
							"Print expected frequencies", "Fisher's exact test")))
	tkgrid(getFrame(rowBox), labelPMgui(variablesFrame, text="    "), getFrame(columnBox), sticky="nw")
	tkgrid(variablesFrame, sticky="w")
	tkgrid(percentsFrame, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Hypothesis Tests"), fg="blue"), sticky="w")
	tkgrid(testsFrame, sticky="w")
	tkgrid(subsetFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=6, columns=1)
}

multiWayTable <- function (){
	Library("abind")
	defaults <- list (initial.row = NULL, initial.column = NULL, initial.control = NULL, 
			initial.percents = "none", initial.subset=gettextPMgui("<all valid cases>"))
	dialog.values <- getDialog ("multiWayTable", defaults)
	initializeDialog(title = gettextPMgui("Multi-Way Table"))
	variablesFrame <- tkframe(top)
	.factors <- Factors()
	rowBox <- variableListBox(variablesFrame, .factors, title = gettextPMgui("Row variable (pick one)"),
			initialSelection = varPosn (dialog.values$initial.row, "factor"))
	columnBox <- variableListBox(variablesFrame, .factors, title = gettextPMgui("Column variable (pick one)"),
			initialSelection = varPosn (dialog.values$initial.column, "factor"))
	controlBox <- variableListBox(variablesFrame, .factors, selectmode = "multiple", 
			title = gettextPMgui("Control variable(s) (pick one or more)"), 
			initialSelection = varPosn (dialog.values$initial.control, "factor"))
	subsetBox(subset.expression = dialog.values$initial.subset)
	onOK <- function() {
		row <- getSelection(rowBox)
		column <- getSelection(columnBox)
		controls <- getSelection(controlBox)
		if (length(row) == 0 || length(column) == 0 || length(controls) == 
				0) {
			errorCondition(recall = multiWayTable, message = gettextPMgui("You must select row, column, and control variables"))
			return()
		}
		if ((row == column) || is.element(row, controls) || is.element(column, 
				controls)) {
			errorCondition(recall = multiWayTable, message = gettextPMgui("Row, column, and control variables must be different."))
			return()
		}
		percents <- as.character(tclvalue(percentsVariable))
		initial.subset <- subset <- tclvalue(subsetVariable)
		subset <- if (trim.blanks(subset) == gettextPMgui("<all valid cases>")) 
					""
				else paste(", subset=", subset, sep = "")
		putDialog ("multiWayTable", list (initial.row = row, initial.column = column, initial.control = controls, initial.percents = percents, initial.subset=initial.subset))
		closeDialog()
		command <- paste("xtabs(~", row, "+", column, "+", paste(controls, 
						collapse = "+"), ", data=", ActiveDataSet(), subset, 
				")", sep = "")
		logger(paste(".Table <- ", command, sep = ""))
		assign(".Table", justDoIt(command), envir = .GlobalEnv)
		doItAndPrint(".Table")
		if (percents == "row") 
			doItAndPrint("rowPercents(.Table) # Row Percentages")
		if (percents == "column") 
			doItAndPrint("colPercents(.Table) # Column Percentages")
		logger("remove(.Table)")
		remove(.Table, envir = .GlobalEnv)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "xtabs", reset = "multiWayTable")
	radioButtons(name = "percents", buttons = c("rowPercents", 
					"columnPercents", "nonePercents"), values = c("row", 
					"column", "none"),  labels = gettextPMgui(c("Row percentages", 
							"Column percentages", "No percentages")), title = gettextPMgui("Compute Percentages"),
			initialValue = dialog.values$initial.percents)
	tkgrid(getFrame(rowBox), labelPMgui(variablesFrame, text = "    "), 
			getFrame(columnBox), labelPMgui(variablesFrame, text = "    "), 
			getFrame(controlBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "w")
	tkgrid(percentsFrame, sticky = "w")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 4, columns = 1)
}

enterTable <- function(){
	Library("abind")
	env <- environment()
	initializeDialog(title=gettextPMgui("Enter Two-Way Table"))
	outerTableFrame <- tkframe(top)
	assign(".tableFrame", tkframe(outerTableFrame), envir=env)
	setUpTable <- function(...){
		tkdestroy(get(".tableFrame", envir=env))
		assign(".tableFrame", tkframe(outerTableFrame), envir=env)
		nrows <- as.numeric(tclvalue(rowsValue))
		ncols <- as.numeric(tclvalue(colsValue))
		make.col.names <- "labelPMgui(.tableFrame, text='')"
		for (j in 1:ncols) {
			col.varname <- paste(".colname.", j, sep="")
			assign(col.varname, if (is.null(initial.table) || j > length(colnames)) tclVar(j) else tclVar(colnames[j]), envir=env)
			make.col.names <- paste(make.col.names, ", ", "ttkentry(.tableFrame, width='5', textvariable=",
					col.varname, ")", sep="")
		}
		eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
		for (i in 1:nrows){
			varname <- paste(".tab.", i, ".1", sep="")
			assign(varname, if (is.null(initial.table) || i > length(rownames)) tclVar("") else tclVar(initial.table[i, 1]) , envir=env)
			row.varname <- paste(".rowname.", i, sep="")
			assign(row.varname, if (is.null(initial.table) || i > length(rownames)) tclVar(i) else tclVar(rownames[i]), envir=env)
			make.row <- paste("ttkentry(.tableFrame, width='5', textvariable=",
					row.varname, ")", sep="")
			make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=",
					varname, ")", sep="")
			for (j in 2:ncols){
				varname <- paste(".tab.", i, ".", j, sep="")
				assign(varname, if (is.null(initial.table) || i > length(rownames) || j > length(colnames)) 
									tclVar("") else tclVar(initial.table[i, j]), envir=env)
				make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=",
						varname, ")", sep="")
			}
			eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
		}
		tkgrid(get(".tableFrame", envir=env), sticky="w")
	}
	initial.table <- getPMgui("savedTable")
	initial.percentages <- if (is.null(initial.table)) "none" else attr(initial.table, "percentages")
	initial.tests <- if (is.null(initial.table)) c("1", "0", "0", "0") else attr(initial.table, "tests")
	if (is.null(initial.table)){
		rowsValue <- tclVar("2")
		colsValue <- tclVar("2")
	}
	else {
		rowsValue <- tclVar(nrow(initial.table))
		colsValue <- tclVar(ncol(initial.table))
		rownames <- rownames(initial.table)
		colnames <- colnames(initial.table)
	}
	rowColFrame <- tkframe(top)
	rowsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=rowsValue,
			resolution=1, orient="horizontal", command=setUpTable)
	rowsShow <- labelPMgui(rowColFrame, textvariable=rowsValue, width=2, justify="right")
	colsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=colsValue,
			resolution=1, orient="horizontal", command=setUpTable)
	colsShow <- labelPMgui(rowColFrame, textvariable=colsValue, width=2, justify="right")
	onOK <- function(){
		nrows <- as.numeric(tclvalue(rowsValue))
		ncols <- as.numeric(tclvalue(colsValue))
		cell <- 0
		counts <- rep(NA, nrows*ncols)
		row.names <- rep("", nrows)
		col.names <- rep("", ncols)
		for (i in 1:nrows) row.names[i] <-
					eval(parse(text=paste("tclvalue(", paste(".rowname.", i, sep=""),")", sep="")))
		for (j in 1:ncols) col.names[j] <-
					eval(parse(text=paste("tclvalue(", paste(".colname.", j, sep=""),")", sep="")))
		for (i in 1:nrows){
			for (j in 1:ncols){
				cell <- cell+1
				varname <- paste(".tab.", i, ".", j, sep="")
				counts[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
			}
		}
		counts <- na.omit(counts)
		if (length(counts) != nrows*ncols){
			errorCondition(recall=enterTable, message=sprintf(gettextPMgui("Number of valid entries (%d)\nnot equal to number of rows (%d) * number of columns (%d)."), length(counts), nrows, ncols))
			return()
		}
		if (length(unique(row.names)) != nrows){
			errorCondition(recall=enterTable, message=gettextPMgui("Row names are not unique."))
			return()
		}
		if (length(unique(col.names)) != ncols){
			errorCondition(recall=enterTable, message=gettextPMgui("Column names are not unique."))
			return()
		}
		percents <- as.character(tclvalue(percentsVariable))
		chisq <- tclvalue(chisqVariable)
		chisqComp <- tclvalue(chisqComponentsVariable)
		expected <- tclvalue(expFreqVariable)
		fisher <- tclvalue(fisherVariable)
		closeDialog()
		command <- paste("matrix(c(", paste(counts, collapse=","), "), ", nrows, ", ", ncols,
				", byrow=TRUE)", sep="")
		assign(".Table", justDoIt(command), envir=.GlobalEnv)
		logger(paste(".Table <- ", command, sep=""))
		command <- paste("c(",paste(paste("'", row.names, "'", sep=""), collapse=", "), ")", sep="")
		justDoIt(paste("rownames(.Table) <- ", command, sep=""))
		logger(paste("rownames(.Table) <- ", command, sep=""))
		command <- paste("c(",paste(paste("'", col.names, "'", sep=""), collapse=", "), ")", sep="")
		justDoIt(paste("colnames(.Table) <- ", command, sep=""))
		logger(paste("colnames(.Table) <- ", command, sep=""))
		doItAndPrint(".Table  # Counts")
		if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
		if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
		if (percents == "total") doItAndPrint("totPercents(.Table) # Percentage of Total")
		if (chisq == 1) {
			command <- "chisq.test(.Table, correct=FALSE)"
			logger(paste(".Test <- ", command, sep=""))
			assign(".Test", justDoIt(command), envir=.GlobalEnv)
			doItAndPrint(".Test")
			if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
			warnText <- NULL
			if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
						gettextPMgui("expected frequencies are less than 1"))
			if (0 < (nlt5 <- sum(.Test$expected < 5))) warnText <- paste(warnText, "\n", nlt5,
						gettextPMgui(" expected frequencies are less than 5"), sep="")
			if (!is.null(warnText)) Message(message=warnText,
						type="warning")
			if (chisqComp == 1) {
				command <- "round(.Test$residuals^2, 2) # Chi-square Components"
				doItAndPrint(command)
			}
			logger("remove(.Test)")
			remove(.Test, envir=.GlobalEnv)
		}
		if (fisher == 1) doItAndPrint("fisher.test(.Table)")
		if (getPMgui("retain.selections")){
			attr(.Table, "percentages") <- percents
			attr(.Table, "tests") <- c(chisq, chisqComp, expected, fisher)
			putPMgui("savedTable", .Table)
		}
		logger("remove(.Table)")
		remove(.Table, envir=.GlobalEnv)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject="chisq.test", reset="resetEnterTable")
	radioButtons(name="percents", buttons=c("rowPercents", "columnPercents", "totalPercents", "nonePercents"), values=c("row", "column", "total", "none"),
			initialValue=initial.percentages, labels=gettextPMgui(c("Row percentages", "Column percentages",  "Percentages of total", "No percentages")), title=gettextPMgui("Compute Percentages"))
	checkBoxes(frame="testsFrame", boxes=c("chisq", "chisqComponents", "expFreq", "fisher"), initialValues=initial.tests,
			labels=gettextPMgui(c("Chi-square test of independence", "Components of chi-square statistic",
							"Print expected frequencies", "Fisher's exact test")))
	tkgrid(labelPMgui(rowColFrame, text=gettextPMgui("Number of Rows:")), rowsSlider, rowsShow, sticky="w")
	tkgrid(labelPMgui(rowColFrame, text=gettextPMgui("Number of Columns:")), colsSlider, colsShow, sticky="w")
	tkgrid(rowColFrame, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Enter counts:"), fg="blue"), sticky="w")
	tkgrid(outerTableFrame, sticky="w")
	tkgrid(percentsFrame, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Hypothesis Tests"), fg="blue"), sticky="w")
	tkgrid(testsFrame, sticky="w")
	tkgrid(buttonsFrame, columnspan=2, sticky="w")
	dialogSuffix(rows=7, columns=2)
}

resetEnterTable <- function(){
	putPMgui("savedTable", NULL)
	enterTable()
}
