# Statistics Menu dialogs

# last modified 2011-12-27 by J. Fox

# Variances menu

twoVariancesFTest <- function () {
	defaults <- list(initial.groups = NULL, initial.response = NULL, initial.alternative = "two.sided", 
			initial.confidenceLevel = ".95", initial.label=NULL)
	dialog.values <- getDialog("twoVariancesFTest", defaults)
	initializeDialog(title = gettextPMgui("Two Variances F-Test"))
	variablesFrame <- tkframe(top)
	groupBox <- variableListBox(variablesFrame, TwoLevelFactors(), 
			title = gettextPMgui("Groups (pick one)"), 
			initialSelection = varPosn(dialog.values$initial.groups, "twoLevelFactor"))
	responseBox <- variableListBox(variablesFrame, Numeric(), 
			title = gettextPMgui("Response Variable (pick one)"), 
			initialSelection = varPosn(dialog.values$initial.response, "numeric"))
	onOK <- function() {
		group <- getSelection(groupBox)
		if (length(group) == 0) {
			errorCondition(recall = twoVariancesFTest, message = gettextPMgui("You must select a groups variable."))
			return()
		}
		response <- getSelection(responseBox)
		if (length(response) == 0) {
			errorCondition(recall = twoVariancesFTest, message = gettextPMgui("You must select a response variable."))
			return()
		}
		alternative <- as.character(tclvalue(alternativeVariable))
		level <- tclvalue(confidenceLevel)
		closeDialog()
		putDialog("twoVariancesFTest", list(initial.groups = group, initial.response = response, 
						initial.alternative = alternative, initial.confidenceLevel = level,
						initial.label=.groupsLabel))
		.activeDataSet <- ActiveDataSet()
		doItAndPrint(paste("tapply(", .activeDataSet, "$", response, 
						", ", .activeDataSet, "$", group, ",  var, na.rm=TRUE)", 
						sep = ""))
		doItAndPrint(paste("var.test(", response, " ~ ", group, 
						", alternative='", alternative, "', conf.level=", 
						level, ", data=", .activeDataSet, ")", sep = ""))
		tkfocus(PmetricsWindow())
		tkdestroy(top)
	}
	OKCancelHelp(helpSubject = "var.test", reset = "twoVariancesFTest")
	radioButtons(name = "alternative", buttons = c("twosided", 
					"less", "greater"), values = c("two.sided", "less", "greater"), 
			labels = gettextPMgui(c("Two-sided", "Difference < 0", 
							"Difference > 0")), title = gettextPMgui("Alternative Hypothesis"), 
			initialValue = dialog.values$initial.alternative,)
	confidenceFrame <- tkframe(top)
	confidenceLevel <- tclVar(dialog.values$initial.confidenceLevel)
	confidenceField <- ttkentry(confidenceFrame, width = "6", 
			textvariable = confidenceLevel)
	tkgrid(getFrame(groupBox), labelPMgui(variablesFrame, text = "    "), 
			getFrame(responseBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "w")
	groupsLabel(groupsBox = groupBox, initialText=dialog.values$initial.label)
	tkgrid(labelPMgui(confidenceFrame, text = gettextPMgui("Confidence Level:  "), 
					fg = "blue"), confidenceField, sticky = "w")
	tkgrid(alternativeFrame, sticky = "w")
	tkgrid(confidenceFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 5, columns = 1)
}

BartlettTest <- function () {
	defaults <- list(initial.group = NULL, initial.response = NULL)
	dialog.values <- getDialog("BartlettTest", defaults)
	initializeDialog(title = gettextPMgui("Bartlett's Test"))
	variableFrame <- tkframe(top)
	groupBox <- variableListBox(variableFrame, Factors(), selectmode = "multiple", 
			title = gettextPMgui("Factors (pick one or more)"),
			initialSelection = varPosn(dialog.values$initial.group, "factor"))
	responseBox <- variableListBox(variableFrame, Numeric(),  
			initialSelection = varPosn(dialog.values$initial.response, "numeric"),
			title = gettextPMgui("Response Variable (pick one)"))
	onOK <- function() {
		group <- getSelection(groupBox)
		if (length(group) == 0) {
			errorCondition(recall = BartlettTest, message = gettextPMgui("You must select a groups variable."))
			return()
		}
		response <- getSelection(responseBox)
		if (length(response) == 0) {
			errorCondition(recall = BartlettTest, message = gettextPMgui("You must select a response variable."))
			return()
		}
		closeDialog()
		putDialog("BartlettTest", list(initial.group = group, initial.response = response))
		.activeDataSet <- ActiveDataSet()
		if (length(group) == 1){
			doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", 
									response, sep = ""), ", ", paste(.activeDataSet, 
									"$", group, sep = ""), ", var, na.rm=TRUE)", sep = ""))
			doItAndPrint(paste("bartlett.test(", response, " ~ ", 
							group, ", data=", .activeDataSet, ")", sep = ""))
		}
		else{
			command <- paste("with(", .activeDataSet, ", tapply(", response, 
					", list(", paste(paste(group, "=", group, sep=""), collapse=", "), "), var, na.rm=TRUE))", sep="")
			doItAndPrint(command)
			doItAndPrint(paste("bartlett.test(", response, " ~ interaction(", 
							paste(group, collapse=", "), "), data=", .activeDataSet, ")", sep = ""))
		}
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "bartlett.test", reset = "BartlettTest")
	tkgrid(getFrame(groupBox), labelPMgui(variableFrame, text = "    "), 
			getFrame(responseBox), sticky = "nw")
	tkgrid(variableFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

LeveneTest <- function () {
	require("car")
	defaults <- list(initial.group = NULL, initial.response = NULL, initial.center = "median")
	dialog.values <- getDialog("LeveneTest", defaults)
	initializeDialog(title = gettextPMgui("Levene's Test"))
	variableFrame <- tkframe(top)
	groupBox <- variableListBox(variableFrame, Factors(), selectmode = "multiple", 
			title = gettextPMgui("Factors (pick one or more)"),
			initialSelection = varPosn(dialog.values$initial.group, "factor"))
	responseBox <- variableListBox(variableFrame, Numeric(), 
			title = gettextPMgui("Response Variable (pick one)"),
			initialSelection = varPosn(dialog.values$initial.response, "numeric"))
	radioButtons(name = "center", buttons = c("median", "mean"), 
			labels = c(gettextPMgui("median"), gettextPMgui("mean")), 
			title = gettextPMgui("Center"), initialValue = dialog.values$initial.center)
	onOK <- function() {
		group <- getSelection(groupBox)
		center <- as.character(tclvalue(centerVariable))
		if (length(group) == 0) {
			errorCondition(recall = LeveneTest, message = gettextPMgui("You must select a groups variable."))
			return()
		}
		response <- getSelection(responseBox)
		if (length(response) == 0) {
			errorCondition(recall = LeveneTest, message = gettextPMgui("You must select a response variable."))
			return()
		}
		closeDialog()
		putDialog("LeveneTest", list(initial.group = group, initial.response = response, 
						initial.center = center))
		.activeDataSet <- ActiveDataSet()
		if (length(group) == 1){
			doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", 
									response, sep = ""), ", ", paste(.activeDataSet, 
									"$", group, sep = ""), ", var, na.rm=TRUE)", sep = ""))
			doItAndPrint(paste("leveneTest(", paste(.activeDataSet, 
									"$", response, sep = ""), ", ", paste(.activeDataSet, 
									"$", group, sep = ""), ", center=", center, ")", 
							sep = ""))
		}
		else{
			command <- paste("with(", .activeDataSet, ", tapply(", response, 
					", list(", paste(paste(group, "=", group, sep=""), collapse=", "), "), var, na.rm=TRUE))", sep="")
			doItAndPrint(command)
			doItAndPrint(paste("leveneTest(", response, " ~ ",
							paste(group, collapse="*"), ", data=", .activeDataSet, ", center=", center, ")", sep = ""))
			
		}
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "leveneTest", reset = "LeveneTest")
	tkgrid(getFrame(groupBox), labelPMgui(variableFrame, text = "    "), 
			getFrame(responseBox), sticky = "nw")
	tkgrid(variableFrame, sticky = "w")
	tkgrid(centerFrame, sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1)
}

