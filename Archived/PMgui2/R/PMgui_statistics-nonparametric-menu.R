# Statistics Menu dialogs

# last modified 2012-01-27 by J. Fox

# Nonparametric tests menu

twoSampleWilcoxonTest <- function () {
	defaults <- list(initial.group = NULL, initial.response = NULL, initial.alternative = "two.sided", 
			initial.test = "default", initial.label=NULL)
	dialog.values <- getDialog("twoSampleWilcoxonTest", defaults)
	initializeDialog(title = gettextPMgui("Two-Sample Wilcoxon Test"))
	groupBox <- variableListBox(top, TwoLevelFactors(), title = gettextPMgui("Groups (pick one)"),
			initialSelection = varPosn(dialog.values$initial.group, "twoLevelFactor"))
	responseBox <- variableListBox(top, Numeric(), title = gettextPMgui("Response Variable (pick one)"),
			initialSelection = varPosn(dialog.values$initial.response, "numeric"))
	onOK <- function() {
		group <- getSelection(groupBox)
		if (length(group) == 0) {
			errorCondition(recall = twoSampleWilcoxonTest, message = gettextPMgui("You must select a groups variable."))
			return()
		}
		response <- getSelection(responseBox)
		if (length(response) == 0) {
			errorCondition(recall = twoSampleWilcoxonTest, message = gettextPMgui("You must select a response variable."))
			return()
		}
		alternative <- as.character(tclvalue(alternativeVariable))
		test <- as.character(tclvalue(testVariable))
		closeDialog()
		putDialog("twoSampleWilcoxonTest", list(initial.group = group, initial.response = response, 
						initial.test = test, initial.alternative = alternative, initial.label=.groupsLabel))
		.activeDataSet <- ActiveDataSet()
		doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", 
								response, sep = ""), ", ", paste(.activeDataSet, 
								"$", group, sep = ""), ", median, na.rm=TRUE)", sep = ""))
		if (test == "default") {
			doItAndPrint(paste("wilcox.test(", response, " ~ ", 
							group, ", alternative=\"", alternative, "\", data=", 
							.activeDataSet, ")", sep = ""))
		}
		else doItAndPrint(paste("wilcox.test(", response, " ~ ", 
							group, ", alternative='", alternative, "', exact=", 
							test == "exact", ", correct=", test == "correct", 
							", data=", .activeDataSet, ")", sep = ""))
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "wilcox.test", reset = "twoSampleWilcoxonTest")
	radioButtons(name = "alternative", buttons = c("twosided", 
					"less", "greater"), values = c("two.sided", "less", "greater"), 
			labels = gettextPMgui(c("Two-sided", "Difference < 0", 
							"Difference > 0")), initialValue = dialog.values$initial.alternative,
			title = gettextPMgui("Alternative Hypothesis"))
	radioButtons(name = "test", buttons = c("default", "exact", 
					"normal", "correct"), labels = gettextPMgui(c("Default", 
							"Exact", "Normal approximation", "Normal approximation with\ncontinuity correction")), 
			initialValue = dialog.values$initial.test,
			title = gettextPMgui("Type of Test"))
	tkgrid(getFrame(groupBox), getFrame(responseBox), sticky = "nw")
	groupsLabel(groupsBox = groupBox, columnspan = 2, initialText=dialog.values$initial.label)
	tkgrid(alternativeFrame, testFrame, sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 4, columns = 2)
}

pairedWilcoxonTest <- function () {
	defaults <- list(initial.x = NULL, initial.y = NULL, initial.alternative = "two.sided", 
			initial.test = "default")
	dialog.values <- getDialog("pairedWilcoxonTest", defaults)
	initializeDialog(title = gettextPMgui("Paired Wilcoxon Test"))
	.numeric <- Numeric()
	xBox <- variableListBox(top, .numeric, title = gettextPMgui("First variable (pick one)"), 
			initialSelection = varPosn(dialog.values$initial.x, "numeric"))
	yBox <- variableListBox(top, .numeric, title = gettextPMgui("Second variable (pick one)"), 
			initialSelection = varPosn(dialog.values$initial.y, "numeric"))
	onOK <- function() {
		x <- getSelection(xBox)
		y <- getSelection(yBox)
		closeDialog()
		alternative <- as.character(tclvalue(alternativeVariable))
		test <- as.character(tclvalue(testVariable))
		putDialog("pairedWilcoxonTest", list(initial.x = x, initial.y = y, 
						initial.test = test, initial.alternative = alternative))
		if (length(x) == 0 | length(y) == 0) {
			errorCondition(recall = pairedWilcoxonTest, message = gettextPMgui("You must select two variables."))
			return()
		}
		if (x == y) {
			errorCondition(recall = pairedWilcoxonTest, message = gettextPMgui("The two variables must be different."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		doItAndPrint(paste("median(", .activeDataSet, "$", x, 
						" - ", .activeDataSet, "$", y, ", na.rm=TRUE) # median difference", 
						sep = ""))
		if (test == "default") {
			doItAndPrint(paste("wilcox.test(", .activeDataSet, 
							"$", x, ", ", .activeDataSet, "$", y, ", alternative='", 
							alternative, "', paired=TRUE)", sep = ""))
		}
		else if (test == "exact") {
			doItAndPrint(paste("wilcox.test(", .activeDataSet, 
							"$", x, ", ", .activeDataSet, "$", y, ", alternative='", 
							alternative, "', exact=TRUE, paired=TRUE)", sep = ""))
		}
		else {
			doItAndPrint(paste("wilcox.test(", .activeDataSet, 
							"$", x, ", ", .activeDataSet, "$", y, ", alternative='", 
							alternative, "', correct=", test == "correct", 
							", exact=FALSE, paired=TRUE)", sep = ""))
		}
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "wilcox.test", reset = "pairedWilcoxonTest")
	radioButtons(name = "alternative", buttons = c("twosided", 
					"less", "greater"), values = c("two.sided", "less", "greater"), 
			labels = gettextPMgui(c("Two-sided", "Difference < 0", 
							"Difference > 0")), title = gettextPMgui("Alternative Hypothesis"), 
			initialValue = dialog.values$initial.alternative)
	radioButtons(name = "test", buttons = c("default", "exact", 
					"normal", "correct"), labels = gettextPMgui(c("Default", 
							"Exact", "Normal approximation", "Normal approximation with\ncontinuity correction")), 
			title = gettextPMgui("Type of Test"), initialValue = dialog.values$initial.test)
	tkgrid(getFrame(xBox), getFrame(yBox), sticky = "nw")
	tkgrid(alternativeFrame, testFrame, sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 3, columns = 2)
}

KruskalWallisTest <- function () {
	defaults <- list(initial.group = NULL, initial.response = NULL)
	dialog.values <- getDialog("KruskalWallisTest", defaults)
	initializeDialog(title = gettextPMgui("Kruskal-Wallis Rank Sum Test"))
	groupBox <- variableListBox(top, Factors(), title = gettextPMgui("Groups (pick one)"),
			initialSelection = varPosn(dialog.values$initial.group, "factor"))
	responseBox <- variableListBox(top, Numeric(), title = gettextPMgui("Response Variable (pick one)"),
			initialSelection = varPosn(dialog.values$initial.response, "numeric"))
	onOK <- function() {
		group <- getSelection(groupBox)
		if (length(group) == 0) {
			errorCondition(recall = KruskalWallisTest, message = gettextPMgui("You must select a groups variable."))
			return()
		}
		response <- getSelection(responseBox)
		closeDialog()
		putDialog("KruskalWallisTest", list(initial.group = group, initial.response = response))
		if (length(response) == 0) {
			errorCondition(recall = KruskalWallisTest, message = gettextPMgui("You must select a response variable."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", 
								response, sep = ""), ", ", paste(.activeDataSet, 
								"$", group, sep = ""), ", median, na.rm=TRUE)", sep = ""))
		doItAndPrint(paste("kruskal.test(", response, " ~ ", 
						group, ", data=", .activeDataSet, ")", sep = ""))
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "kruskal.test", reset = "KruskalWallisTest")
	tkgrid(getFrame(groupBox), getFrame(responseBox), sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 2, columns = 2)
}

FriedmanTest <- function () {
	defaults <- list(initial.response = NULL)
	dialog.values <- getDialog("FriedmanTest", defaults)
	initializeDialog(title = gettextPMgui("Friedman Rank Sum Test"))
	responseBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
			initialSelection = varPosn(dialog.values$initial.response, "numeric"),
			title = gettextPMgui("Repeated-Measures Variables (pick two or more)"))
	onOK <- function() {
		responses <- getSelection(responseBox)
		closeDialog()
		putDialog("FriedmanTest", list (initial.response = responses))
		if (length(responses) < 2) {
			errorCondition(recall = FriedmanTest, message = gettextPMgui("You must select at least two variables."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		command <- paste("na.omit(with(", .activeDataSet, ", cbind(", 
				paste(responses, collapse = ", "), ")))", sep = "")
		logger(paste(".Responses <- ", command, sep = ""))
		assign(".Responses", justDoIt(command), envir = .GlobalEnv)
		doItAndPrint("apply(.Responses, 2, median)")
		doItAndPrint("friedman.test(.Responses)")
		logger("remove(.Responses)")
		remove(.Responses, envir = .GlobalEnv)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "friedman.test", reset = "FriedmanTest")
	tkgrid(getFrame(responseBox), sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

