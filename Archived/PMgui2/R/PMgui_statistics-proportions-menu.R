# Statistics Menu dialogs

# last modified 2011-12-27 by J. Fox

    # Proportions menu

singleProportionTest <- function () {
	defaults <- list (initial.x = NULL, initial.alternative = "two.sided", initial.level = ".95", 
			initial.test = "normal" , initial.p = ".5")
	dialog.values <- getDialog ("singleProportionTest", defaults)
	initializeDialog(title = gettextPMgui("Single-Sample Proportion Test"))
	xBox <- variableListBox(top, TwoLevelFactors(), title = gettextPMgui("Variable (pick one)"),
			initialSelection = varPosn(dialog.values$initial.x,"factor"))
	onOK <- function() {
		x <- getSelection(xBox)
		if (length(x) == 0) {
			errorCondition(recall = singleProportionTest, message = gettextPMgui("You must select a variable."))
			return()
		}
		alternative <- as.character(tclvalue(alternativeVariable))
		level <- tclvalue(confidenceLevel)
		test <- as.character(tclvalue(testVariable))
		p <- tclvalue(pVariable)
		putDialog ("singleProportionTest", list (initial.x = x, initial.alternative = alternative, 
						initial.level = level, initial.test = test , initial.p = p))
		closeDialog()
		command <- paste("xtabs(~", x, ", data=", ActiveDataSet(), 
				")")
		logger(paste(".Table <-", command))
		assign(".Table", justDoIt(command), envir = .GlobalEnv)
		doItAndPrint(".Table")
		if (test == "normal") 
			doItAndPrint(paste("prop.test(rbind(.Table), alternative='", 
							alternative, "', p=", p, ", conf.level=", level, 
							", correct=FALSE)", sep = ""))
		else if (test == "corrected") 
			doItAndPrint(paste("prop.test(rbind(.Table), alternative='", 
							alternative, "', p=", p, ", conf.level=", level, 
							", correct=TRUE)", sep = ""))
		else doItAndPrint(paste("binom.test(rbind(.Table), alternative='", 
							alternative, "', p=", p, ", conf.level=", level, 
							")", sep = ""))
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "prop.test", reset = "singleProportionTest")
	radioButtons(top, name = "alternative", buttons = c("twosided", 
					"less", "greater"), values = c("two.sided", "less", "greater"), 
			labels = gettextPMgui(c("Population proportion != p0", 
							"Population proportion < p0", "Population proportion > p0")), 
			title = gettextPMgui("Alternative Hypothesis"), initialValue = dialog.values$initial.alternative)
	rightFrame <- tkframe(top)
	confidenceFrame <- tkframe(rightFrame)
	confidenceLevel <- tclVar(dialog.values$initial.level)
	confidenceField <- ttkentry(confidenceFrame, width = "6", 
			textvariable = confidenceLevel)
	pFrame <- tkframe(rightFrame)
	pVariable <- tclVar(dialog.values$initial.p)
	pField <- ttkentry(pFrame, width = "6", textvariable = pVariable)
	radioButtons(name = "test", buttons = c("normal", "corrected", 
					"exact"), labels = gettextPMgui(c("Normal approximation", 
							"Normal approximation with\ncontinuity correction", "Exact binomial")), 
			title = gettextPMgui("Type of Test"), initialValue = dialog.values$initial.test)
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelPMgui(pFrame, text = gettextPMgui("Null hypothesis: p = "), 
					fg = "blue"), pField, sticky = "w")
	tkgrid(pFrame, sticky = "w")
	tkgrid(labelPMgui(rightFrame, text = ""))
	tkgrid(labelPMgui(confidenceFrame, text = gettextPMgui("Confidence Level: "), 
					fg = "blue"), confidenceField, sticky = "w")
	tkgrid(confidenceFrame, sticky = "w")
	tkgrid(alternativeFrame, rightFrame, sticky = "nw")
	tkgrid(testFrame, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	tkgrid.configure(confidenceField, sticky = "e")
	dialogSuffix(rows = 4, columns = 2)
}

twoSampleProportionsTest <- function () {
	Library("abind")
	defaults <- list(initial.groups = NULL, initial.response = NULL, initial.alternative = "two.sided", 
			initial.confidenceLevel = ".95", initial.test = "normal", initial.label=NULL)
	dialog.values <- getDialog("twoSampleProportionsTest", defaults)
	initializeDialog(title = gettextPMgui("Two-Sample Proportions Test"))
	.twoLevelFactors <- TwoLevelFactors()
	groupsBox <- variableListBox(top, .twoLevelFactors, title = gettextPMgui("Groups (pick one)"), 
			initialSelection = varPosn(dialog.values$initial.groups, "twoLevelFactor"))
	xBox <- variableListBox(top, .twoLevelFactors, title = gettextPMgui("Response Variable (pick one)"), 
			initialSelection = varPosn(dialog.values$initial.response, "twoLevelFactor"))
	onOK <- function() {
		groups <- getSelection(groupsBox)
		if (length(groups) == 0) {
			errorCondition(recall = twoSampleProportionsTest, 
					message = gettextPMgui("You must select a groups variable."))
			return()
		}
		x <- getSelection(xBox)
		if (length(x) == 0) {
			errorCondition(recall = twoSampleProportionsTest, 
					message = gettextPMgui("You must select a response variable."))
			return()
		}
		if (x == groups) {
			errorCondition(recall = twoSampleProportionsTest, 
					message = gettextPMgui("Groups and response variables must be different."))
			return()
		}
		alternative <- as.character(tclvalue(alternativeVariable))
		level <- tclvalue(confidenceLevel)
		test <- as.character(tclvalue(testVariable))
		closeDialog()
		putDialog("twoSampleProportionsTest", list(initial.groups = groups, initial.response = x, 
						initial.test = test, initial.alternative = alternative, initial.confidenceLevel = level,
						initial.label=.groupsLabel))
		command <- paste("xtabs(~", groups, "+", x, ", data=", 
				ActiveDataSet(), ")", sep = "")
		logger(paste(".Table <-", command))
		assign(".Table", justDoIt(command), envir = .GlobalEnv)
		doItAndPrint("rowPercents(.Table)")
		if (test == "normal") 
			doItAndPrint(paste("prop.test(.Table, alternative='", 
							alternative, "', conf.level=", level, ", correct=FALSE)", 
							sep = ""))
		else doItAndPrint(paste("prop.test(.Table, alternative='", 
							alternative, "', conf.level=", level, ", correct=TRUE)", 
							sep = ""))
		logger("remove(.Table)")
		remove(.Table, envir = .GlobalEnv)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "prop.test", reset = "twoSampleProportionsTest")
	radioButtons(name = "alternative", buttons = c("twosided", 
					"less", "greater"), values = c("two.sided", "less", "greater"), 
			labels = gettextPMgui(c("Two-sided", "Difference < 0", 
							"Difference > 0")), initialValue = dialog.values$initial.alternative, 
			title = gettextPMgui("Alternative Hypothesis"))
	rightFrame <- tkframe(top)
	confidenceFrame <- tkframe(rightFrame)
	confidenceLevel <- tclVar(dialog.values$initial.confidenceLevel)
	confidenceField <- ttkentry(confidenceFrame, width = "6", 
			textvariable = confidenceLevel)
	radioButtons(name = "test", buttons = c("normal", "corrected"), 
			labels = gettextPMgui(c("Normal approximation", "Normal approximation with\ncontinuity correction")), 
			initialValue = dialog.values$initial.test, 
			title = gettextPMgui("Type of Test"))
	tkgrid(getFrame(groupsBox), getFrame(xBox), sticky = "nw")
	groupsLabel(columnspan = 2, initialText=dialog.values$initial.label)
	tkgrid(labelPMgui(confidenceFrame, text = gettextPMgui("Confidence Level: "), 
					fg = "blue"), confidenceField, sticky = "w")
	tkgrid(confidenceFrame, sticky = "w")
	tkgrid(alternativeFrame, rightFrame, sticky = "nw")
	tkgrid(testFrame, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	tkgrid.configure(confidenceField, sticky = "e")
	dialogSuffix(rows = 5, columns = 2)
}

