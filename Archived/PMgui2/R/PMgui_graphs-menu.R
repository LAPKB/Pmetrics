# Graphs menu dialogs

# last modified 2012-02-02 by J. Fox
#  applied patch to improve window behaviour supplied by Milan Bouchet-Valat 2011-09-22

indexPlot <- function () {
	defaults <- list(initial.x = NULL, initial.type = "spikes", initial.identify = 0) 
	dialog.values <- getDialog("indexPlot", defaults)
	initializeDialog(title = gettextPMgui("Index Plot"))
	xBox <- variableListBox(top, Numeric(), title = gettextPMgui("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	onOK <- function() {
		x <- getSelection(xBox)
		initial.type <- type <- tclvalue(typeVariable)
		identify <- tclvalue(identifyVariable) == "1"
		putDialog ("indexPlot", list(initial.x = x, initial.type = type, initial.identify = identify))
		closeDialog()
		if (length(x) == 0) {
			errorCondition(recall = indexPlot, message = gettextPMgui("You must select a variable"))
			return()
		}
		type <- if (tclvalue(typeVariable) == "spikes") 
					"h"
				else "p"
		.activeDataSet <- ActiveDataSet()
		command <- paste("plot(", .activeDataSet, "$", x, ", type=\"", 
				type, "\")", sep = "")
		doItAndPrint(command)
		if (par("usr")[3] <= 0) 
			doItAndPrint("abline(h=0, col=\"gray\")")
		if (identify) {
			PMguiTkmessageBox(title = "Identify Points", message = paste(gettextPMgui("Use left mouse button to identify points,\n"), 
							gettextPMgui(if (MacOSXP()) 
												"esc key to exit."
											else "right button to exit."), sep = ""), icon = "info", 
					type = "ok")
			command <- paste("identify(", .activeDataSet, "$", 
					x, ", labels=rownames(", .activeDataSet, "))", 
					sep = "")
			doItAndPrint(command)
		}
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "plot", reset = "indexPlot")
	optionsFrame <- tkframe(top)
	typeVariable <- tclVar(dialog.values$initial.type)
	spikesButton <- ttkradiobutton(optionsFrame, variable = typeVariable, 
			value = "spikes")
	pointsButton <- ttkradiobutton(optionsFrame, variable = typeVariable, 
			value = "points")
	identifyVariable <- tclVar(dialog.values$initial.identify)
	identifyCheckBox <- tkcheckbutton(optionsFrame, variable = identifyVariable)
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Identify observations\nwith mouse"), 
					justify = "left"), identifyCheckBox, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Spikes")), 
			spikesButton, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Points")), 
			pointsButton, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

Histogram <- function () {
	defaults <- list(initial.x = NULL, initial.scale = "frequency", 
			initial.bins = gettextPMgui ("<auto>")) 
	dialog.values <- getDialog("Histogram", defaults)
	initializeDialog(title = gettextPMgui("Histogram"))
	xBox <- variableListBox(top, Numeric(), title = gettextPMgui("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	onOK <- function() {
		x <- getSelection(xBox)
		closeDialog()
		if (length(x) == 0) {
			errorCondition(recall = Histogram, message = gettextPMgui("You must select a variable"))
			return()
		}
		bins <- tclvalue(binsVariable)
		opts <- options(warn = -1)
		binstext <- if (bins == gettextPMgui("<auto>")) 
					"\"Sturges\""
				else as.numeric(bins)
		options(opts)
		scale <- tclvalue(scaleVariable)
		putDialog ("Histogram", list (initial.x = x, initial.bins = bins, initial.scale = scale))
		command <- paste("Hist(", ActiveDataSet(), "$", x, ", scale=\"", 
				scale, "\", breaks=", binstext, ", col=\"darkgray\")", 
				sep = "")
		doItAndPrint(command)
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "Hist", reset = "Histogram")
	radioButtons(name = "scale", buttons = c("frequency", "percent", 
					"density"), labels = gettextPMgui(c("Frequency counts", 
							"Percentages", "Densities")), title = gettextPMgui("Axis Scaling"), 
			initialValue = dialog.values$initial.scale)
	binsFrame <- tkframe(top)
	binsVariable <- tclVar(dialog.values$initial.bins)
	binsField <- ttkentry(binsFrame, width = "8", textvariable = binsVariable)
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelPMgui(binsFrame, text = gettextPMgui("Number of bins: ")), 
			binsField, sticky = "w")
	tkgrid(binsFrame, sticky = "w")
	tkgrid(scaleFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	tkgrid.configure(binsField, sticky = "e")
	dialogSuffix(rows = 4, columns = 1)
}

stemAndLeaf <- function () {
	Library("aplpack")
	defaults <- list(initial.x = NULL, initial.leafs.auto="1", initial.unit = 0,  initial.m = "auto", 
			initial.trim = 1, initial.depths = 1, initial.reverse = 1, initial.style = "Tukey") 
	dialog.values <- getDialog("stemAndLeaf", defaults)
	initializeDialog(title = gettextPMgui("Stem and Leaf Display"), 
			preventCrisp = TRUE)
	xBox <- variableListBox(top, Numeric(), title = gettextPMgui("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	displayDigits <- tclVar(formatC(10^dialog.values$initial.unit))
	leafsDigitValue <- tclVar(dialog.values$initial.unit)
	onDigits <- function(...) {
		tclvalue(displayDigits) <- formatC(10^as.numeric(tclvalue(leafsDigitValue)), 
				format = "fg", big.mark = ",")
		tclvalue(leafsAutoVariable) <- "0"
	}
	radioButtons(name = "parts", buttons = c("auto", "one", "two", 
					"five"), values = c("auto", "1", "2", "5"), labels = c(gettextPMgui("Automatic"), 
					"   1", "   2", "   5"), title = gettextPMgui("Parts Per Stem"), 
			initialValue = dialog.values$initial.m)
	radioButtons(name = "style", buttons = c("Tukey", "bare"), 
			labels = gettextPMgui(c("Tukey", "Repeated stem digits")), 
			title = gettextPMgui("Style of Divided Stems"), 
			initialValue = dialog.values$initial.style)
	checkBoxes(frame = "optionsFrame", boxes = c("trimOutliers", 
					"showDepths", "reverseNegative"), initialValues = c(dialog.values$initial.trim,
					dialog.values$initial.depths, dialog.values$initial.reverse),
			labels = gettextPMgui(c("Trim outliers", "Show depths", 
							"Reverse negative leaves")))
	leafsFrame <- tkframe(top)
	leafsDigitValue <- tclVar(dialog.values$initial.unit) #tclVar("0")
	leafsDigitSlider <- tkscale(leafsFrame, from = -6, to = 6, 
			showvalue = FALSE, variable = leafsDigitValue, resolution = 1, 
			orient = "horizontal", command = onDigits)
	leafsDigitShow <- labelPMgui(leafsFrame, textvariable = displayDigits, 
			width = 8, justify = "right")
	leafsAutoVariable <- tclVar("1") # tclVar(dialog.values$initial.leafs.auto)
	leafsDigitCheckBox <- tkcheckbutton(leafsFrame, variable = leafsAutoVariable)
	onOK <- function() {
		x <- getSelection(xBox)
		m <- tclvalue(partsVariable)
		style <- tclvalue (styleVariable)
		trim <- tclvalue (trimOutliersVariable)
		depths <- tclvalue (showDepthsVariable)
		reverse <- tclvalue (reverseNegativeVariable)
		unit <- if (tclvalue(leafsAutoVariable) == "1") 
					""
				else paste(", unit=", 10^as.numeric(tclvalue(leafsDigitValue)), 
							sep = "")
		putDialog ("stemAndLeaf", list(initial.x = x, initial.leafs.auto=tclvalue(leafsAutoVariable),
						initial.unit = as.numeric(tclvalue(leafsDigitValue)),  initial.m = m, 
						initial.trim = trim, initial.depths = depths, initial.reverse = reverse, 
						initial.style = style))
		closeDialog()
		if (length(x) == 0) {
			errorCondition(recall = stemAndLeaf, message = gettextPMgui("You must select a variable"))
			return()
		}
		trim <- if (tclvalue(trimOutliersVariable) == "1") 
					""
				else ", trim.outliers=FALSE"
		depths <- if (tclvalue(showDepthsVariable) == "1") 
					""
				else ", depths=FALSE"
		reverse <- if (tclvalue(reverseNegativeVariable) == "1") 
					""
				else ", reverse.negative.leaves=FALSE"
		m <- if (tclvalue(partsVariable) == "auto") 
					""
				else paste(", m=", tclvalue(partsVariable), sep = "")
		style <- if (tclvalue(styleVariable) == "Tukey") 
					""
				else ", style=\"bare\""
		command <- paste("stem.leaf(", ActiveDataSet(), "$", 
				x, style, unit, m, trim, depths, reverse, ", na.rm=TRUE)", 
				sep = "")
		doItAndPrint(command)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "stem.leaf", reset = "stemAndLeaf")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelPMgui(leafsFrame, text = gettextPMgui("Leafs Digit:  "), 
					fg = "blue"), labelPMgui(leafsFrame, text = gettextPMgui("Automatic")), 
			leafsDigitCheckBox, labelPMgui(leafsFrame, text = gettextPMgui("  or set:"), 
					fg = "red"), leafsDigitShow, leafsDigitSlider, sticky = "w")
	tkgrid(leafsFrame, sticky = "w")
	tkgrid(partsFrame, sticky = "w")
	tkgrid(styleFrame, sticky = "w")
	tkgrid(labelPMgui(top, text = gettextPMgui("Options"), fg = "blue"), 
			sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	tclvalue(leafsAutoVariable) <- dialog.values$initial.leafs.auto
	dialogSuffix(rows = 7, columns = 1, preventCrisp = TRUE)
}

boxPlot <- function () {
	defaults <- list(initial.x = NULL, initial.identifyPoints = 0, initialGroup=NULL) 
	dialog.values <- getDialog("boxPlot", defaults)
	initializeDialog(title = gettextPMgui("Boxplot"))
	xBox <- variableListBox(top, Numeric(), title = gettextPMgui("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	identifyVariable <- tclVar(dialog.values$initial.identifyPoints)
	identifyFrame <- tkframe(top)
	identifyCheckBox <- tkcheckbutton(identifyFrame, variable = identifyVariable)
	initial.group <- dialog.values$initial.group
	.groups <- if (is.null(initial.group)) FALSE else initial.group
	onOK <- function() {
		x <- getSelection(xBox)
		identifyPoints <- "1" == tclvalue(identifyVariable)
		putDialog ("boxPlot", list(initial.x = x, initial.identifyPoints = identifyPoints, 
						initial.group=if (.groups == FALSE) NULL else .groups))
		closeDialog()
		if (length(x) == 0) {
			errorCondition(recall = boxPlot, message = gettextPMgui("You must select a variable"))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		var <- paste(.activeDataSet, "$", x, sep = "")
		if (is.null(.groups) || .groups == FALSE) {
			command <- (paste("boxplot(", var, ", ylab=\"", x, 
								"\")", sep = ""))
			logger(command)
			justDoIt(command)
			if (identifyPoints) {
				PMguiTkmessageBox(title = "Identify Points", 
						message = paste(gettextPMgui("Use left mouse button to identify points,\n"), 
								gettextPMgui(if (MacOSXP()) 
													"esc key to exit."
												else "right button to exit."), sep = ""), 
						icon = "info", type = "ok")
				doItAndPrint(paste("identify(rep(1, length(", 
								var, ")), ", var, ", rownames(", .activeDataSet, 
								"))", sep = ""))
			}
		}
		else {
			command <- (paste("boxplot(", x, "~", .groups, ", ylab=\"", 
								x, "\", xlab=\"", .groups, "\"", ", data=", .activeDataSet, 
								")", sep = ""))
			logger(command)
			justDoIt(command)
			if (identifyPoints) {
				PMguiTkmessageBox(title = "Identify Points", 
						message = paste(gettextPMgui("Use left mouse button to identify points,\n"), 
								gettextPMgui(if (MacOSXP()) 
													"esc key to exit."
												else "right button to exit."), sep = ""), 
						icon = "info", type = "ok")
				doItAndPrint(paste("identify(", .activeDataSet, 
								"$", .groups, ", ", var, ", rownames(", .activeDataSet, 
								"))", sep = ""))
			}
		}
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	groupsBox(boxPlot, initialGroup=initial.group, 
			initialLabel=if (is.null(initial.group)) gettextPMgui("Plot by groups") else paste(gettextPMgui("Plot by:"), initial.group))
	OKCancelHelp(helpSubject = "boxplot", reset = "boxPlot")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelPMgui(identifyFrame, text = gettextPMgui("Identify outliers with mouse"), 
					justify = "left"), identifyCheckBox, sticky = "w")
	tkgrid(identifyFrame, stick = "w")
	tkgrid(groupsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 4, columns = 1)
}

scatterPlot <- function () {
	require("car")
	defaults <- list(initial.x = NULL, initial.y = NULL, initial.jitterx = 0, initial.jittery = 0, 
			initial.logstringx = 0, initial.logstringy = 0, initial.log = 0, initial.box = 1, 
			initial.line = 1, initial.smooth = 1, initial.spread = 1, initial.span = 50,
			initial.subset = gettextPMgui ("<all valid cases>"), initial.ylab = gettextPMgui ("<auto>"), 
			initial.xlab = gettextPMgui("<auto>"), initial.pch = gettextPMgui("<auto>"), 
			initial.cexValue = 1, initial.cex.axisValue = 1, initial.cex.labValue = 1, initialGroup=NULL, initial.lines.by.group=1) 
	dialog.values <- getDialog("scatterPlot", defaults)
	initial.group <- dialog.values$initial.group
	.linesByGroup <- if (dialog.values$initial.lines.by.group == 1) TRUE else FALSE
	.groups <- if (is.null(initial.group)) FALSE else initial.group
	initializeDialog(title = gettextPMgui("Scatterplot"))
	.numeric <- Numeric()
	variablesFrame <- tkframe(top)
	xBox <- variableListBox(variablesFrame, .numeric, title = gettextPMgui("x-variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	yBox <- variableListBox(variablesFrame, .numeric, title = gettextPMgui("y-variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.y, "numeric"))
	optionsParFrame <- tkframe(top)
	checkBoxes(window = optionsParFrame, frame = "optionsFrame", 
			boxes = c("identify", "jitterX", "jitterY", "logX", "logY", 
					"boxplots", "lsLine", "smoothLine", "spread"), initialValues = c(dialog.values$initial.log, 
					dialog.values$initial.jitterx, dialog.values$initial.jittery, 
					dialog.values$initial.logstringx, dialog.values$initial.logstringy,
					dialog.values$initial.box, dialog.values$initial.line, dialog.values$initial.smooth,
					dialog.values$initial.spread),labels = gettextPMgui(c("Identify points", 
							"Jitter x-variable", "Jitter y-variable", "Log x-axis", 
							"Log y-axis", "Marginal boxplots", "Least-squares line", 
							"Smooth line", "Show spread")), title = "Options")
	sliderValue <- tclVar(dialog.values$initial.span)
	slider <- tkscale(optionsFrame, from = 0, to = 100, showvalue = TRUE, 
			variable = sliderValue, resolution = 5, orient = "horizontal")
	subsetBox(subset.expression = dialog.values$initial.subset)
	labelsFrame <- tkframe(top)
	xlabVar <- tclVar(dialog.values$initial.xlab)
	ylabVar <- tclVar(dialog.values$initial.ylab)
	xlabFrame <- tkframe(labelsFrame)
	xlabEntry <- ttkentry(xlabFrame, width = "25", textvariable = xlabVar)
	xlabScroll <- ttkscrollbar(xlabFrame, orient = "horizontal", 
			command = function(...) tkxview(xlabEntry, ...))
	tkconfigure(xlabEntry, xscrollcommand = function(...) tkset(xlabScroll, 
						...))
	tkgrid(labelPMgui(xlabFrame, text = gettextPMgui("x-axis label"), 
					fg = "blue"), sticky = "w")
	tkgrid(xlabEntry, sticky = "w")
	tkgrid(xlabScroll, sticky = "ew")
	ylabFrame <- tkframe(labelsFrame)
	ylabEntry <- ttkentry(ylabFrame, width = "25", textvariable = ylabVar)
	ylabScroll <- ttkscrollbar(ylabFrame, orient = "horizontal", 
			command = function(...) tkxview(ylabEntry, ...))
	tkconfigure(ylabEntry, xscrollcommand = function(...) tkset(ylabScroll, 
						...))
	tkgrid(labelPMgui(ylabFrame, text = gettextPMgui("y-axis label"), 
					fg = "blue"), sticky = "w")
	tkgrid(ylabEntry, sticky = "w")
	tkgrid(ylabScroll, sticky = "ew")
	tkgrid(xlabFrame, labelPMgui(labelsFrame, text = "     "), 
			ylabFrame, sticky = "w")
	parFrame <- tkframe(optionsParFrame)
	pchVar <- tclVar(dialog.values$initial.pch)
	pchEntry <- ttkentry(parFrame, width = 25, textvariable = pchVar)
	cexValue <- tclVar(dialog.values$initial.cexValue)
	cex.axisValue <- tclVar(dialog.values$initial.cex.axisValue)
	cex.labValue <- tclVar(dialog.values$initial.cex.labValue)
	cexSlider <- tkscale(parFrame, from = 0.5, to = 2.5, showvalue = TRUE, 
			variable = cexValue, resolution = 0.1, orient = "horizontal")
	cex.axisSlider <- tkscale(parFrame, from = 0.5, to = 2.5, 
			showvalue = TRUE, variable = cex.axisValue, resolution = 0.1, 
			orient = "horizontal")
	cex.labSlider <- tkscale(parFrame, from = 0.5, to = 2.5, 
			showvalue = TRUE, variable = cex.labValue, resolution = 0.1, 
			orient = "horizontal")
	onOK <- function() {
		x <- getSelection(xBox)
		y <- getSelection(yBox)
		jitter <- if ("1" == tclvalue(jitterXVariable) && "1" == 
						tclvalue(jitterYVariable)) 
					", jitter=list(x=1, y=1)"
				else if ("1" == tclvalue(jitterXVariable)) 
					", jitter=list(x=1)"
				else if ("1" == tclvalue(jitterYVariable)) 
					", jitter=list(y=1)"
				else ""
		logstring <- ""
		if ("1" == tclvalue(logXVariable)) 
			logstring <- paste(logstring, "x", sep = "")
		if ("1" == tclvalue(logYVariable)) 
			logstring <- paste(logstring, "y", sep = "")
		log <- tclvalue(identifyVariable)
		box <- tclvalue(boxplotsVariable)
		line <- tclvalue(lsLineVariable)
		smooth <-  tclvalue(smoothLineVariable)
		spread <- tclvalue(spreadVariable)
		span <- as.numeric(tclvalue(sliderValue))
		initial.subset <- subset <- tclvalue(subsetVariable)
		subset <- if (trim.blanks(subset) == gettextPMgui("<all valid cases>")) 
					""
				else paste(", subset=", subset, sep = "")
		cex.axis <- as.numeric(tclvalue(cex.axisValue))
		cex <- as.numeric(tclvalue(cexValue))
		cex.lab <- as.numeric(tclvalue(cex.labValue))
		xlab <- trim.blanks(tclvalue(xlabVar))
		xlab <- if (xlab == gettextPMgui("<auto>")) 
					""
				else paste(", xlab=\"", xlab, "\"", sep = "")
		ylab <- trim.blanks(tclvalue(ylabVar))
		ylab <- if (ylab == gettextPMgui("<auto>")) 
					""
				else paste(", ylab=\"", ylab, "\"", sep = "")
		pch <- gsub(" ", ",", tclvalue(pchVar))
		putDialog ("scatterPlot", list (initial.x = x, initial.y = y, initial.jitterx = tclvalue(jitterXVariable),
						initial.jittery = tclvalue(jitterYVariable), initial.logstringx = tclvalue(logXVariable),
						initial.logstringy = tclvalue(logYVariable), initial.log = log, initial.box = box, 
						initial.line = line, initial.smooth = smooth, initial.spread = spread,
						initial.span = span, initial.subset = initial.subset, initial.xlab = tclvalue(xlabVar),
						initial.ylab = tclvalue(ylabVar), initial.cexValue = tclvalue(cexValue), 
						initial.cex.axisValue = tclvalue(cex.axisValue), initial.cex.labValue = tclvalue(cex.labValue), 
						initial.pch = pch, initial.group=if (.groups == FALSE) NULL else .groups,
						initial.lines.by.group=if (.linesByGroup) 1 else 0))
		closeDialog()
		if ("" == pch) {
			errorCondition(recall = scatterPlot, message = gettextPMgui("No plotting characters."))
			return()
		}
		pch <- if (trim.blanks(pch) == gettextPMgui("<auto>")) 
					""
				else paste(", pch=c(", pch, ")", sep = "")
		if (length(x) == 0 || length(y) == 0) {
			errorCondition(recall = scatterPlot, message = gettextPMgui("You must select two variables"))
			return()
		}
		if (x == y) {
			errorCondition(recall = scatterPlot, message = gettextPMgui("x and y variables must be different"))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		log <- if (logstring != "") 
					paste(", log=\"", logstring, "\"", sep = "")
				else ""
		if ("1" == tclvalue(identifyVariable)) {
			PMguiTkmessageBox(title = "Identify Points", message = paste(gettextPMgui("Use left mouse button to identify points,\n"), 
							gettextPMgui(if (MacOSXP()) 
												"esc key to exit."
											else "right button to exit."), sep = ""), icon = "info", 
					type = "ok")
			idtext <- ", id.method=\"identify\""
		}
		else idtext <- ""
		box <- if ("1" == tclvalue(boxplotsVariable)) 
					"'xy'"
				else "FALSE"
		line <- if ("1" == tclvalue(lsLineVariable)) 
					"lm"
				else "FALSE"
		smooth <- as.character("1" == tclvalue(smoothLineVariable))
		spread <- as.character("1" == tclvalue(spreadVariable))
		cex <- if (cex == 1) 
					""
				else paste(", cex=", cex, sep = "")
		cex.axis <- if (cex.axis == 1) 
					""
				else paste(", cex.axis=", cex.axis, sep = "")
		cex.lab <- if (cex.lab == 1) 
					""
				else paste(", cex.lab=", cex.lab, sep = "")
		if (.groups == FALSE) {
			doItAndPrint(paste("scatterplot(", y, "~", x, log, 
							", reg.line=", line, ", smooth=", smooth, ", spread=", 
							spread, idtext, ", boxplots=", box, ", span=", 
							span/100, jitter, xlab, ylab, cex, cex.axis, 
							cex.lab, pch, ", data=", .activeDataSet, subset, 
							")", sep = ""))
		}
		else {
			doItAndPrint(paste("scatterplot(", y, "~", x, " | ", 
							.groups, log, ", reg.line=", line, ", smooth=", smooth, 
							", spread=", spread, idtext, ", boxplots=", box, 
							", span=", span/100, jitter, xlab, ylab, cex, 
							cex.axis, cex.lab, pch, ", by.groups=", .linesByGroup, 
							", data=", .activeDataSet, subset, ")", sep = ""))
		}
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	groupsBox(scatterPlot, plotLinesByGroup = TRUE, initialGroup=initial.group, initialLinesByGroup=dialog.values$initial.lines.by.group,
			initialLabel=if (is.null(initial.group)) gettextPMgui("Plot by groups") else paste(gettextPMgui("Plot by:"), initial.group))
	OKCancelHelp(helpSubject = "scatterplot", reset = "scatterPlot")
	tkgrid(getFrame(xBox), getFrame(yBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Span for smooth")), 
			slider, sticky = "w")
	tkgrid(labelPMgui(parFrame, text = gettextPMgui("Plotting Parameters"), 
					fg = "blue"), sticky = "w")
	tkgrid(labelPMgui(parFrame, text = gettextPMgui("Plotting characters")), 
			pchEntry, stick = "w")
	tkgrid(labelPMgui(parFrame, text = gettextPMgui("Point size")), 
			cexSlider, sticky = "w")
	tkgrid(labelPMgui(parFrame, text = gettextPMgui("Axis text size")), 
			cex.axisSlider, sticky = "w")
	tkgrid(labelPMgui(parFrame, text = gettextPMgui("Axis-labels text size")), 
			cex.labSlider, sticky = "w")
	tkgrid(optionsFrame, parFrame, sticky = "nw")
	tkgrid(optionsParFrame, sticky = "w")
	tkgrid(labelsFrame, sticky = "w")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(groupsFrame, sticky = "w")
	tkgrid(labelPMgui(top, text = " "))
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 8, columns = 2)
}

scatterPlotMatrix <- function () {
	require("car")
	defaults <- list(initial.variables = NULL, initial.line = 1, initial.smooth = 1, initial.spread = 0, 
			initial.span = 50, initial.diag = "density", initial.subset = gettextPMgui ("<all valid cases>"),
			initialGroup=NULL, initial.lines.by.group=1) 
	dialog.values <- getDialog("scatterPlotMatrix", defaults)
	initial.group <- dialog.values$initial.group
	.linesByGroup <- if (dialog.values$initial.lines.by.group == 1) TRUE else FALSE
	.groups <- if (is.null(initial.group)) FALSE else initial.group
	initializeDialog(title = gettextPMgui("Scatterplot Matrix"))
	variablesBox <- variableListBox(top, Numeric(), title = gettextPMgui("Select variables (three or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.variables, "numeric"))
	checkBoxes(frame = "optionsFrame", boxes = c("lsLine", "smoothLine", 
					"spread"), initialValues = c(dialog.values$initial.line, dialog.values$initial.smooth,
					dialog.values$initial.spread), labels = gettextPMgui(c("Least-squares lines", 
							"Smooth lines", "Show spread")))
	sliderValue <- tclVar(dialog.values$initial.span)
	slider <- tkscale(optionsFrame, from = 0, to = 100, showvalue = TRUE, 
			variable = sliderValue, resolution = 5, orient = "horizontal")
	radioButtons(name = "diagonal", buttons = c("density", "histogram", 
					"boxplot", "oned", "qqplot", "none"), labels = gettextPMgui(c("Density plots", 
							"Histograms", "Boxplots", "One-dimensional scatterplots", 
							"Normal QQ plots", "Nothing (empty)")), title = gettextPMgui("On Diagonal"), 
			initialValue = dialog.values$initial.diag)
	subsetBox(subset.expression = dialog.values$initial.subset)
	onOK <- function() {
		variables <- getSelection(variablesBox)
		closeDialog()
		if (length(variables) < 3) {
			errorCondition(recall = scatterPlotMatrix, message = gettextPMgui("Fewer than 3 variable selected."))
			return()
		}
		line <- if ("1" == tclvalue(lsLineVariable)) 
					"lm"
				else "FALSE"
		smooth <- as.character("1" == tclvalue(smoothLineVariable))
		spread <- as.character("1" == tclvalue(spreadVariable))
		span <- as.numeric(tclvalue(sliderValue))
		diag <- as.character(tclvalue(diagonalVariable))
		initial.subset <- subset <- tclvalue(subsetVariable)
		subset <- if (trim.blanks(subset) == gettextPMgui("<all valid cases>")) ""
				else paste(", subset=", subset, sep="")
		putDialog("scatterPlotMatrix", list(initial.variables = variables, initial.line = tclvalue (lsLineVariable), 
						initial.smooth = tclvalue(smoothLineVariable),initial.spread = tclvalue (spreadVariable), 
						initial.span = span, initial.diag = diag, initial.subset = initial.subset, 
						initial.group=if (.groups == FALSE) NULL else .groups,
						initial.lines.by.group=if (.linesByGroup) 1 else 0))
		.activeDataSet <- ActiveDataSet()
		if (.groups == FALSE) {
			command <- paste("scatterplotMatrix(~", paste(variables, 
							collapse = "+"), ", reg.line=", line, ", smooth=", 
					smooth, ", spread=", spread, ", span=", span/100, 
					", diagonal = '", diag, "', data=", .activeDataSet, 
					subset, ")", sep = "")
			logger(command)
			justDoIt(command)
		}
		else {
			command <- paste("scatterplotMatrix(~", paste(variables, 
							collapse = "+"), " | ", .groups, ", reg.line=", 
					line, ", smooth=", smooth, ", spread=", spread, 
					", span=", span/100, ", diagonal= '", diag, "', by.groups=", 
					.linesByGroup, ", data=", .activeDataSet, subset, 
					")", sep = "")
			logger(command)
			justDoIt(command)
		}
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	groupsBox(scatterPlot, plotLinesByGroup = TRUE, initialGroup=initial.group, initialLinesByGroup=dialog.values$initial.lines.by.group,
			initialLabel=if (is.null(initial.group)) gettextPMgui("Plot by groups") else paste(gettextPMgui("Plot by:"), initial.group))
	OKCancelHelp(helpSubject = "scatterplotMatrix", reset = "scatterPlotMatrix")
	tkgrid(getFrame(variablesBox), sticky = "nw")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Span for smooth")), 
			slider, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(diagonalFrame, sticky = "w")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(groupsFrame, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 6, columns = 2)
}

barGraph <- function () {
	defaults <- list (initial.variable = NULL)
	dialog.values <- getDialog ("barGraph", defaults)
	initializeDialog(title = gettextPMgui("Bar Graph"))
	variableBox <- variableListBox(top, Factors(), title = gettextPMgui("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.variable, "factor"))
	onOK <- function() {
		variable <- getSelection(variableBox)
		putDialog ("barGraph", list (initial.variable = variable))
		closeDialog()
		if (length(variable) == 0) {
			errorCondition(recall = barGraph, message = gettextPMgui("You must select a variable"))
			return()
		}
		command <- paste("barplot(table(", ActiveDataSet(), "$", 
				variable, "), xlab=\"", variable, "\", ylab=\"Frequency\")", 
				sep = "")
		logger(command)
		justDoIt(command)
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "barplot", reset = "barGraph")
	tkgrid(getFrame(variableBox), sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

pieChart <- function () {
	Library("colorspace")
	defaults <- list (initial.variable = NULL)
	dialog.values <- getDialog ("pieChart", defaults)
	initializeDialog(title = gettextPMgui("Pie Chart"))
	variableBox <- variableListBox(top, Factors(), title = gettextPMgui("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.variable, "factor"))
	onOK <- function() {
		variable <- getSelection(variableBox)
		putDialog ("pieChart", list (initial.variable = variable))
		closeDialog()
		if (length(variable) == 0) {
			errorCondition(recall = pieChart, message = gettextPMgui("You must select a variable"))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		command <- (paste("pie(table(", .activeDataSet, "$", 
							variable, "), labels=levels(", .activeDataSet, "$", 
							variable, "), main=\"", variable, "\", col=rainbow_hcl(length(levels(", 
							.activeDataSet, "$", variable, "))))", sep = ""))
		logger(command)
		justDoIt(command)
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "pie", reset = "pieChart")
	tkgrid(getFrame(variableBox), sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1)
}

linePlot <- function () {
	defaults <- list(initial.x = NULL, initial.y = NULL, initial.axisLabel = gettextPMgui ("<use y-variable names>"), 
			initial.legend = 0) 
	dialog.values <- getDialog("linePlot", defaults)
	initializeDialog(title = gettextPMgui("Line Plot"))
	variablesFrame <- tkframe(top)
	.numeric <- Numeric()
	xBox <- variableListBox(variablesFrame, .numeric, title = gettextPMgui("x variable (pick one)"),
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	yBox <- variableListBox(variablesFrame, .numeric, title = gettextPMgui("y variables (pick one or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.y, "numeric"))
	axisLabelVariable <- tclVar(dialog.values$initial.axisLabel)
	axisLabelFrame <- tkframe(top)
	axisLabelEntry <- ttkentry(axisLabelFrame, width = "40", 
			textvariable = axisLabelVariable)
	axisLabelScroll <- ttkscrollbar(axisLabelFrame, orient = "horizontal", 
			command = function(...) tkxview(axisLabelEntry, ...))
	tkconfigure(axisLabelEntry, xscrollcommand = function(...) tkset(axisLabelScroll, 
						...))
	legendFrame <- tkframe(top)
	legendVariable <- tclVar(dialog.values$initial.legend)
	legendCheckBox <- tkcheckbutton(legendFrame, variable = legendVariable)
	onOK <- function() {
		y <- getSelection(yBox)
		x <- getSelection(xBox)
		closeDialog()
		if (0 == length(x)) {
			errorCondition(recall = linePlot, message = gettextPMgui("No x variable selected."))
			return()
		}
		if (0 == length(y)) {
			errorCondition(recall = linePlot, message = gettextPMgui("No y variables selected."))
			return()
		}
		if (is.element(x, y)) {
			errorCondition(recall = linePlot, message = gettextPMgui("x and y variables must be different."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		.x <- na.omit(eval(parse(text = paste(.activeDataSet, 
										"$", x, sep = "")), envir = .GlobalEnv))
		if (!identical(order(.x), seq(along.with = .x))) {
			response <- tclvalue(PMguiTkmessageBox(message = gettextPMgui("x-values are not in order.\nContinue?"), 
							icon = "warning", type = "okcancel", default = "cancel"))
			if (response == "cancel") {
				onCancel()
				return()
			}
		}
		axisLabel <- tclvalue(axisLabelVariable)
		legend <- tclvalue(legendVariable) == "1"
		putDialog ("linePlot", list(initial.x = x, initial.y = y, initial.axisLabel = axisLabel, 
						initial.legend = legend))
		if (axisLabel == gettextPMgui("<use y-variable names>")) {
			axisLabel <- if (legend) 
						""
					else if (length(y) == 1) 
						y
					else paste(paste("(", 1:length(y), ") ", y, sep = ""), 
								collapse = ", ")
		}
		pch <- if (length(y) == 1) 
					", pch=1"
				else ""
		if (legend && length(y) > 1) {
			mar <- par("mar")
			top <- 3.5 + length(y)
			command <- paste(".mar <- par(mar=c(", mar[1], ",", 
					mar[2], ",", top, ",", mar[4], "))", sep = "")
			logger(command)
			justDoIt(command)
		}
		command <- paste("matplot(", .activeDataSet, "$", x, 
				", ", .activeDataSet, "[, ", paste("c(", paste(paste("\"", 
										y, "\"", sep = ""), collapse = ","), ")", sep = ""), 
				"], type=\"b\", lty=1, ylab=\"", axisLabel, "\"", 
				pch, ")", sep = "")
		logger(command)
		justDoIt(command)
		if (legend && length(y) > 1) {
			n <- length(y)
			cols <- rep(1:6, 1 + n%/%6)[1:n]
			logger(".xpd <- par(xpd=TRUE)")
			justDoIt(".xpd <- par(xpd=TRUE)")
			usr <- par("usr")
			command <- paste("legend(", usr[1], ", ", usr[4] + 
							1.2 * top * strheight("x"), ", legend=", paste("c(", 
							paste(paste("\"", y, "\"", sep = ""), collapse = ","), 
							")", sep = ""), ", col=c(", paste(cols, collapse = ","), 
					"), lty=1, pch=c(", paste(paste("\"", as.character(1:n), 
									"\"", sep = ""), collapse = ","), "))", sep = "")
			logger(command)
			justDoIt(command)
			logger("par(mar=.mar)")
			justDoIt("par(mar=.mar)")
			logger("par(xpd=.xpd)")
			justDoIt("par(xpd=.xpd)")
		}
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "matplot", reset = "linePlot")
	tkgrid(getFrame(xBox), labelPMgui(variablesFrame, text = "    "), 
			getFrame(yBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "nw")
	tkgrid(labelPMgui(axisLabelFrame, text = gettextPMgui("Label for y-axis"), 
					fg = "blue"), sticky = "w")
	tkgrid(axisLabelEntry, sticky = "w")
	tkgrid(axisLabelScroll, sticky = "ew")
	tkgrid(axisLabelFrame, sticky = "w")
	tkgrid(labelPMgui(legendFrame, text = gettextPMgui("Plot legend")), 
			legendCheckBox, sticky = "w")
	tkgrid(legendFrame, sticky = "w")
	tkgrid(buttonsFrame, stick = "w")
	dialogSuffix(rows = 4, columns = 1)
}

QQPlot <- function () {
# this function modified by Martin Maechler
	require("car")
	defaults <- list(initial.x = NULL, initial.identify = 0, initial.dist = "norm", initial.df = "",
			initial.chisqdf = "", initial.fdf1 = "", initial.fdf2 = "", initial.othername = "", 
			initial.otherparam = "")
	dialog.values <- getDialog("QQPlot", defaults)
	initializeDialog(title = gettextPMgui("Quantile-Comparison (QQ) Plot"))
	xBox <- variableListBox(top, Numeric(), title = gettextPMgui("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	onOK <- function() {
		x <- getSelection(xBox)
		initial.dist <-dist <- tclvalue(distVariable)
		identify <- tclvalue(identifyVariable)
		tdf <- tclvalue(tDfVariable)
		chisqdf <- tclvalue(chisqDfVariable)
		fdf1 <- tclvalue(FDf1Variable)
		fdf2 <- tclvalue(FDf2Variable)
		othername <- tclvalue(otherNameVariable)
		otherparam <- tclvalue(otherParamsVariable)
		putDialog ("QQPlot", list (initial.x = x, initial.dist = initial.dist,
						initial.identify = identify, initial.df = tdf, initial.chisqdf = chisqdf,
						initial.fdf1 = fdf1, initial.fdf2 = fdf2, initial.othername = othername, 
						initial.otherparam = otherparam))
		closeDialog()
		if (0 == length(x)) {
			errorCondition(recall = QQPlot, message = gettextPMgui("You must select a variable."))
			return()
		}
		save <- options(warn = -1)
		on.exit(save)
		retryMe <- function(msg) {
			Message(message = msg, type = "error")
			QQPlot()
		}
		switch(dist, norm = {
					args <- "dist=\"norm\""
				}, t = {
					df <- tclvalue(tDfVariable)
					df.num <- as.numeric(df)
					if (is.na(df.num) || df.num < 1) {
						retryMe(gettextPMgui("df for t must be a positive number."))
						return()
					}
					args <- paste("dist=\"t\", df=", df, sep = "")
				}, chisq = {
					df <- tclvalue(chisqDfVariable)
					df.num <- as.numeric(df)
					if (is.na(df.num) || df.num < 1) {
						retryMe(gettextPMgui("df for chi-square must be a positive number."))
						return()
					}
					args <- paste("dist=\"chisq\", df=", df, sep = "")
				}, f = {
					df1 <- tclvalue(FDf1Variable)
					df2 <- tclvalue(FDf2Variable)
					df.num1 <- as.numeric(df1)
					df.num2 <- as.numeric(df2)
					if (is.na(df.num1) || df.num1 < 1 || is.na(df.num2) || 
							df.num2 < 1) {
						retryMe(gettextPMgui("numerator and denominator \ndf for F must be positive numbers."))
						return()
					}
					args <- paste("dist=\"f\", df1=", df1, ", df2=", 
							df2, sep = "")
				}, {
					dist <- tclvalue(otherNameVariable)
					params <- tclvalue(otherParamsVariable)
					args <- paste("dist=\"", dist, "\", ", params, sep = "")
				})
		.activeDataSet <- ActiveDataSet()
		if ("1" == tclvalue(identifyVariable)) {
			PMguiTkmessageBox(title = "Identify Points", message = paste(gettextPMgui("Use left mouse button to identify points,\n"), 
							gettextPMgui(if (MacOSXP()) 
												"esc key to exit."
											else "right button to exit."), sep = ""), icon = "info", 
					type = "ok")
			idtext <- paste(", labels=rownames(", .activeDataSet, 
					"), id.method=\"identify\"", sep = "")
		}
		else idtext <- ""
		command <- paste("qqPlot", "(", .activeDataSet, "$", 
				x, ", ", args, idtext, ")", sep = "")
		doItAndPrint(command)
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "qqPlot", reset = "QQPlot")
	distFrame <- tkframe(top)
	distVariable <- tclVar(dialog.values$initial.dist)
	normalButton <- ttkradiobutton(distFrame, variable = distVariable, 
			value = "norm")
	tButton <- ttkradiobutton(distFrame, variable = distVariable, 
			value = "t")
	chisqButton <- ttkradiobutton(distFrame, variable = distVariable, 
			value = "chisq")
	FButton <- ttkradiobutton(distFrame, variable = distVariable, 
			value = "f")
	otherButton <- ttkradiobutton(distFrame, variable = distVariable, 
			value = "other")
	tDfFrame <- tkframe(distFrame)
	tDfVariable <- tclVar(dialog.values$initial.df)
	tDfField <- ttkentry(tDfFrame, width = "6", textvariable = tDfVariable)
	chisqDfFrame <- tkframe(distFrame)
	chisqDfVariable <- tclVar(dialog.values$initial.chisqdf)
	chisqDfField <- ttkentry(chisqDfFrame, width = "6", textvariable = chisqDfVariable)
	FDfFrame <- tkframe(distFrame)
	FDf1Variable <- tclVar(dialog.values$initial.fdf1)
	FDf1Field <- ttkentry(FDfFrame, width = "6", textvariable = FDf1Variable)
	FDf2Variable <- tclVar(dialog.values$initial.fdf2)
	FDf2Field <- ttkentry(FDfFrame, width = "6", textvariable = FDf2Variable)
	otherParamsFrame <- tkframe(distFrame)
	otherParamsVariable <- tclVar(dialog.values$initial.otherparam)
	otherParamsField <- ttkentry(otherParamsFrame, width = "30", 
			textvariable = otherParamsVariable)
	otherNameVariable <- tclVar(dialog.values$initial.othername)
	otherNameField <- ttkentry(otherParamsFrame, width = "10", 
			textvariable = otherNameVariable)
	identifyVariable <- tclVar(dialog.values$initial.identify)
	identifyFrame <- tkframe(top)
	identifyCheckBox <- tkcheckbutton(identifyFrame, variable = identifyVariable)
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelPMgui(identifyFrame, text = gettextPMgui("Identify observations with mouse")), 
			identifyCheckBox, sticky = "w")
	tkgrid(identifyFrame, sticky = "w")
	tkgrid(labelPMgui(distFrame, text = gettextPMgui("Distribution"), 
					fg = "blue"), columnspan = 6, sticky = "w")
	tkgrid(labelPMgui(distFrame, text = gettextPMgui("Normal")), 
			normalButton, sticky = "w")
	tkgrid(labelPMgui(tDfFrame, text = gettextPMgui("df = ")), 
			tDfField, sticky = "w")
	tkgrid(labelPMgui(distFrame, text = "t"), tButton, tDfFrame, 
			sticky = "w")
	tkgrid(labelPMgui(chisqDfFrame, text = gettextPMgui("df = ")), 
			chisqDfField, sticky = "w")
	tkgrid(labelPMgui(distFrame, text = gettextPMgui("Chi-square")), 
			chisqButton, chisqDfFrame, sticky = "w")
	tkgrid(labelPMgui(FDfFrame, text = gettextPMgui("Numerator df = ")), 
			FDf1Field, labelPMgui(FDfFrame, text = gettextPMgui("Denominator df = ")), 
			FDf2Field, sticky = "w")
	tkgrid(labelPMgui(distFrame, text = "F"), FButton, FDfFrame, 
			sticky = "w")
	tkgrid(labelPMgui(otherParamsFrame, text = gettextPMgui("Specify: ")), 
			otherNameField, labelPMgui(otherParamsFrame, text = gettextPMgui("Parameters: ")), 
			otherParamsField, sticky = "w")
	tkgrid(labelPMgui(distFrame, text = gettextPMgui("Other")), 
			otherButton, otherParamsFrame, sticky = "w")
	tkgrid(distFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 5, columns = 1)
}

PlotMeans <- function () {
	defaults <- list(initial.groups = NULL, initial.response = NULL, initial.error.bars = "se",
			initial.level = "0.95") 
	dialog.values <- getDialog("PlotMeans", defaults)
	initializeDialog(title = gettextPMgui("Plot Means"))
	groupBox <- variableListBox(top, Factors(), title = gettextPMgui("Factors (pick one or two)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.groups, "factor"))
	responseBox <- variableListBox(top, Numeric(), title = gettextPMgui("Response Variable (pick one)"),
			initialSelection = varPosn (dialog.values$initial.response, "numeric"))
	onOK <- function() {
		groups <- getSelection(groupBox)
		response <- getSelection(responseBox)
		closeDialog()
		if (0 == length(groups)) {
			errorCondition(recall = PlotMeans, message = gettextPMgui("No factors selected."))
			return()
		}
		if (2 < length(groups)) {
			errorCondition(recall = PlotMeans, message = gettextPMgui("More than two factors selected."))
			return()
		}
		if (0 == length(response)) {
			errorCondition(recall = PlotMeans, message = gettextPMgui("No response variable selected."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		error.bars <- tclvalue(errorBarsVariable)
		level <- if (error.bars == "conf.int") 
					paste(", level=", tclvalue(levelVariable), sep = "")
				else ""
		putDialog ("PlotMeans", list(initial.groups = groups, initial.response = response, 
						initial.error.bars = error.bars, initial.level = tclvalue(levelVariable)))
		if (length(groups) == 1) 
			doItAndPrint(paste("plotMeans(", .activeDataSet, 
							"$", response, ", ", .activeDataSet, "$", groups[1], 
							", error.bars=\"", error.bars, "\"", level, ")", 
							sep = ""))
		else {
			if (eval(parse(text = paste("length(levels(", .activeDataSet, 
									"$", groups[1], ")) < length(levels(", .activeDataSet, 
									"$", groups[2], "))", sep = "")))) 
				groups <- rev(groups)
			doItAndPrint(paste("plotMeans(", .activeDataSet, 
							"$", response, ", ", .activeDataSet, "$", groups[1], 
							", ", .activeDataSet, "$", groups[2], ", error.bars=\"", 
							error.bars, "\"", level, ")", sep = ""))
		}
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	optionsFrame <- tkframe(top)
	errorBarsVariable <- tclVar(dialog.values$initial.error.bars)
	seButton <- ttkradiobutton(optionsFrame, variable = errorBarsVariable, 
			value = "se")
	sdButton <- ttkradiobutton(optionsFrame, variable = errorBarsVariable, 
			value = "sd")
	confIntButton <- ttkradiobutton(optionsFrame, variable = errorBarsVariable, 
			value = "conf.int")
	noneButton <- ttkradiobutton(optionsFrame, variable = errorBarsVariable, 
			value = "none")
	levelVariable <- tclVar(dialog.values$initial.level)
	levelEntry <- ttkentry(optionsFrame, width = "6", textvariable = levelVariable)
	buttonsFrame <- tkframe(top)
	OKCancelHelp(helpSubject = "plotMeans", reset = "PlotMeans")
	tkgrid(getFrame(groupBox), getFrame(responseBox), sticky = "nw")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Error Bars"), 
					fg = "blue"), sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Standard errors")), 
			seButton, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Standard deviations")), 
			sdButton, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Confidence intervals")), 
			confIntButton, labelPMgui(optionsFrame, text = gettextPMgui("   Level of confidence:")), 
			levelEntry, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("No error bars")), 
			noneButton, sticky = "w")
	tkgrid(optionsFrame, columnspan = 2, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 3, columns = 2)
}

Scatter3D <- function () {
	use.rgl <- options("PMgui")[[1]]$use.rgl
	if (length(use.rgl) == 0 || use.rgl) {
		Library("car")
		Library("rgl")
		Library("mgcv")
	}
	defaults <- list (initial.x = NULL, initial.y = NULL, initial.scales = 1, initial.grid = 1, 
			initial.resids = 0, initial.lin = 1, initial.quad = 0, initial.nonpar = 0, 
			initial.additive = 0, initial.ellips = 0, initial.dfNonpar = gettextPMgui ("<auto>"), 
			initial.dfAdd = gettextPMgui ("<auto>"), initial.bg = "white", initial.identify = 0,
			initialGroup=NULL, initial.lines.by.group=0)
	dialog.values <- getDialog ("Scatter3D", defaults)
	initial.group <- dialog.values$initial.group
	.linesByGroup <- if (dialog.values$initial.lines.by.group == 1) TRUE else FALSE
	.groups <- if (is.null(initial.group)) FALSE else initial.group
	initializeDialog(title = gettextPMgui("3D Scatterplot"))
	variablesFrame <- tkframe(top)
	.numeric <- Numeric()
	xBox <- variableListBox(variablesFrame, .numeric, title = gettextPMgui("Explanatory variables (pick two)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	yBox <- variableListBox(variablesFrame, .numeric, title = gettextPMgui("Response variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.y, "numeric"))
	surfacesFrame <- tkframe(top)
	identifyPoints <- tclVar(dialog.values$initial.identify)
	identifyPointsCheckBox <- tkcheckbutton(surfacesFrame, variable = identifyPoints)
	axisScales <- tclVar(dialog.values$initial.scales)
	axisScalesCheckBox <- tkcheckbutton(surfacesFrame, variable = axisScales)
	gridLines <- tclVar(dialog.values$initial.grid)
	gridLinesCheckBox <- tkcheckbutton(surfacesFrame, variable = gridLines)
	squaredResiduals <- tclVar(dialog.values$initial.resids)
	squaredResidualsCheckBox <- tkcheckbutton(surfacesFrame, 
			variable = squaredResiduals)
	linearLSSurface <- tclVar(dialog.values$initial.lin)
	linearLSCheckBox <- tkcheckbutton(surfacesFrame, variable = linearLSSurface)
	quadLSSurface <- tclVar(dialog.values$initial.quad)
	quadLSCheckBox <- tkcheckbutton(surfacesFrame, variable = quadLSSurface)
	nonparSurface <- tclVar(dialog.values$initial.nonpar)
	nonparCheckBox <- tkcheckbutton(surfacesFrame, variable = nonparSurface)
	dfNonparVariable <- tclVar(dialog.values$initial.dfNonpar)
	dfNonparField <- ttkentry(surfacesFrame, width = "6", textvariable = dfNonparVariable)
	additiveSurface <- tclVar(dialog.values$initial.additive)
	additiveCheckBox <- tkcheckbutton(surfacesFrame, variable = additiveSurface)
	dfAddVariable <- tclVar(dialog.values$initial.dfAdd)
	dfAddField <- ttkentry(surfacesFrame, width = "6", textvariable = dfAddVariable)
	ellipsoid <- tclVar(dialog.values$initial.ellips)
	ellipsoidCheckBox <- tkcheckbutton(surfacesFrame, variable = ellipsoid)
	bgFrame <- tkframe(top)
	bgVariable <- tclVar(dialog.values$initial.bg)
	whiteButton <- ttkradiobutton(bgFrame, variable = bgVariable, 
			value = "white")
	blackButton <- ttkradiobutton(bgFrame, variable = bgVariable, 
			value = "black")
	onOK <- function() {
		x <- getSelection(xBox)
		y <- getSelection(yBox)
		scales <- tclvalue(axisScales)
		grid <- tclvalue(gridLines)
		resids <- tclvalue(squaredResiduals)
		lin <- tclvalue(linearLSSurface)
		quad <- tclvalue(quadLSSurface)
		nonpar <- tclvalue(nonparSurface)
		additive <- tclvalue(additiveSurface)
		ellips <- tclvalue(ellipsoid) 
		dfNonpar <- tclvalue(dfNonparVariable)
		dfAdd <- tclvalue(dfAddVariable)
		bg <- tclvalue(bgVariable)
		identify <- tclvalue(identifyPoints)
		putDialog ("Scatter3D", list (initial.x = x, initial.y = y, initial.scales = scales, initial.grid = grid, 
						initial.resids = resids, initial.lin = lin, initial.quad = quad, initial.nonpar = nonpar, 
						initial.additive = additive, initial.ellips = ellips, initial.dfNonpar = dfNonpar, 
						initial.dfAdd = dfAdd, initial.bg = bg, initial.identify = identify,
						initial.group=if (.groups == FALSE) NULL else .groups,
						initial.lines.by.group=if (.linesByGroup) 1 else 0))
		closeDialog()
		if (length(y) == 0) {
			errorCondition(recall = Scatter3D, message = gettextPMgui("You must select a response variable."))
			return()
		}
		if (2 != length(x)) {
			errorCondition(recall = Scatter3D, message = gettextPMgui("You must select 2 explanatory variables."))
			return()
		}
		if (is.element(y, x)) {
			errorCondition(recall = Scatter3D, message = gettextPMgui("Response and explanatory variables must be different."))
			return()
		}
		scales <- if (tclvalue(axisScales) == 1) 
					"TRUE"
				else "FALSE"
		grid <- if (tclvalue(gridLines) == 1) 
					"TRUE"
				else "FALSE"
		resids <- if (tclvalue(squaredResiduals) == 1) 
					", residuals=\"squares\""
				else ", residuals=TRUE"
		lin <- if (tclvalue(linearLSSurface) == 1) 
			"\"linear\""
		quad <- if (tclvalue(quadLSSurface) == 1) 
			"\"quadratic\""
		nonpar <- if (tclvalue(nonparSurface) == 1) 
			"\"smooth\""
		additive <- if (tclvalue(additiveSurface) == 1) 
			"\"additive\""
		surfaces <- c(lin, quad, nonpar, additive)
		nsurfaces <- length(surfaces)
		if (nsurfaces > 1) 
			resids <- ""
		ellips <- if (tclvalue(ellipsoid) == 1) 
					"TRUE"
				else "FALSE"
		opts <- options(warn = -1)
		dfNonpar <- if (dfNonpar == gettextPMgui("<auto>")) 
					""
				else paste(", df.smooth=", as.numeric(dfNonpar), sep = "")
		dfAdd <- if (dfAdd == gettextPMgui("<auto>")) 
					""
				else paste(", df.additive=", as.numeric(dfAdd), sep = "")
		options(opts)
		fit <- if (nsurfaces == 0) 
					", surface=FALSE"
				else if (nsurfaces == 1) 
					paste(", fit=", surfaces, sep = "")
				else paste(", fit=c(", paste(surfaces, collapse = ","), 
							")", sep = "")
		.activeDataSet <- ActiveDataSet()
		if (.groups != FALSE) {
			groups <- paste(", groups=", .activeDataSet, "$", 
					.groups, sep = "")
			parallel <- paste(", parallel=", .linesByGroup, sep = "")
		}
		else groups <- parallel <- ""
		command <- paste("scatter3d(", .activeDataSet, "$", x[1], 
				", ", .activeDataSet, "$", y, ", ", .activeDataSet, 
				"$", x[2], fit, resids, dfNonpar, dfAdd, groups, 
				parallel, ", bg=\"", bg, "\", axis.scales=", scales, 
				", grid=", grid, ", ellipsoid=", ellips, ", xlab=\"", 
				x[1], "\", ylab=\"", y, "\", zlab=\"", x[2], "\")", 
				sep = "")
		doItAndPrint(command)
		putPMgui("rgl", TRUE)
		command <- paste("identify3d(", .activeDataSet, "$", 
				x[1], ", ", .activeDataSet, "$", y, ", ", .activeDataSet, 
				"$", x[2], groups, ", axis.scales=", scales, ", labels=row.names(", 
				.activeDataSet, "))", sep = "")
		putPMgui("Identify3d", command)
		.Tcl("update")
		if (tclvalue(identifyPoints) == 1) {
			PMguiTkmessageBox(title = "Identify Points", message = paste(gettextPMgui("Use left mouse button to identify points,\n"), 
							gettextPMgui(if (MacOSXP()) 
												"esc key to exit."
											else "right button to exit."), sep = ""), icon = "info", 
					type = "ok")
			doItAndPrint(command)
		}
		activateMenus()
		tkfocus(PmetricsWindow())
		rgl.bringtotop()
	}
	groupsBox(Scatter3D, plotLinesByGroup = TRUE, plotLinesByGroupsText = gettextPMgui("Parallel regression surfaces"),
			initialGroup=initial.group, initialLinesByGroup=dialog.values$initial.lines.by.group,
			initialLabel=if (is.null(initial.group)) gettextPMgui("Plot by groups") else paste(gettextPMgui("Plot by:"), initial.group))
	OKCancelHelp(helpSubject = "Scatter3DDialog", reset = "Scatter3D")
	tkgrid(getFrame(yBox), labelPMgui(variablesFrame, text = "  "), 
			getFrame(xBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "nw")
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Identify observations\nwith mouse")), 
			identifyPointsCheckBox, sticky = "w")
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Show axis scales")), 
			axisScalesCheckBox, sticky = "w")
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Show surface grid lines")), 
			gridLinesCheckBox, sticky = "w")
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Show squared residuals")), 
			squaredResidualsCheckBox, sticky = "w")
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Surfaces to Fit"), 
					fg = "blue"), sticky = "w")
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Linear least-squares")), 
			linearLSCheckBox, sticky = "w")
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Quadratic least-squares")), 
			quadLSCheckBox, sticky = "w")
	dfLabel <- labelPMgui(surfacesFrame, text = gettextPMgui("df = "))
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Smooth regression")), 
			nonparCheckBox, dfLabel, dfNonparField, sticky = "w")
	tkgrid.configure(dfLabel, sticky = "e")
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Additive regression")), 
			additiveCheckBox, labelPMgui(surfacesFrame, text = gettextPMgui("df(each term) = ")), 
			dfAddField, sticky = "w")
	tkgrid(labelPMgui(surfacesFrame, text = gettextPMgui("Plot 50% concentration ellipsoid")), 
			ellipsoidCheckBox, sticky = "w")
	tkgrid(surfacesFrame, sticky = "w")
	tkgrid(labelPMgui(bgFrame, text = gettextPMgui("Background Color"), 
					fg = "blue"), sticky = "w", columnspan = 2)
	tkgrid(labelPMgui(bgFrame, text = gettextPMgui("Black")), 
			blackButton, sticky = "w")
	tkgrid(labelPMgui(bgFrame, text = gettextPMgui("White")), 
			whiteButton, sticky = "w")
	tkgrid(bgFrame, sticky = "w")
	tkgrid(groupsFrame, sticky = "w")
	tkgrid(buttonsFrame, stick = "w")
	dialogSuffix(rows = 5, columns = 1)
}

Identify3D <- function(){
	if (0 == rgl.cur()) {
		Message(message=gettextPMgui("There is no current RGL graphics device."),
				type="error")
		return()
	}
	PMguiTkmessageBox(title="Identify Points",
			message=gettextPMgui("Drag right mouse button to identify points,\nclick right button to exit."),
			icon="info", type="ok")
	command <- getPMgui("Identify3d")
	doItAndPrint(command)
}

saveBitmap <- function () {
	if (1 == dev.cur()) {
		Message(gettextPMgui("There is no current graphics device to save."), 
				type = "error")
		return()
	}
	defaults <- list (initial.width = 500, initial.height = 500, initial.type = "png")
	dialog.values <- getDialog ("saveBitmap", defaults)
	initializeDialog(title = gettextPMgui("Save Graph as Bitmap"))
	radioButtons(name = "filetype", buttons = c("png", "jpeg"), 
			labels = c("PNG", "JPEG"), title = gettextPMgui("Graphics File Type"),
			initialValue = dialog.values$initial.type)
	sliderFrame <- tkframe(top)
	widthVariable <- tclVar(dialog.values$initial.width)
	widthSlider <- tkscale(sliderFrame, from = 200, to = 1000, 
			showvalue = TRUE, variable = widthVariable, resolution = 25, 
			orient = "horizontal")
	heightVariable <- tclVar(dialog.values$initial.height)
	heightSlider <- tkscale(sliderFrame, from = 200, to = 1000, 
			showvalue = TRUE, variable = heightVariable, resolution = 25, 
			orient = "horizontal")
	onOK <- function() {
		closeDialog()
		width <- tclvalue(widthVariable)
		height <- tclvalue(heightVariable)
		type <- tclvalue(filetypeVariable)
		putDialog ("saveBitmap", list (initial.width = width, initial.height = height, initial.type = type))
		if (type == "png") {
			ext <- "png"
			filetypes <- gettextPMgui("{\"All Files\" {\"*\"}} {\"PNG Files\" {\".png\" \".PNG\"}}")
			initial <- "RGraph.png"
		}
		else {
			ext <- "jpg"
			filetypes <- gettextPMgui("{\"All Files\" {\"*\"}} {\"JPEG Files\" {\".jpg\" \".JPG\" \".jpeg\" \".JPEG\"}}")
			initial <- "RGraph.jpg"
		}
		filename <- tclvalue(tkgetSaveFile(filetypes = filetypes, 
						defaultextension = ext, initialfile = initial, parent = PmetricsWindow()))
		if (filename == "") 
			return()
		command <- paste("dev.print(", type, ", filename=\"", 
				filename, "\", width=", width, ", height=", height, 
				")", sep = "")
		doItAndPrint(command)
		Message(paste(gettextPMgui("Graph saved to file"), filename), 
				type = "note")
	}
	OKCancelHelp(helpSubject = "png", reset = "saveBitmap")
	tkgrid(filetypeFrame, sticky = "w")
	tkgrid(labelPMgui(sliderFrame, text = gettextPMgui("Width (pixels)")), 
			widthSlider, sticky = "sw")
	tkgrid(labelPMgui(sliderFrame, text = gettextPMgui("Height (pixels)")), 
			heightSlider, sticky = "sw")
	tkgrid(sliderFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1)
}

savePDF <- function () {
	if (1 == dev.cur()) {
		Message(gettextPMgui("There is no current graphics device to save."), 
				type = "error")
		return()
	}
	defaults <- list (initial.width = 5.0, initial.height = 5.0, initial.type = "pdf", initial.pointsize = 10)
	dialog.values <- getDialog ("savePDF", defaults)
	initializeDialog(title = gettextPMgui("Save Graph as PDF/Postscript"))
	radioButtons(name = "filetype", buttons = c("pdf", "postscript", 
					"eps"), labels = gettextPMgui(c("PDF", "Postscript", 
							"Encapsulated Postscript")), title = gettextPMgui("Graphics File Type"), 
			initialValue = dialog.values$initial.type)
	sliderFrame <- tkframe(top)
	widthVariable <- tclVar(dialog.values$initial.width)
	widthSlider <- tkscale(sliderFrame, from = 3, to = 10, showvalue = TRUE, 
			variable = widthVariable, resolution = 0.1, orient = "horizontal")
	heightVariable <- tclVar(dialog.values$initial.height)
	heightSlider <- tkscale(sliderFrame, from = 3, to = 10, showvalue = TRUE, 
			variable = heightVariable, resolution = 0.1, orient = "horizontal")
	pointSizeVariable <- tclVar(dialog.values$initial.pointsize)
	pointSizeSlider <- tkscale(sliderFrame, from = 6, to = 14, 
			showvalue = TRUE, variable = pointSizeVariable, resolution = 1, 
			orient = "horizontal")
	onOK <- function() {
		closeDialog()
		width <- tclvalue(widthVariable)
		height <- tclvalue(heightVariable)
		type <- tclvalue(filetypeVariable)
		pointsize <- tclvalue(pointSizeVariable)
		putDialog ("savePDF", list (initial.width = width, initial.height = height, initial.type = type,
						initial.pointsize = pointsize))
		if (type == "pdf") {
			ext <- "pdf"
			filetypes <- gettextPMgui("{\"All Files\" {\"*\"}} {\"PDF Files\" {\".pdf\" \".PDF\"}}")
			initial <- "RGraph.pdf"
		}
		else if (type == "postscript") {
			ext <- "ps"
			filetypes <- gettextPMgui("{\"All Files\" {\"*\"}} {\"Postscript Files\" {\".ps\" \".PS\"}}")
			initial <- "RGraph.ps"
		}
		else {
			ext <- "eps"
			filetypes <- gettextPMgui("{\"All Files\" {\"*\"}} {\"Encapsulated Postscript Files\" {\".eps\" \".EPS\"}}")
			initial <- "RGraph.eps"
		}
		filename <- tclvalue(tkgetSaveFile(filetypes = filetypes, 
						defaultextension = ext, initialfile = initial, parent = PmetricsWindow()))
		if (filename == "") 
			return()
		command <- if (type == "eps") 
					paste("dev.copy2eps(file=\"", filename, "\", width=", 
							width, ", height=", height, ", pointsize=", pointsize, 
							")", sep = "")
				else paste("dev.print(", type, ", file=\"", filename, 
							"\", width=", width, ", height=", height, ", pointsize=", 
							pointsize, ")", sep = "")
		doItAndPrint(command)
		Message(paste(gettextPMgui("Graph saved to file"), filename), 
				type = "note")
	}
	OKCancelHelp(helpSubject = "pdf", reset = "savePDF")
	tkgrid(filetypeFrame, sticky = "w")
	tkgrid(labelPMgui(sliderFrame, text = gettextPMgui("Width (inches)")), 
			widthSlider, sticky = "sw")
	tkgrid(labelPMgui(sliderFrame, text = gettextPMgui("Height (inches)")), 
			heightSlider, sticky = "sw")
	tkgrid(labelPMgui(sliderFrame, text = gettextPMgui("Text size (points)")), 
			pointSizeSlider, sticky = "sw")
	tkgrid(sliderFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1)
}

saveRglGraph <- function(){
	if (0 == rgl.cur()) {
		Message(message=gettextPMgui("There is no current RGL graphics device to save."),
				type="error")
		return()
	}
	ext <- "png"
	filetypes <- gettextPMgui('{"All Files" {"*"}} {"PNG Files" {".png" ".PNG"}}')
	initial <- "RGLGraph.png"
	filename <- tclvalue(tkgetSaveFile(filetypes=filetypes,
					defaultextension=ext,
					initialfile=initial,
					parent=PmetricsWindow()))
	if (filename == "") return()
	command <- paste('rgl.snapshot("', filename, '")', sep="")
	doItAndPrint(command)
	Message(paste(gettextPMgui("Graph saved to file"), filename), type="note")
}

## The following function by Richard Heiberger, with small modifications by J. Fox
## with more modifications by Richard Heiberger.
## 2008-01-03 added conditions, layout, and multiple colors

Xyplot <- function () {
	Library("lattice")
	defaults <- list(initial.predictor = NULL, initial.response = NULL, initial.auto.key = 1, 
			initial.outer = 0, initial.x.relation = "same", initial.y.relation = "same",
			initial.layoutColumns = "", initial.layoutRows = "", initial.conditions = FALSE,
			initial.groups = FALSE) 
	dialog.values <- getDialog("Xyplot", defaults)
	initializeDialog(title = gettextPMgui("XY Conditioning Plot"))
	predictorFrame <- tkframe(top)
	predictorBox <- variableListBox(predictorFrame, Numeric(), 
			title = gettextPMgui("Explanatory variables (pick one or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.predictor, "numeric"))
	responseBox <- variableListBox(predictorFrame, Numeric(), 
			title = gettextPMgui("Response variables (pick one or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.response, "numeric"))
	cgFrame <- tkframe(top)
	conditionsBox <- variableListBox(cgFrame, Factors(), title = gettextPMgui("Conditions '|' (pick zero or more)"), 
			selectmode = "multiple", 
			initialSelection = if (dialog.values$initial.conditions == FALSE) FALSE else varPosn (dialog.values$initial.conditions, "factor"))
	groupsBox <- variableListBox(cgFrame, Factors(), title = gettextPMgui("Groups 'groups=' (pick zero or more)"), 
			selectmode = "multiple", 
			initialSelection = if (dialog.values$initial.groups == FALSE) FALSE else varPosn (dialog.values$initial.groups, "factor"))
	checkBoxes(frame = "optionsFrame", boxes = c("auto.key", 
					"outer"), initialValues = c(dialog.values$initial.auto.key, dialog.values$initial.outer), 
			labels = gettextPMgui(c("Automatically draw key", "Different panels for different y~x combinations")))
	relationFrame <- tkframe(top)
	radioButtons(window = relationFrame, name = "x.relation", 
			buttons = c("same", "free", "sliced"), labels = gettextPMgui(c("Identical", 
							"Free", "Same range")), title = gettextPMgui("X-Axis Scales in Different Panels"), 
			initialValue = dialog.values$initial.x.relation)
	radioButtons(window = relationFrame, name = "y.relation", 
			buttons = c("same", "free", "sliced"), labels = gettextPMgui(c("Identical", 
							"Free", "Same range")), title = gettextPMgui("Y-Axis Scales in Different Panels"), 
			initialValue = dialog.values$initial.y.relation)
	scalarsFrame <- tkframe(top)
	layoutColumnsVar <- tclVar(dialog.values$initial.layoutColumns)
	layoutColumnsEntry <- tkentry(scalarsFrame, width = "6", 
			textvariable = layoutColumnsVar)
	layoutRowsVar <- tclVar(dialog.values$initial.layoutRows)
	layoutRowsEntry <- tkentry(scalarsFrame, width = "6", textvariable = layoutRowsVar)
	onOK <- function() {
		predictor <- getSelection(predictorBox)
		response <- getSelection(responseBox)
		conditions <- getSelection(conditionsBox)
		groups <- getSelection(groupsBox)
		closeDialog()
		if (0 == length(response)) {
			errorCondition(recall = Xyplot.HH, message = gettextPMgui("At least one response variable must be selected."))
			return()
		}
		if (0 == length(predictor)) {
			errorCondition(recall = Xyplot.HH, message = gettextPMgui("At least one explanatory variable must be selected."))
			return()
		}
		auto.key <- ("1" == tclvalue(auto.keyVariable))
		outer <- ("1" == tclvalue(outerVariable))
		x.relation <- as.character(tclvalue(x.relationVariable))
		y.relation <- as.character(tclvalue(y.relationVariable))
		layoutColumns <- as.numeric(tclvalue(layoutColumnsVar))
		layoutRows <- as.numeric(tclvalue(layoutRowsVar))
		putDialog ("Xyplot", list(initial.predictor = predictor, initial.response = response, 
						initial.auto.key = auto.key, initial.outer = outer, initial.x.relation = x.relation, 
						initial.y.relation = y.relation, initial.layoutColumns = tclvalue(layoutColumnsVar), 
						initial.layoutRows = tclvalue(layoutRowsVar), initial.conditions = if (length(conditions) != 0) conditions else FALSE, 
						initial.groups = if (length(groups) != 0) groups else FALSE))
		layout.command <- ""
		number.na <- is.na(layoutColumns) + is.na(layoutRows)
		if (number.na == 1) {
			errorCondition(recall = Xyplot.HH, message = gettextPMgui("Both or neither layout values must be numbers."))
			return()
		}
		if (number.na == 0) 
			layout.command <- deparse(c(layoutColumns, layoutRows))
		.activeDataSet <- ActiveDataSet()
		condtions.command <- if (length(conditions) == 0) {
					if (outer) {
						if (layout.command == "") 
							paste(", layout=c(", length(predictor), ",", 
									length(response), ")")
						else paste(", layout=", layout.command, sep = "")
					}
				}
				else {
					if (outer) {
						condition.levels <- prod(sapply(conditions, d.f = get(.activeDataSet), 
										function(g, d.f) length(levels(d.f[[g]]))))
						paste(", layout=c(", condition.levels, "*", length(predictor), 
								",", length(response), ")", ", between=list(x=c(", 
								paste(rep(c(rep(0, condition.levels - 1), 1), 
												length = condition.levels * length(predictor) - 
														1), collapse = ","), "), y=1)")
					}
				}
		groups.command <- if (length(groups) == 1) 
					paste(", groups=", groups, sep = "")
				else ""
		xyplot.command <- paste("xyplot(", paste(response, collapse = " + "), 
				" ~ ", paste(predictor, collapse = " + "), if (length(conditions) > 
								0) 
							paste(" | ", paste(conditions, collapse = " + "))
						else "", if (outer) 
					",\n outer=TRUE", condtions.command, groups.command, 
				", pch=16", if (auto.key) 
							",\n auto.key=list(border=TRUE), par.settings = simpleTheme(pch=16)"
						else "", paste(", scales=list(x=list(relation='", 
						x.relation, "'), y=list(relation='", y.relation, 
						"'))", sep = ""), ",\n data=", .activeDataSet, 
				")", sep = "")
		doItAndPrint(xyplot.command)
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "xyplot", reset = "Xyplot")
	tkgrid(getFrame(predictorBox), getFrame(responseBox), columnspan = 1, 
			sticky = "w")
	tkgrid(predictorFrame, sticky = "w")
	tkgrid(getFrame(conditionsBox), tklabel(cgFrame, text = gettextPMgui("           ")), 
			getFrame(groupsBox), columnspan = 1, sticky = "w")
	tkgrid(cgFrame, sticky = "w")
	tkgrid(tklabel(top, text = gettextPMgui("Options"), fg = "blue"), 
			sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(x.relationFrame, y.relationFrame, columnspan = 2, 
			sticky = "w")
	tkgrid(relationFrame, sticky = "w")
	tkgrid(tklabel(top, text = gettextPMgui("Layout"), fg = "blue"), 
			sticky = "w")
	tkgrid(tklabel(scalarsFrame, text = gettextPMgui("number of columns:")), 
			layoutColumnsEntry, sticky = "w")
	tkgrid(tklabel(scalarsFrame, text = gettextPMgui("number of rows:")), 
			layoutRowsEntry, sticky = "w")
	tkgrid(scalarsFrame, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 6, columns = 2)
}


# set the colour palette

setPalette <- function() {
	cval <- function(x,y) -sum((x-y)^2)
	contrasting <- function(x)
		optim(rep(127, 3),cval,lower=0,upper=255,method="L-BFGS-B",y=x)$par
	# the following local function from Thomas Lumley via r-help
	convert <- function (color){
		rgb <- col2rgb(color)/255
		L <- c(0.2, 0.6, 0) %*% rgb
		ifelse(L >= 0.2, "#000060", "#FFFFA0")
	}
	env <- environment()
	pal <- palette()
	pickColor <- function(initialcolor, parent){
		tclvalue(.Tcl(paste("tk_chooseColor", .Tcl.args(title = "Select a Color",
										initialcolor=initialcolor, parent=parent))))
	}
	initializeDialog(title=gettextPMgui("Set Color Palette"))
	hexcolor <- colorConverter(toXYZ = function(hex,...) {
				rgb <- t(col2rgb(hex))/255
				colorspaces$sRGB$toXYZ(rgb,...) },
			fromXYZ = function(xyz,...) {
				rgb <- colorspaces$sRGB$fromXYZ(xyz,..)
				rgb <- round(rgb,5)
				if (min(rgb) < 0 || max(rgb) > 1) as.character(NA)
				else rgb(rgb[1],rgb[2],rgb[3])},
			white = "D65", name = "#rrggbb")
	cols <- t(col2rgb(pal))
	hex <- convertColor(cols, from="sRGB", to=hexcolor, scale.in=255, scale.out=NULL)
	for (i in 1:8) assign(paste("hex", i, sep="."), hex[i], envir=env)
	paletteFrame <- tkframe(top)
	button1 <- tkbutton(paletteFrame, text=hex[1], bg = hex[1],
			fg=convert(hex[1]),
			command=function() {
				color <- pickColor(hex[1], parent=button1)
				fg <- convert(color)
				tkconfigure(button1, bg=color, fg=fg)
				assign("hex.1", color, envir=env)
			}
	)
	button2 <- tkbutton(paletteFrame, text=hex[2], bg = hex[2],
			fg=convert(hex[2]),
			command=function() {
				color <- pickColor(hex[2], parent=button2)
				fg <- convert(color)
				tkconfigure(button2, bg=color, fg=fg)
				assign("hex.2", color, envir=env)
			}
	)
	button3 <- tkbutton(paletteFrame, text=hex[3], bg = hex[3],
			fg=convert(hex[3]),
			command=function() {
				color <- pickColor(hex[3], parent=button3)
				fg <- convert(color)
				tkconfigure(button3, bg=color, fg=fg)
				assign("hex.3", color, envir=env)
			}
	)
	button4 <- tkbutton(paletteFrame, text=hex[4], bg = hex[4],
			fg=convert(hex[4]),
			command=function() {
				color <- pickColor(hex[4], parent=button4)
				fg <- convert(color)
				tkconfigure(button4, bg=color, fg=fg)
				assign("hex.4", color, envir=env)
			}
	)
	button5 <- tkbutton(paletteFrame, text=hex[5], bg = hex[5],
			fg=convert(hex[5]),
			command=function() {
				color <- pickColor(hex[5], parent=button5)
				fg <- convert(color)
				tkconfigure(button5, bg=color, fg=fg)
				assign("hex.5", color, envir=env)
			}
	)
	button6 <- tkbutton(paletteFrame, text=hex[6], bg = hex[6],
			fg=convert(hex[6]),
			command=function() {
				color <- pickColor(hex[6], parent=button6)
				fg <- convert(color)
				tkconfigure(button6, bg=color, fg=fg)
				assign("hex.6", color, envir=env)
			}
	)
	button7 <- tkbutton(paletteFrame, text=hex[7], bg = hex[7],
			fg=convert(hex[7]),
			command=function() {
				color <- pickColor(hex[7], parent=button7)
				fg <- convert(color)
				tkconfigure(button7, bg=color, fg=fg)
				assign("hex.7", color, envir=env)
			}
	)
	button8 <- tkbutton(paletteFrame, text=hex[8], bg = hex[8],
			fg=convert(hex[8]),
			command=function() {
				color <- pickColor(hex[8], parent=button8)
				fg <- convert(color)
				tkconfigure(button8, bg=color, fg=fg)
				assign("hex.8", color, envir=env)
			}
	)
	onOK <- function(){
		closeDialog(top)
		palette(c(hex.1, hex.2, hex.3, hex.4, hex.5, hex.6, hex.7, hex.8))
		Message(gettextPMgui("Color palette reset.", type="note"))
	}
	OKCancelHelp(helpSubject="palette")
	tkgrid(button1, button2, button3, button4, button5, button6, button7, button8)
	tkgrid(paletteFrame)
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=2)
}

stripChart <- function () {
	defaults <- list (initial.group = NULL, initial.response = NULL, initial.plotType = "stack")
	dialog.values <- getDialog("stripChart", defaults)
	initializeDialog(title = gettextPMgui("Strip Chart"))
	groupBox <- variableListBox(top, Factors(), title = gettextPMgui("Factors (pick zero or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.group, "factor"))
	responseBox <- variableListBox(top, Numeric(), title = gettextPMgui("Response Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.response, "numeric"))
	onOK <- function() {
		groups <- getSelection(groupBox)
		response <- getSelection(responseBox)
		closeDialog()
		if (0 == length(response)) {
			errorCondition(recall = stripChart, message = gettextPMgui("No response variable selected."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		plotType <- tclvalue(plotTypeVariable)
		putDialog ("stripChart", list (initial.group = groups, initial.response = response, 
						initial.plotType = plotType))
		method <- paste(", method=\"", plotType, "\"", sep = "")
		if (length(groups) == 0) 
			doItAndPrint(paste("stripchart(", .activeDataSet, 
							"$", response, method, ", xlab=\"", response, 
							"\")", sep = ""))
		else {
			groupNames <- paste(groups, collapse = "*")
			doItAndPrint(paste("stripchart(", response, " ~ ", 
							groupNames, ", vertical=TRUE", method, ", xlab=\"", 
							groupNames, "\", ylab=\"", response, "\", data=", 
							.activeDataSet, ")", sep = ""))
		}
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	radioButtons(name = "plotType", buttons = c("stack", "jitter"), 
			labels = gettextPMgui(c("Stack", "Jitter")), title = gettextPMgui("Duplicate Values"), 
			initialValue = dialog.values$initial.plotType)
	buttonsFrame <- tkframe(top)
	OKCancelHelp(helpSubject = "stripchart", reset = "stripChart")
	tkgrid(getFrame(groupBox), getFrame(responseBox), sticky = "nw")
	tkgrid(plotTypeFrame, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 3, columns = 2)
}


