# last modified 2012-01-27 by J. Fox
#  applied patch to improve window behaviour supplied by Milan Bouchet-Valat 2011-09-22

# File menu dialogs

loadLog <- function(){
	logFile <- tclvalue(tkgetOpenFile(filetypes=gettextPMgui('{"All Files" {"*"}} {"Script Files" {".R"}}'),
					defaultextension="log",
					parent=PmetricsWindow()))
	if (logFile == "") return()
	fileCon <- file(logFile, "r")
	contents <- readLines(fileCon)
	close(fileCon)
	currentLogFileName <- getPMgui("logFileName")
	putPMgui("logFileName", logFile)
	.log <- LogWindow()
	if (tclvalue(tkget(.log, "1.0", "end")) != "\n"){
		response2 <- PMguiTkmessageBox(message=gettextPMgui("Save current log file?"),
				icon="question", type="yesno", default="yes")
		if ("yes" == tclvalue(response2)) saveLog(currentLogFileName)
	}
	tkdelete(.log, "1.0", "end")
	tkinsert(.log, "end", paste(contents, collapse="\n"))
}

saveLog <- function(logfilename) {
	.logFileName <- if(missing(logfilename)) getPMgui("logFileName") else logfilename
	if (is.null(.logFileName)) {
		saveLogAs()
		return()
	}
	log <- tclvalue(tkget(LogWindow(), "1.0", "end"))
	fileCon <- file(.logFileName, "w")
	cat(log, file = fileCon)
	close(fileCon)
	Message(paste(gettextPMgui("Script saved to"), .logFileName), type="note")
}

saveLogAs <- function() {
	logFile <- tclvalue(tkgetSaveFile(filetypes=gettextPMgui('{"All Files" {"*"}} {"Script Files" {".R"}}'),
					defaultextension="R",
					initialfile="RPmetrics.R",
					parent=PmetricsWindow()))
	if (logFile == "") return()
	log <- tclvalue(tkget(LogWindow(), "1.0", "end"))
	fileCon <- file(logFile, "w")
	cat(log, file = fileCon)
	close(fileCon)
	putPMgui("logFileName", logFile)
	Message(paste(gettextPMgui("Script saved to"), logFile), type="note")
}

saveOutput <- function() {
	.outputFileName <- getPMgui("outputFileName")
	if (is.null(.outputFileName)) {
		saveOutputAs()
		return()
	}
	output <- tclvalue(tkget(OutputWindow(), "1.0", "end"))
	fileCon <- file(.outputFileName, "w")
	cat(output, file = fileCon)
	close(fileCon)
	Message(paste(gettextPMgui("Output saved to"), .outputFileName), type="note")
}

saveOutputAs <- function() {
	outputFile <- tclvalue(tkgetSaveFile(filetypes=gettextPMgui('{"All Files" {"*"}} {"Output Files" {".txt"}}'),
					defaultextension="txt",
					initialfile="RPmetrics.txt",
					parent=PmetricsWindow()))
	if (outputFile == "") return()
	output <- tclvalue(tkget(OutputWindow(), "1.0", "end"))
	fileCon <- file(outputFile, "w")
	cat(output, file = fileCon)
	close(fileCon)
	putPMgui("outputFileName", outputFile)
	Message(paste(gettextPMgui("Output saved to"), outputFile), type="note")
}

saveWorkspaceAs <- function(){
	saveFile <- tclvalue(tkgetSaveFile(filetypes=gettextPMgui('{"All Files" {"*"}} {"R Data Files" {".RData" ".rda" ".Rda" ".RDA"}}'),
					defaultextension="",
					initialfile=".RData",
					parent=PmetricsWindow()))
	if (saveFile == "") return()
	save(list=ls(envir=.GlobalEnv), file=saveFile)
	putPMgui("saveFileName", saveFile)
	Message(paste(gettextPMgui("R workspace saved to"), saveFile), type="note")
}

saveWorkspace <- function() {
	.saveFileName <- getPMgui("saveFileName")
	if (is.null(.saveFileName)) {
		saveWorkspaceAs()
		return()
	}
	else save(list=ls(envir=.GlobalEnv), file=.saveFileName)
	Message(paste(gettextPMgui("R workspace saved to"), .saveFileName), type="note")
}

ClosePmetrics <- function() closePmetrics(ask=getPMgui("ask.to.exit"), ask.save=getPMgui("ask.on.exit"))

closePmetrics <- function(ask=TRUE, ask.save=ask){
	if (ask){
		response <- tclvalue(PMguiTkmessageBox(message=gettextPMgui("Exit?"),
						icon="question", type="okcancel", default="cancel"))
		if (response == "cancel") return(invisible(response))
	}
	else {
		ask.save=FALSE
		response <- "ok"
	}
	sink(type="message")
#    if (rglLoaded()) rgl.quit()
	if (!is.null(ActiveDataSet()) && getPMgui("attach.data.set"))
		justDoIt(logger(paste("detach(", ActiveDataSet(), ")", sep="")))
	putPMgui(".activeDataSet", NULL)
	putPMgui(".activeModel", NULL)
	if (ask.save && getPMgui("log.commands") && tclvalue(tkget(LogWindow(), "1.0", "end")) != "\n"){
		response2 <- PMguiTkmessageBox(message=gettextPMgui("Save script file?"),
				icon="question", type="yesno", default="yes")
		if ("yes" == tclvalue(response2)) saveLog()
	}
	if (ask.save && !getPMgui("console.output") && tclvalue(tkget(OutputWindow(), "1.0", "end")) != "\n"){
		response3 <- PMguiTkmessageBox(message=gettextPMgui("Save output file?"),
				icon="question", type="yesno", default="yes")
		if ("yes" == tclvalue(response3)) saveOutput()
	}
	if (.Platform$OS.type != "windows") options(getPMgui("oldPager"))
	if (getPMgui("suppress.X11.warnings")) {
		sink(type = "message")
		close(getPMgui("messages.connection"))
	}
	options(getPMgui("saveOptions"))
	tkdestroy(PmetricsWindow())
	putPMgui("PmetricsWindow", NULL)
	putPMgui("logWindow", NULL)
	putPMgui("messagesWindow", NULL)
	putPMgui("outputWindow", NULL)
	options(getPMgui("quotes"))
	tkwait <- options("PMgui")[[1]]$tkwait  # to address problem in Debian Linux
	if ((!is.null(tkwait)) && tkwait) tclvalue(.Pmetrics.done) <<- "1"
	return(invisible(response))
}

closePmetricsAndR <- function(){
	response <- ClosePmetrics()
	if (response == "cancel") return()
	cat("\n")
	quit(save="no")
}

Options <- function(){
	setOption <- function(option, default) {
		if (is.null(current[[option]])) default else current[[option]]
	}
	initializeDialog(title=gettextPMgui("Pmetrics Options"))
	current <- options("PMgui")[[1]]
	console.output <- setOption("console.output", FALSE)
	log.commands <- setOption("log.commands", TRUE)
	log.font.size <- setOption("log.font.size", 10)
	log.width <- setOption("log.width", 80)
	log.height <- if (!is.null(current$log.height)) current$log.height
			else if (!log.commands) 0 else 10
	output.height <- if (!is.null(current$output.height)) current$output.height
			else if (console.output) 0 else 2*log.height
	contrasts <- setOption("default.contrasts", c("contr.Treatment", "contr.poly"))
	grab.focus <- setOption("grab.focus", TRUE)
	double.click <- setOption("double.click", FALSE)
	sort.names <- setOption("sort.names", TRUE)
	show.edit.button <- setOption("show.edit.button", TRUE)
	scale.factor <- current$scale.factor
	default.font.size <- setOption("default.font.size", 10)
#			if (.Platform$OS.type != "windows") 12 else 10)
	consoleOutputVar <- tclVar(console.output)
	consoleOutputCheckBox <- tkcheckbutton(top, variable=consoleOutputVar)
	logCommandsVar <- tclVar(log.commands)
	logCommandsCheckBox <- tkcheckbutton(top, variable=logCommandsVar)
	logFontSizeVar <- tclVar(log.font.size)
	logFontSizeSlider <- tkscale(top, from=6, to=20, showvalue=TRUE, variable=logFontSizeVar,
			resolution=1, orient="horizontal")
	logWidthVar <- tclVar(log.width)
	logWidthSlider <- tkscale(top, from=30, to=120, showvalue=TRUE, variable=logWidthVar,
			resolution=5, orient="horizontal")
	logHeightVar <- tclVar(log.height)
	logHeightSlider <- tkscale(top, from=0, to=25, showvalue=TRUE, variable=logHeightVar,
			resolution=1, orient="horizontal")
	outputHeightVar <- tclVar(output.height)
	outputHeightSlider <- tkscale(top, from=0, to=50, showvalue=TRUE, variable=outputHeightVar,
			resolution=5, orient="horizontal")
	contrasts1 <- tclVar(contrasts[1])
	contrasts2 <- tclVar(contrasts[2])
	contrastsFrame <- tkframe(top)
	contrasts1Entry <- ttkentry(contrastsFrame, width="15", textvariable=contrasts1)
	contrasts2Entry <- ttkentry(contrastsFrame, width="15", textvariable=contrasts2)
	grabFocusVar <- tclVar(as.numeric(grab.focus))
	grabFocusCheckBox <- tkcheckbutton(top, variable=grabFocusVar)
	doubleClickVar <- tclVar(as.numeric(double.click))
	doubleClickCheckBox <- tkcheckbutton(top, variable=doubleClickVar)
	sortNamesVar <- tclVar(as.numeric(sort.names))
	sortNamesCheckBox <- tkcheckbutton(top, variable=sortNamesVar)
	showEditButtonVar <- tclVar(as.numeric(show.edit.button))
	showEditButtonCheckBox <- tkcheckbutton(top, variable=showEditButtonVar)
	scaleFactorVar <- tclVar(if (is.null(scale.factor)) 1.0 else scale.factor)
	scaleFactorSlider <- tkscale(top, from=0.2, to=3.0, showvalue=TRUE, variable=scaleFactorVar,
			resolution=0.2, orient="horizontal")
	defaultFontSizeVar <- tclVar(default.font.size)
	defaultFontSizeSlider <- tkscale(top, from=6, to=20, showvalue=TRUE, variable=defaultFontSizeVar,
			resolution=1, orient="horizontal")
	onOK <- function(){
		closeDialog(top)
		log.font.size <- round(as.numeric(tclvalue(logFontSizeVar)))
		log.width <- round(as.numeric(tclvalue(logWidthVar)))
		log.height <- as.numeric(tclvalue(logHeightVar))
		log.commands <- as.logical(tclvalue(logCommandsVar) == "1") && (log.height != 0)
		output.height <- as.numeric(tclvalue(outputHeightVar))
		console.output <- as.logical(tclvalue(consoleOutputVar) == "1") || (output.height == 0)
		contrasts <- c(tclvalue(contrasts1), tclvalue(contrasts2))
		grab.focus <- tclvalue(grabFocusVar) == 1
		double.click <- tclvalue(doubleClickVar) == 1
		sort.names <- tclvalue(sortNamesVar) == 1
		show.edit.button <- tclvalue(showEditButtonVar) == 1
		scale.factor <- round(as.numeric(tclvalue(scaleFactorVar)), 1)
		if (scale.factor == 1) scale.factor <- NULL
#        default.font <- tclvalue(defaultFont)
		default.font.size <- tclvalue(defaultFontSizeVar)
		options <- current
		options$log.font.size <- log.font.size
		options$log.width <- log.width
		options$log.height <- log.height
		options$log.commands <- log.commands
		options$output.height <- output.height
		options$console.output <- console.output
		options$default.contrasts <- contrasts
		options$grab.focus <- grab.focus
		options$double.click <- double.click
		options$sort.names <- sort.names
		options$show.edit.button <- show.edit.button
		if (.Platform$OS.type == "windows") options$scale.factor <- scale.factor
		else options$default.font.size <- default.font.size
		options(PMgui=options)
		closePmetrics()
		Pmetrics()
	}
	OKCancelHelp(helpSubject="Pmetrics")
	if (.Platform$OS.type == "windows"){
		tkgrid(labelPMgui(top, text=gettextPMgui("Scale factor for Tk elements")), scaleFactorSlider, sticky="se")
		tkgrid.configure(scaleFactorSlider, sticky="w")
	}
	else {
		tkgrid(labelPMgui(top, text=gettextPMgui("Default-font size (points)")), defaultFontSizeSlider, sticky="e")
		tkgrid.configure(defaultFontSizeSlider, sticky="w")
	}
	tkgrid(labelPMgui(top, text=gettextPMgui("Log-font size (points)")), logFontSizeSlider, sticky="se")
	tkgrid.configure(logFontSizeSlider, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Log width (characters)")), logWidthSlider, sticky="se")
	tkgrid.configure(logWidthSlider, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Log height (lines)")), logHeightSlider, sticky="se")
	tkgrid.configure(logHeightSlider, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Output height (lines)")), outputHeightSlider, sticky="se")
	tkgrid.configure(outputHeightSlider, sticky="w")
	tkgrid(labelPMgui(top, text=" "), sticky="w")	
	tkgrid(labelPMgui(top, text=gettextPMgui("Log commands to script window")), logCommandsCheckBox, sticky="e")
	tkgrid.configure(logCommandsCheckBox, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Send output to R Console")), consoleOutputCheckBox, sticky="e")
	tkgrid.configure(consoleOutputCheckBox, sticky="w")
	tkgrid(labelPMgui(contrastsFrame, text=gettextPMgui("Unordered factors")), labelPMgui(contrastsFrame, text="   "),
			labelPMgui(contrastsFrame, text=gettextPMgui("Ordered factors")), sticky="w")
	tkgrid(contrasts1Entry, labelPMgui(contrastsFrame, text="   "), contrasts2Entry, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Contrasts")), contrastsFrame, sticky="se")
	tkgrid.configure(contrastsFrame, sticky="sw")
	tkgrid(labelPMgui(top, text=gettextPMgui("Active window grabs focus")), grabFocusCheckBox, sticky="e")
	tkgrid.configure(grabFocusCheckBox, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Double-click presses OK button")), doubleClickCheckBox, sticky="e")
	tkgrid.configure(doubleClickCheckBox, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Sort variable names alphabetically")), sortNamesCheckBox, sticky="e")
	tkgrid.configure(sortNamesCheckBox, sticky="w")
	tkgrid(labelPMgui(top, text=gettextPMgui("Show edit button")), showEditButtonCheckBox, sticky="e")
	tkgrid.configure(showEditButtonCheckBox, sticky="w")
	tkconfigure(OKbutton, text=gettextPMgui("Exit and Restart\nPmetrics"), width=18)
	tkgrid(buttonsFrame, columnspan=2, sticky="w")
	dialogSuffix(rows=11, columns=2)
}

loadPackages <- function(){
	availablePackages <- sort(setdiff(.packages(all.available = TRUE), .packages()))
	if (length(availablePackages) == 0){
		errorCondition(message=gettextPMgui("No packages available to load."))
		return()
	}
	initializeDialog(title=gettextPMgui("Load Packages"))
	packagesBox <- variableListBox(top, availablePackages, title=gettextPMgui("Packages (pick one or more)"),
			selectmode="multiple", listHeight=10)
	onOK <- function(){
		packages <- getSelection(packagesBox)
		closeDialog(top)
		if (length(packages) == 0){
			errorCondition(recall=loadPackages, message=gettextPMgui("You must select at least one package."))
			return()
		}
		for (package in packages) {
			Library(package)
		}
		Message(paste(gettextPMgui("Packages loaded:"), paste(packages, collapse=", ")), type="note")
	}
	OKCancelHelp(helpSubject="library")
	tkgrid(getFrame(packagesBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=1, columns=1)
}

Setwd <- function(){
	wd <- tclvalue(tkchooseDirectory(initialdir=getwd(), parent=PmetricsWindow()))
	if (wd != "") doItAndPrint(paste('setwd("', wd, '")', sep=""))
}

