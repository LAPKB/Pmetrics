# last modified 2012-01-27 by J. Fox
#  applied patch to improve window behaviour supplied by Milan Bouchet-Valat 2011-09-22

# Data menu dialogs

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


variablesDataSet <- function(){
	doItAndPrint(paste("names(", ActiveDataSet(), ")", sep=""))
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


refreshActiveDataSet <- function() activeDataSet(ActiveDataSet())



#load PMmatrix file
loadPMmatrix <- function() {
  file <- tclvalue(tkgetOpenFile(filetypes=
    gettextPMgui('{"Text Files" {".csv" ".ssv" ".txt" }} {"All Files" {"*"}}'),
                                 defaultextension=".csv"))
  if (file == "") return()
  command <- paste('PMreadMatrix("', file,'")', sep="")
  dsname <- sub("\\.[[:alnum:]]*","",basename(file))
  temp <- justDoIt(command)
  assign(dsname,temp,envir = .GlobalEnv)
  logger(command)
  activeDataSet(dsname)
  tkfocus(PmetricsWindow())
}

saveDataSet <- function() {
	file <- tclvalue(tkgetSaveFile(filetypes=
							gettextPMgui('{"Text Files" {".csv" ".ssv" ".txt" }} {"All Files" {"*"}}'),
					defaultextension=".RData", initialfile=paste(activeDataSet(), ".RData", sep="")))
	if (file == "") return()
	command <- paste('save("', activeDataSet(), '", file="', file, '")', sep="")
	justDoIt(command)
	logger(command)
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


# edit and view data sets

onEdit <- function(){
  if (activeDataSet() == FALSE) {
    tkfocus(PmetricsWindow())
    return()
  }
  dsnameValue <- ActiveDataSet()
  save.dataset <- get(dsnameValue, envir=.GlobalEnv)
  command <- paste("fix(", dsnameValue, ")", sep="")
  result <- justDoIt(command)
  if (class(result)[1] !=  "try-error"){   		
    if (nrow(get(dsnameValue)) == 0){
      errorCondition(window=NULL, message=gettextPMgui("empty data set."))
      assign(dsnameValue, save.dataset, envir=.GlobalEnv)
      return()
    }
    else{
      logger(command)
      activeDataSet(dsnameValue)
    }
  }
  else{
    errorCondition(window=NULL, message=gettextPMgui("data set edit error."))
    assign(dsnameValue, save.dataset, envir=.GlobalEnv)
    return()
  }
  tkwm.deiconify(PmetricsWindow())
  tkfocus(PmetricsWindow())
}
onView <- function(){
  if (packageAvailable("relimp")) Library("relimp")
  if (activeDataSet() == FALSE) {
    tkfocus(PmetricsWindow())
    return()
  }
  suppress <- if(getPMgui("suppress.X11.warnings")) ", suppress.X11.warnings=FALSE" else ""
  view.height <- max(as.numeric(getPMgui("output.height")) + as.numeric(getPMgui("log.height")), 10)
  ncols <- ncol(get(ActiveDataSet()))
  command <- if (packageAvailable("relimp") && ncols <= getPMgui("showData.threshold")){
    paste("showData(", ActiveDataSet(), ", placement='-20+200', font=getPMgui('logFont'), maxwidth=",
          log.width, ", maxheight=", view.height, suppress, ")", sep="")
  }
  else paste("View(", ActiveDataSet(), ")", sep="")
  logger(command)
  justDoIt(command)
}

