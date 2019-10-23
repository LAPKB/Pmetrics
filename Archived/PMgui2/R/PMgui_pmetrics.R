
# The Pmetrics GUI
# modified from the RPmetrics code by J. Fox

# last modified 2012-04-30 by Michael Neely, MD


Pmetrics <- function(){
	RStudioP <- function() exists("RStudio.version", where=1)
	DESCRIPTION <- packageDescription("PMgui")
	PMguiVersion <- packageVersion("PMgui")
	putPMgui("quotes", options(useFancyQuotes=FALSE))
	putPMgui("messageNumber", 0)
	# the following test suggested by Richard Heiberger
	if ("PMguiEnv" %in% search() &&
			exists("PmetricsWindow", "PMguiEnv") &&
			!is.null(get("PmetricsWindow", "PMguiEnv"))) {
		warning("The PMetrics GUI is already open.")
		return(invisible(NULL))
	}
	if (is.SciViews()) return(invisible(svPmetrics(Version=PMguiVersion))) # +PhG
	setOption <- function(option, default, global=TRUE) {
		opt <- if (is.null(current[option][[1]])) default else current[option][[1]]
		if (global) putPMgui(option, opt)
		else opt
	}
	current <- options("PMgui")[[1]]
	setOption("number.messages", TRUE)
	etc <- setOption("etc", file.path(.path.package(package="PMgui")[1], "etc"))
	etcMenus <- setOption("etcMenus", etc)
	putPMgui("etcMenus", etcMenus)
	onCopy <- function(){
		focused <- tkfocus()
		if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) && (tclvalue(focused) != MessagesWindow()$ID))
			focused <- LogWindow()
		selection <- strsplit(tclvalue(tktag.ranges(focused, "sel")), " ")[[1]]
		if (is.na(selection[1])) return()
		text <- tclvalue(tkget(focused, selection[1], selection[2]))
		tkclipboard.clear()
		tkclipboard.append(text)
	}
	onDelete <- function(){
		focused <- tkfocus()
		if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) && (tclvalue(focused) != MessagesWindow()$ID))
			focused <- LogWindow()
		selection <- strsplit(tclvalue(tktag.ranges(focused, "sel")), " ")[[1]]
		if (is.na(selection[1])) return()
		tkdelete(focused, selection[1], selection[2])
	}
	onCut <- function(){
		onCopy()
		onDelete()
	}
	onPaste <- function(){
		onDelete()
		focused <- tkfocus()
		if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID)  && (tclvalue(focused) != MessagesWindow()$ID))
			focused <- LogWindow()
		text <- tclvalue(.Tcl("selection get -selection CLIPBOARD"))
		if (length(text) == 0) return()
		tkinsert(focused, "insert", text)
	}
	onFind <- function(){
		focused <- tkfocus()
		if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID)  && (tclvalue(focused) != MessagesWindow()$ID))
			focused <- LogWindow()
		initializeDialog(title=gettextPMgui("Find"))
		textFrame <- tkframe(top)
		textVar <- tclVar("")
		textEntry <- ttkentry(textFrame, width="20", textvariable=textVar)
		checkBoxes(frame="optionsFrame", boxes=c("regexpr", "case"), initialValues=c("0", "1"),
				labels=gettextPMgui(c("Regular-expression search", "Case sensitive")))
		radioButtons(name="direction", buttons=c("foward", "backward"), labels=gettextPMgui(c("Forward", "Backward")),
				values=c("-forward", "-backward"), title=gettextPMgui("Search Direction"))
		onOK <- function(){
			text <- tclvalue(textVar)
			if (text == ""){
				errorCondition(recall=onFind, message=gettextPMgui("No search text specified."))
				return()
			}
			type <- if (tclvalue(regexprVariable) == 1) "-regexp" else "-exact"
			case <- tclvalue(caseVariable) == 1
			direction <- tclvalue(directionVariable)
			stop <- if (direction == "-forward") "end" else "1.0"
			where <- if (case) tksearch(focused, type, direction, "--", text, "insert", stop)
					else tksearch(focused, type, direction, "-nocase", "--", text, "insert", stop)
			where <- tclvalue(where)
			if (where == "") {
				Message(message=gettextPMgui("Text not found."),
						type="note")
				if (GrabFocus()) tkgrab.release(top)
				tkdestroy(top)
				tkfocus(PmetricsWindow())
				return()
			}
			if (GrabFocus()) tkgrab.release(top)
			tkfocus(focused)
			tkmark.set(focused, "insert", where)
			tksee(focused, where)
			tkdestroy(top)
		}
		OKCancelHelp()
		tkgrid(labelPMgui(textFrame, text=gettextPMgui("Search for:")), textEntry, sticky="w")
		tkgrid(textFrame, sticky="w")
		tkgrid(optionsFrame, sticky="w")
		tkgrid(directionFrame, sticky="w")
		tkgrid(buttonsFrame, sticky="w")
		dialogSuffix(rows=4, columns=1, focus=textEntry)
	}
	onSelectAll <- function() {
		focused <- tkfocus()
		if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) && (tclvalue(focused) != MessagesWindow()$ID))
			focused <- LogWindow()
		tktag.add(focused, "sel", "1.0", "end")
		tkfocus(focused)
	}
	onClear <- function(){
		onSelectAll()
		onDelete()
	}
	onUndo <- function(){
		focused <- tkfocus()
		if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) && (tclvalue(focused) != MessagesWindow()$ID))
			focused <- LogWindow()
		tcl(focused, "edit", "undo")
	}
	onRedo <- function(){
		focused <- tkfocus()
		if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) && (tclvalue(focused) != MessagesWindow()$ID))
			focused <- LogWindow()
		tcl(focused, "edit", "redo")
	}
	messageTag(reset=TRUE)
	putPMgui("PMguiVersion", PMguiVersion)
	putPMgui(".activeDataSet", NULL)
	putPMgui(".activeModel", NULL)
  putPMgui(".activeOutput",NULL)
	putPMgui("logFileName", NULL)
	putPMgui("outputFileName", NULL)
	putPMgui("saveFileName", NULL)
	putPMgui("modelNumber", 0)
	putPMgui("reset.model", FALSE)
	putPMgui("rgl", FALSE)
	putPMgui("Identify3d", NULL)
	setOption("log.font.size", 10) # if (.Platform$OS.type == "windows") 10 else 12)
	putPMgui("logFont", tkfont.create(family="courier", size=getPMgui("log.font.size")))
	scale.factor <- current$scale.factor
	if (!is.null(scale.factor)) .Tcl(paste("tk scaling ", scale.factor, sep=""))
	if (packageAvailable("car")){
		require("car")
		setOption("default.contrasts", c("contr.Treatment", "contr.poly"))
	}
	else setOption("default.contrasts", c("contr.treatment", "contr.poly"))
	setOption("log.commands", TRUE)
	setOption("RStudio", RStudioP())
	setOption("console.output", getPMgui("RStudio"))
	setOption("retain.selections", TRUE)
	putPMgui("dialog.values", list())
	putPMgui("dialog.values.noreset", list())
	putPMgui("savedTable", NULL)
	log.height <- as.character(setOption("log.height", if (!getPMgui("log.commands")) 0 else 10, global=FALSE))
	log.width <- as.character(setOption("log.width", 80, global=FALSE))
	output.height <- as.character(setOption("output.height",
					if (getPMgui("console.output")) 0
							else if ((as.numeric(log.height) != 0) || (!getPMgui("log.commands"))) 2*as.numeric(log.height)
							else 20, global=FALSE))
	messages.height <- as.character(setOption("messages.height", 3))
	putPMgui("saveOptions", options(warn=1, contrasts=getPMgui("default.contrasts"), width=as.numeric(log.width),
					na.action="na.exclude", graphics.record=TRUE))
	setOption("ask.to.exit", TRUE)
	setOption("ask.on.exit", TRUE)
	setOption("double.click", FALSE)
	setOption("sort.names", TRUE)
	setOption("grab.focus", TRUE)
	setOption("attach.data.set", FALSE)
	setOption("log.text.color", "black")
	setOption("command.text.color", "red")
	setOption("output.text.color", "darkblue")
	setOption("error.text.color", "red")
	setOption("warning.text.color", "darkgreen")
	setOption("prefixes", c("PMgui> ", "PMgui+ ", "PMguiMsg: ", "PMguiMsg+ "))
	setOption("multiple.select.mode", "extended")
	setOption("suppress.X11.warnings",
			interactive() && .Platform$GUI == "X11") # to address problem in X11 (Linux or Mac OS X)
#		interactive() && .Platform$GUI == "X11" && getRversion() < "2.4.0")
	setOption("showData.threshold", 100)
	setOption("retain.messages", TRUE)
	setOption("crisp.dialogs",  TRUE)
	setOption("length.output.stack", 10)
	setOption("length.command.stack", 10)
	putPMgui("outputStack", as.list(rep(NA, getPMgui("length.output.stack"))))
	putPMgui("commandStack", as.list(rep(NA, getPMgui("length.command.stack"))))
	setOption("variable.list.height", 4)
	setOption("variable.list.width", c(20, Inf))
	if (getPMgui("suppress.X11.warnings")) {
		putPMgui("messages.connection", file(open = "w+"))
		sink(getPMgui("messages.connection"), type="message")
#        putPMgui("length.messages", 0)
	}
	if (.Platform$OS.type != "windows") {
		putPMgui("oldPager", options(pager=PMguiPager))
	}
	default.font.size <- as.character(setOption("default.font.size", 10, global=FALSE)) # if (.Platform$OS.type == "windows")10 else 12, global=FALSE))
	default.font <- setOption("default.font", NULL, global=FALSE) 
	if (!("PMguiDefaultFont" %in% as.character(.Tcl("font names")))){
		if (is.null(default.font)) .Tcl(paste("font create PMguiDefaultFont -size ", default.font.size))
		else .Tcl(paste("font create PMguiDefaultFont ", default.font))
		.Tcl("option add *font PMguiDefaultFont")
	}
	else {
		if (is.null(default.font)) .Tcl(paste("font configure PMguiDefaultFont -size ", default.font.size))
		else .Tcl(paste("font configure PMguiDefaultFont ", default.font))
	}
	.Tcl("ttk::style configure TButton -font PMguiDefaultFont")
	placement <- setOption("placement", "-40+20", global=FALSE)
	source.files <- list.files(etc, pattern="\\.[Rr]$")
	for (file in source.files) {
		source(file.path(etc, file))
		cat(paste(gettextPMgui("Sourced:"), file, "\n"))
	}
	Plugins <- options()$PMgui$plugins
	allPlugins <- listPlugins(loaded=TRUE)
	for (plugin in Plugins){
		if (!require(plugin, character.only=TRUE)){
			putPMgui("PmetricsWindow", NULL)
			stop(sprintf(gettextPMgui("the plug-in package %s is missing"), plugin))
		}
		if (!is.element(plugin, allPlugins)){
			putPMgui("PmetricsWindow", NULL)
			stop(sprintf(gettextPMgui("the package %s is not an PMgui plug-in"), plugin))
		}
	}
	Menus <- read.table(file.path(etcMenus, "PMgui-menus.txt"), colClasses = "character")
	addMenus <- function(Menus){
		removeMenus <- function(what){
			children <- Menus[Menus[,3] == what, 2]
			which <- what == Menus[,2] |  what == Menus[,5]
			Menus <<- Menus[!which,]
			for (child in children) removeMenus(child)
		}
		nms <- c("type", "menuOrItem", "operationOrParent", "label",
				"commandOrMenu", "activation", "install")
		names(Menus) <- nms
		for (plugin in Plugins) {
			MenusToAdd <- read.table(file.path(.path.package(package=plugin)[1], "etc/menus.txt"),
					colClasses = "character")
			names(MenusToAdd) <- nms
			for (i in 1:nrow(MenusToAdd)){
				line <- MenusToAdd[i,]
				line[, "label"] <- gettext(line[,"label"], domain=paste("R=", plugin, sep=""))
				if (line[1, "type"] == "remove"){
					##					which <- line[1, "menuOrItem"] == Menus[,2] | line[1, "menuOrItem"] == Menus[,3] | line[1, "menuOrItem"] == Menus[,5]
					##					Menus <- Menus[!which,]
					removeMenus(line[1, "menuOrItem"])
					next
				}
				if (line[1, "type"] == "menu"){
					where <- if (line[1, "operationOrParent"] == "topMenu") 0
							else max(which((Menus[, "type"] == "menu") &
														(Menus[, "menuOrItem"] == line[1, "operationOrParent"])))
				}
				else if (line[1, "type"] == "item"){
					if (line[1, "operationOrParent"] == "command"){
						which <- which((Menus[, "operationOrParent"] == "command") &
										(Menus[, "menuOrItem"] == line[1, "menuOrItem"]))
						where <- if (length(which) == 0)
									which((Menus[, "type"] == "menu")
													& (Menus[, "menuOrItem"] == line[1, "menuOrItem"]))
								else max(which)
					}
					else if (line[1, "operationOrParent"] == "cascade"){
						where <- if (line[1, "menuOrItem"] != "topMenu")
									max(which((Menus[, "operationOrParent"] == "cascade") &
															(Menus[, "menuOrItem"] == line[1, "menuOrItem"]) | (Menus[, "commandOrMenu"] == line[1, "menuOrItem"])))
								else {
									max(which((Menus[, "operationOrParent"] == "cascade") &
															(Menus[, "menuOrItem"] == "topMenu") &
															(Menus[, "commandOrMenu"] != "toolsMenu") &
															(Menus[, "commandOrMenu"] != "helpMenu")))
								}
					}
					else stop(sprintf(gettextPMgui('unrecognized operation, "%s", in plugin menu line %i'),
										line[1, "operation"], i))
				}
				else stop(sprintf(gettextPMgui('unrecognized type, "%s", in plugin menu line %i'),
									line[1, "type"], i))
				Menus <- insertRows(Menus, line, where)
			}
		}
		Menus
	}
	Menus <- addMenus(Menus)
	menuNames <- Menus[Menus[,1] == "menu",]
	duplicateMenus <- duplicated(menuNames)
	if (any(duplicateMenus)) stop(paste(gettextPMgui("Duplicate menu names:"),
						menuNames[duplicateMenus]))
	.Menus <- menus <- list()
	menuItems <- 0
	oldMenu <- ncol(Menus) == 6
	setOption("suppress.menus", FALSE)
	## added by EN ###############################
	if (RExcelSupported())
		putRExcel(".rexcel.menu.dataframe", Menus)
	## end of change ###############################
	modelClasses <- scan(file.path(etc, "model-classes.txt"), what="", quiet=TRUE, comment.char="#")
	for (plugin in Plugins){
		description <- packageDescription(pkg=plugin)
		addModels <- description[grep("Models:", description)]
		addModels <- gsub(" ", "", sub("^Models:", "", addModels))
		addModels <- unlist(strsplit(addModels, ","))
		if (length(addModels) > 0) modelClasses <- c(modelClasses, addModels)
	}
	putPMgui("modelClasses", modelClasses)

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
		view.height <- max(as.numeric(output.height) + as.numeric(log.height), 10)
		ncols <- ncol(get(ActiveDataSet()))
		command <- if (packageAvailable("relimp") && ncols <= getPMgui("showData.threshold")){
					paste("showData(", ActiveDataSet(), ", placement='-20+200', font=getPMgui('logFont'), maxwidth=",
							log.width, ", maxheight=", view.height, suppress, ")", sep="")
				}
				else paste("View(", ActiveDataSet(), ")", sep="")
		logger(command)
		justDoIt(command)
	}
	# the following function modified 14 July 07 by Erich Neuwirth
	onSubmit <- function(){
		.log <- LogWindow()
		selection <- strsplit(tclvalue(tktag.ranges(.log, "sel")), " ")[[1]]
		if (is.na(selection[1])) {
			tktag.add(.log, "currentLine", "insert linestart", "insert lineend")
			selection <- strsplit(tclvalue(tktag.ranges(.log,"currentLine")), " ")[[1]]
			tktag.delete(.log, "currentLine")
			if (is.na(selection[1])) {
				Message(message=gettextPMgui("Nothing is selected."),
						type="error")
				tkfocus(PmetricsWindow())
				return()
			}
		}
		lines <- tclvalue(tkget(.log, selection[1], selection[2]))
		lines <- strsplit(lines, "\n")[[1]]
		.console.output <- getPMgui("console.output")
		.output <- OutputWindow()
		iline <- 1
		nlines <- length(lines)
		while (iline <= nlines){
			while (nchar(lines[iline])==0) iline <- iline + 1
			if (iline > nlines) break
			current.line <- lines[iline]
			if (.console.output) cat(paste("\n", getPMgui("prefixes")[1], current.line,"\n", sep=""))
			else{
				tkinsert(.output, "end", paste("\n> ", current.line,"\n", sep="")) ### end of changed
				tktag.add(.output, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
				tktag.configure(.output, "currentLine", foreground=getPMgui("command.text.color"))
			}
			jline <- iline + 1
			while (jline <= nlines){
				if (class(try(parse(text=current.line),silent=TRUE))!="try-error") break
				if (.console.output)cat(paste(getPMgui("prefixes")[2], lines[jline],"\n", sep=""))
				else{
					tkinsert(.output, "end", paste("+ ", lines[jline],"\n", sep=""))
					tktag.add(.output, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
					tktag.configure(.output, "currentLine", foreground=getPMgui("command.text.color"))
				}
				current.line <- paste(current.line, lines[jline],sep="\n")
				jline <- jline + 1
				iline <- iline + 1
			}
			if (!(is.null(current.line) || is.na(current.line))) doItAndPrint(current.line, log=FALSE)
			iline <- iline + 1
			tkyview.moveto(.output, 1)
			tkfocus(.log)
		}
	}
	contextMenuLog <- function(){
		.log <- LogWindow()
		contextMenu <- tkmenu(tkmenu(.log), tearoff=FALSE)
		tkadd(contextMenu, "command", label=gettextPMgui("Submit"), command=onSubmit)
		tkadd(contextMenu, "command", label=gettextPMgui("Cut"), command=onCut)
		tkadd(contextMenu, "command", label=gettextPMgui("Copy"), command=onCopy)
		tkadd(contextMenu, "command", label=gettextPMgui("Paste"), command=onPaste)
		tkadd(contextMenu, "command", label=gettextPMgui("Delete"), command=onDelete)
		tkadd(contextMenu, "command", label=gettextPMgui("Find..."), command=onFind)
		tkadd(contextMenu, "command", label=gettextPMgui("Select all"), command=onSelectAll)
		tkadd(contextMenu, "command", label=gettextPMgui("Undo"), command=onUndo)
		tkadd(contextMenu, "command", label=gettextPMgui("Redo"), command=onRedo)
		tkadd(contextMenu, "command", label=gettextPMgui("Clear window"), command=onClear)
		tkpopup(contextMenu, tkwinfo("pointerx", .log), tkwinfo("pointery", .log))
	}
	contextMenuOutput <- function(){
		.output <- OutputWindow()
		contextMenu <- tkmenu(tkmenu(.output), tearoff=FALSE)
		tkadd(contextMenu, "command", label=gettextPMgui("Cut"), command=onCut)
		tkadd(contextMenu, "command", label=gettextPMgui("Copy"), command=onCopy)
		tkadd(contextMenu, "command", label=gettextPMgui("Paste"), command=onPaste)
		tkadd(contextMenu, "command", label=gettextPMgui("Delete"), command=onDelete)
		tkadd(contextMenu, "command", label=gettextPMgui("Find..."), command=onFind)
		tkadd(contextMenu, "command", label=gettextPMgui("Select all"), command=onSelectAll)
		tkadd(contextMenu, "command", label=gettextPMgui("Undo"), command=onUndo)
		tkadd(contextMenu, "command", label=gettextPMgui("Redo"), command=onRedo)
		tkadd(contextMenu, "command", label=gettextPMgui("Clear window"), command=onClear)
		tkpopup(contextMenu, tkwinfo("pointerx", .output), tkwinfo("pointery", .output))
	}
	contextMenuMessages <- function(){
		.messages <- MessagesWindow()
		contextMenu <- tkmenu(tkmenu(.messages), tearoff=FALSE)
		tkadd(contextMenu, "command", label=gettextPMgui("Cut"), command=onCut)
		tkadd(contextMenu, "command", label=gettextPMgui("Copy"), command=onCopy)
		tkadd(contextMenu, "command", label=gettextPMgui("Paste"), command=onPaste)
		tkadd(contextMenu, "command", label=gettextPMgui("Delete"), command=onDelete)
		tkadd(contextMenu, "command", label=gettextPMgui("Find..."), command=onFind)
		tkadd(contextMenu, "command", label=gettextPMgui("Select all"), command=onSelectAll)
		tkadd(contextMenu, "command", label=gettextPMgui("Undo"), command=onUndo)
		tkadd(contextMenu, "command", label=gettextPMgui("Redo"), command=onRedo)
		tkadd(contextMenu, "command", label=gettextPMgui("Clear window"), command=onClear)
		tkpopup(contextMenu, tkwinfo("pointerx", .messages), tkwinfo("pointery", .messages))
	}
	if (getPMgui("crisp.dialogs")) tclServiceMode(on=FALSE)
	putPMgui("PmetricsWindow", tktoplevel(class="Pmetrics"))
	.Pmetrics <- PmetricsWindow()
	tkwm.geometry(.Pmetrics, placement)
	tkwm.title(.Pmetrics, gettextPMgui("Pmetrics"))
	tkwm.protocol(.Pmetrics, "WM_DELETE_WINDOW", ClosePmetrics)
	topMenu <- tkmenu(.Pmetrics)
	tkconfigure(.Pmetrics, menu=topMenu)
	position <- numeric(0)
	if (!getPMgui("suppress.menus")){
		for (m in 1:nrow(Menus)){
			install <- if (oldMenu) "" else Menus[m, 7]
			if ((install != "") && (!eval(parse(text=install)))) next
			if (Menus[m, 1] == "menu") {
				position[Menus[m, 2]] <- 0
				assign(Menus[m, 2], tkmenu(get(Menus[m, 3]), tearoff=FALSE))
				menus[[Menus[m, 2]]] <- list(ID=get(Menus[m, 2])$ID, position=0)
			}
			else if (Menus[m, 1] == "item") {
				if (Menus[m, 3] == "command"){
					position[Menus[m, 2]] <- position[Menus[m, 2]] + 1
					if (Menus[m, 6] == "")
						tkadd(get(Menus[m, 2]), "command", label=gettextMenus(Menus[m, 4]),
								command=get(Menus[m, 5]))
					else {
						tkadd(get(Menus[m, 2]), "command", label=gettextMenus(Menus[m, 4]),
								command=get(Menus[m, 5]), state="disabled")
						menuItems <- menuItems + 1
						menus[[Menus[m, 2]]]$position <- position[Menus[m, 2]]
						.Menus[[menuItems]] <- list(ID=menus[[Menus[m, 2]]]$ID, position=position[Menus[m, 2]],
								activation=eval(parse(text=paste("function()", Menus[m, 6]))))
					}
				}
				else if (Menus[m, 3] == "cascade")
					tkadd(get(Menus[m, 2]), "cascade", label=gettextMenus(Menus[m, 4]),
							menu=get(Menus[m, 5]))
				else stop(paste(gettextPMgui("menu definition error:"), Menus[m, ], collapse=" "),
							domain=NA)
			}
			else stop(paste(gettextPMgui("menu definition error:"), Menus[m, ], collapse=" "),
						domain=NA)
		}
	}
	putPMgui("Menus", .Menus)
	putPMgui("autoRestart", FALSE)
	activateMenus()
	controlsFrame <- tkframe(PmetricsWindow())
	setwdButton <- buttonPMgui(controlsFrame, text=gettextPMgui("Set WD"), command=Setwd)
	editButton <- buttonPMgui(controlsFrame, text=gettextPMgui("Edit data set"), command=onEdit)
	viewButton <- buttonPMgui(controlsFrame, text=gettextPMgui("View data set"), command=onView)
	putPMgui("dataSetName", tclVar(gettextPMgui("<No active dataset>")))
	putPMgui("dataSetLabel", tkbutton(controlsFrame, textvariable=getPMgui("dataSetName"), foreground="red",
					relief="groove", command=selectActiveDataSet))
	logFrame <- tkframe(PmetricsWindow())
	putPMgui("logWindow", tktext(logFrame, bg="white", foreground=getPMgui("log.text.color"),
					font=getPMgui("logFont"), height=log.height, width=log.width, wrap="none", undo=TRUE))
	.log <- LogWindow()
	logXscroll <- ttkscrollbar(logFrame, orient="horizontal",
			command=function(...) tkxview(.log, ...))
	logYscroll <- ttkscrollbar(logFrame,
			command=function(...) tkyview(.log, ...))
	tkconfigure(.log, xscrollcommand=function(...) tkset(logXscroll, ...))
	tkconfigure(.log, yscrollcommand=function(...) tkset(logYscroll, ...))
	outputFrame <- tkframe(.Pmetrics)
	submitIm <- tcl("image", "create", "bitmap", file=file.path(etc, "submit.xbm"))
	if (getPMgui("console.output"))
		submitButton <- if (English()) buttonPMgui(logFrame, image=submitIm,
							borderwidth="2", command=onSubmit)
				else buttonPMgui(logFrame, text=gettextPMgui("Submit"), borderwidth="2", command=onSubmit)
	else submitButton <- if (English()) buttonPMgui(outputFrame, image=submitIm,
							borderwidth="2", command=onSubmit)
				else buttonPMgui(outputFrame, text=gettextPMgui("Submit"), borderwidth="2", command=onSubmit)
	putPMgui("outputWindow", tktext(outputFrame, bg="white", foreground=getPMgui("output.text.color"),
					font=getPMgui("logFont"), height=output.height, width=log.width, wrap="none", undo=TRUE))
	.output <- OutputWindow()
	outputXscroll <- ttkscrollbar(outputFrame, orient="horizontal",
			command=function(...) tkxview(.output, ...))
	outputYscroll <- ttkscrollbar(outputFrame,
			command=function(...) tkyview(.output, ...))
	tkconfigure(.output, xscrollcommand=function(...) tkset(outputXscroll, ...))
	tkconfigure(.output, yscrollcommand=function(...) tkset(outputYscroll, ...))
	messagesFrame <- tkframe(.Pmetrics)
	putPMgui("messagesWindow", tktext(messagesFrame, bg="lightgray",
					font=getPMgui("logFont"), height=messages.height, width=log.width, wrap="none", undo=TRUE))
	.messages <- MessagesWindow()
	messagesXscroll <- ttkscrollbar(messagesFrame, orient="horizontal",
			command=function(...) tkxview(.messages, ...))
	messagesYscroll <- ttkscrollbar(messagesFrame,
			command=function(...) tkyview(.messages, ...))
	tkconfigure(.messages, xscrollcommand=function(...) tkset(messagesXscroll, ...))
	tkconfigure(.messages, yscrollcommand=function(...) tkset(messagesYscroll, ...))
	putPMgui("modelName", tclVar(gettextPMgui("<No active model>")))
	putPMgui("modelLabel", tkbutton(controlsFrame, textvariable=getPMgui("modelName"), foreground="red",
					relief="groove", command=selectActiveModel))
	show.edit.button <- options("PMgui")[[1]]$show.edit.button
	show.edit.button <- if (is.null(show.edit.button)) TRUE else show.edit.button
	if (!getPMgui("suppress.menus")){
    tkgrid(
				setwdButton,
        labelPMgui(controlsFrame, text=gettextPMgui("  Data set:")), getPMgui("dataSetLabel"),
				labelPMgui(controlsFrame, text="  "), if(show.edit.button) editButton, viewButton,
				labelPMgui(controlsFrame, text=gettextPMgui("    Model: ")), getPMgui("modelLabel"),sticky="w")
		tkgrid(controlsFrame, sticky="w")
	}
	.log.commands <-  getPMgui("log.commands")
	.console.output <- getPMgui("console.output")
	if (.log.commands) tkgrid(labelPMgui(logFrame, text=gettextPMgui("Script Window"), foreground="blue"),
				if (.log.commands && .console.output) submitButton, sticky="w")
	tkgrid(.log, logYscroll, sticky="news", columnspan=2)
	tkgrid(logXscroll)
	if (.log.commands) tkgrid(logFrame, sticky="news", padx=10, pady=0, columnspan=2)
	tkgrid(labelPMgui(outputFrame, text=gettextPMgui("Output Window"), foreground="blue"),
			if (.log.commands && !.console.output) submitButton, sticky="w")
	tkgrid(.output, outputYscroll, sticky="news", columnspan=2)
	tkgrid(outputXscroll, columnspan=1 + (.log.commands && !.console.output))
	if (!.console.output) tkgrid(outputFrame, sticky="news", padx=10, pady=0, columnspan=2)
	tkgrid(labelPMgui(messagesFrame, text=gettextPMgui("Messages"), foreground=getPMgui("error.text.color")), sticky="w")
	tkgrid(.messages, messagesYscroll, sticky="news", columnspan=2)
	tkgrid(messagesXscroll)
	if (!.console.output) tkgrid(messagesFrame, sticky="news", padx=10, pady=0, columnspan=2) ##rmh & J. Fox
	tkgrid.configure(logYscroll, sticky="ns")
	tkgrid.configure(logXscroll, sticky="ew")
	tkgrid.configure(outputYscroll, sticky="ns")
	tkgrid.configure(outputXscroll, sticky="ew")
	tkgrid.configure(messagesYscroll, sticky="ns")
	tkgrid.configure(messagesXscroll, sticky="ew")
	.Pmetrics <- PmetricsWindow()
	tkgrid.rowconfigure(.Pmetrics, 0, weight=0)
	tkgrid.rowconfigure(.Pmetrics, 1, weight=1)
	tkgrid.rowconfigure(.Pmetrics, 2, weight=1)
	tkgrid.columnconfigure(.Pmetrics, 0, weight=1)
	tkgrid.columnconfigure(.Pmetrics, 1, weight=0)
	if (.log.commands){
		tkgrid.rowconfigure(logFrame, 0, weight=0)
		tkgrid.rowconfigure(logFrame, 1, weight=1)
		tkgrid.rowconfigure(logFrame, 2, weight=0)
		tkgrid.columnconfigure(logFrame, 0, weight=1)
		tkgrid.columnconfigure(logFrame, 1, weight=0)
	}
	if (!.console.output){
		tkgrid.rowconfigure(outputFrame, 0, weight=0)
		tkgrid.rowconfigure(outputFrame, 1, weight=1)
		tkgrid.rowconfigure(outputFrame, 2, weight=0)
		tkgrid.columnconfigure(outputFrame, 0, weight=1)
		tkgrid.columnconfigure(outputFrame, 1, weight=0)
	}
	tkgrid.rowconfigure(messagesFrame, 0, weight=0)
	tkgrid.rowconfigure(messagesFrame, 1, weight=0)
	tkgrid.rowconfigure(messagesFrame, 2, weight=0)
	tkgrid.columnconfigure(messagesFrame, 0, weight=1)
	tkgrid.columnconfigure(messagesFrame, 1, weight=0)
	.Tcl("update idletasks")
	tkbind(.Pmetrics, "<Control-x>", onCut)
	tkbind(.Pmetrics, "<Control-X>", onCut)
	tkbind(.Pmetrics, "<Control-c>", onCopy)
	tkbind(.Pmetrics, "<Control-C>", onCopy)
	tkbind(.Pmetrics, "<Control-r>", onSubmit)
	tkbind(.Pmetrics, "<Control-R>", onSubmit)
	tkbind(.Pmetrics, "<Control-Tab>", onSubmit)
	tkbind(.Pmetrics, "<Control-f>", onFind)
	tkbind(.Pmetrics, "<Control-F>", onFind)
	tkbind(.Pmetrics, "<Control-s>", saveLog)
	tkbind(.Pmetrics, "<Control-S>", saveLog)
	tkbind(.Pmetrics, "<Control-a>", onSelectAll)
	tkbind(.Pmetrics, "<Control-A>", onSelectAll)
	tkbind(.Pmetrics, "<Control-w>", onRedo)
	tkbind(.Pmetrics, "<Control-W>", onRedo)
	tkbind(.Pmetrics, "<Alt-BackSpace>", onUndo)
	tkbind(.log, "<ButtonPress-3>", contextMenuLog)
	tkbind(.output, "<ButtonPress-3>", contextMenuOutput)
	tkbind(.messages, "<ButtonPress-3>", contextMenuMessages)
	tkbind(.log, "<Control-ButtonPress-1>", contextMenuLog)
	tkbind(.output, "<Control-ButtonPress-1>", contextMenuOutput)
	tkbind(.messages, "<Control-ButtonPress-1>", contextMenuMessages)
	tkwm.deiconify(.Pmetrics)
	tkfocus(.Pmetrics)
	if (getPMgui("crisp.dialogs")) tclServiceMode(on=TRUE)
	tkwait <- options("PMgui")[[1]]$tkwait  # to address problem in Debian Linux
	if ((!is.null(tkwait)) && tkwait) {
		.Pmetrics.done <<- tclVar("0")
		tkwait.variable(.Pmetrics.done)
	}
	Message(paste(gettextPMgui("PMetrics GUI Version "), getPMgui("PMguiVersion"), ": ", date(), sep=""))
	if (.Platform$GUI == "Rgui"  && ismdi()) Message(gettextPMgui(
						"The Windows version of the Pmetrics GUI works best under RGui\nwith the single-document interface (SDI); see ?Pmetrics."),
				type="warning")
}


# the following function modified 24 July 07 by Richard Heiberger
#  and subsequently by J. Fox 26 July 07
# last modified 10 January 2010 by J. Fox

logger <- function(command){
	pushCommand(command)
	if (is.SciViews()) return(svlogger(command))    # +PhG
	.log <- LogWindow()
	.output <- OutputWindow()
	command <- splitCmd(command)
	if (getPMgui("log.commands")) {
		last2 <- tclvalue(tkget(.log, "end -2 chars", "end"))
		if (last2 != "\n\n") tkinsert(.log, "end", "\n")
		tkinsert(.log, "end", paste(command,"\n", sep=""))
		tkyview.moveto(.log, 1)
	}
	lines <- strsplit(command, "\n")[[1]]
	tkinsert(.output, "end", "\n")
	if (getPMgui("console.output")) {
		for (line in seq(along.with=lines)) {
			prompt <- ifelse (line==1, paste("\n", getPMgui("prefixes")[1], sep=""), paste("\n", getPMgui("prefixes")[2], sep=""))
			cat(paste(prompt, lines[line]))  ##rmh
		}
		cat("\n")                          ##rmh
	}
	else {
		for (line in  seq(along.with=lines)) {
			prompt <- ifelse(line==1, "> ", "+ ")
			tkinsert(.output, "end", paste(prompt, lines[line], "\n", sep=""))
			tktag.add(.output, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
			tktag.configure(.output, "currentLine", foreground=getPMgui("command.text.color"))
			tkyview.moveto(.output, 1)
		}
	}
	command
}

justDoIt <- function(command) {
	Message()
	if (!getPMgui("suppress.X11.warnings")){
		messages.connection <- file(open="w+")
		sink(messages.connection, type="message")
		on.exit({
					sink(type="message")
					close(messages.connection)
				})
	}
	else messages.connection <- getPMgui("messages.connection")
	capture.output(result <- try(eval(parse(text=command), envir=.GlobalEnv), silent=TRUE))
	if (class(result)[1] ==  "try-error"){
		Message(message=paste(strsplit(result, ":")[[1]][2]), type="error")
		tkfocus(PmetricsWindow())
		return(result)
	}
	checkWarnings(readLines(messages.connection))
	if (getPMgui("RStudio")) Sys.sleep(0)
	result
}

doItAndPrint <- function(command, log=TRUE) {
	# with modifications from Duncan Murdoch 4 Jan 08
	Message()
	.console.output <- getPMgui("console.output")
	.output <- OutputWindow()
	if (!.console.output) {
		width <- (as.numeric(tkwinfo("width", .output)) - 2*as.numeric(tkcget(.output, borderwidth=NULL)) - 2)/
				as.numeric(tkfont.measure(tkcget(.output, font=NULL), "0"))
		eval(parse(text=paste("options(width=", floor(width), ")", sep="")))
	}
	if (!getPMgui("suppress.X11.warnings")){
		messages.connection <- file(open="w+")
		sink(messages.connection, type="message")
		on.exit({
					sink(type="message")
					close(messages.connection)
				})
	}
	else messages.connection <- getPMgui("messages.connection")
	output.connection <- file(open="w+")
	sink(output.connection, type="output")
	on.exit({
				if (!.console.output) sink(type="output") # if .console.output, output connection already closed
				close(output.connection)
			}, add=TRUE)
	if (log) logger(command) else pushCommand(command)
	result <- try(parse(text=paste(command)), silent=TRUE)
	if (class(result)[1] == "try-error"){
		Message(message=paste(strsplit(result, ":")[[1]][2]), type="error")
		if (.console.output) sink(type="output")
		tkfocus(PmetricsWindow())
		return(result)
	} else {
		exprs <- result
		result <- NULL
	}
	for (i in seq_along(exprs)) {
		ei <- exprs[i]
		result <-  try(withVisible(eval(ei, envir=.GlobalEnv)), silent=TRUE)
		if (class(result)[1] ==  "try-error"){
			Message(message=paste(strsplit(result, ":")[[1]][2]), type="error")
			if (.console.output) sink(type="output")
			tkfocus(PmetricsWindow())
			return(result)
		}
		result <- if (result$visible == FALSE) NULL else result$value
		if (!is.null(result)) pushOutput(result)
		if (isS4object(result)) show(result) else print(result)
		.Output <- readLines(output.connection)
		if (length(.Output) > 0 && .Output[length(.Output)] == "NULL")
			.Output <- .Output[-length(.Output)] # suppress "NULL" line at end of output
		if (length(.Output) != 0) {  # is there output to print?
			if (.console.output) {
				out <- .Output
				sink(type="output")
				for (line in out) cat(paste(line, "\n", sep=""))
			}
			else{
				for (line in .Output) tkinsert(.output, "end", paste(line, "\n", sep=""))
				tkyview.moveto(.output, 1)
			}
		}
		else if (.console.output) sink(type="output")
		###### added by EN  ######################
		if (RExcelSupported())
			putRExcel(".rexcel.last.output",.Output)
		###### end of change  #####################
		# errors already intercepted, display any warnings
		checkWarnings(readLines(messages.connection))
	}
	if (getPMgui("RStudio")) Sys.sleep(0)
	result
}

checkWarnings <- function(messages){
	if (getPMgui("suppress.X11.warnings")){
		X11.warning <- grep("X11 protocol error|Warning in structure", messages)
		if (length(X11.warning) > 0){
			messages <- messages[-X11.warning]
		}
		if (length(messages) == 0) Message()
		else if (length(messages) > 10) {
			messages <- c(paste(length(messages), "warnings."),
					gettextPMgui("First and last 5 warnings:"),
					head(messages,5), ". . .", tail(messages, 5))
			Message(message=paste(messages, collapse="\n"), type="warning")
		}
		else {
			if (length(grep("warning", messages, ignore.case=TRUE)) > 0)
				Message(message=paste(messages, collapse="\n"), type="warning")
			else Message(message=paste(messages, collapse="\n"), type="note")
		}
	}
	else{
		if (length(messages) == 0) Message()
		else if (length(messages) > 10){
			messages <- c(paste(length(messages), "warnings."),
					gettextPMgui("First and last 5 warnings:"),
					head(messages, 5), ". . .", tail(messages, 5))
			Message(message=paste(messages, collapse="\n"), type="warning")
		}
		else {
			if (length(grep("warning", messages, ignore.case=TRUE)) > 0)
				Message(message=paste(messages, collapse="\n"), type="warning")
			else Message(message=paste(messages, collapse="\n"), type="note")
		}
	}
	tkfocus(PmetricsWindow())
}

pause <- function(seconds = 1){
	if (seconds <= 0) stop("seconds must be positive")
	start <- proc.time()[3]
	while (as.numeric(elapsed <- (proc.time()[3] - start)) < seconds) {}
	elapsed
}

Message <- function(message, type=c("note", "error", "warning")){
	if (is.SciViews()) return(svMessage(message, type))    # +PhG
	tcl("update") 
	.message <- MessagesWindow()
	type <- match.arg(type)
	if (type != "note") tkbell()
	if (getPMgui("retain.messages")) {
		if (missing(message) && !is.null(getPMgui("last.message"))) {
			putPMgui("last.message", NULL)
			tkyview.moveto(.message, 1.0)
		}
	}
	else if (type == "note"){
		lastMessage <- tclvalue(tkget(MessagesWindow(),  "end - 2 lines", "end"))
		if (length(c(grep(gettextPMgui("ERROR:"), lastMessage), grep(gettextPMgui("WARNING:"), lastMessage))) == 0)
			tkdelete(.message, "1.0", "end")
	}
	else tkdelete(.message, "1.0", "end")
	col <- if (type == "error") getPMgui("error.text.color")
			else if (type == "warning") getPMgui("warning.text.color")
			else getPMgui("output.text.color")
	prefix <- switch(type, error=gettextPMgui("ERROR"), warning=gettextPMgui("WARNING"), note=gettextPMgui("NOTE"))
	if (missing(message)){
		return()
	}
	putPMgui("last.message", type)
	message <- paste(prefix, ": ", message, sep="")
	if (getPMgui("retain.messages") && getPMgui("number.messages")) {
		messageNumber <- getPMgui("messageNumber") + 1
		putPMgui("messageNumber", messageNumber)
		message <- paste("[", messageNumber, "] ", message, sep="")
	}
	######### added by EN #####################
	if (RExcelSupported())
		putRExcel(".rexcel.last.message",message)
	######### end of change ###############
	lines <- strsplit(message, "\n")[[1]]
	
	######### added by rmh #####################                   ##rmh
	if (console.output) {                                        ##rmh & J. Fox
		if (sink.number() != 0) sink()							## fixed by J. Fox
		for (jline in seq(along.with=lines)) {                            ##rmh
			Header <- if (jline==1) getPMgui("prefixes")[3] else getPMgui("prefixes")[4]     ##rmh
			cat(paste(Header, lines[jline], "\n", sep=""))             ##rmh
		}                                                            ##rmh
	}                                                              ##rmh
	else                                                           ##rmh
		######### end of change ###############                        ##rmh
		
		for (line in lines){
			tagName <- messageTag()
			tkinsert(.message, "end", paste(line, "\n", sep=""))
			tktag.add(.message, tagName, "end - 2 lines linestart", "end - 2 lines lineend")
			tktag.configure(.message, tagName, foreground=col)
			tkyview.moveto(.message, 1.0)
		}
}

messageTag <- function(reset=FALSE){
	if (reset){
		putPMgui("tagNumber", 0)
		return()
	}
	tagNumber <- getPMgui("tagNumber") + 1
	putPMgui("tagNumber", tagNumber)
	paste("message", tagNumber, sep="")
}

pushOutput <- function(element) {
	stack <- getPMgui("outputStack")
	stack <- c(list(element), stack[-getPMgui("length.output.stack")])
	putPMgui("outputStack", stack)
}

popOutput <- function(){
	stack <- getPMgui("outputStack")
	lastOutput <- stack[[1]]
	putPMgui("outputStack", c(stack[-1], NA))
	lastOutput
}

pushCommand <- function(element) {
	stack <- getPMgui("commandStack")
	stack <- c(list(element), stack[-getPMgui("length.command.stack")])
	putPMgui("commandStack", stack)
}

popCommand <- function(){
	stack <- getPMgui("commandStack")
	lastCommand <- stack[[1]]
	putPMgui("commandStack", c(stack[-1], NA))
	lastCommand
}
