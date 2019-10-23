
# The Pmetrics GUI
# modified from the RPmetrics code by J. Fox

# last modified 2012-04-30 by Michael Neely, MD


Pmetrics <- function(){
	RStudioP <- function() exists("RStudio.version", where=1)
	PMguiVersion <- packageVersion("Pmetrics")
	putPMgui("quotes", options(useFancyQuotes=FALSE))
	putPMgui("messageNumber", 0)
	# the following test suggested by Richard Heiberger
	if ("PMguiEnv" %in% search() &&
			exists("PmetricsWindow", "PMguiEnv") &&
			!is.null(get("PmetricsWindow", "PMguiEnv"))) {
		warning("The PMetrics GUI is already open.")
		return(invisible(NULL))
	}
#	if (is.SciViews()) return(invisible(svPmetrics(Version=PMguiVersion))) # +PhG
	
	setOption("number.messages", TRUE)
  #get path to etc subdirectory where menu structure is stored
#	etc <- setOption("etc", file.path(.path.package(package="Pmetrics")[1], "etc"))
	etc <- setOption("etc", "~/LAPK/PmetricsSource/PMgui/inst/etc")
	
	etcMenus <- setOption("etcMenus", etc)
	putPMgui("etcMenus", etcMenus)
	
  #initialize state
  
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
	setOption("log.font.size", 14) 
	putPMgui("logFont", tkfont.create(family="courier", size=getPMgui("log.font.size")))
	current <- options("PMgui")[[1]]
  scale.factor <- current$scale.factor
	if (!is.null(scale.factor)) .Tcl(paste("tk scaling ", scale.factor, sep=""))
  setOption("default.contrasts", c("contr.treatment", "contr.poly"))
	setOption("log.commands", TRUE)
	setOption("RStudio", RStudioP())
	setOption("console.output", getPMgui("RStudio"))
	setOption("retain.selections", TRUE)
	putPMgui("dialog.values", list())
	putPMgui("dialog.values.noreset", list())
	putPMgui("savedTable", NULL)
	log.height <- as.character(setOption("log.height", if (!getPMgui("log.commands")) 0 else 14, global=FALSE))
	log.width <- as.character(setOption("log.width", 80, global=FALSE))
	output.height <- as.character(setOption("output.height",
					if (getPMgui("console.output")) 0
							else if ((as.numeric(log.height) != 0) || (!getPMgui("log.commands"))) 2*as.numeric(log.height)
							else 20, global=FALSE))
	putPMgui("log.height",log.height)
	putPMgui("log.width",log.width)
  putPMgui("output.height",output.height)
	messages.height <- as.character(setOption("messages.height", 3))
	putPMgui("messages.height",messages.height)
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
	setOption("prefixes", c("Pmetrics> ", "Pmetrics+ ", "PmetricsMsg: ", "PmetricsMsg+ "))
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
	default.font.size <- as.character(setOption("default.font.size", 14, global=FALSE))
	default.font <- setOption("default.font", NULL, global=FALSE) 
	if (!("PMguiDefaultFont" %in% as.character(.Tcl("font names")))){
		if (is.null(default.font)) {.Tcl(paste("font create PMguiDefaultFont -size ", default.font.size))}
		else {.Tcl(paste("font create PMguiDefaultFont ", default.font))}
		.Tcl("option add *font PMguiDefaultFont")
	} else {
		if (is.null(default.font)) {.Tcl(paste("font configure PMguiDefaultFont -size ", default.font.size))}
		else {.Tcl(paste("font configure PMguiDefaultFont ", default.font))}
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
	Menus <- read.table(file.path(etcMenus, "PMgui-menus.csv"), colClasses = "character",sep=",",skip=2)
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
#	if (RExcelSupported())
#		putRExcel(".rexcel.menu.dataframe", Menus)
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
  
  
	
	##### BUILD GUI  ########
  
	if (getPMgui("crisp.dialogs")) tclServiceMode(on=TRUE)
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
	putPMgui("dataSetName", tclVar(gettextPMgui("<None>")))
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
      labelPMgui(controlsFrame, text=gettextPMgui("  Active object:")), getPMgui("dataSetLabel"),sticky="ew"
    )
		tkgrid(controlsFrame, sticky="ew")
	}
	.log.commands <-  getPMgui("log.commands")
	.console.output <- getPMgui("console.output")
  #make log window if logging
	if (.log.commands) tkgrid(labelPMgui(logFrame, text=gettextPMgui("Script Window"), foreground="blue"),
				if (.log.commands && .console.output) submitButton, sticky="w")
	tkgrid(.log, logYscroll, sticky="news", columnspan=2)
	tkgrid(logXscroll)
	if (.log.commands) tkgrid(logFrame, sticky="news", padx=10, pady=0, columnspan=2)
	tkgrid(labelPMgui(outputFrame, text=gettextPMgui("Output Window"), foreground="blue"),
			if (.log.commands && !.console.output) submitButton, sticky="w")
	tkgrid(.output, outputYscroll, sticky="news", columnspan=2)
	tkgrid(outputXscroll, columnspan=1 + (.log.commands && !.console.output))
  
  #make output window if needed
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







