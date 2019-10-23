# this code originally by Dan Putler, used with permission 

# last modified 2012-01-27 by J. Fox

assignCluster <- function(clusterData, origData, clusterVec){
	rowsDX <- row.names(clusterData)
	rowsX <- row.names(origData)
	clustAssign <- rep(NA, length(rowsX))
	validData <- rowsX %in% rowsDX
	clustAssign[validData] <- clusterVec
	return(as.factor(clustAssign))
}

KMeans <- function (x, centers, iter.max=10, num.seeds=10) {
	# fixed 15 Mar 05 by J. Fox
	if(mode(x)=="numeric") x<-data.frame(new.x=x)
	KM <- kmeans(x=x, centers=centers, iter.max=iter.max)
	for(i in 2:num.seeds) {
		newKM <- kmeans(x=x, centers=centers, iter.max=iter.max)
		if(sum(newKM$withinss) < sum(KM$withinss)) {
			KM <- newKM
		}
	}
	KM$tot.withinss <- sum(KM$withinss)
	xmean <- apply(x, 2, mean)
	centers <- rbind(KM$centers, xmean)
	bss1 <- as.matrix(dist(centers)^2)
	KM$betweenss <- sum(as.vector(bss1[nrow(bss1),])*c(KM$size,0))
	return(KM)
}

listKmeansSolutions <- function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects, 
						function(.x) {
							.x <- get(.x, envir=envir)
							if (mode(.x) != "list")
								return(FALSE)
							else "cluster" == names(.x)[1] && "centers" == names(.x)[2]
						}
				)]
}

kmeansClustering <- function () {
	defaults <- list(initial.x = NULL, initial.subset  = gettextPMgui("<all valid cases>"), 
			initial.nClusters = "2", initial.seeds  = "10", initial.iters = "10", 
			initial.clusterSummary  = 1, initial.clusterPlot = 1, initial.clusterAssign = 0, 
			initial.clusterVariable = gettextPMgui("KMeans"))
	dialog.values <- getDialog ("kmeansClustering", defaults)
	initializeDialog (title = gettextPMgui("KMeans Clustering"))
	dataFrame <- tkframe(top)
	xBox <- variableListBox(dataFrame, Numeric(), selectmode = "multiple", 
			initialSelection = varPosn(dialog.values$initial.x, "numeric"), 
			title = gettextPMgui("Variables (pick one or more)"))
	subsetBox(subset.expression = dialog.values$initial.subset)
	optionsFrame <- tkframe(top)
	clusterNumber <- tclVar(dialog.values$initial.nClusters)
	clusterNumSlider <- tkscale(optionsFrame, from = 2, to = 10, 
			showvalue = TRUE, variable = clusterNumber, resolution = 1, 
			orient = "horizontal")
	seedNumber <- tclVar(dialog.values$initial.seeds)
	seedNumSlider <- tkscale(optionsFrame, from = 1, to = 20, 
			showvalue = TRUE, variable = seedNumber, resolution = 1, 
			orient = "horizontal")
	iterNumber <- tclVar(dialog.values$initial.iters)
	iterNumSlider <- tkscale(optionsFrame, from = 5, to = 30, 
			showvalue = TRUE, variable = iterNumber, resolution = 5, 
			orient = "horizontal")
	summaryClusters <- tclVar(dialog.values$initial.clusterSummary)
	summaryCB <- tkcheckbutton(optionsFrame)
	tkconfigure(summaryCB, variable = summaryClusters)
	plotClusters <- tclVar(dialog.values$initial.clusterPlot)
	plotCB <- tkcheckbutton(optionsFrame)
	tkconfigure(plotCB, variable = plotClusters)
	assignClusters <- tclVar(dialog.values$initial.clusterAssign)
	assignCB <- tkcheckbutton(optionsFrame)
	tkconfigure(assignCB, variable = assignClusters)
	assignName <- tclVar(dialog.values$initial.clusterVariable)
	assignField <- ttkentry(optionsFrame, width = "15", textvariable = assignName)
	onOK <- function() {
		x <- getSelection(xBox)
		nvar <- length(x)
		subset <- trim.blanks(tclvalue(subsetVariable))
		nClusters <- tclvalue(clusterNumber)
		seeds <- tclvalue(seedNumber)
		iters <- tclvalue(iterNumber)
		clusterSummary <- tclvalue(summaryClusters)
		clusterPlot <- tclvalue(plotClusters)
		clusterAssign <- tclvalue(assignClusters)
		clusterVariable <- trim.blanks(tclvalue(assignName))
		closeDialog()
		putDialog("kmeansClustering", list(initial.x = x, initial.subset  = subset, 
						initial.nClusters = nClusters, initial.seeds  = seeds, initial.iters = iters, 
						initial.clusterSummary  = clusterSummary,  initial.clusterPlot = clusterPlot, 
						initial.clusterAssign = clusterAssign, initial.clusterVariable = clusterVariable))
		if (clusterAssign == "1") {
			if (is.element(clusterVariable, Variables())) {
				if ("no" == tclvalue(checkReplace(clusterVariable))) {
					kmeansClustering()
					return()
				}
			}
		}
		if (length(x) == 0) {
			errorCondition(recall = kmeansClustering, message = gettextPMgui("No variables selected."))
			return()
		}
		varFormula <- paste(x, collapse = " + ")
		vars <- paste(x, collapse = ",", sep = "")
		.activeDataSet <- ActiveDataSet()
		dset <- if (trim.blanks(subset) == gettextPMgui("<all valid cases>")) 
					.activeDataSet
				else {
					paste(.activeDataSet, "[", .activeDataSet, "$", subset, 
							", ]", sep = "")
				}
		xmat <- paste("model.matrix(~-1 + ", varFormula, ", ", 
				dset, ")", sep = "")
		command <- paste("KMeans(", xmat, ", centers = ", nClusters, 
				", iter.max = ", iters, ", num.seeds = ", seeds, 
				")", sep = "")
		assign(".cluster", justDoIt(command), envir = .GlobalEnv)
		logger(paste(".cluster <- ", command, sep = ""))
		if (clusterSummary == "1") {
			doItAndPrint(paste(".cluster$size # Cluster Sizes"))
			doItAndPrint(paste(".cluster$centers # Cluster Centroids"))
			doItAndPrint(paste(".cluster$withinss # Within Cluster Sum of Squares"))
			doItAndPrint(paste(".cluster$tot.withinss # Total Within Sum of Squares"))
			doItAndPrint(paste(".cluster$betweenss # Between Cluster Sum of Squares"))
		}
		if (clusterPlot == "1") {
			plotCommand <- paste("biplot(princomp(", xmat, "), xlabs = as.character(.cluster$cluster))", 
					sep = "")
			justDoIt(plotCommand)
			logger(plotCommand)
		}
		if (clusterAssign == "1") {
			assignCommand <- paste(.activeDataSet, "$", clusterVariable, 
					" <- assignCluster(", xmat, ", ", .activeDataSet, 
					", .cluster$cluster)", sep = "")
			justDoIt(assignCommand)
			logger(assignCommand)
			activeDataSet(.activeDataSet, flushDialogMemory=FALSE)
		}
		justDoIt(paste("remove(.cluster)"))
		logger(paste("remove(.cluster)"))
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "KMeans", reset = "kmeansClustering")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Number of clusters:")), 
			clusterNumSlider, sticky = "sw")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Number of starting seeds:")), 
			seedNumSlider, sticky = "sw")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Maximum iterations:")), 
			iterNumSlider, sticky = "sw")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Print cluster summary")), 
			summaryCB, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Bi-plot of clusters")), 
			plotCB, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Assign clusters to\nthe data set         ")), 
			assignCB, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Assignment variable: ")), 
			assignField, sticky = "w")
	tkgrid(dataFrame, labelPMgui(top, text = "  "), optionsFrame, 
			sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 3, sticky = "w")
	dialogSuffix(rows = 3, columns = 3)
}

listHclustSolutions <- function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects,
						function(.x) "hclust" == class(get(.x, envir=envir))[1]) ]
}

hierarchicalCluster <- function () {
	solutionNumber = length(listHclustSolutions())
	defaults <- list(initial.x = NULL, initial.clusMethod = "ward", initial.distance = "euc",  
			initial.subset = gettextPMgui ("<all valid cases>"),
			initial.dendro = 1)
	dialog.values <- getDialog("hierarchicalCluster", defaults)
	initializeDialog(title = gettextPMgui("Hierarchical Clustering"))
	solutionFrame <- tkframe(top)
	solutionName <- tclVar(paste("HClust.", (solutionNumber + 
								1), sep = ""))
	solutionField <- ttkentry(solutionFrame, width = "20", textvariable = solutionName)
	dataFrame <- tkframe(top)
	xBox <- variableListBox(dataFrame, Numeric(), selectmode = "multiple", 
			title = gettextPMgui("Variables (pick one or more)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	subsetBox(dataFrame, subset.expression = dialog.values$initial.subset)
	radioButtons(name = "method", buttons = c("ward", "single", 
					"complete", "average", "mcquitty", "median", "centroid"), 
			labels = gettextPMgui(c("Ward's Method", "Single Linkage", 
							"Complete Linkage", "Average Linkage", "McQuitty's Method", 
							"Median Linkage", "Centroid Linkage")), title = gettextPMgui("Clustering Method"), 
			initialValue = dialog.values$initial.clusMethod)
	optionsFrame <- tkframe(top)
	radioButtons(optionsFrame, name = "distanceType", buttons = c("euc", 
					"euc2", "city", "none"), labels = gettextPMgui(c("Euclidean", 
							"Squared-Euclidian", "Manhattan (City Block)", "No Transformation")), 
			title = gettextPMgui("Distance Measure"), 
			initialValue = dialog.values$initial.distance)
	checkFrame <- tkframe(optionsFrame)
	plotDendro <- tclVar(dialog.values$initial.dendro)
	plotCB <- tkcheckbutton(checkFrame)
	tkconfigure(plotCB, variable = plotDendro)
	onOK <- function() {
		x <- getSelection(xBox)
		nvar <- length(x)
		clusMethod <- tclvalue(methodVariable)
		distance <- tclvalue(distanceTypeVariable)
		subset <- trim.blanks(tclvalue(subsetVariable))
		dendro <- tclvalue(plotDendro)
		solution <- trim.blanks(tclvalue(solutionName))
		if (length(x) == 0) {
			errorCondition(recall = hierarchicalCluster, message = gettextPMgui("No variables selected."))
			return()
		}
		putDialog("hierarchicalCluster", list(initial.x = x, initial.clusMethod = clusMethod, 
						initial.distance = distance, initial.subset = subset,
						initial.dendro = dendro))
		closeDialog()
		varFormula <- paste(x, collapse = "+")
		vars <- paste(x, collapse = ",", sep = "")
		.activeDataSet <- ActiveDataSet()
		dset <- if (subset == gettextPMgui("<all valid cases>")) 
					.activeDataSet
				else {
					paste(.activeDataSet, "[", .activeDataSet, "$", subset, 
							", ]", sep = "")
				}
		xmat <- paste("model.matrix(~-1 + ", varFormula, ", ", 
				dset, ")", sep = "")
		if (distance == "euc") {
			dx <- paste("dist(", xmat, ")", sep = "")
			distlab <- "euclidian"
		}
		else if (distance == "euc2") {
			dx <- paste("dist(", xmat, ")^2", sep = "")
			distlab <- "squared-euclidian"
		}
		else if (distance == "city") {
			dx <- paste("dist(", xmat, ", method= ", "\"manhattan\"", 
					")", sep = "")
			distlab <- "city-block"
		}
		else {
			dx <- xmat
			distlab <- "untransformed"
		}
		command <- paste("hclust(", dx, " , method= ", "\"", 
				clusMethod, "\"", ")", sep = "")
		assign(solution, justDoIt(command), envir = .GlobalEnv)
		logger(paste(solution, " <- ", command, sep = ""))
		if (dendro == "1") {
			justDoIt(paste("plot(", solution, ", main= ", "\"", 
							"Cluster Dendrogram for Solution ", solution, 
							"\"", ", xlab= ", "\"", "Observation Number in Data Set ", 
							dset, "\"", ", sub=", "\"", "Method=", clusMethod, 
							"; Distance=", distlab, "\"", ")", sep = ""))
			logger(paste("plot(", solution, ", main= ", "\"", 
							"Cluster Dendrogram for Solution ", solution, 
							"\"", ", xlab= ", "\"", "Observation Number in Data Set ", 
							dset, "\"", ", sub=", "\"", "Method=", clusMethod, 
							"; Distance=", distlab, "\"", ")", sep = ""))
		}
		activateMenus()
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "hclust", reset = "hierarchicalCluster", model = TRUE)
	tkgrid(solutionField, sticky = "w")
	tkgrid(labelPMgui(top, text = gettextPMgui("Clustering solution name:")), 
			solutionFrame, sticky = "w")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(distanceTypeFrame, sticky = "w")
	tkgrid(labelPMgui(checkFrame, text = "  "), sticky = "w")
	tkgrid(labelPMgui(checkFrame, text = gettextPMgui("Plot Dendrogram  ")), 
			plotCB, sticky = "w")
	tkgrid(checkFrame, sticky = "w")
	tkgrid(dataFrame, methodFrame, optionsFrame, sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 3, sticky = "w")
	dialogSuffix(rows = 3, columns = 3)
}

hclustSummary <- function () {
	defaults <- list(initial.clusters = 2, initial.clusterSummary = 1, initial.clusterPlot = 1)
	dialog.values <- getDialog("hclustSummary", defaults)
	parseDataSet <- function(x) {
		y <- get(x)$call
		string1 <- unlist(strsplit(as.character(y)[2], "\\("))
		string2 <- unlist(strsplit(string1[3], ","))
		if (length(grep("\\[", string2[2])) == 0) {
			out <- gsub(")", "", gsub(" ", "", gsub("\\^2", "", 
									string2[2])))
		}
		else {
			string3 <- unlist(strsplit(string2[2], "\\["))
			out <- gsub(" ", "", string3[1])
		}
		return(out)
	}
	hclustObjects <- listHclustSolutions()
	testDataSet <- tapply(hclustObjects, as.factor(1:length(hclustObjects)), 
			parseDataSet)
	.activeDataSet <- ActiveDataSet()
	validHclust <- hclustObjects[testDataSet == .activeDataSet]
	initializeDialog(title = gettextPMgui("Hierarchical Cluster Summary"))
	hclustBox <- variableListBox(top, validHclust, selectmode = "single", 
			title = gettextPMgui("Select One Clustering Solution"))
	optionsFrame <- tkframe(top)
	clusterNumber <- tclVar(dialog.values$initial.clusters)
	slider <- tkscale(optionsFrame, from = 2, to = 10, showvalue = TRUE, 
			variable = clusterNumber, resolution = 1, orient = "horizontal")
	summaryClusters <- tclVar(dialog.values$initial.clusterSummary)
	summaryCB <- tkcheckbutton(optionsFrame)
	tkconfigure(summaryCB, variable = summaryClusters)
	plotClusters <- tclVar(dialog.values$initial.clusterPlot)
	plotCB <- tkcheckbutton(optionsFrame)
	tkconfigure(plotCB, variable = plotClusters)
	if (length(hclustObjects) == 0) {
		errorCondition(recall = return, message = gettextPMgui("There are no hierachical clustering solutions"))
	}
	if (length(validHclust) == 0) {
		errorCondition(recall = return, message = gettextPMgui("No hierachical clustering solutions are associated with this data set."))
	}
	onOK <- function() {
		solution <- getSelection(hclustBox)
		if (length(solution) == 0) {
			errorCondition(recall = hclustSummary, message = gettextPMgui("A clustering solution has not been selected."))
			return()
		}
		clusters <- as.numeric(tclvalue(clusterNumber))
		clusterVar <- paste("cutree(", solution, ", k = ", clusters, 
				")", sep = "")
		clusterSummary <- tclvalue(summaryClusters)
		clusterPlot <- tclvalue(plotClusters)
		putDialog ("hclustSummary", list(initial.clusters = clusters, 
						initial.clusterSummary = clusterSummary, initial.clusterPlot = clusterPlot))
		hclustCall <- get(solution)$call
		string1 <- unlist(strsplit(as.character(hclustCall)[2], 
						"\\("))
		string2 <- unlist(strsplit(string1[3], ","))
		form.vars <- string2[1]
		closeDialog()
		if (length(grep("\\[", string2[2])) == 0) {
			xmat <- paste("model.matrix(", form.vars, ", ", .activeDataSet, 
					")", sep = "")
		}
		else {
			string3 <- unlist(strsplit(string2[2], "\\["))
			xmat <- paste("model.matrix(", form.vars, ", ", .activeDataSet, 
					"[", string3[2], ", ]", ")", sep = "")
		}
		if (clusterSummary == "1") {
			doItAndPrint(paste("summary(as.factor(", clusterVar, 
							")) # Cluster Sizes", sep = ""))
			centroidsCommand <- paste("by(", xmat, ", as.factor(", 
					clusterVar, "), colMeans) # Cluster Centroids", sep = "")
			doItAndPrint(centroidsCommand)
		}
		if (clusterPlot == "1") {
			plotCommand <- paste("biplot(princomp(", xmat, "), xlabs = as.character(", 
					clusterVar, "))", sep = "")
			justDoIt(plotCommand)
			logger(plotCommand)
		}
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "biplot", reset = "hclustSummary")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Number of clusters:")), 
			slider, sticky = "sw")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Print cluster summary")), 
			summaryCB, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("Bi-plot of clusters")), 
			plotCB, sticky = "w")
	tkgrid(getFrame(hclustBox), optionsFrame, sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 2, columns = 3)
}

appendHclustGroup <- function () {
	defaults <- list(initial.clusters = 2, initial.label = gettextPMgui ("hclus.label"))
	dialog.values <- getDialog("appendHclustGroup", defaults)
	parseDataSet <- function(x) {
		y <- get(x)$call
		string1 <- unlist(strsplit(as.character(y)[2], "\\("))
		string2 <- unlist(strsplit(string1[3], ","))
		if (length(grep("\\[", string2[2])) == 0) {
			out <- gsub(")", "", gsub(" ", "", gsub("\\^2", "", 
									string2[2])))
		}
		else {
			string3 <- unlist(strsplit(string2[2], "\\["))
			out <- gsub(" ", "", string3[1])
		}
		return(out)
	}
	hclustObjects <- listHclustSolutions()
	if (length(hclustObjects) == 0) {
		Message(message = gettextPMgui("There are no hierachical clustering solutions"), 
				type = "error")
		return()
	}
	testDataSet <- tapply(hclustObjects, as.factor(1:length(hclustObjects)), 
			parseDataSet)
	.activeDataSet <- ActiveDataSet()
	validHclust <- hclustObjects[testDataSet == .activeDataSet]
	if (length(validHclust) == 0) {
		Message(message = gettextPMgui("No hierachical clustering solutions are associated with this data set."), 
				type = "error")
		return()
	}
	initializeDialog(title = gettextPMgui("Append Cluster Groups to the Active Data Set"))
	hclustBox <- variableListBox(top, validHclust, selectmode = "single", 
			title = gettextPMgui("Select One Clustering Solution"))
	optionsFrame <- tkframe(top)
	labelName <- tclVar(dialog.values$initial.label)
	labelNameField <- ttkentry(optionsFrame, width = "15", textvariable = labelName)
	clusterNumber <- tclVar(dialog.values$initial.clusters)
	slider <- tkscale(optionsFrame, from = 2, to = 10, showvalue = TRUE, 
			variable = clusterNumber, resolution = 1, orient = "horizontal")
	onOK <- function() {
		clusters <- as.numeric(tclvalue(clusterNumber))
		label <- trim.blanks(tclvalue(labelName))
		putDialog ("appendHclustGroup", list(initial.clusters = tclvalue(clusterNumber), initial.label = label))
		solution <- getSelection(hclustBox)
		if (length(solution) == 0) {
			errorCondition(recall = appendHclustGroup, message = gettextPMgui("A clustering solution has not been selected."))
			return()
		}
		
		closeDialog()
		if (is.element(label, Variables())) {
			if ("no" == tclvalue(checkReplace(label))) {
				appendHclustGroup()
				return()
			}
		}
		hclustCall <- get(solution)$call
		string1 <- unlist(strsplit(as.character(hclustCall)[2], 
						"\\("))
		string2 <- unlist(strsplit(string1[3], ","))
		form.vars <- string2[1]
		if (length(grep("\\[", string2[2])) == 0) {
			xmat <- paste("model.matrix(", form.vars, ", ", .activeDataSet, 
					")", sep = "")
		}
		else {
			string3 <- unlist(strsplit(string2[2], "\\["))
			xmat <- paste("model.matrix(", form.vars, ", ", .activeDataSet, 
					"[", string3[2], ", ]", ")", sep = "")
		}
		clusterVar <- paste("cutree(", solution, ", k = ", clusters, 
				")", sep = "")
		command <- paste(.activeDataSet, "$", label, " <- assignCluster(", 
				xmat, ", ", .activeDataSet, ", ", clusterVar, ")", 
				sep = "")
		result <- justDoIt(command)
		logger(command)
		if (class(result)[1] != "try-error") 
			activeDataSet(.activeDataSet, flushDialogMemory=FALSE)
		tkfocus(PmetricsWindow())
	}
	OKCancelHelp(helpSubject = "assignCluster", reset = "appendHclustGroup")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("  Assigned cluster label:")), 
			labelNameField, sticky = "w")
	tkgrid(labelPMgui(optionsFrame, text = gettextPMgui("  Number of clusters:")), 
			slider, sticky = "sw")
	tkgrid(getFrame(hclustBox), optionsFrame, sticky = "nw")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 2, columns = 3)
} 

