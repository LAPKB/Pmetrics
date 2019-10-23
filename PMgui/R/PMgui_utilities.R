# last modified 2012-03-16 by J. Fox
#  applied patch to improve window behaviour supplied by Milan Bouchet-Valat 2011-09-22
#  slight changes 12 Aug 04 by Ph. Grosjean

# utility functions

#set options
setOption <- function(option, default, global=TRUE) {
  current <- options("PMgui")[[1]]
  opt <- if (is.null(current[option][[1]])) default else current[option][[1]]
  if (global) putPMgui(option, opt)
  else opt
}

# listing objects etc.

listDataSets <- function(envir=.GlobalEnv, ...) {
	Vars <- ls(envir = envir, all.names = TRUE) # + PhG
	if (length(Vars) == 0) return(Vars) # + PhG
	
	names(which(sapply(Vars, function(.x) is.data.frame(get(.x, envir=envir)))))
}

listLinearModels <- function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects,
						function(.x) "lm" == (class(get(.x, envir=envir))[1]))]
}

listAOVModels <- function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects,
						function(.x) "aov" == (class(get(.x, envir=envir))[1]))]
}

listGeneralizedLinearModels <- function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects,
						function(.x) "glm" == (class(get(.x, envir=envir))[1]))]
}

listMultinomialLogitModels <- function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects,
						function(.x) "multinom" == (class(get(.x, envir=envir))[1]))]
}

listProportionalOddsModels <- function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects,
						function(.x) "polr" == (class(get(.x, envir=envir))[1]))]
}

listAllModels <- function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects,
						function(.x) (class(get(.x, envir=envir))[1])) %in% getPMgui("modelClasses")]
}

activeDataSet <- function(dsname, flushModel=TRUE, flushDialogMemory=TRUE){
	.activeDataSet <- ActiveDataSet()
	if (missing(dsname)) {
		if (is.null(.activeDataSet)){
			Message(message=gettextPMgui("There is no active data set."), type="error")
			return(FALSE)
		}
		else return(.activeDataSet)
	}
	if (!is.data.frame(ds <- get(dsname, envir=.GlobalEnv))){
		if (!exists.method("as.data.frame", ds, default=FALSE)){
			Message(message=paste(dsname, gettextPMgui(" is not a data frame and cannot be attached."),
							sep=""), type="error")
			tkfocus(PmetricsWindow())
			return()
		}
		command <- paste(dsname, " <- as.data.frame(", dsname, ")", sep="")
		justDoIt(command)
		logger(command)
		Message(message=paste(dsname, gettextPMgui(" has been coerced to a data frame"), sep=""),
				type="warning")
	}
	varnames <- names(get(dsname, envir=.GlobalEnv))
	newnames <- make.names(varnames)
	badnames <- varnames != newnames
	if (any(badnames)){
		command <- paste("names(", dsname, ") <- make.names(names(",
				dsname, "))", sep="")
		doItAndPrint(command)
	}
	if (!is.null(.activeDataSet) && getPMgui("attach.data.set")
			&& (length(grep(.activeDataSet, search())) !=0)) {
		detach(pos = match(.activeDataSet, search()))
		logger(paste("detach(", .activeDataSet, ")", sep=""))
	}
	if (flushModel) {
		putPMgui(".activeModel", NULL)
		PMguiTclSet("modelName", gettextPMgui("<No active model>"))
		if (!is.SciViews()) tkconfigure(getPMgui("modelLabel"), foreground="red") else refreshStatus()
	}
	if (flushDialogMemory) putPMgui("dialog.values", list())
	# -PhG tkconfigure(.modelLabel, foreground="red")
	ActiveDataSet(dsname)
	Message(sprintf(gettextPMgui("The dataset %s has %d rows and %d columns."), dsname,
					nrow(get(dsname, envir=.GlobalEnv)), ncol(get(dsname, envir=.GlobalEnv))), type="note")
	if (any(badnames)) Message(message=paste(dsname, gettextPMgui(" contains non-standard variable names:\n"),
						paste(varnames[badnames], collapse=", "),
						gettextPMgui("\nThese have been changed to:\n"), paste(newnames[badnames], collapse=", "),
						sep=""), type="warning")
	Variables(listVariables())
	Numeric(listNumeric())
	Factors(listFactors())
	TwoLevelFactors(listTwoLevelFactors())
	PMguiTclSet("dataSetName", paste(" ", dsname, " "))
	# -PhG tkconfigure(.dataSetLabel, foreground="blue")
	if (!is.SciViews()) tkconfigure(getPMgui("dataSetLabel"), foreground="blue") else refreshStatus() # +PhG
	if (getPMgui("attach.data.set")){
		attach(get(dsname, envir=.GlobalEnv), name=dsname)
		logger(paste("attach(", dsname, ")", sep=""))
	}
	if (is.SciViews()) refreshStatus() else if (flushModel) tkconfigure(getPMgui("modelLabel"), foreground="red") # +PhG (& J.Fox, 25Dec04)
	activateMenus()
	dsname
}


activeModel <- function(model){
	if (missing(model)) {
		.activeModel <- ActiveModel()
		if (is.null(.activeModel)){
			Message(message=gettextPMgui("There is no active model."), type="error")
			return(FALSE)
		}
		else return(.activeModel)
	}
	ActiveModel(model)
	PMguiTclSet("modelName", paste(" ", model, " "))
	# -PhG tkconfigure(.modelLabel, foreground="blue")
	if (!is.SciViews()) tkconfigure(getPMgui("modelLabel"), foreground="blue") else refreshStatus() # +PhG
	activateMenus()
	model
}

listVariables <- function(dataSet=ActiveDataSet()) {
	vars <- names(get(dataSet, envir=.GlobalEnv))
	if (getPMgui("sort.names")) sortVarNames(vars) else vars
}

listFactors <- function(dataSet=ActiveDataSet()) {
	variables <- if (exists("variables", envir=PMguiEnv())) getPMgui("variables") else listVariables(dataSet)
	variables[sapply(variables, function(.x)
						is.factor(eval(parse(text=.x), envir=get(dataSet, envir=.GlobalEnv))))]
}

listTwoLevelFactors <- function(dataSet=ActiveDataSet()){
	factors <- listFactors(dataSet)
	if(length(factors) == 0) return(NULL)
	factors[sapply(factors, function(.x)
						2 == length(levels(eval(parse(text=.x), envir=get(dataSet, envir=.GlobalEnv)))))]
}

listNumeric <- function(dataSet=ActiveDataSet()) {
	variables <- if (exists("variables", envir=PMguiEnv())) getPMgui("variables") else listVariables(dataSet)
	variables[sapply(variables,function(.x)
						is.numeric(eval(parse(text=.x), envir=get(dataSet, envir=.GlobalEnv))))]
}

trim.blanks <- function(text){
	gsub("^\ *", "", gsub("\ *$", "", text))
}

is.valid.name <- function(x){
	length(x) == 1 && is.character(x) && x == make.names(x)
}


# statistical

colPercents <- function(tab, digits=1){
	dim <- length(dim(tab))
	if (is.null(dimnames(tab))){
		dims <- dim(tab)
		dimnames(tab) <- lapply(1:dim, function(i) 1:dims[i])
	}
	sums <- apply(tab, 2:dim, sum)
	per <- apply(tab, 1, function(x) x/sums)
	dim(per) <- dim(tab)[c(2:dim,1)]
	per <- aperm(per, c(dim, 1:(dim-1)))
	dimnames(per) <- dimnames(tab)
	per <- round(100*per, digits)
	result <- abind(per, Total=apply(per, 2:dim, sum), Count=sums, along=1)
	names(dimnames(result)) <- names(dimnames(tab))
	result
}

rowPercents <- function(tab, digits=1){
	dim <- length(dim(tab))
	if (dim == 2) return(t(colPercents(t(tab), digits=digits)))
	tab <- aperm(tab, c(2,1,3:dim))
	aperm(colPercents(tab, digits=digits), c(2,1,3:dim))
}

totPercents <- function(tab, digits=1){
	dim <- length(dim(tab))
	if (is.null(dimnames(tab))){
		dims <- dim(tab)
		dimnames(tab) <- lapply(1:dim, function(i) 1:dims[i])
	}
	tab <- 100*tab/sum(tab)
	tab <- cbind(tab, rowSums(tab))
	tab <- rbind(tab, colSums(tab))
	rownames(tab)[nrow(tab)] <- "Total"
	colnames(tab)[ncol(tab)] <- "Total"
	round(tab, digits=digits)
}

reliability <- function(S){
	reliab <- function(S, R){
		k <- dim(S)[1]
		ones <- rep(1, k)
		v <- as.vector(ones %*% S %*% ones)
		alpha <- (k/(k - 1)) * (1 - (1/v)*sum(diag(S)))
		rbar <- mean(R[lower.tri(R)])
		std.alpha <- k*rbar/(1 + (k - 1)*rbar)
		c(alpha=alpha, std.alpha=std.alpha)
	}
	result <- list()
	if ((!is.numeric(S)) || !is.matrix(S) || (nrow(S) != ncol(S))
			|| any(abs(S - t(S)) > max(abs(S))*1e-10) || nrow(S) < 2)
		stop(gettextPMgui("argument must be a square, symmetric, numeric covariance matrix"))
	k <- dim(S)[1]
	s <- sqrt(diag(S))
	R <- S/(s %o% s)
	rel <- reliab(S, R)
	result$alpha <- rel[1]
	result$st.alpha <- rel[2]
	if (k < 3) {
		warning(gettextPMgui("there are fewer than 3 items in the scale"))
		return(invisible(NULL))
	}
	rel <- matrix(0, k, 3)
	for (i in 1:k) {
		rel[i, c(1,2)] <- reliab(S[-i, -i], R[-i, -i])
		a <- rep(0, k)
		b <- rep(1, k)
		a[i] <- 1
		b[i] <- 0
		cov <- a %*% S %*% b
		var <- b %*% S %*% b
		rel[i, 3] <- cov/(sqrt(var * S[i,i]))
	}
	rownames(rel) <- rownames(S)
	colnames(rel) <- c("Alpha", "Std.Alpha", "r(item, total)")
	result$rel.matrix <- rel
	class(result) <- "reliability"
	result
}

print.reliability <- function(x, digits=4, ...){
	cat(paste("Alpha reliability = ", round(x$alpha, digits), "\n"))
	cat(paste("Standardized alpha = ", round(x$st.alpha, digits), "\n"))
	cat("\nReliability deleting each item in turn:\n")
	print(round(x$rel.matrix, digits))
	invisible(x)
}

partial.cor <- function(X, ...){
	R <- cor(X, ...)
	RI <- solve(R)
	D <- 1/sqrt(diag(RI))
	R <- - RI * (D %o% D)
	diag(R) <- 0
	rownames(R) <- colnames(R) <- colnames(X)
	R
}

Confint <- function(object, parm, level=0.95, ...) UseMethod("Confint")

Confint.default <- function(object, parm, level = 0.95, ...) {
	ci <- confint(object, parm, level, ...)
	ci <- cbind(coef(object), ci)
	colnames(ci)[1] <- "Estimate"
	ci
}

Confint.glm <- function (object, parm, level=0.95, type=c("LR", "Wald"), ...){
	# adapted from stats:::confint.lm
	type <- match.arg(type)
	cf <- coef(object)
	pnames <- names(cf)
	if (type == "LR") 
		ci <- (MASS:::confint.glm(object, parm, level, ...))
	else {
		if (missing(parm))
			parm <- seq(along = pnames)
		else if (is.character(parm))
			parm <- match(parm, pnames, nomatch = 0)
		a <- (1 - level)/2
		a <- c(a, 1 - a)
		pct <- paste(round(100 * a, 1), "%")
		ci <- array(NA, dim = c(length(parm), 2), dimnames = list(pnames[parm],
						pct))
		ses <- sqrt(diag(vcov(object)))[parm]
		fac <- qnorm(a)
		ci[] <- cf[parm] + ses %o% fac
	}
	ci <- cbind(cf, ci)
	colnames(ci)[1] <- "Estimate"
	fam <- family(object)
	if (fam$family == "binomial" && fam$link == "logit"){
		expci <- exp(ci)
		colnames(expci)[1] <- "exp(Estimate)"
		ci <- cbind(ci, expci)
	}
	ci
}

confint.polr <- function (object, parm, level=0.95, ...){
	# adapted from stats:::confint.lm
	cf <- coef(object)
	pnames <- names(cf)
	if (missing(parm))
		parm <- seq(along = pnames)
	else if (is.character(parm))
		parm <- match(parm, pnames, nomatch = 0)
	a <- (1 - level)/2
	a <- c(a, 1 - a)
	pct <- paste(round(100 * a, 1), "%")
	ci <- array(NA, dim = c(length(parm), 2), dimnames = list(pnames[parm],
					pct))
	ses <- sqrt(diag(vcov(object)))[parm]
	fac <- qnorm(a)
	ci[] <- cf[parm] + ses %o% fac
	ci
}

confint.multinom <- function (object, parm, level=0.95, ...){
	# adapted from stats:::confint.lm
	require("abind")
	cf <- coef(object)
	if (is.vector(cf)) cf <- matrix(cf, nrow=1,
				dimnames=list(object$lev[2], names(cf)))
	pnames <- colnames(cf)
	if (missing(parm))
		parm <- seq(along = pnames)
	else if (is.character(parm))
		parm <- match(parm, pnames, nomatch = 0)
	a <- (1 - level)/2
	a <- c(a, 1 - a)
	ses <- matrix(sqrt(diag(vcov(object))),
			ncol=ncol(cf), byrow=TRUE)[,parm, drop=FALSE]
	cf <- cf[,parm, drop=FALSE]
	fac <- qnorm(a)
	ci <- abind(cf + fac[1]*ses, cf + fac[2]*ses, along=3)
	dimnames(ci)[[3]] <- paste(round(100 * a, 1), "%")
	aperm(ci, c(2,3,1))
}

Confint.multinom <- function(object, parm, level = 0.95, ...) confint (object, parm=parm, level=0.95, ...)

numSummary <- function(data, 
		statistics=c("mean", "sd", "quantiles", "cv", "skewness", "kurtosis"),
		type=c("1", "2", "3"),
		quantiles=c(0, .25, .5, .75, 1), groups){
	sd <- function(x, type, ...){
		apply(as.matrix(x), 2, stats::sd, na.rm=TRUE)
	}
	cv <- function(x, ...){
		x <- as.matrix(x)
		mean <- colMeans(x, na.rm=TRUE)
		sd <- sd(x)
		if (any(x <= 0, na.rm=TRUE)) warning("not all values are positive")
		cv <- sd/mean
		cv[mean <= 0] <- NA
		cv
	}
	skewness <- function(x, type, ...){
		if (is.vector(x)) return(e1071::skewness(x, type=type, na.rm=TRUE))
		apply(x, 2, skewness, type=type)
	}
	kurtosis <- function(x, type, ...){
		if (is.vector(x)) return(e1071::kurtosis(x, type=type, na.rm=TRUE))
		apply(x, 2, kurtosis, type=type)
	}
	if(!require(abind)) stop("abind package missing")
	if(!require(e1071)) stop("e1071 package missing")
	data <- as.data.frame(data)
	if (!missing(groups)) groups <- as.factor(groups)
	variables <- names(data)
	if (missing(statistics)) statistics <- c("mean", "sd", "quantiles")
	statistics <- match.arg(statistics, c("mean", "sd", "quantiles", "cv", "skewness", "kurtosis"),
			several.ok=TRUE)
	type <- match.arg(type)
	type <- as.numeric(type)
	ngroups <- if(missing(groups)) 1 else length(grps <- levels(groups))
	quantiles <- if ("quantiles" %in% statistics) quantiles else NULL
#	quants <- if (length(quantiles) > 1) paste(100*quantiles, "%", sep="")
#			else NULL
	quants <- paste(100*quantiles, "%", sep="")
	nquants <- length(quants)
	stats <- c(c("mean", "sd", "cv", "skewness", "kurtosis")[c("mean", "sd", "cv", "skewness", "kurtosis") %in% statistics], quants)
	nstats <- length(stats)
	nvars <- length(variables)
	result <- list()
	if ((ngroups == 1) && (nvars == 1) && (length(statistics) == 1)){
		if (statistics == "quantiles")
			table <- quantile(data[,variables], probs=quantiles, na.rm=TRUE)
		else {
			table <- do.call(statistics, list(x=data[,variables], na.rm=TRUE, type=type))
			names(table) <- statistics
		}
		NAs <- sum(is.na(data[,variables]))
		n <- nrow(data) - NAs
		result$type <- 1
	}
	else if ((ngroups > 1)  && (nvars == 1) && (length(statistics) == 1)){
		if (statistics == "quantiles"){
			table <- matrix(unlist(tapply(data[, variables], groups,
									quantile, probs=quantiles, na.rm=TRUE)), ngroups, nquants,
					byrow=TRUE)
			rownames(table) <- grps
			colnames(table) <- quants
		}
		else table <- tapply(data[,variables], groups, statistics,
					na.rm=TRUE, type=type)
		NAs <- tapply(data[, variables], groups, function(x)
					sum(is.na(x)))
		n <- table(groups) - NAs
		result$type <- 2
	}
	else if ((ngroups == 1) ){
		X <- as.matrix(data[, variables])
		table <- matrix(0, nvars, nstats)
		rownames(table) <- if (length(variables) > 1) variables else ""
		colnames(table) <- stats
		if ("mean" %in% stats) table[,"mean"] <- colMeans(X, na.rm=TRUE)
		if ("sd" %in% stats) table[,"sd"] <- sd(X)
		if ("cv" %in% stats) table[,"cv"] <- cv(X)
		if ("skewness" %in% statistics) table[, "skewness"] <- skewness(X, type=type)
		if ("kurtosis" %in% statistics) table[, "kurtosis"] <- kurtosis(X, type=type)
		if ("quantiles" %in% statistics){
			table[,quants] <- t(apply(data[, variables, drop=FALSE], 2, quantile,
							probs=quantiles, na.rm=TRUE))
		}
		NAs <- colSums(is.na(data[,variables, drop=FALSE]))
		n <- nrow(data) - NAs
		result$type <- 3
	}
	else {
		table <- array(0, c(ngroups, nstats, nvars),
				dimnames=list(Group=grps, Statistic=stats, Variable=variables))
		NAs <- matrix(0, nvars, ngroups)
		rownames(NAs) <- variables
		colnames(NAs) <- grps
		for (variable in variables){
			if ("mean" %in% stats)
				table[, "mean", variable] <- tapply(data[, variable],
						groups, mean, na.rm=TRUE)
			if ("sd" %in% stats)
				table[, "sd", variable] <- tapply(data[, variable],
						groups, sd, na.rm=TRUE)
			if ("cv" %in% stats)
				table[, "cv", variable] <- tapply(data[, variable],
						groups, cv)
			if ("skewness" %in% stats)
				table[, "skewness", variable] <- tapply(data[, variable],
						groups, skewness, type=type)
			if ("kurtosis" %in% stats)
				table[, "kurtosis", variable] <- tapply(data[, variable],
						groups, kurtosis, type=type)
			if ("quantiles" %in% statistics) {
				res <- matrix(unlist(tapply(data[, variable], groups,
										quantile, probs=quantiles, na.rm=TRUE)), ngroups, nquants,
						byrow=TRUE)
				table[, quants, variable] <- res
			}
			NAs[variable,] <- tapply(data[, variable], groups, function(x)
						sum(is.na(x)))
		}
		if (nstats == 1) table <- table[,1,]
		if (nvars == 1) table <- table[,,1]
		n <- table(groups)
		n <- matrix(n, nrow=nrow(NAs), ncol=ncol(NAs), byrow=TRUE)
		n <- n - NAs
		result$type <- 4
	}
	result$table <- table
	result$statistics <- statistics
	result$n <- n
	if (any(NAs > 0)) result$NAs <- NAs
	class(result) <- "numSummary"
	result
}

print.numSummary <- function(x, ...){
	NAs <- x$NAs
	table <- x$table
	n <- x$n
	statistics <- x$statistics
	switch(x$type,
			"1" = {
				if (!is.null(NAs)) {
					table <- c(table, n, NAs)
					names(table)[length(table) - 1:0] <- c("n", "NA")
				}
				print(table)
			},
			"2" = {
				if (statistics == "quantiles") {
					table <- cbind(table, n)
					colnames(table)[ncol(table)] <- "n"
					if (!is.null(NAs)) {
						table <- cbind(table, NAs)
						colnames(table)[ncol(table)] <- "NA"
					}
				}
				else {
					table <- rbind(table, n)
					rownames(table)[c(1, nrow(table))] <- c(statistics, "n")
					if (!is.null(NAs)) {
						table <- rbind(table, NAs)
						rownames(table)[nrow(table)] <- "NA"
					}
					table <- t(table)
				}
				print(table)
			},
			"3" = {
				table <- cbind(table, n)
				colnames(table)[ncol(table)] <- "n"
				if (!is.null(NAs)) {
					table <- cbind(table, NAs)
					colnames(table)[ncol(table)] <- "NA"
				}
				print(table)
			},
			"4" = {
				if (length(dim(table)) == 2){
					n <- t(n)
					nms <- colnames(n)
					colnames(n) <- paste(nms, ":n", sep="")
					table <- cbind(table, n)
					if (!is.null(NAs)) {
						NAs <- t(NAs)
						nms <- colnames(NAs)
						colnames(NAs) <- paste(nms, ":NA", sep="")
						table <- cbind(table, NAs)
					}
					print(table)
				}
				else {
					table <- abind(table, t(n), along=2)
					dimnames(table)[[2]][dim(table)[2]] <- "n"
					if (!is.null(NAs)) {
						table <- abind(table, t(NAs), along=2)
						dimnames(table)[[2]][dim(table)[2]] <- "NA"
					}
					nms <- dimnames(table)[[3]]
					for (name in nms){
						cat("\nVariable:", name, "\n")
						print(table[,,name])
					}
				}
			}
	)
	invisible(x)
}

stepwise <- function(mod, 
		direction=c("backward/forward", "forward/backward", "backward", "forward"), 
		criterion=c("BIC", "AIC"), ...){
	if (!require(MASS)) stop("MASS package not available")
	criterion <- match.arg(criterion)
	cat("\nDirection: ", direction)
	cat("\nCriterion: ", criterion, "\n\n")
	k <- if (criterion == "BIC") log(nrow(model.matrix(mod))) else 2
	rhs <- paste(c("~", deparse(formula(mod)[[3]])), collapse="")
	rhs <- gsub(" ", "", rhs)
	if (direction == "forward" || direction == "forward/backward")
		mod <- update(mod, . ~ 1)
	if (direction == "backward/forward" || direction == "forward/backward") direction <- "both"
	lower <- ~ 1
	upper <- eval(parse(text=rhs))   
	stepAIC(mod, scope=list(lower=lower, upper=upper), direction=direction, k=k, ...)
}

# wrapper function for histograms

Hist <- function(x, scale=c("frequency", "percent", "density"), xlab=deparse(substitute(x)), 
		ylab=scale, main="", ...){
	xlab # evaluate
	x <- na.omit(x)
	scale <- match.arg(scale)
	if (scale == "frequency") hist(x, xlab=xlab, ylab=ylab, main=main, ...)
	else if (scale == "density") hist(x, freq=FALSE, xlab=xlab, ylab=ylab, main=main, ...)
	else {
		n <- length(x)
		hist(x, axes=FALSE, xlab=xlab, ylab=ylab, main=main, ...)
		axis(1)
		max <- ceiling(10*par("usr")[4]/n)
		at <- if (max <= 3) (0:(2*max))/20
				else (0:max)/10
		axis(2, at=at*n, labels=at*100)
	}
	box()
	abline(h=0)
	invisible(NULL)
}

plotMeans <- function(response, factor1, factor2, error.bars = c("se", "sd", "conf.int", "none"),
		level=0.95, xlab=deparse(substitute(factor1)), ylab=paste("mean of", deparse(substitute(response))),
		legend.lab=deparse(substitute(factor2)), main="Plot of Means",
		pch=1:n.levs.2, lty=1:n.levs.2, col=palette(), ...){
	if (!is.numeric(response)) stop(gettextPMgui("Argument response must be numeric."))
	xlab # force evaluation
	ylab
	legend.lab
	error.bars <- match.arg(error.bars)
	if (missing(factor2)){
		if (!is.factor(factor1)) stop(gettextPMgui("Argument factor1 must be a factor."))
		valid <- complete.cases(factor1, response)
		factor1 <- factor1[valid]
		response <- response[valid]
		means <- tapply(response, factor1, mean)
		sds <- tapply(response, factor1, sd)
		ns <- tapply(response, factor1, length)
		if (error.bars == "se") sds <- sds/sqrt(ns)
		if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
		sds[is.na(sds)] <- 0
		yrange <-  if (error.bars != "none") c( min(means - sds, na.rm=TRUE), max(means + sds, na.rm=TRUE)) else range(means, na.rm=TRUE)
		levs <- levels(factor1)
		n.levs <- length(levs)
		plot(c(1, n.levs), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main, ...)
		points(1:n.levs, means, type="b", pch=16, cex=2)
		box()
		axis(2)
		axis(1, at=1:n.levs, labels=levs)
		if (error.bars != "none") arrows(1:n.levs, means - sds, 1:n.levs, means + sds,
					angle=90, lty=2, code=3, length=0.125)
	}
	else {
		if (!(is.factor(factor1) | is.factor(factor2))) stop(gettextPMgui("Arguments factor1 and factor2 must be factors."))
		valid <- complete.cases(factor1, factor2, response)
		factor1 <- factor1[valid]
		factor2 <- factor2[valid]
		response <- response[valid]
		means <- tapply(response, list(factor1, factor2), mean)
		sds <- tapply(response, list(factor1, factor2), sd)
		ns <- tapply(response, list(factor1, factor2), length)
		if (error.bars == "se") sds <- sds/sqrt(ns)
		if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
		sds[is.na(sds)] <- 0
		yrange <-  if (error.bars != "none") c( min(means - sds, na.rm=TRUE), max(means + sds, na.rm=TRUE)) else range(means, na.rm=TRUE)
		levs.1 <- levels(factor1)
		levs.2 <- levels(factor2)
		n.levs.1 <- length(levs.1)
		n.levs.2 <- length(levs.2)
		if (length(pch) == 1) pch <- rep(pch, n.levs.2)
		if (length(col) == 1) col <- rep(col, n.levs.2)
		if (length(lty) == 1) lty <- rep(lty, n.levs.2)
		if (n.levs.2 > length(col)) stop(sprintf(gettextPMgui("Number of groups for factor2, %d, exceeds number of distinct colours, %d."), n.levs.2, length(col)))		
		plot(c(1, n.levs.1 * 1.4), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main, ...)
		box()
		axis(2)
		axis(1, at=1:n.levs.1, labels=levs.1)
		for (i in 1:n.levs.2){
			points(1:n.levs.1, means[, i], type="b", pch=pch[i], cex=2, col=col[i], lty=lty[i])
			if (error.bars != "none") arrows(1:n.levs.1, means[, i] - sds[, i],
						1:n.levs.1, means[, i] + sds[, i], angle=90, code=3, col=col[i], lty=lty[i], length=0.125)
		}
		x.posn <- n.levs.1 * 1.1
		y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3,4)])
		text(x.posn, y.posn, legend.lab, adj=c(0, -.5))
		legend(x.posn, y.posn, levs.2, pch=pch, col=col, lty=lty)
	}
	invisible(NULL)
}

bin.var <- function (x, bins=4, method=c("intervals", "proportions", "natural"), labels=FALSE){
	method <- match.arg(method)
# Author: Dan Putler (revision by J. Fox, 5 Dec 04)
	if(length(x) < bins) {
		stop(gettextPMgui("The number of bins exceeds the number of data values"))
	}
	x <- if(method == "intervals") cut(x, bins, labels=labels)
			else if (method == "proportions") cut(x, quantile(x, probs=seq(0,1,1/bins), na.rm=TRUE),
						include.lowest = TRUE, labels=labels)
			else {
				xx <- na.omit(x)
				breaks <- c(min(xx), tapply(xx, KMeans(xx, bins)$cluster, max))
				cut(x, breaks, include.lowest=TRUE, labels=labels)
			}
	as.factor(x)
}

# the following function is adapted from a suggestion by Robert Muenchen

rcorr.adjust <- function(x, type=c("pearson", "spearman"), 
		use=c("complete.obs", "pairwise.complete.obs")){
	require("Hmisc")
	type <- match.arg(type)
	use <- match.arg(use)
	x <- if (use == "complete.obs") as.matrix(na.omit(x)) else as.matrix(x)
	R <- rcorr(x, type=type)
	P <- R$P
	p <- P[lower.tri(P)]
	adj.p <- p.adjust(p, method="holm")
	P[lower.tri(P)] <- adj.p
	P[upper.tri(P)] <- 0
	P <- P + t(P)
	P <- ifelse(P < 1e-04, 0, P)
	P <- format(round(P, 4))
	P[grep("NA", P)] <- ""
	res <- list(R=R, P=P)
	class(res) <- "rcorr.adjust"
	res
}

print.rcorr.adjust <- function(x, ...){
	print(x$R)
	cat("\n Adjusted p-values (Holm's method)\n")
	print(x$P, quote = FALSE)
}

# Pager

# this is slightly modified from tkpager to use the PMgui monospaced font
#   and a white background

PMguiPager <- function (file, header, title, delete.file)
{
	title <- paste(title, header)
	for (i in seq(along = file)) {
		zfile <- file[[i]]
		tt <- tktoplevel()
		tkwm.title(tt, if (length(title))
							title[(i - 1)%%length(title) + 1]
						else "")
		txt <- tktext(tt, bg = "white", font = getPMgui("logFont"))
		scr <- ttkscrollbar(tt, command = function(...) tkyview(txt,
							...))
		tkconfigure(txt, yscrollcommand = function(...) tkset(scr,
							...))
		tkpack(txt, side = "left", fill = "both", expand = TRUE)
		tkpack(scr, side = "right", fill = "y")
		chn <- tcl("open", zfile)
		tkinsert(txt, "end", gsub("_\b", "", tclvalue(tcl("read",
										chn))))
		tcl("close", chn)
		tkconfigure(txt, state = "disabled")
		tkmark.set(txt, "insert", "0.0")
		tkfocus(txt)
		if (delete.file)
			tcl("file", "delete", zfile)
	}
}

# help functions

helpPmetrics <- function() {
	PDF <- file.access(paste(file.path(.path.package(package="PMgui")[1], "doc"), 
					"/", gettextPMgui("Pmetrics"), ".pdf", sep=""), mode=4)
	if (PDF == 0){
		browseURL(paste(file.path(.path.package(package="PMgui")[1], "doc"),
						"/", gettextPMgui("Pmetrics"), ".pdf", sep=""))
	} 
	else if (as.numeric(R.Version()$major) >= 2) print(help(gettextPMgui("Pmetrics")))
	else help(gettextPMgui("Pmetrics"))
}

helpAboutPmetrics <- function() {
	if (as.numeric(R.Version()$major) >= 2) print(help("PMgui"))
	else help("PMgui")
}

browseManual <- function() {
	browseURL(paste(file.path(.path.package(package="PMgui")[1], "doc"),
					"/", gettextPMgui("Getting-Started-with-the-PMgui"), ".pdf", sep=""))
}



# functions for building dialog boxes

# the following function is slightly modified from Thomas Lumley,
#   "Programmer's Niche: Macros in R," R-News, Sept. 2001, Vol. 1, No. 3, pp.11-13.
defmacro <- function(..., expr){
	expr <- substitute(expr)
	len <- length(expr)
	expr[3:(len+1)] <- expr[2:len]
	## delete "macro" variables starting in ..
	expr[[2]] <- quote(on.exit(remove(list=objects(pattern="^\\.\\.", all.names=TRUE))))
	a <- substitute(list(...))[-1]
	## process the argument list
	nn <- names(a)
	if (is.null(nn)) nn <- rep("", length(a))
	for (i in seq(length.out=length(a))){
		if (nn[i] == "") {
			nn[i] <- paste(a[[i]])
			msg <- paste(a[[i]], gettext("not supplied", domain="R-PMgui"))
			a[[i]] <- substitute(stop(foo), list(foo = msg))
		}
	}
	names(a) <- nn
	a <- as.list(a)
	ff <- eval(substitute(
					function(){
						tmp <- substitute(body)
						eval(tmp, parent.frame())
					},
					list(body = expr)))
	## add the argument list
	formals(ff) <- a
	## create a fake source attribute
	mm <- match.call()
	mm$expr <- NULL
	mm[[1]] <- as.name("macro")
	expr[[2]] <- NULL # get "local" variable removal out of source
	attr(ff, "source") <- c(deparse(mm), deparse(expr))
	## return the macro
	ff
}

OKCancelHelp <- defmacro(window=top, helpSubject=NULL,  model=FALSE, reset=NULL,
		expr={
			memory <- getPMgui("retain.selections")
			buttonsFrame <- tkframe(window, borderwidth=5)
			OKbutton <- buttonPMgui(buttonsFrame, text=gettextPMgui("OK"), foreground="darkgreen", width="12", command=onOK, default="active",
					borderwidth=3)
			onCancel <- function() {
				if (model) putPMgui("modelNumber", getPMgui("modelNumber") - 1)
				if (GrabFocus()) tkgrab.release(window)
				tkdestroy(window)
				tkfocus(PmetricsWindow())
			}
			cancelButton <- buttonPMgui(buttonsFrame, text=gettextPMgui("Cancel"), foreground="red", width="12", command=onCancel, borderwidth=3)
			if (!is.null(helpSubject)){
				onHelp <- function() {
					if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
					if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
					else help(helpSubject)
				}
				helpButton <- buttonPMgui(buttonsFrame, text=gettextPMgui("Help"), width="12", command=onHelp, borderwidth=3)
			}
			if (!is.null(reset) && memory){
				onReset <- function(){
					if (model) putPMgui("modelNumber", getPMgui("modelNumber") - 1)
					putDialog(reset, NULL)
					putDialog(reset, NULL, resettable=FALSE)
					closeDialog()
					eval(parse(text=paste(reset, "()")))
				}
				resetButton <- buttonPMgui(buttonsFrame, text=gettextPMgui("Reset"), width=12, command=onReset)
			}
			tkgrid(OKbutton, labelPMgui(buttonsFrame, text="  "), cancelButton, labelPMgui(buttonsFrame, text="            "),
					if(!is.null(reset) && memory) resetButton, if(!is.null(reset) && memory) labelPMgui(buttonsFrame, text="  "), 
					if (!is.null(helpSubject)) helpButton, sticky="w")
		})

subOKCancelHelp <- defmacro(window=subdialog, helpSubject=NULL,
		expr={
			subButtonsFrame <- tkframe(window, borderwidth=5)
			subOKbutton <- buttonPMgui(subButtonsFrame, text=gettextPMgui("OK"), foreground="darkgreen", width="12", command=onOKsub, default="active",
					borderwidth=3)
			onCancelSub <- function() {
				if (GrabFocus()) tkgrab.release(window)
				tkdestroy(window)
				tkfocus(PmetricsWindow())
			}
			subCancelButton <- buttonPMgui(subButtonsFrame, text=gettextPMgui("Cancel"), foreground="red", width="12", command=onCancelSub,
					borderwidth=3)
			if (!is.null(helpSubject)){
				onHelpSub <- function(){
					if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
					if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
					else help(helpSubject)
				}
				subHelpButton <- buttonPMgui(subButtonsFrame, text=gettextPMgui("Help"), width="12", command=onHelpSub, borderwidth=3)
			}
			tkgrid(subOKbutton, labelPMgui(subButtonsFrame, text="  "), subCancelButton,
					labelPMgui(subButtonsFrame, text="            "), if (!is.null(helpSubject)) subHelpButton, sticky="w")
		})

checkActiveDataSet <- function(){
	if (activeDataSet() == FALSE) {
		tkfocus(PmetricsWindow())
		FALSE
	}
	else TRUE
}

checkActiveModel <- function(){
	if (activeModel() == FALSE) {
		tkfocus(PmetricsWindow())
		FALSE
	}
	else TRUE
}

checkFactors <- function(n=1){
	if (length(Factors()) < n){
		if (n > 1)
			Message(message=sprintf(gettextPMgui("There fewer than %d factors in the active data set."), n),
					type="error")
		else Message(message=gettextPMgui("There are no factors in the active data set."),
					type="error")
		tkfocus(PmetricsWindow())
		FALSE
	}
	else TRUE
}

checkTwoLevelFactors <- function(n=1){
	if (length(TwoLevelFactors()) < n){
		if (n > 1)
			Message(message=sprintf(gettextPMgui("There fewer than %d two-level factors in the active data set."), n),
					type="error")
		else Message(message=gettextPMgui("There are no two-level factors in the active data set."),
					type="error")
		tkfocus(PmetricsWindow())
		FALSE
	}
	else TRUE
}

checkNumeric <- function(n=1){
	if (length(Numeric()) < n){
		if (n > 1)
			Message(message=sprintf(gettextPMgui("There fewer than %d numeric variables in the active data set."), n),
					type="error")
		else Message(message=gettextPMgui("There are no numeric variables in the active data set."),
					type="error")
		tkfocus(PmetricsWindow())
		FALSE
	}
	else TRUE
}

checkVariables <- function(n=1){
	if (length(Variables()) < n){
		if (n > 1)
			Message(message=sprintf(gettextPMgui("There fewer than %d variables in the active data set."), n),
					type="error")
		else Message(message=gettextPMgui("There are no variables in the active data set."),
					type="error")
		tkfocus(PmetricsWindow())
		FALSE
	}
	else TRUE
}

PmetricsPosition <- function (){
	ID <- PmetricsWindow()$ID
	as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
					tclvalue(.Tcl(paste("winfo rooty", ID)))))
}

initializeDialog <- defmacro(window=top, title="", offset=10, preventCrisp=FALSE,
		expr={
			if ((!preventCrisp) && getPMgui("crisp.dialogs")) tclServiceMode(on=FALSE)
			window <- tktoplevel(borderwidth=10)
			tkwm.title(window, title)
			tkwm.transient(window, PmetricsWindow())
			position <- if (is.SciViews()) -1 else PmetricsPosition() # +PhG
			position <- if (any(position < 0)) "-50+50"
					else paste("+", paste(offset + position, collapse="+"), sep="")
			tkwm.geometry(window, position)
		}
)

closeDialog <- defmacro(window=top, release=TRUE,
		expr={
			if (release && GrabFocus()) tkgrab.release(window)
			tkdestroy(window)
		}
)

dialogSuffix <- defmacro(window=top, onOK=onOK, onCancel=onCancel, rows=1, columns=1, focus=top,
		bindReturn=TRUE, preventGrabFocus=FALSE, preventDoubleClick=FALSE,
		preventCrisp=FALSE,
		expr={
			for (row in 0:(rows-1)) tkgrid.rowconfigure(window, row, weight=0)
			for (col in 0:(columns-1)) tkgrid.columnconfigure(window, col, weight=0)
			.Tcl("update idletasks")
			tkwm.resizable(window, 0, 0)
			if (bindReturn) tkbind(window, "<Return>", onOK)
			tkbind(window, "<Escape>", onCancel)
			if (getPMgui("double.click") && (!preventDoubleClick)) tkbind(window, "<Double-ButtonPress-1>", onOK)
			tkwm.deiconify(window)
			# focus grabs appear to cause problems for some dialogs
			if (GrabFocus() && (!preventGrabFocus)) tkgrab.set(window)
			tkfocus(focus)
			tkwait.window(window)
			if ((!preventCrisp) && getPMgui("crisp.dialogs")) tclServiceMode(on=TRUE)
		}
)

variableListBox <- function(parentWindow, variableList=Variables(), bg="white",
		selectmode="single", export="FALSE", initialSelection=NULL, listHeight=getPMgui("variable.list.height"), title){
	if (selectmode == "multiple") selectmode <- getPMgui("multiple.select.mode")
	if (length(variableList) == 1 && is.null(initialSelection)) initialSelection <- 0
	frame <- tkframe(parentWindow)
	minmax <- getPMgui("variable.list.width")
	listbox <- tklistbox(frame, height=min(listHeight, length(variableList)),
			selectmode=selectmode, background=bg, exportselection=export, 
			width=min(max(minmax[1], nchar(variableList)), minmax[2]))
	scrollbar <- ttkscrollbar(frame, command=function(...) tkyview(listbox, ...))
	tkconfigure(listbox, yscrollcommand=function(...) tkset(scrollbar, ...))
	for (var in variableList) tkinsert(listbox, "end", var)
	if (is.numeric(initialSelection)) for (sel in initialSelection) tkselection.set(listbox, sel)
	firstChar <- tolower(substr(variableList, 1, 1))
	len <- length(variableList)
	onLetter <- function(letter){
		letter <- tolower(letter)
		current <- 1 + round(as.numeric(unlist(strsplit(tclvalue(tkyview(listbox) ), " "))[1])*len)
		mat <- match(letter, firstChar[-(1:current)])
		if (is.na(mat)) return()
		tkyview.scroll(listbox, mat, "units")
	}
	onA <- function() onLetter("a")
	onB <- function() onLetter("b")
	onC <- function() onLetter("c")
	onD <- function() onLetter("d")
	onE <- function() onLetter("e")
	onF <- function() onLetter("f")
	onG <- function() onLetter("g")
	onH <- function() onLetter("h")
	onI <- function() onLetter("i")
	onJ <- function() onLetter("j")
	onK <- function() onLetter("k")
	onL <- function() onLetter("l")
	onM <- function() onLetter("m")
	onN <- function() onLetter("n")
	onO <- function() onLetter("o")
	onP <- function() onLetter("p")
	onQ <- function() onLetter("q")
	onR <- function() onLetter("r")
	onS <- function() onLetter("s")
	onT <- function() onLetter("t")
	onU <- function() onLetter("u")
	onV <- function() onLetter("v")
	onW <- function() onLetter("w")
	onX <- function() onLetter("x")
	onY <- function() onLetter("y")
	onZ <- function() onLetter("z")
	for (letter in c(letters, LETTERS)){
		tkbind(listbox, paste("<", letter, ">", sep=""),
				get(paste("on", toupper(letter), sep="")))
	}
	onClick <- function() tkfocus(listbox)
	toggleSelection <- function(){
		active <- tclvalue(tkindex(listbox, "active"))
		selected <- tclvalue(tkcurselection(listbox))
		if (selected == active) tkselection.clear(listbox, "active") else tkselection.set(listbox, "active")
	}
	tkbind(listbox, "<ButtonPress-1>", onClick)
	if (selectmode == "single") tkbind(listbox, "<Control-ButtonPress-1>", toggleSelection)
	tkgrid(labelPMgui(frame, text=title, foreground="blue"), columnspan=2, sticky="w")
	tkgrid(listbox, scrollbar, sticky="nw")
	tkgrid.configure(scrollbar, sticky="wns")
	tkgrid.configure(listbox, sticky="ew")
	result <- list(frame=frame, listbox=listbox, scrollbar=scrollbar,
			selectmode=selectmode, varlist=variableList)
	class(result) <- "listbox"
	result
}

getSelection <- function(object) UseMethod("getSelection")

getSelection.listbox <- function(object){
	object$varlist[as.numeric(tkcurselection(object$listbox)) + 1]
}

getFrame <- function(object) UseMethod("getFrame")

getFrame.listbox <- function(object){
	object$frame
}

# This function modified based on code by Liviu Andronic (13 Dec 09):
radioButtons <- defmacro(window=top, name, buttons, values=NULL, initialValue=..values[1], labels, 
		title="", title.color="blue", right.buttons=TRUE,
		expr={
			..values <- if (is.null(values)) buttons else values
			..frame <- paste(name, "Frame", sep="")
			assign(..frame, tkframe(window))
			..variable <- paste(name, "Variable", sep="")
			assign(..variable, tclVar(initialValue))
			if(title != ""){
				tkgrid(labelPMgui(eval(parse(text=..frame)), text=title, foreground=title.color), columnspan=2, sticky="w")
			}
			for (i in 1:length(buttons)) {
				..button <- paste(buttons[i], "Button", sep="")
				assign(..button,
						ttkradiobutton(eval(parse(text=..frame)), variable=eval(parse(text=..variable)), value=..values[i]))
				if (right.buttons) tkgrid(labelPMgui(eval(parse(text=..frame)), text=labels[i], justify="left"), eval(parse(text=..button)), sticky="w")
				else  tkgrid(eval(parse(text=..button)), labelPMgui(eval(parse(text=..frame)), text=labels[i], justify="left"), sticky="w")
			}
		}
)


checkBoxes <- defmacro(window=top, frame, boxes, initialValues=NULL, labels, title=NULL,
		expr={
			..initialValues <- if (is.null(initialValues)) rep("1", length(boxes)) else initialValues
			assign(frame, tkframe(window))
			if (!is.null(title)) tkgrid(labelPMgui(eval(parse(text=frame)), text=title, foreground="blue"), sticky="w")
			..variables <- paste(boxes, "Variable", sep="")
			for (i in 1:length(boxes)) {
				assign(..variables[i], tclVar(..initialValues[i]))
				..checkBox <- paste(boxes[i], "CheckBox", sep="")
				assign(..checkBox,
						tkcheckbutton(eval(parse(text=frame)), variable=eval(parse(text=..variables[i]))))
				tkgrid(labelPMgui(eval(parse(text=frame)), text=labels[i]), eval(parse(text=..checkBox)), sticky="w")
			}
		}
)

checkReplace <- function(name, type=gettextPMgui("Variable")){
	PMguiTkmessageBox(message=sprintf(gettextPMgui("%s %s already exists.\nOverwrite %s?"),
					type, name, tolower(type)), icon="warning", type="yesno", default="no")
}

errorCondition <- defmacro(window=top, recall=NULL, message, model=FALSE,
		expr={
			if (model) putPMgui("modelNumber", getPMgui("modelNumber") - 1)
			if (!is.null(window)){
				if (GrabFocus()) tkgrab.release(window)
				tkdestroy(window)
			}
			Message(message=message, type="error")
			if (!is.null(recall)) recall()
			else tkfocus(PmetricsWindow())
		})

subsetBox <- defmacro(window=top, subset.expression=NULL, model=FALSE,
		expr={
			subsetVariable <- if (!is.null(subset.expression)) tclVar(gettextPMgui(subset.expression))
					else if (model){
						if (currentModel && currentFields$subset != "")
							tclVar(currentFields$subset) else tclVar(gettextPMgui("<all valid cases>"))
					}
					else tclVar(gettextPMgui("<all valid cases>"))
			subsetFrame <- tkframe(window)
			subsetEntry <- ttkentry(subsetFrame, width="20", textvariable=subsetVariable)
			subsetScroll <- ttkscrollbar(subsetFrame, orient="horizontal",
					command=function(...) tkxview(subsetEntry, ...))
			tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
			tkgrid(labelPMgui(subsetFrame, text=gettextPMgui("Subset expression"), foreground="blue"), sticky="w")
			tkgrid(subsetEntry, sticky="w")
			tkgrid(subsetScroll, sticky="ew")
		})


groupsBox <- defmacro(recall=NULL, label=gettextPMgui("Plot by:"), initialLabel=gettextPMgui("Plot by groups"),
		plotLinesByGroup=FALSE, positionLegend=FALSE, plotLinesByGroupsText=gettextPMgui("Plot lines by group"),
		initialGroup=NULL, initialLinesByGroup=1,
		expr={
			env <- environment()
			.groups <- if (is.null(initialGroup)) FALSE else initialGroup
			.linesByGroup <- FALSE
			.groupsLabel <- tclVar(if (!is.null(initialGroup)) initialLabel else paste(initialLabel, "...", sep=""))
			.factors <- Factors()
			onGroups <- function(){
				if (length(.factors) == 0){
					errorCondition(recall=recall, message=gettextPMgui("There are no factors in the active data set."))
					return()
				}
				initializeDialog(subdialog, title=gettextPMgui("Groups"))
				groupsBox <- variableListBox(subdialog, .factors, title=gettextPMgui("Groups variable (pick one)"),
						initialSelection=varPosn(initialGroup, "factor"))
				if (plotLinesByGroup){
					linesByGroupFrame <- tkframe(subdialog)
					linesByGroup <- tclVar(if(initialLinesByGroup == 1) "1" else "0")
					linesCheckBox <- tkcheckbutton(linesByGroupFrame, variable=linesByGroup)
					tkgrid(labelPMgui(linesByGroupFrame, text=plotLinesByGroupsText), linesCheckBox, sticky="w")
				}
				onOKsub <- function() {
					groups <- getSelection(groupsBox)
					if (length(groups) == 0){
						assign(".groups", FALSE, envir=env)
						tclvalue(.groupsLabel) <- paste(gettextPMgui("Plot by groups"), "...", sep="")
						tkconfigure(groupsButton, foreground="black")
						if (GrabFocus()) tkgrab.release(subdialog)
						tkdestroy(subdialog)
						tkwm.deiconify(top)
						if (GrabFocus()) tkgrab.set(top)
						tkfocus(top)
						tkwait.window(top)
						return()
					}
					assign(".groups", groups, envir=env)
					tclvalue(.groupsLabel) <- paste(label, groups)
					tkconfigure(groupsButton, foreground="blue")
					if (plotLinesByGroup) {
						lines <- as.character("1" == tclvalue(linesByGroup))
						assign(".linesByGroup", lines, envir=env)
					}
					if (GrabFocus()) tkgrab.release(subdialog)
					tkdestroy(subdialog)
					tkwm.deiconify(top)
					if (GrabFocus()) tkgrab.set(top)
					tkfocus(top)
					tkwait.window(top)
				}
				subOKCancelHelp()
				tkgrid(getFrame(groupsBox), sticky="nw")
				if (plotLinesByGroup) tkgrid(linesByGroupFrame, sticky="w")
				tkgrid(subButtonsFrame, sticky="w")
				if (positionLegend) tkgrid(labelPMgui(subdialog, text=gettextPMgui("Position legend with mouse click"), fg="blue"))
				dialogSuffix(subdialog, onOK=onOKsub, rows=3+plotLinesByGroup+positionLegend, columns=2, focus=subdialog)
			}
			groupsFrame <- tkframe(top)
			groupsButton <- tkbutton(groupsFrame, textvariable=.groupsLabel, command=onGroups, borderwidth=3)
			if (!is.null(initialGroup)) tkconfigure(groupsButton, foreground="blue")
			tkgrid(labelPMgui(groupsFrame, text="    "), groupsButton, sticky="w")
		})

groupsLabel <- defmacro(frame=top, groupsBox=groupsBox, columnspan=1, initialText=NULL,
		expr={
			groupsFrame <- tkframe(frame)
			.groupsLabel <- if (is.null(initialText)) gettextPMgui("<No groups selected>") else initialText
			groupsLabel <- labelPMgui(groupsFrame, text=.groupsLabel)
			tkgrid(labelPMgui(groupsFrame, text=gettextPMgui("Difference: "), fg="blue"), groupsLabel, sticky="w")
			tkgrid(groupsFrame, sticky="w", columnspan=columnspan)
			onSelect <- function(){
				group <- getSelection(groupsBox)
				levels <- eval(parse(text=paste("levels(", ActiveDataSet(), "$", group, ")", sep="")))
				.groupsLabel <<- paste(levels[1], "-", levels[2])
				tkconfigure(groupsLabel, text=.groupsLabel)
			}
			tkbind(groupsBox$listbox, "<ButtonRelease-1>", onSelect)
		})

modelFormula <- defmacro(frame=top, hasLhs=TRUE, expr={
			checkAddOperator <- function(rhs){
				rhs.chars <- rev(strsplit(rhs, "")[[1]])
				if (length(rhs.chars) < 1) return(FALSE)
				check.char <- if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1))
							rhs.chars[1] else rhs.chars[2]
				!is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%"))
			}
			.variables <- Variables()
			word <- paste("\\[", gettextPMgui("factor"), "\\]", sep="")
			variables <- paste(.variables,
					ifelse(is.element(.variables, Factors()), paste("[", gettextPMgui("factor"), "]", sep=""), ""))
			xBox <- variableListBox(frame, variables, selectmode="multiple", title=gettextPMgui("Variables (double-click to formula)"))
			onDoubleClick <- if (!hasLhs){
						function(){
							var <- getSelection(xBox)
							tkselection.clear(xBox$listbox, "0", "end")					
							if (length(grep(word, var)) == 1) var <- sub(word, "",  var)
							tkfocus(rhsEntry)
							rhs <- tclvalue(rhsVariable)
							rhs.chars <- rev(strsplit(rhs, "")[[1]])
							check.char <- if (length(rhs.chars) > 0){
										if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1))
											rhs.chars[1] else rhs.chars[2]
									}
									else ""
							tclvalue(rhsVariable) <- if (rhs == "" ||
											is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%")))
										paste(rhs, var, sep="")
									else paste(rhs, "+", var)
							tkicursor(rhsEntry, "end")
							tkxview.moveto(rhsEntry, "1")
						}
					}
					else{
						function(){
							var <- getSelection(xBox)
							which <- tkcurselection(xBox$listbox)
							tkselection.clear(xBox$listbox, "0", "end")
							if (length(grep(word, var)) == 1) var <- sub(word, "",  var)
							lhs <- tclvalue(lhsVariable)
							if (lhs == "" || tclvalue(tkselection.present(lhsEntry)) == "1"){
								tclvalue(lhsVariable) <- var
								tkselection.clear(lhsEntry)
								tkfocus(rhsEntry)
							}
							else {
								tkfocus(rhsEntry)
								rhs <- tclvalue(rhsVariable)
								rhs.chars <- rev(strsplit(rhs, "")[[1]])
								check.char <- if (length(rhs.chars) > 0){
											if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1))
												rhs.chars[1] else rhs.chars[2]
										}
										else ""
								tclvalue(rhsVariable) <- if (rhs == "" ||
												is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%")))
											paste(rhs, var, sep="")
										else paste(rhs, "+", var)
							}
							tkicursor(rhsEntry, "end")
							tkxview.moveto(rhsEntry, "1")
						}
					}
			tkbind(xBox$listbox, "<Double-ButtonPress-1>", onDoubleClick)
			onPlus <- function(){
				rhs <- tclvalue(rhsVariable)
				var <- getSelection(xBox)
				tkselection.clear(xBox$listbox, "0", "end")										
				if ((check <- !checkAddOperator(rhs)) && length(var) == 0) return()
				if (length(var) > 1){
					if (length(grep(word, var)) > 0) var <- sub(word, "",  var)
					if (length(var) > 1) var <- paste(var, collapse=" + ")
				}
				tclvalue(rhsVariable) <- paste(rhs, if (!check) " + ", var, sep="")
				tkicursor(rhsEntry, "end")
				tkxview.moveto(rhsEntry, "1")
			}
			onTimes <- function(){
				rhs <- tclvalue(rhsVariable)
				var <- getSelection(xBox)
				tkselection.clear(xBox$listbox, "0", "end")						
				if ((check <- !checkAddOperator(rhs)) && length(var) == 0) return()
				if (length(var) > 1){
					if (length(grep(word, var)) > 0) var <- sub(word, "",  var)
					var <- trim.blanks(var)
					if (length(var) > 1) var <- paste(var, collapse="*")
					tclvalue(rhsVariable) <- paste(rhs, if (!check) " + ", var, sep="")
				}
				else tclvalue(rhsVariable) <- paste(rhs, if (!check) "*", sep="")
				tkicursor(rhsEntry, "end")
				tkxview.moveto(rhsEntry, "1")
			}
			onColon <- function(){
				rhs <- tclvalue(rhsVariable)
				var <- getSelection(xBox)
				tkselection.clear(xBox$listbox, "0", "end")						
				if ((check <- !checkAddOperator(rhs)) && length(var) == 0) return()
				if (length(var) > 1){
					if (length(grep(word, var)) > 0) var <- sub(word, "",  var)
					var <- trim.blanks(var)
					if (length(var) > 1) var <- paste(var, collapse=":")
					tclvalue(rhsVariable) <- paste(rhs, if (!check) " + ", var, sep="")
				}
				else tclvalue(rhsVariable) <- paste(rhs, if (!check) ":", sep="")
				tkicursor(rhsEntry, "end")
				tkxview.moveto(rhsEntry, "1")
			}
			onSlash <- function(){
				rhs <- tclvalue(rhsVariable)
				if (!checkAddOperator(rhs)) return()
				tclvalue(rhsVariable) <- paste(rhs, "/",  sep="")
				tkicursor(rhsEntry, "end")
				tkxview.moveto(rhsEntry, "1")
			}
			onIn <- function(){
				rhs <- tclvalue(rhsVariable)
				if (!checkAddOperator(rhs)) return()
				tclvalue(rhsVariable) <- paste(rhs, "%in% ")
				tkicursor(rhsEntry, "end")
				tkxview.moveto(rhsEntry, "1")
			}
			onMinus <- function(){
				rhs <- tclvalue(rhsVariable)
				if (!checkAddOperator(rhs)) return()
				tclvalue(rhsVariable) <- paste(rhs, "- ")
				tkicursor(rhsEntry, "end")
				tkxview.moveto(rhsEntry, "1")
			}
			onPower <- function(){
				rhs <- tclvalue(rhsVariable)
				if (!checkAddOperator(rhs)) return()
				tclvalue(rhsVariable) <- paste(rhs, "^", sep="")
				tkicursor(rhsEntry, "end")
				tkxview.moveto(rhsEntry, "1")
			}
			onLeftParen <- function(){
				tkfocus(rhsEntry)
				rhs <- tclvalue(rhsVariable)
				tclvalue(rhsVariable) <- paste(rhs, "(", sep="")
				tkicursor(rhsEntry, "end")
				tkxview.moveto(rhsEntry, "1")
			}
			onRightParen <- function(){
				rhs <- tclvalue(rhsVariable)
				if (!checkAddOperator(rhs)) return()
				tclvalue(rhsVariable) <- paste(rhs, ")", sep="")
				tkicursor(rhsEntry, "end")
				tkxview.moveto(rhsEntry, "1")
			}
			outerOperatorsFrame <- tkframe(frame)
			operatorsFrame <- tkframe(outerOperatorsFrame)
			plusButton <- buttonPMgui(operatorsFrame, text="+", width="3", command=onPlus)
			timesButton <- buttonPMgui(operatorsFrame, text="*", width="3", command=onTimes)
			colonButton <- buttonPMgui(operatorsFrame, text=":", width="3", command=onColon)
			slashButton <- buttonPMgui(operatorsFrame, text="/", width="3", command=onSlash)
			inButton <- buttonPMgui(operatorsFrame, text="%in%", width="5", command=onIn)
			minusButton <- buttonPMgui(operatorsFrame, text="-", width="3", command=onMinus)
			powerButton <- buttonPMgui(operatorsFrame, text="^", width="3", command=onPower)
			leftParenButton <- buttonPMgui(operatorsFrame, text="(", width="3", command=onLeftParen)
			rightParenButton <- buttonPMgui(operatorsFrame, text=")", width="3", command=onRightParen)
			
			tkgrid(plusButton, timesButton, colonButton, slashButton, inButton, minusButton,
					powerButton, leftParenButton, rightParenButton, sticky="w")
			formulaFrame <- tkframe(frame)
			if (hasLhs){
				tkgrid(labelPMgui(outerOperatorsFrame, text=gettextPMgui("Model Formula:     "), fg="blue"), operatorsFrame)
				lhsVariable <- if (currentModel) tclVar(currentFields$lhs) else tclVar("")
				rhsVariable <- if (currentModel) tclVar(currentFields$rhs) else tclVar("")
				rhsEntry <- ttkentry(formulaFrame, width="50", textvariable=rhsVariable)
				rhsXscroll <- ttkscrollbar(formulaFrame,
						orient="horizontal", command=function(...) tkxview(rhsEntry, ...))
				tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsXscroll, ...))
				lhsEntry <- ttkentry(formulaFrame, width="10", textvariable=lhsVariable)
				lhsScroll <- ttkscrollbar(formulaFrame,
						orient="horizontal", command=function(...) tkxview(lhsEntry, ...))
				tkconfigure(lhsEntry, xscrollcommand=function(...) tkset(lhsScroll, ...))
				tkgrid(lhsEntry, labelPMgui(formulaFrame, text=" ~    "), rhsEntry, sticky="w")
				tkgrid(lhsScroll, labelPMgui(formulaFrame, text=""), rhsXscroll, sticky="w")
				tkgrid.configure(lhsScroll, sticky="ew")
			}
			else{
				rhsVariable <- if (currentModel) tclVar(currentFields$rhs) else tclVar("")
				rhsEntry <- ttkentry(formulaFrame, width="50", textvariable=rhsVariable)
				rhsXscroll <- ttkscrollbar(formulaFrame,
						orient="horizontal", command=function(...) tkxview(rhsEntry, ...))
				tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsXscroll, ...))
				tkgrid(labelPMgui(formulaFrame, text="   ~ "), rhsEntry, sticky="w")
				tkgrid(labelPMgui(formulaFrame, text=""), rhsXscroll, sticky="w")
			}
			tkgrid.configure(rhsXscroll, sticky="ew")
		})

exists.method <- function(generic, object, default=TRUE, strict=FALSE){
	classes <- class(object)
	if (default) classes <- c(classes, "default")
	if (strict) classes <- classes[1]
	any(paste(generic, ".", classes, sep="") %in%
					as.character(methods(generic)))
}

checkMethod <- defmacro(generic, object, message=NULL, default=FALSE, strict=FALSE, reportError=TRUE,
		expr={
			msg <- if (is.null(message)) sprintf(gettextPMgui("No appropriate %s method exists\nfor a model of this class."), generic)
					else message
			method <- exists.method(generic, get(object), default=default, strict=strict)
			if ((!method) && reportError) Message(message=msg, type="error")
			method
		}
)

checkClass <- defmacro(object, class, message=NULL,
		expr={
			msg <- if (is.null(message)) sprintf(gettextPMgui('The model is not of class "%s".'), class)
					else message
			properClass <- class(get(object))[1] == class
			if (!properClass) Message(message=msg, type="error")
			properClass
		}
)


# the following function is from John Chambers (plus new test for R 2.4.0)

isS4object <- function(object) {
	if (getRversion() < "2.4.0"){
		if (length(attr(object, "class"))!= 1)
			return(FALSE)
		!isVirtualClass(getClass(class(object), TRUE))
	}
	else isS4(object)
}

# the following three functions are slightly adapted with permission from Philippe Grosjean

PMguiEnv <- function() {
	pos <-  match("PMguiEnv", search())
	if (is.na(pos)) { # Must create it
		PMguiEnv <- list()
		attach(PMguiEnv, pos = length(search()) - 1)
		rm(PMguiEnv)
		pos <- match("PMguiEnv", search())
	}
	return(pos.to.env(pos))
}

putPMgui <- function(x, value)
	assign(x, value, envir = PMguiEnv())

getPMgui <- function(x, mode="any")
	get(x, envir = PMguiEnv(), mode = mode, inherits = FALSE)

PMguiTclSet <- function(name, value){
	if (is.SciViews()) return()   # + PhG
	name <- ls(unclass(getPMgui(name))$env)
	tcl("set", name, value)
}

# functions to store or retrieve PMgui state information

Variables <- function(names){
	if (missing(names)) getPMgui("variables")
	else putPMgui("variables", names)
}

Numeric <- function(names){
	if (missing(names)) getPMgui("numeric")
	else putPMgui("numeric", names)
}

Factors <- function(names){
	if (missing(names)) getPMgui("factors")
	else putPMgui("factors", names)
}

TwoLevelFactors <- function(names){
	if (missing(names)) getPMgui("twoLevelFactors")
	else putPMgui("twoLevelFactors", names)
}

# The following two functions were modified by Erich Neuwrith
#  and subsequently by John Fox (23 July 07)

ActiveDataSet <- function(name){
	if (missing(name)) {
		temp <- getPMgui(".activeDataSet")
		if (is.null(temp))
			return(NULL)
		else
		if (!exists(temp) || !is.data.frame(get(temp,envir=.GlobalEnv))) {
			Message(sprintf(gettextPMgui("the dataset %s is no longer available"),
							temp), type="error")
			putPMgui(".activeDataSet", NULL)
			PMguiTclSet("dataSetName", gettextPMgui("<None>"))
			putPMgui(".activeModel", NULL)
			PMguiTclSet("modelName", gettextPMgui("<Nonel>"))
			if (!is.SciViews()) {
				tkconfigure(getPMgui("dataSetLabel"), foreground="red") 
				tkconfigure(getPMgui("modelLabel"), foreground="red") 
			} 
			else refreshStatus()
			activateMenus()
			if (getPMgui("suppress.menus") && RExcelSupported()) return(NULL)
		}
		return(temp)
	}
	else putPMgui(".activeDataSet", name)
}

ActiveModel <- function(name){
	if (missing(name)) {
		temp <- getPMgui(".activeModel")
		if (is.null(temp))
			return(NULL)
		else
		if (!exists(temp) || !is.model(get(temp,envir=.GlobalEnv))) {
			Message(sprintf(gettextPMgui("the model %s is no longer available"),
							temp), type="error")
			putPMgui(".activeModel", NULL)
			PMguiTclSet("modelName", gettextPMgui("<No active model>"))
			if (!is.SciViews()) tkconfigure(getPMgui("modelLabel"), foreground="red") else refreshStatus()
			activateMenus()
			return(NULL)
		}
		else return(temp)
	}
	else putPMgui(".activeModel", name)
}

GrabFocus <- function(value){
	if (missing(value)) getPMgui("grab.focus")
	else putPMgui("grab.focus", value)
}

UpdateModelNumber <- function(increment=1){
	modelNumber <- getPMgui("modelNumber")
	modelNumber <- modelNumber + increment
	if (modelNumber < 1) modelNumber <- 1 # sanity check
	putPMgui("modelNumber", modelNumber)
}

PmetricsWindow <- function() getPMgui("PmetricsWindow")

LogWindow <- function() getPMgui("logWindow")

OutputWindow <- function() getPMgui("outputWindow")

MessagesWindow <- function() getPMgui("messagesWindow")

# some predicates for the menu system

activeDataSetP <- function() !is.null(ActiveDataSet())

dataSetsP <- function(n=1){
	datasets <- listDataSets()
	(!is.null(datasets)) && length(datasets) >= n
}

numericP <- function(n=1) activeDataSetP() && length(listNumeric()) >= n

factorsP <- function(n=1) activeDataSetP() && length(listFactors()) >= n

twoLevelFactorsP <- function(n=1) activeDataSetP() && length(listTwoLevelFactors()) >= n

modelsP <- function(n=1) activeDataSetP() && length(listAllModels()) >= n

activeModelP <- function() !is.null(ActiveModel())

lmP <- function() activeModelP() && any(class(get(ActiveModel()))[1] == c('lm', 'aov'))

glmP <- function() activeModelP() && class(get(ActiveModel()))[1] == 'glm'

aicP <- function() activeModelP() && exists.method("extractAIC", get(ActiveModel()))

polrP <- function() activeModelP() && class(get(ActiveModel()))[1] == 'polr'

multinomP <- function() activeModelP() && class(get(ActiveModel()))[1] == 'multinom'

hclustSolutionsP <- function() length(listHclustSolutions()) > 0

MacOSXP <- function() {
	sys <- Sys.info()
	!is.null(sys) && length(grep("[Dd]arwin", sys["sysname"]) > 0)
}

packageAvailable <- function(name) 0 != length(.find.package(name, quiet=TRUE))

rglLoaded <- function() 0 != length(grep("^rgl", loadedNamespaces()))

activateMenus <- function(){
	if (getPMgui("suppress.menus")) return()
	for (item in getPMgui("Menus")){
		if (item$activation()) .Tcl(paste(item$ID, " entryconfigure ", item$position - 1," -state normal", sep=""))
		else .Tcl(paste(item$ID, " entryconfigure ", item$position - 1," -state disabled", sep=""))
	}
}


# for internationalization

gettextPMgui <- function(...) gettext(..., domain="R-PMgui")

gettextMenus <- function(...){
	text <- gettextPMgui(...)
	plugins <- getOption("PMgui")$plugins
	if (is.null(plugins)) return(text)
	plugins <- paste("R-", plugins, sep="")
	for (plugin in plugins){
		text <- gettext(text, domain=plugin)
	}
	text
}

English <- function() {
	env <- Sys.getenv()
	names(env) <- toupper(names(env))
	LANG <- env["LANGUAGE"]
	LC_CTYPE <- Sys.getlocale("LC_CTYPE")
	if (!is.na(LANG)) length(grep("^en", LANG, ignore.case=TRUE)) > 0
	else LC_CTYPE == "C" || length(grep("^en", LC_CTYPE, ignore.case=TRUE)) > 0
}


# to replace tkmessageBox on non-English Windows systems,
#  to allow for translation of button text

PMguiTkmessageBox <- function(message, icon=c("info", "question", "warning",
				"error"), type=c("okcancel", "yesno", "ok"), default, title="") {
	if ( (English()) || (.Platform$OS.type != "windows") ){
		if (missing(default)){
			default <- switch(type,
					okcancel="ok",
					yesno="yes",
					ok="ok")}
		return(tkmessageBox(message=message, icon=icon, type=type,
						default=default, title=title))
	}
	icon <- match.arg(icon)
	type <- match.arg(type)
	initializeDialog(messageBox, title=title)
	messageFrame <- tkframe(messageBox, borderwidth=5)
	buttonFrame <- tkframe(messageBox,  borderwidth=5)
	if (icon != "question") tkbell()
	result <- tclVar()
	iconColor <- switch(icon, info="blue", question="blue", warning="black",
			error="red")
	onOK <- function() {
		if (GrabFocus()) tkgrab.release(messageBox)
		tkdestroy(messageBox)
		tkfocus(PmetricsWindow())
		tclvalue(result) <- "ok"
	}
	OKbutton <- buttonPMgui(buttonFrame, text=gettextPMgui("OK"),
			foreground="darkgreen", width="12", command=onOK, borderwidth=3,
			default=if (missing(default)) "active"
					else if (default == "ok") "active" else "normal")
	onCancel <- function() {
		if (GrabFocus()) tkgrab.release(messageBox)
		tkdestroy(messageBox)
		tkfocus(PmetricsWindow())
		tclvalue(result) <- "cancel"
	}
	cancelButton <- buttonPMgui(buttonFrame, text=gettextPMgui("Cancel"),
			foreground="red", width="12", command=onCancel, borderwidth=3,
			default=if (missing(default)) "normal"
					else if (default == "cancel") "active" else "normal")
	onYes <- function() {
		if (GrabFocus()) tkgrab.release(messageBox)
		tkdestroy(messageBox)
		tkfocus(PmetricsWindow())
		tclvalue(result) <- "yes"
	}
	yesButton <- buttonPMgui(buttonFrame, text=gettextPMgui("Yes"),
			foreground="darkgreen", width="12", command=onYes, borderwidth=3,
			default=if (missing(default)) "active"
					else if (default == "yes") "active" else "normal")
	onNo <- function() {
		if (GrabFocus()) tkgrab.release(messageBox)
		tkdestroy(messageBox)
		tkfocus(PmetricsWindow())
		tclvalue(result) <- "no"
	}
	noButton <- buttonPMgui(buttonFrame, text=gettextPMgui("No"),
			foreground="red", width="12", command=onNo, borderwidth=3,
			default=if (missing(default)) "normal"
					else if (default == "no") "active" else "normal")
	## FIXME -- left in old style
	tkgrid(tklabel(messageFrame, bitmap=icon, fg=iconColor),
			tklabel(messageFrame, text="    "),
			tklabel(messageFrame, text=message))
	tkgrid(messageFrame)
	switch(type,
			okcancel = {
				tkgrid(OKbutton, labelPMgui(buttonFrame, text="    "), cancelButton)
				if (missing(default) || default == "ok") tkbind(messageBox, "<Return>",
							onOK)
				else if (default == "cancel") tkbind(messageBox, "<Return>", onCancel)
			},
			yesno =  {
				tkgrid(yesButton, labelPMgui(buttonFrame, text="    "), noButton)
				if (missing(default) || default == "yes") tkbind(messageBox, "<Return>",
							onYes)
				else if (default == "no") tkbind(messageBox, "<Return>", onNo)
			},
			ok = {
				tkgrid(OKbutton)
				if (missing(default) || default == "ok") tkbind(messageBox, "<Return>",
							onOK)
			}
	)
	tkgrid(buttonFrame)
	dialogSuffix(messageBox, rows=2, focus=messageBox, bindReturn=FALSE)
	result
}

# The following function was contributed by Matthieu Lesnoff (added 20 July 06)

trim.col.na <- function(dat){
# Remove variables with only missing values (occurs sometimes with modified Excel file)
	colsup <- NULL
	for (i in 1:ncol(dat))
	{
		if (length(dat[is.na(dat[,i])==T,i]) ==length(dat[,i]))
			colsup <- c(colsup,i)
	}
	if (length(colsup) > 0)
		dat <- dat[,-colsup]
	dat
}

# check whether packages are available

packagesAvailable <- function(packages){
	sapply(sapply(packages, .find.package, quiet=TRUE),
			function(x) length(x) != 0)
}

# insert a row (or rows) in a matrix or data frame

insertRows <- function(object1, object2, where=NULL, ...){
	if (ncol(object1) != ncol(object2))
		stop(gettextPMgui("objects have different numbers of columns"))
	if (!(TRUE == all.equal(colnames(object1), colnames(object2))))
		stop(gettextPMgui("objects have different column names"))
	n <- nrow(object1)
	if (is.null(where) || where >= n) rbind(object1, object2)
	else if (where < 1) rbind(object2, object1)
	else rbind(object1[1:floor(where),], object2,
				object1[(floor(where) + 1):n,])
}

# functions for handling PMgui plug-in packages

# the following function based on a suggestion by Brian Ripley

listPlugins <- function(loaded=FALSE){
	plugins <- unlist(lapply(.libPaths(),
					function(x) Sys.glob(file.path(x, "*/etc/menus.txt"))))
	plugins <- sub(".*/([^/]*)/etc/menus.txt", "\\1", plugins)
	if (loaded) plugins else sort(setdiff(plugins, .packages()))
}


loadPlugins <- function(){
	plugins <- listPlugins()
	initializeDialog(title=gettextPMgui("Load Plug-ins"))
	packagesBox <- variableListBox(top, plugins, title=gettextPMgui("Plug-ins (pick one or more)"),
			selectmode="multiple", listHeight=10)
	onOK <- function(){
		plugins <- getSelection(packagesBox)
		closeDialog(top)
		if (length(plugins) == 0){
			errorCondition(recall=loadPlugins, message=gettextPMgui("You must select at least one plug-in."))
			return()
		}
		opts <- options("PMgui")
		opts$PMgui$plugins <- c(plugins, opts$PMgui$plugins)
		options(opts)
		for (plugin in plugins) {
			command <- paste('library("', plugin, '", character.only=TRUE)', sep="")
			justDoIt(command)
		}
		Message(paste(gettextPMgui("Plug-ins loaded:"), paste(plugins, collapse=", ")), type="note")
		response <- tkmessageBox(message=paste(gettextPMgui(
								"The plug-in(s) will not be available until the Pmetrics is restarted.\nRestart now?")),
				icon="question", type="yesno")
		if (tclvalue(response) == "yes") {
			putPMgui("autoRestart", TRUE)
			closePmetrics(ask=FALSE)
			Pmetrics()
		}
	}
	OKCancelHelp(helpSubject="Plugins")
	tkgrid(getFrame(packagesBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=1, columns=1)
}

# the following two functions contributed by Erich Neuwirth (added 22 July 07)

whitespaceonly <- function(str) sub('[[:space:]]+$', '', str) == ''

is.model <- function(object) {
	any(class(object) %in% getPMgui("modelClasses"))
}

# the following lines, adding support for ttk widgets, adapted from code by Brian Ripley
if (!(as.character(tcl("info", "tclversion")) >= "8.5" && getRversion() >= "2.7.0")){
	buttonPMgui <- tkbutton
	labelPMgui <- tklabel
	ttkentry <- function(parent, ...) tkentry(parent, ...)
	ttkframe <- tkframe
	ttkradiobutton <- tkradiobutton
	ttkscrollbar <- function(...) tkscrollbar(..., repeatinterval=5)
} else {
	buttonPMgui <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	labelPMgui <- function(..., fg)
		if(missing(fg)) ttklabel(...) else ttklabel(..., foreground=fg)
}

# the following function alters the default behaviour of tclvalue() by trimming leading and trailing blanks

tclvalue <- function(x) trim.blanks(tcltk::tclvalue(x))

# the following function splits a character string at blanks and commas according to width

splitCmd <- function(cmd, width=getOption("width") - 4, at="[ ,]"){
	if (nchar(cmd) <= width) return(cmd)
	where <- gregexpr(at, cmd)[[1]]
	if (where[1] < 0) return(cmd)
	singleQuotes <- gregexpr("'", cmd)[[1]]
	doubleQuotes <- gregexpr('"', cmd)[[1]]
	comment <- regexpr("#", cmd)
	if (singleQuotes[1] > 0 && (singleQuotes[1] < doubleQuotes[1] || doubleQuotes[1] < 0 ) && (singleQuotes[1] < comment[1] || comment[1] < 0 )){
		nquotes <- length(singleQuotes)
		if (nquotes < 2) stop("unbalanced quotes")
		where[(where > singleQuotes[1]) & (where < singleQuotes[2])] <- NA
		where <- na.omit(where)
	}  
	else if (doubleQuotes[1] > 0 && (doubleQuotes[1] < singleQuotes[1] || singleQuotes[1] < 0) && (doubleQuotes[1] < comment[1] || comment[1] < 0 )){
		nquotes <- length(doubleQuotes)
		if (nquotes < 2) stop("unbalanced quotes")
		where[(where > doubleQuotes[1]) & (where < doubleQuotes[2])] <- NA
		where <- na.omit(where)
	}
	else if (comment > 0){
		where[where > comment] <- NA
		where <- na.omit(where)
	}
	if (length(where) == 0) return(cmd)
	where2 <- where[where <= width]
	where2 <- if (length(where2) == 0) where[1]
			else where2[length(where2)]
	paste(substr(cmd, 1, where2), "\n  ", 
			Recall(substr(cmd, where2 + 1, nchar(cmd)), width, at), sep="")
} 

# the following function sorts names containing numerals "more naturally" than does sort()

sortVarNames <- function(x){
	sort.helper <- function(x){
		prefix <- strsplit(x, "[0-9]+")
		prefix <- sapply(prefix, "[", 1)
		prefix[is.na(prefix)] <- ""
		suffix <- strsplit(x, "[^0-9]+")
		suffix <- as.numeric(sapply(suffix, "[", 2))
		suffix[is.na(suffix)] <- -Inf
		remainder <- sub("[^0-9]+", "", x)
		remainder <- sub("[0-9]+", "", remainder)
		if (all (remainder == "")) list(prefix, suffix)
		else c(list(prefix, suffix), Recall(remainder))
	}
	ord <- do.call("order", sort.helper(x))
	x[ord]
}

# to load packages

Library <- function(package, pos=4){
	loaded <- search()
	loaded <- loaded[grep("^package:", loaded)]
	loaded <- sub("^package:", "", loaded)
	if (!getPMgui("suppress.X11.warnings")){
		messages.connection <- file(open="w+")
		sink(messages.connection, type="message")
		on.exit({
					sink(type="message")
					close(messages.connection)
				})
	}
	if (!(package %in% loaded)){
		command <- paste("library(", package, ", pos=", pos, ")", sep="")
		logger(command)
		result <- try(eval(parse(text=command), envir=.GlobalEnv), silent=TRUE)
		if (class(result)[1] ==  "try-error"){
			Message(message=paste(strsplit(result, ":")[[1]][2]), type="error")
			tkfocus(PmetricsWindow())
			return("error")
		}
		return(package)
	}
	else return(invisible(NULL))
}

# to merge data frames by rows

mergeRows <- function(X, Y, common.only=FALSE, ...){
	UseMethod("mergeRows")
}

mergeRows.data.frame <- function(X, Y, common.only=FALSE, ...){
	cols1 <- names(X)
	cols2 <- names(Y)
	if (common.only){
		common <- intersect(cols1, cols2)
		rbind(X[, common], Y[, common])
	}
	else {
		all <- union(cols1, cols2)
		miss1 <- setdiff(all, cols1)
		miss2 <- setdiff(all, cols2)
		X[, miss1] <- NA
		Y[, miss2] <- NA
		rbind(X, Y)
	}
}

# start help system

startHelp <- function(){
	Sys.sleep(2)
	help.start()
}

# dialog memory support

putDialog <- function (dialog, values=NULL, resettable=TRUE){
	if (resettable){
		dialog.values <- getPMgui("dialog.values")
		dialog.values[[dialog]] <- values
		putPMgui("dialog.values", dialog.values)
	}
	else{
		dialog.values <- getPMgui("dialog.values.noreset")
		dialog.values[[dialog]] <- values
		putPMgui("dialog.values.noreset", dialog.values)
	}
}

getDialog <- function(dialog, defaults=NULL){
	values <- getPMgui("dialog.values.noreset")[[dialog]]
	if (getPMgui("retain.selections") && !is.null(values)) return(values)
	values <- getPMgui("dialog.values")[[dialog]]
	if (!getPMgui("retain.selections") || is.null(values)) return(defaults)
	else return (values)
}

varPosn <- function(variables, type=c("all", "factor", "numeric", "nonfactor", "twoLevelFactor")){
	if (is.null(variables)) return(NULL)
	type <- match.arg(type)
	vars <- switch(type,
			all = Variables(),
			factor = Factors(),
			numeric = Numeric(),
			nonfactor = setdiff(Variables(), Factors()),
			twoLevelFactor = TwoLevelFactors()
	)
	if (any(!variables %in% vars)) NULL
	else apply(outer(variables, vars, "=="), 1, which) - 1
}

flushDialogMemory <- function(what){
	if (missing(what)) putPMgui("dialog.values", list())
	else{
		dialog.values <- getPMgui("dialog.values")
		dialog.values.noreset <- getPMgui("dialog.values.noreset")
		for (dialog in what){
			dialog.values[dialog] <- NULL
			dialog.values.noreset[dialog] <- NULL
		}
		putPMgui("dialog.values", dialog.values)
		putPMgui("dialog.values.noreset", dialog.values.noreset)
	}
}


# submit lines typed into script window
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
  } else messages.connection <- getPMgui("messages.connection")
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
      } else{
        for (line in .Output) tkinsert(.output, "end", paste(line, "\n", sep=""))
        tkyview.moveto(.output, 1)
      }
    } else if (.console.output) sink(type="output")
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
    } else {
      if (length(grep("warning", messages, ignore.case=TRUE)) > 0)
        Message(message=paste(messages, collapse="\n"), type="warning")
      else Message(message=paste(messages, collapse="\n"), type="note")
    }
  } else{
    if (length(messages) == 0) Message()
    else if (length(messages) > 10){
      messages <- c(paste(length(messages), "warnings."),
                    gettextPMgui("First and last 5 warnings:"),
                    head(messages, 5), ". . .", tail(messages, 5))
      Message(message=paste(messages, collapse="\n"), type="warning")
    } else {
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

