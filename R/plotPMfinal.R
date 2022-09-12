#' Plot Pmetrics Final Cycle Parameter Value Distributions
#' 
#' Plot R6 [PM_final] objects made by [makeFinal] and loaded as a field in the
#' [PM_result] object, e.g. `PM_result$final`.
#'
#' @details
#' If `formula` is omitted, this will generate a marginal plot for each parameter.  
#' For NPAG data, this will be a histogram of marginal values for each parameter and the associated probability
#' of that value.  For IT2B, this will be a series of normal distributions with mean and standard deviation
#' equal to the mean and standard deviation of each parameter marginal distribution.  IF `formula` IS specified,
#' this will generate one of two plots.
#' 
#' On the other hand, if `formula` is two parameters, e.g. CL~V, this will generate a bivariate plot.  
#' For NPAG data, it will be support point with size proportional to the probability
#' of each point.  For IT2B, it will be an elliptical distribution of a bivariate normal distribution centered at the mean
#' of each plotted variable and surrounding quantiles of the bivariate distribution plotted in decreasing shades of grey.
#' 
#' The `line` argument is used to format:
#' * the density line drawn from an NPAG [PM_final object] if `density = T` 
#' * the drop lines from an NPAG [PM_final object] when a `formula` is specified
#' to generate a bivariate plot
#' * the lines drawing the normal distribution
#' of parameter values from an IT2B [PM_Final] object. 
#' The `marker` argument is used to format:
#' * the bar fill color and opacity, and the outline color and width
#'  from an NPAG [PM_final object]
#' * the markers used from an NPAG [PM_final object] when a `formula` is specified
#' to generate a bivariate plot
#' @method plot PM_final
#' @param x The name of an [PM_final] data object as a field in a [PM_result] R6
#' object, e.g `PM_result$final`. 
#' @param formula An optional formula of the form `y ~ x`, where `y` and `x`
#' are two model parameters to plot in a 3-dimensional bivariate plot.  See details.
#' @param marker See details for which objects the `marker` argument controls. 
#' This argument maps to the plotly marker object.
#' It can be boolean or a list.
#' `TRUE` will plot the marker with default characteristics.
#' `FALSE` will suppress marker plotting.
#' If a list, can control many marker characteristics, including overriding defaults.
#' Use the plotly `plotly::schema()` command in the console and navigate
#' to traces > scatter > attributes > marker to see all the ways the marker
#' can be formatted. Most common will be:
#' * `color` Fill color for NPAG bars, marker color for bivariate NPAG plots.
#' Ignored for IT2B plots.
#' * `symbol` Plotting character. See `plotly::schema()`, traces > scatter > 
#' attributes > marker > symbol > values. Only relevant for bivariate NPAG plots.
#' * `size` Character size in points. Only relevant for bivariate NPAG plots.
#' * `opacity` Bar fill color for univariate NPAG plots or marker opacity for
#' bivariate NPAG plots. Ignored for IT2B plots. 
#' Ranges between 0 (fully transparent) to 1 (fully opaque).
#' * `line` A list of  additional attributes governing the outline for bars in 
#' univariate NPAG plots or markers in bivariate NPAG plots. Ignored for IT2B plots.
#'     - `color` Outline color. Default is "black".
#'     - `width` Outline width. Default is 1.
#' Example: `marker = list(color = "red", symbol = "triangle", opacity = 0.8, line = list(color = "black", width = 2))`

#' @template line
#' @param density Boolean operator to plot a kernel density function overlying the histogram
#' of a univariate marginal parameter distribution from NPAG; the default is `FALSE`.
#' See [density].  Ignored for IT2B output.
#' @template grid
#' @param xlab Define x-axis label for bivariate NPAG or IT2B plot.  Default is the name of the plotted x-variable.
#' @param ylab Define y-axis label for bivariate NPAG or IT2B plot.  Default is the name of the plotted y-variable.
#' @param standardize Standardize the normal parameter distribution plots from IT2B to the same
#' scale x-axis.  Ignored for NPAG output.
#' @return Plots the object.
#' @author Michael Neely
#' @seealso [PM_final], [schema]
#' @importFrom mvtnorm dmvnorm
#' @export
#' @examples
#' #NPAG
#' NPex$final$plot()
#' NPex$final$plot(density = T)
#' NPex$final$plot(Ke ~ V)
#' #IT2B
#' ITex$final$plot()
#' ITex$final$plot(Ke ~ V)
#' @family PMplots

plot.PM_final <- function(x, 
                          formula, 
                          marker = T,
                          line = T, 
                          density = F, 
                          grid = T,
                          xlab, 
                          ylab, 
                          standardize){
  
  
  
  #housekeeping
  if(inherits(x,"NPAG")){
    type <- "NPAG"
    densityFormat <- amendLine(density, default = list(color = "black"))
    if(inherits(density,"list")) density <- T
    bar <- amendMarker(marker, default = list(color = "dodgerblue", size = 5,
                                              width = 0.02, opacity = 0.5))
    line <- amendLine(line)
  } else {
    type <- "IT2B"
    line <- amendLine(line)
  }
  
  
  
  
  
  ab <- data.frame(x$ab)
  names(ab) <- c("min","max")
  ab$par <- names(x$popMean)
  
  #plot functions for univariate
  uniPlot <- function(.par, .data, .min, .max, type, bar){
    p <- .data %>%
      plotly::plot_ly(x = ~value , y = ~prob, height = 2000) 
    
    if(type == "NPAG"){
      barWidth <- bar$width * (.max - .min) #normalize
      p <- p %>% 
        plotly::add_bars( 
          marker = bar,
          hovertemplate = "Value: %{x:0.3f}<br>Prob: %{y:0.3f}<extra></extra>",
          width = I(barWidth)
        ) 
      
      if(density){
        densList <- density(.data$value, weights = .data$prob)
        dens <- data.frame(x=densList$x, y=densList$y)
        normalize <- max(.data$prob)
        
        p <- p %>% plotly::add_lines(data = dens, x = ~x, y = ~y/max(y) * I(normalize), 
                                     line = densityFormat,
                                     text = round(dens$y,2),
                                     hovertemplate = "Value: %{x:0.2f}<br>Prob: %{text}<extra></extra>") 
      }
    } else { #IT2B
      p <- p %>%
        plotly::add_lines(
          line = line,
          hovertemplate = "Value: %{x:0.2f}<br>Prob: %{y:0.2f}<extra></extra>") 
    }
    
    #common to both
    p <- p %>% 
      plotly::layout(showlegend = F, 
                     xaxis = list(title = list(text = paste0("<b>",.par,"</b>"), size = 18)),
                     yaxis = list(title = list(text = "<b>Probability</b>"), size = 18),
                     shapes = list(vline(.min, width=3, dash="dash"),
                                   vline(.max, width=3, dash="dash")),
                     barmode = "overlay")
    
    return(p)
  }
  
  
  biPlot <- function(formula, x, xlab, ylab){
    yCol <- as.character(attr(terms(formula),"variables")[2])
    xCol <- as.character(attr(terms(formula),"variables")[3])
    if(missing(xlab)) xlab <- xCol
    if(missing(ylab)) ylab <- yCol
    whichX <- which(ab$par == xCol)
    whichY <- which(ab$par == yCol)
    
    if(type == "IT2B"){
      rangeX <- as.numeric(ab[whichX,1:2])
      rangeY <- as.numeric(ab[whichY,1:2])
      coords <- data.frame(x = seq(rangeX[1],rangeX[2],
                                   (rangeX[2] - rangeX[1])/100),
                           y = seq(rangeY[1],rangeY[2],
                                   (rangeY[2] - rangeY[1])/100)) %>%
        tidyr::expand(x,y)
      
      z <- mvtnorm::dmvnorm(coords,mean=as.numeric(x$popMean[1,c(whichX,whichY)]),
                            sigma=as.matrix(x$popCov[c(whichX,whichY),c(whichX,whichY)])
      )
      z <- matrix(z, nrow=101)
      
      p <- plot_ly(x = ~unique(coords$x), y = ~unique(coords$y), z = ~z) %>% 
        plotly::add_surface(
          hovertemplate = paste0(xlab,": %{x:0.2f}<br>",ylab,":%{y:0.2f}<br>Prob: %{z:0.2f}<extra></extra>")) %>%
        plotly::layout(scene = list( 
          xaxis = list(title = list(text = xlab, font = list(size = 18))),
          yaxis = list(title = list(text = ylab, font = list(size = 18))),
          zaxis = list(title = list(text = "Probability", font = list(size = 18))))) %>%
        plotly::hide_colorbar()
      return(p)
    } else { #NPAG
      
      x$popPoints$id <- seq_len(nrow(x$popPoints))
      pp <- replicate(3, x$popPoints, simplify = F)
      x$popPoints <- x$popPoints %>% select(-id) #undo modification
      
      #make object for drop lines
      pp[[2]]$prob <- min(pp[[1]]$prob)
      pp2 <- dplyr::bind_rows(pp, .id = "key") %>% dplyr::arrange(id,key) 
      pp2[pp2$key == 3,] <- NA
      pp2 <- pp2 %>%
        dplyr::select(x=whichX[1]+1, y=whichY[1]+1, prob=prob)
      
      p <- x$popPoints %>% select(x=whichX[1], y=whichY[1], prob=prob) %>%
        plotly::plot_ly(x = ~x, y = ~y, z = ~prob,
                        hovertemplate = paste0(xlab,": %{x:0.2f}<br>",ylab,":%{y:0.2f}<br>Prob: %{z:0.2f}<extra></extra>")) %>%
        plotly::add_markers(marker = bar) %>% 
        plotly::add_paths(data = pp2, x = ~x, y = ~y, z = ~prob,
                          line = line) %>%
        plotly::layout(showlegend = F,
                       scene = list( 
                         xaxis = list(title = list(text = xlab, font = list(size = 18))),
                         yaxis = list(title = list(text = ylab, font = list(size = 18))),
                         zaxis = list(title = list(text = "Probability", font = list(size = 18)))))
      return(p)
    }
  } #end bivariate plot function
  
  
  #set up the plots
  
  if(missing(formula)){ #univariate
    #NPAG
    if(type == "NPAG"){
      ab_alpha <- ab %>% arrange(par)
      p <- x$popPoints %>% pivot_longer(cols = !prob, names_to = "par") %>%
        dplyr::nest_by(par) %>%
        dplyr::full_join(ab_alpha, by = "par") %>%
        dplyr::mutate(panel = list(uniPlot(par, data, min, max, type = "NPAG", bar = bar))) %>%
        plotly::subplot(margin = 0.02, nrows = nrow(.), titleX = T, titleY = T)
      
    } else {
      #IT2B
      if(!missing(standardize)){ #standardize plot
        if(tolower(standardize[1])=="all"){
          to_standardize <- 1:nrow(ab)
        } else {
          to_standardize <- which(names(x$popMean) %in% standardize)
        }
        if(length(to_standardize)>0){
          ab[to_standardize,1] <- min(ab[to_standardize,1])
          ab[to_standardize,2] <- max(ab[to_standardize,2])
        } else {stop("Requested standardization parameters are not in model.")}
      }
      ab_alpha <- ab %>% arrange(par)
      p <- data.frame(mean = purrr::as_vector(x$popMean), sd = purrr::as_vector(x$popSD), min = ab[,1], max = ab[,2]) %>%
        purrr::pmap(.f = function(mean, sd, min, max){tibble::tibble(value = seq(min, max, (max-min)/1000),
                                                                     prob = dnorm(value, mean, sd))}) %>% 
        purrr::set_names(names(x$popMean)) %>%
        dplyr::bind_rows(.id = "par") %>%
        dplyr::nest_by(par) %>% 
        dplyr::full_join(ab_alpha, by = "par") %>%
        dplyr::mutate(panel = list(uniPlot(par, data, min, max, type = "IT2B"))) %>%
        plotly::subplot(margin = 0.02, nrows = nrow(.), titleX = T, titleY = T)
      
    }
  } else { #bivariate
    p <- biPlot(formula, x, xlab, ylab)
  }
  print(p)
  return(p)
  
}


#' Plot Pmetrics Final Cycle Parameter Value Distributions
#' 
#' Plot objects made by [makeFinal].
#'
#' @details
#' If `formula` is omitted, this will generate a marginal plot for each parameter.  
#' For NPAG data, this will be a histogram of marginal values for each parameter and the associated probability
#' of that value.  For IT2B, this will be a series of normal distributions with mean and standard deviation
#' equal to the mean and standard deviation of each parameter marginal distribution, and the standard deviation and 95% distribution
#' indicated at the bottom of each plot.  IF `formula` IS specified,
#' this will generate one of two plots.  Specifying "prob" as the y-value vs. a parameter
#' will generate a marginal plot of Bayesian posterior parameter distributions for included/excluded
#' subjects.  For example, `prob~CL` will plot Bayesian posterior distributions for CL for each 
#' included/excluded subject.
#' 
#' On the other hand, if `formula` is two parameters, e.g. CL~V, this will generate a bivariate plot.  
#' For NPAG data, it will be support point with size proportional to the probability
#' of each point.  For IT2B, it will be an elliptical distribution of a bivariate normal distribution centered at the mean
#' of each plotted variable and surrounding quantiles of the bivariate distribution plotted in decreasing shades of grey.
#' 
#' @method plot PMfinal
#' @param x The name of an *PMfinal* data object generated by [makeFinal]
#' @param formula An optional formula of the form `y ~ x`, where `y` and `x`
#' are two model parameters to plot in a 3-dimensional bivariate plot.  See details.
#' @param include A vector of subject IDs to include in a Bayesian posterior marginal parameter 
#' distribution plot, e.g. c(1:3,5,15).  Only relevant
#' for Bayesian posterior plots generated by `formula` values of the form `prob ~ par`, where
#' *par* is a parameter in the model.
#' @param exclude A vector of subject IDs to exclude in a Bayesian posterior marginal parameter 
#' distribution plot, e.g. c(4,6:14,16:20). Only relevant
#' for Bayesian posterior plots generated by `formula` values of the form `prob ~ par`, where
#' *par* is a parameter in the model.
#' @param ref Boolean operator to include (if `TRUE` which is the default) the population marginals
#' in posterior marginal plot as reference.
#' @param cex.lab Size of the plot labels for any univariate or bivariate marginal plot.
#' @param col This parameter will be applied to the histogram lines of a univariate marginal
#' plot, or the central point of a bivariate plot and is "red" by default for the former, 
#' and "white" for the latter.
#' @param col.ref Color of reference population marginals included in posterior marginal plots.
#' @param alpha.ref Alpha value for transparency of reference marginals. Default is 0.5, with 0=invisible and 1=opaque.
#' @param pch The plotting character for points in bivariate plots.  Default is a cross (pch=3).
#' @param cex The size of the points in bivariate plots
#' @param lwd Width of the histogram lines in the univariate marginal parameter distributions
#' or the thickness of the central points and lines around points in bivariate NPAG plots or around quantiles in the bivariate
#' IT2B plots.
#' @param lwd.ref Width of histogram lines for population marginals included in posterior marginal plots.
#' @param density Boolean operator to plot a kernel density function overlying the histogram
#' of a univarite marginal parameter distribution from NPAG; the default is `False`.
#' See [density].  Ignored for IT2B output.
#' @param scale How large to scale the points in a bivariate NPAG plot, relative to their probability.
#' Ignored for IT2B output.
#' @param bg Background fill for points in bivariate NPAG plot.  Ignored for IT2B output.
#' @param standard Standardize the normal parameter distribution plots from IT2B to the same
#' scale x-axis.  Ignored for NPAG output.
#' @param probs Vector of quantiles to plot on bivariate IT2B plot.  Ignored for NPAG plot.
#' @param legend Boolean operator for default if `True` or list of parameters to be supplied to legend function to plot 
#' quantile legend on bivariate IT2B plot.  Ignored for NPAG plot.
#' @param grid Boolean operator to plot a grid on either a bivariate NPAG or IT2B plot.
#' @param layout Specify the layout for the plot as `c(row,col)`.  Default is as many as needed for all parameters.
#' @param xlab Define x-axis label for bivariate NPAG or IT2B plot.  Default is the name of the plotted x-variable.
#' @param ylab Define y-axis label for bivariate NPAG or IT2B plot.  Default is the name of the plotted y-variable.
#' @param xlim Limits for the x-axis in a bivariate NPAG or IT2B plot.  Default is the range of the x-variable.
#' @param ylim Limits for the y-axis in a bivariate NPAG or IT2B plot.  Default is the range of the y-variable.
#' @param out Direct output to a PDF, EPS or image file.  Format is a named list whose first argument, 
#' `type` is one of the following character vectors: "pdf", "eps" (maps to `postscript`),
#' "`png], "`tiff], "`jpeg], or "`bmp].  Other named items in the list
#' are the arguments to each graphic device. PDF and EPS are vector images acceptable to most journals
#' in a very small file size, with scalable (i.e. infinite) resolution.  The others are raster images which may be very
#' large files at publication quality dots per inch (DPI), e.g. 800 or 1200. Default value is `NA` which means the 
#' output will go to the current graphic device (usually the monitor). For example, to output an eps file,
#' out=list("eps") will generate a 7x7 inch (default) graphic.
#' @param add If `TRUE`, add plot to existing plot. Default is `FALSE`.
#' @param \dots Other parameters as found in [plot.default].
#' @return Plots the object.
#' @author Michael Neely
#' @seealso [makeFinal], [plot], [par], [axis]
#' @export
#' @examples
#' #NPAG
#' data(final.1)
#' plot(final.1)
#' #IT2B
#' data(final.2)
#' plot(final.2)
plot.PMfinal <- function(x,formula,include,exclude,ref=T,cex.lab=1.2,col,col.ref,alpha.ref=0.5,pch,cex,lwd,lwd.ref,density=F,scale=20,bg,standard=F,
                         probs=c(0.05,0.25,0.5,0.75,0.95),legend=T,grid=T,layout,xlab,ylab,xlim,ylim,out=NA,add=F,...){
  #choose output
  if(inherits(out,"list") & !add){
    if(out$type=="eps") {setEPS();out$type <- "postscript"}
    if(length(out)>1) {do.call(out$type,args=out[-1])} else {do.call(out$type,list())}
  }
  .par <- par("mfrow") #save current layout
  if(missing(formula)){ #univariate plot
    data <- x
    if(missing(col)) col <- "red"
    if(missing(lwd)) lwd <- 4
    if(!add){
      if(missing(layout)){
        par(mfrow=c(ceiling(length(data$popMean)/3),ifelse(length(data$popMean)>2,3,length(data$popMean))))
      } else {par(mfrow=layout)}
      par(mar=c(5,5,4,2)+0.1)
    }
    
    
    if(inherits(data,"NPAG")){
      if(missing(ylim) & !add) ylim <- c(0,max(data$popPoints[,"prob"]))
      for (i in 1:(ncol(data$popPoints)-1)){
        if(!add){
          plot(data$popPoints[,"prob"]~data$popPoints[,i],type="h",lwd=lwd,col=col,xlab=names(data$popPoints)[i],xlim=data$ab[i,],
               ylim=ylim,ylab="Probability",cex.lab=cex.lab,...)
          abline(v=data$ab[i,],lty=2,lwd=1,col="black")
        } else {
          points(y=data$popPoints[,"prob"],x=data$popPoints[,i],type="h",lwd=lwd,col=col,...)
        }
        
        if(density & nrow(data$popPoints)>1){
          den <- density(data$popPoints[,i])
          den$y <- den$y/(max(den$y)/max(data$popPoints[,"prob"]))
          lines(den)
        }
      }
    }
    if(inherits(data,"IT2B")){
      for (i in 1:(length(data$popMean))){
        x <- seq(data$ab[i,1],data$ab[i,2],(data$ab[i,2]-data$ab[i,1])/1000)
        y <- dnorm(x,mean=data$popMean[i],sd=data$popSD[i])
        if (standard){xlim <- base::range(data$ab)} else {xlim <- data$ab[i,]}
        if(!add){
          plot(x=x,y=y,type="l",lwd=lwd,col=col,xlab=names(data$popMean)[i],xlim=xlim,
               ,ylab="Probability",cex.lab=cex.lab,...)
        } else {
          points(x=x,y=y,type="l",lwd=lwd,col=col,xlab=names(data$popMean)[i],xlim=xlim,
                 ,ylab="Probability",cex.lab=cex.lab,...)
        }
        
        abline(v=data$ab[i,],lty=2,lwd=1,col="black")
        abline(v=data$popMean[i],lwd=1,col="black")
        for(j in c(qnorm(0.975),1)){
          lines(x=c(data$popMean[i]-j*data$popSD[i],data$popMean[i]+j*data$popSD[i]),y=c(0,0),lwd=4,col=c("gray80","black")[1+as.numeric(j==1)])
        }
      }
    }
    par(mfrow=c(1,1))
    par(mar=c(5,4,4,2)+0.1)
    
  } else {  #bivariate plot
    data <- x
    keep <- NULL
    yCol <- as.character(attr(terms(formula),"variables")[2])
    xCol <- as.character(attr(terms(formula),"variables")[3])
    if(missing(xlab)) xlab <- xCol
    if(missing(ylab)) ylab <- yCol
    if(missing(pch)) pch <- 3
    if(missing(cex)) cex <- 1
    
    if(inherits(data,"IT2B")){
      if(yCol=="prob"){ #posterior plot
        #filter includes and excludes
        if(!missing(include)) data$postMean <- subset(data$postMean,as.character(data$postMean$id) %in% as.character(include))
        if(!missing(exclude)) data$postMean <- subset(data$postMean,!sub("[[:space:]+","",as.character(data$postMean$id)) %in% as.character(exclude))
        #number of subjects to plot
        subjID <- unique(data$postMean$id)
        nsub <- length(subjID)
        #default values and layout
        if(missing(col)) col <- "red"
        if(missing(lwd)) lwd <- 4
        if(missing(layout)){
          if(nsub>4){
            par(mfrow=c(2,2))
            devAskNewPage(T)
          } else {
            par(mfrow=c(ceiling(nsub/2),ifelse(nsub>2,2,nsub)))
          }
        } else {
          par(mfrow=layout)
          if(nsub>sum(layout)) {devAskNewPage(T)}
        }
        par(mar=c(5,5,4,2)+0.1)
        
        if(ref){ #adding in population marginal as reference
          
          i <- which(names(data$postMean)==all.vars(formula)[2])-1 #choose the right variable
          x.pop <- seq(data$ab[i,1],data$ab[i,2],(data$ab[i,2]-data$ab[i,1])/1000)
          y.pop <- dnorm(x.pop,mean=data$popMean[i],sd=data$popSD[i])
          
          if(missing(col.ref)) col.ref <- "gray50"
          #make semi transparent
          col.ref.rgb <- col2rgb(col.ref,T)/255
          col.ref.trans <- rgb(col.ref.rgb[1,],col.ref.rgb[2,],col.ref.rgb[3,],alpha.ref)
          if(missing(lwd.ref)) lwd.ref <- 3
          if(missing(xlim)) xlim <- base::range(x.pop)
        }
        
        this.x <- seq(data$ab[i,1],data$ab[i,2],(data$ab[i,2]-data$ab[i,1])/1000)
        #cycle through subjects to get y coordinates
        this.y <- matrix(NA,ncol=length(this.x),nrow=nsub)
        for (j in 1:nsub){
          #get the x & y parameters
          this.y[j,] <- dnorm(this.x,mean=data$postMean[j,i+1],sd=data$postSD[j,i+1])
        }
        
        if(standard){
          xlim <- base::range(this.x)
          ylim <- base::range(this.y)
        }
        if(missing(xlim)){xlim <- NULL}
        if(missing(ylim)){ylim <- NULL}
        
        for(j in 1:nsub){
          plot(this.y[j,]~this.x,type="l",lwd=lwd,col=col,ylab="Probability",
               xlab=attr(terms(formula),"term.labels"),cex.lab=cex.lab,
               main=paste("Subject",data$postMean$id[j]),xlim=xlim,ylim=ylim,...)
          if(ref){lines(y.pop~x.pop,type="l",col=col.ref.trans,lwd=lwd.ref)}
        }
      } else {
        #internal ellipse function from package mixtools
        ellipse <- function(mu,sigma,alpha = 0.05,npoints=250,newplot=FALSE,draw=TRUE, ...)
        {
          es <- eigen(sigma)
          e1 <- es$vec %*% diag(sqrt(es$val))
          r1 <- sqrt(qchisq(1 - alpha, 2))
          theta <- seq(0, 2 * pi, len = npoints)
          v1 <- cbind(r1 * cos(theta), r1 * sin(theta))
          pts = t(mu - (e1 %*% t(v1)))
          if (newplot && draw) {
            plot(pts, ...)
          }
          else if (!newplot && draw) {
            lines(pts, ...)
          }
          invisible(pts)
        }
        ell <- array(NA,dim=c(length(probs),250,2))
        for(i in 1:length(probs)){
          ell[i,,] <- ellipse(mu=c(data$popMean[xCol],data$popMean[yCol]),sigma=data$popCov[c(xCol,yCol),c(xCol,yCol)],type="l",alpha=probs[i],draw=F)
        }
        graycols <- rev(gray.colors(n=length(probs),start=0,end=0.9))
        if(missing(xlim)) xlim <- base::range(ell[,,1])
        if(missing(ylim)) ylim <- base::range(ell[,,2])
        if(missing(col)) col="white"
        if(missing(lwd)) lwd <- 1
        if(!missing(legend)){
          if(class(legend)=="list"){
            legend$plot<- T
            if(is.null(legend$x)) legend$x <- "topright"
            if(is.null(legend$fill)) legend$fill <- graycols
            if(is.null(legend$legend)) legend$legend <- 1-probs
            if(is.null(legend$title)) legend$title <- "Quantile"
          } else {
            if(legend) legend <- {list(plot=T,x="topright",legend=1-probs,fill=graycols,title="Quantile")} else {legend <- list(plot=F)}
          }
        } else {legend <- list(plot=F)}
        
        plot(NA,cex.lab=cex.lab,xlim=xlim,ylim=ylim,type="n",xlab=xlab,ylab=ylab,...)
        
        
        for(i in 1:length(probs)){
          polygon(ell[i,,],col=graycols[i],lwd=lwd)
        }
        if(grid) abline(v=c(axTicks(1),diff(axTicks(1))/2 + axTicks(1)[-length(axTicks(1))]),h=c(axTicks(2),diff(axTicks(2))/2 + axTicks(2)[-length(axTicks(2))]),col="lightgrey")
        points(x=data$popMean[xCol],y=data$popMean[yCol],pch=pch,col=col,cex=cex,lwd=lwd,...)
        if(legend$plot) do.call("legend",legend)
      }
    }
    if(inherits(data,"NPAG")){
      if(yCol=="prob"){ #posterior plot
        #filter includes and excludes
        if(length(data$postPoints)==0){ #old final object
          stop("Use makeFinal and PMsave to update your PMfinal object.\n\nExample:\n\nfinal.1 <- makeFinal(NPdata.1)\nPMsave(1)\n")
        }
        if(!missing(include)) data$postPoints <- subset(data$postPoints,as.character(data$postPoints$id) %in% as.character(include))
        if(!missing(exclude)) data$postPoints <- subset(data$postPoints,!sub("[[:space:]+","",as.character(data$postPoints$id)) %in% as.character(exclude))
        #number of subjects to plot
        subjID <- unique(data$postPoints$id)
        nsub <- length(subjID)
        #default values and layout
        if(missing(col)) col <- "red"
        if(missing(lwd)) lwd <- 4
        if(missing(layout)){
          if(nsub>4){
            par(mfrow=c(2,2))
            devAskNewPage(T)
          } else {
            par(mfrow=c(ceiling(nsub/2),ifelse(nsub>2,2,nsub)))
          }
        } else {
          par(mfrow=layout)
          if(nsub>sum(layout)) {devAskNewPage(T)}
        }
        par(mar=c(5,5,4,2)+0.1)
        if(missing(ylim)) ylim <- c(0,max(data$postPoints$prob))
        if(ref){ #adding in population marginal as reference
          x.pop <- model.frame(formula=formula,data=data$popPoints)[,2] #parameter
          y.pop <- model.frame(formula=formula,data=data$popPoints)[,1] #prob
          if(missing(col.ref)) col.ref <- "gray50"
          #make semi transparent
          col.ref.rgb <- col2rgb(col.ref,T)/255
          col.ref.trans <- rgb(col.ref.rgb[1,],col.ref.rgb[2,],col.ref.rgb[3,],alpha.ref)
          if(missing(lwd.ref)) lwd.ref <- 3
          if(missing(xlim)) xlim <- base::range(x.pop)
        }
        if(missing(xlim)) xlim <- base::range(model.frame(formula=formula,data=data$postPoints)[,2])
        #cycle through subjects
        for (i in 1:nsub){
          temp <- subset(data$postPoints, data$postPoints$id==subjID[i])
          #get the x & y parameters
          x <- model.frame(formula=formula,data=temp)[,2] #parameter
          y <- model.frame(formula=formula,data=temp)[,1] #prob
          plot(y~x,type="h",lwd=lwd,col=col,xlim=xlim,ylim=ylim,ylab="Probability",
               xlab=attr(terms(formula),"term.labels"),cex.lab=cex.lab,main=paste("Subject",subjID[i]),...)
          if(ref){lines(y.pop~x.pop,type="h",col=col.ref.trans,lwd=lwd.ref)}
        }
      }else{ #population plot
        if(missing(bg)) bg <- "gray50"
        if(missing(col)) col <- "white"
        if(missing(lwd)) lwd <- 1
        x <- model.frame(formula=formula,data=data$popPoints)[2][,1]
        y <- model.frame(formula=formula,data=data$popPoints)[1][,1]
        z <- data$popPoints[,"prob"]
        if(missing(xlim)){
          minx <- which(x==min(x))[1]
          maxx <- which(x==max(x))[1]
          xlim <- c(x[minx]-scale/10*z[minx],x[maxx]+scale/10*z[maxx])
        }
        if(missing(ylim)){
          miny <- which(y==min(y))[1]
          maxy <- which(y==max(y))[1]
          ylim <- c(y[miny]-scale/10*z[miny],y[maxy]+scale/10*z[maxy])
        }
        plot(y=y,x=x,pch=21,bg=bg,cex=scale*z,xlab=xlab,ylab=ylab,lwd=lwd,cex.lab=cex.lab,xlim=xlim,ylim=ylim,...)
        points(y=y,x=x,pch=pch,col=col,cex=cex,lwd=lwd,...)
        if(grid) abline(v=c(axTicks(1),diff(axTicks(1))/2 + axTicks(1)[-length(axTicks(1))]),h=c(axTicks(2),diff(axTicks(2))/2 + axTicks(2)[-length(axTicks(2))]),col="lightgrey")
      }
    }
    
    
  }
  #close device if necessary
  if(inherits(out,"list")) dev.off()
  #restore layout
  par(.par)
  devAskNewPage(F)
  return(invisible(1))
}








