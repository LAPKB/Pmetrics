# functions for Plots menu

PMgui_PMmatrix <- function(){
  .activeDataSet <- ActiveDataSet()
  if(!is.null(.activeDataSet) && (inherits(get(.activeDataSet, envir=.GlobalEnv),"PMmatrix"))){
    
    defaults <- list(initial.subset = gettextPMgui ("<all subjects>"), initial.post = NULL, initial.mult = 1, initial.outeq = NULL, 
                     initial.group = NULL, initial.block = 1, initial.filename = NULL, initial.layout = c(3,3), 
                     initial.log = FALSE, initial.pch = NA, initial.doses = FALSE, initial.join = TRUE,
                     initial.grid = NULL, initial.ident = FALSE, initial.overlay = TRUE, initial.main = NULL, initial.xlim = NULL,
                     initial.ylim = NULL, initial.xlab = "Time (h)", initial.ylab = "Observation", initial.col = NULL, initial.cexValue = 1, 
                     initial.cex.axisValue = 1, initial.cex.labValue = 1, initial.legend = NULL) 
    
          dialog.values <- getDialog("plotPMmatrix", defaults)
    #       initial.group <- dialog.values$initial.group
    # #      .linesByGroup <- if (dialog.values$initial.lines.by.group == 1) TRUE else FALSE
    #       .groups <- if (is.null(initial.group)) FALSE else initial.group
    initializeDialog(title = gettextPMgui("plot.PMmatrix"))
    #       .numeric <- Numeric()
    
    #set Boolean arguments
    optionsParFrame <- tkframe(top)
    checkBoxes(window = optionsParFrame, frame = "optionsFrame",boxes = c("log", "doses", "join", "ident", "overlay"), 
               initialValues = c(dialog.values$initial.log, dialog.values$initial.doses, dialog.values$initial.join, dialog.values$initial.ident, dialog.values$initial.overlay),
               labels = gettextPMgui(c("Semi-log", "Show doses", "Join observations", "Identify points", "Overlay plots")), 
               title = "Options")
    
    #       #set include box
    #       subsetBox(subset.expression = dialog.values$initial.subset)
    #       
    #       #set x and y labels
    #       labelsFrame <- tkframe(top)
    #       xlabVar <- tclVar(dialog.values$initial.xlab)
    #       ylabVar <- tclVar(dialog.values$initial.ylab)
    #       xlabFrame <- tkframe(labelsFrame)
    #       xlabEntry <- ttkentry(xlabFrame, width = "25", textvariable = xlabVar)
    #       xlabScroll <- ttkscrollbar(xlabFrame, orient = "horizontal", 
    #                                  command = function(...) tkxview(xlabEntry, ...))
    #       tkconfigure(xlabEntry, xscrollcommand = function(...) tkset(xlabScroll, 
    #                                                                   ...))
    #       tkgrid(labelPMgui(xlabFrame, text = gettextPMgui("x-axis label"), 
    #                         fg = "blue"), sticky = "w")
    #       tkgrid(xlabEntry, sticky = "w")
    #       tkgrid(xlabScroll, sticky = "ew")
    #       ylabFrame <- tkframe(labelsFrame)
    #       ylabEntry <- ttkentry(ylabFrame, width = "25", textvariable = ylabVar)
    #       ylabScroll <- ttkscrollbar(ylabFrame, orient = "horizontal", 
    #                                  command = function(...) tkxview(ylabEntry, ...))
    #       tkconfigure(ylabEntry, xscrollcommand = function(...) tkset(ylabScroll, 
    #                                                                   ...))
    #       tkgrid(labelPMgui(ylabFrame, text = gettextPMgui("y-axis label"), 
    #                         fg = "blue"), sticky = "w")
    #       tkgrid(ylabEntry, sticky = "w")
    #       tkgrid(ylabScroll, sticky = "ew")
    #       tkgrid(xlabFrame, labelPMgui(labelsFrame, text = "     "), 
    #              ylabFrame, sticky = "w")
           parFrame <- tkframe(optionsParFrame)
    #       
    #       #set pch
    #       pchVar <- tclVar(dialog.values$initial.pch)
    #       pchEntry <- ttkentry(parFrame, width = 25, textvariable = pchVar)
    #       
    #       #set cex
    #       cexValue <- tclVar(dialog.values$initial.cexValue)
    #       cex.axisValue <- tclVar(dialog.values$initial.cex.axisValue)
    #       cex.labValue <- tclVar(dialog.values$initial.cex.labValue)
    #       cexSlider <- tkscale(parFrame, from = 0.5, to = 2.5, showvalue = TRUE, 
    #                            variable = cexValue, resolution = 0.1, orient = "horizontal")
    #       cex.axisSlider <- tkscale(parFrame, from = 0.5, to = 2.5, 
    #                                 showvalue = TRUE, variable = cex.axisValue, resolution = 0.1, 
    #                                 orient = "horizontal")
    #       cex.labSlider <- tkscale(parFrame, from = 0.5, to = 2.5, 
    #                                showvalue = TRUE, variable = cex.labValue, resolution = 0.1, 
    #                                orient = "horizontal")
    
    #hit the ok button
    onOK <- function() {
      
      log <- paste("log = ",("1" == tclvalue(logVariable)),",",sep="") 
      ident <- paste("ident = ",("1" == tclvalue(identVariable)),",",sep="")
      doses <- paste("doses = ",("1" == tclvalue(dosesVariable)),",",sep="")
      join <- paste("join = ",("1" == tclvalue(joinVariable)),",",sep="")
      overlay <- paste("overlay = ",("1" == tclvalue(overlayVariable)),sep="")
      
      #         initial.subset <- subset <- tclvalue(subsetVariable)
      #         subset <- if (trim.blanks(subset) == gettextPMgui("<all subjects>")) 
      #           ""
      #         else paste(", subset=", subset, sep = "")
      #         
      #         cex.axis <- as.numeric(tclvalue(cex.axisValue))
      #         cex <- as.numeric(tclvalue(cexValue))
      #         cex.lab <- as.numeric(tclvalue(cex.labValue))
      #         
      #         xlab <- trim.blanks(tclvalue(xlabVar))
      #         xlab <- if (xlab == gettextPMgui("<auto>")) 
      #           ""
      #         else paste(", xlab=\"", xlab, "\"", sep = "")
      #         ylab <- trim.blanks(tclvalue(ylabVar))
      #         ylab <- if (ylab == gettextPMgui("<auto>")) 
      #           ""
      #         else paste(", ylab=\"", ylab, "\"", sep = "")
      #         
      #         pch <- gsub(" ", ",", tclvalue(pchVar))
      #         putDialog ("scatterPlot", list (initial.x = x, initial.y = y, initial.jitterx = tclvalue(jitterXVariable),
      #                                         initial.jittery = tclvalue(jitterYVariable), initial.logstringx = tclvalue(logXVariable),
      #                                         initial.logstringy = tclvalue(logYVariable), initial.log = log, initial.box = box, 
      #                                         initial.line = line, initial.smooth = smooth, initial.spread = spread,
      #                                         initial.span = span, initial.subset = initial.subset, initial.xlab = tclvalue(xlabVar),
      #                                         initial.ylab = tclvalue(ylabVar), initial.cexValue = tclvalue(cexValue), 
      #                                         initial.cex.axisValue = tclvalue(cex.axisValue), initial.cex.labValue = tclvalue(cex.labValue), 
      #                                         initial.pch = pch, initial.group=if (.groups == FALSE) NULL else .groups,
      #                                         initial.lines.by.group=if (.linesByGroup) 1 else 0))
      #         
      
      
      closeDialog()
      
      #         pch <- if (trim.blanks(pch) == gettextPMgui("<auto>")) 
      #           ""
      #         else paste(", pch=c(", pch, ")", sep = "")
      #        
      #      
      #         .activeDataSet <- ActiveDataSet()
      #         
      #         
      #         cex <- if (cex == 1) 
      #           ""
      #         else paste(", cex=", cex, sep = "")
      #         cex.axis <- if (cex.axis == 1) 
      #           ""
      #         else paste(", cex.axis=", cex.axis, sep = "")
      #         cex.lab <- if (cex.lab == 1) 
      #           ""
      #         else paste(", cex.lab=", cex.lab, sep = "")
      
      command <- paste("plot(", .activeDataSet, ",", log, doses, join, ident, overlay, ")", sep = "")
      
      doItAndPrint(command)
      
      activateMenus()
      tkfocus(PmetricsWindow())
    }
    
    OKCancelHelp(helpSubject = "scatterplot", reset = "scatterPlot")
    
    
           tkgrid(labelPMgui(parFrame, text = gettextPMgui("Plotting Parameters"), 
                             fg = "blue"), sticky = "w")
    #       tkgrid(labelPMgui(parFrame, text = gettextPMgui("Plotting characters")), 
    #              pchEntry, stick = "w")
    #       tkgrid(labelPMgui(parFrame, text = gettextPMgui("Point size")), 
    #              cexSlider, sticky = "w")
    #       tkgrid(labelPMgui(parFrame, text = gettextPMgui("Axis text size")), 
    #              cex.axisSlider, sticky = "w")
    #       tkgrid(labelPMgui(parFrame, text = gettextPMgui("Axis-labels text size")), 
    #              cex.labSlider, sticky = "w")
           tkgrid(optionsFrame, parFrame, sticky = "nw")
           tkgrid(optionsParFrame, sticky = "w")
    #       tkgrid(labelsFrame, sticky = "w")
    #       tkgrid(subsetFrame, sticky = "w")
    #       tkgrid(groupsFrame, sticky = "w")
    #       tkgrid(labelPMgui(top, text = " "))
           tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
           dialogSuffix(rows = 8, columns = 2)
    #       
    #       command <- paste("plot(",.activeDataSet,")",sep="")
    #       doItAndPrint(command)
  }
     return()
}