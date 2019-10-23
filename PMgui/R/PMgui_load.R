
.onLoad <- function(...){
  packagesAvailable <- function(packages){
    sapply(sapply(packages, .find.package, quiet=TRUE),
           function(x) length(x) != 0)
  }
  if (!interactive()) return()
  save.options <- options(warn=-1)
  on.exit(options(save.options))
  required.packages <- rev(c("chron","R2HTML","tcltk"))
  check <- options("PMgui")[[1]]$check.packages
  if (length(check) > 0 && !check) return()
  packages.to.check <- required.packages
  available.packages <- packagesAvailable(packages.to.check)
  missing.packages <- packages.to.check[!available.packages]
  if (any(!available.packages)) {
    response <- tkmessageBox(message=paste(gettext("The following packages used by PMgui are missing:\n", domain="R-PMgui"),
                                           paste(missing.packages, collapse=", "),
                                           gettext("\nWithout these packages, some features will not be available.", domain="R-PMgui"),
                                           gettext("\nInstall these packages?", domain="R-PMgui")),
                             icon="error", type="yesno")
    if (tclvalue(response) == "yes") {
      top <- tktoplevel(borderwidth=10)
      tkwm.title(top, gettext("Install Missing Packages", domain="R-PMgui"))
      locationFrame <- tkframe(top)
      locationVariable <- tclVar("CRAN")
      CRANbutton <- ttkradiobutton(locationFrame, variable=locationVariable, value="CRAN")
      localButton <- ttkradiobutton(locationFrame, variable=locationVariable, value="local")
      directoryVariable <- tclVar("")
      directoryFrame <- tkframe(locationFrame)
      onBrowse <- function(){
        tclvalue(directoryVariable) <- tclvalue(tkchooseDirectory(parent=top))
      }
      browseButton <- buttonPMgui(directoryFrame, text=gettext("Browse...", domain="R-PMgui"), width="12", command=onBrowse, borderwidth=3)
      locationField <- ttkentry(directoryFrame, width="20", textvariable=directoryVariable)
      locationScroll <- ttkscrollbar(directoryFrame, orient="horizontal",
                                     command=function(...) tkxview(locationField, ...))
      tkconfigure(locationField, xscrollcommand=function(...) tkset(locationScroll, ...))
      tkgrid(labelPMgui(top, text=gettext("Install Packages From:", domain="R-PMgui"), fg="blue"), sticky="nw")
      tkgrid(labelPMgui(directoryFrame, text=gettext("Specify package  \ndirectory:", domain="R-PMgui"), justify="left"),
             locationField, sticky="w")
      tkgrid(browseButton, locationScroll, sticky="w")
      tkgrid(locationScroll, sticky="ew")
      tkgrid(labelPMgui(locationFrame, text="CRAN"), CRANbutton, sticky="w")
      tkgrid(labelPMgui(locationFrame, text=gettext("Local package directory\n(must include PACKAGES index file)", domain="R-PMgui"),
                        justify="left"), localButton, directoryFrame, sticky="nw")
      tkgrid(locationFrame, sticky="w")
      tkgrid(labelPMgui(top, text=""))
      onOK <- function(){
        errorMessage <- function() tkmessageBox(message=paste(
          gettext("The following packages were not found at the specified location:\n", domain="R-PMgui"),
          paste(missing.packages[!present], collapse=", ")),  icon="warning", type="ok")
        tkgrab.release(top)
        tkdestroy(top)
        location <- tclvalue(locationVariable)
        if (location == "CRAN") {
          packages <- utils:::CRAN.packages()[,1]
          present <- missing.packages %in% packages
          if (!all(present)) errorMessage()
          if (!any(present)) return()
          utils:::install.packages(missing.packages[present], lib=.libPaths()[1])		
        } else {
          directory <- paste("file:", tclvalue(directoryVariable), sep="")
          packages <- utils:::CRAN.packages(contriburl=directory)[,1]
          present <- missing.packages %in% packages
          if (!all(present)) errorMessage()
          if (!any(present)) return()
          utils:::install.packages(missing.packages[present], contriburl=directory,
                                   lib=.libPaths()[1])
        }
      }
      onCancel <- function(){
        tkgrab.release(top)
        tkdestroy(top)
        return()
      }
      onHelp <- function() help("install.packages")
      buttonsFrame <- tkframe(top)
      OKbutton <- buttonPMgui(buttonsFrame, text="OK", foreground="darkgreen", width="12", command=onOK, default="active",
                              borderwidth=3)
      cancelButton <- buttonPMgui(buttonsFrame, text=gettext("Cancel", domain="R-PMgui"), foreground="red", width="12", command=onCancel,
                                  borderwidth=3)
      helpButton <- buttonPMgui(buttonsFrame, text=gettext("Help", domain="R-PMgui"), width="12", command=onHelp, borderwidth=3)
      tkgrid(OKbutton, labelPMgui(buttonsFrame, text="  "), cancelButton,
             labelPMgui(buttonsFrame, text="            "),
             helpButton, sticky="w")
      tkgrid(buttonsFrame, sticky="w")
      for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
      tkgrid.columnconfigure(top, 0, weight=0)
      .Tcl("update idletasks")
      tkwm.resizable(top, 0, 0)
      tkbind(top, "<Return>", onOK)
      tkwm.deiconify(top)
      tkgrab.set(top)
      tkfocus(top)
      tkwait.window(top)
    }
  }
}
