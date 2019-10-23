#functions for Edit Menu

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
  

  