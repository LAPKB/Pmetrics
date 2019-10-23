\name{Rcmdr.Utilities}
\alias{Rcmdr.Utilities}
\alias{activateMenus}
\alias{activeDataSet}
\alias{ActiveDataSet}
\alias{activeDataSetP}
\alias{activeModel}
\alias{ActiveModel}
\alias{activeModelP}
\alias{aicP}
\alias{checkActiveDataSet}
\alias{checkActiveModel}
\alias{checkBoxes}
\alias{checkClass}
\alias{checkFactors}
\alias{checkMethod}
\alias{checkNumeric}
\alias{checkReplace}
\alias{checkTwoLevelFactors}
\alias{checkVariables}
\alias{closeCommander}
\alias{closeDialog}
\alias{commanderPosition}
\alias{CommanderWindow}
\alias{dataSetsP}
\alias{defmacro}
\alias{dialogSuffix}
\alias{doItAndPrint}
\alias{errorCondition}
\alias{exists.method}
\alias{Factors}
\alias{factorsP}
\alias{formulaFields}
\alias{flushDialogMemory}
\alias{getDialog}
\alias{getFrame}
\alias{getFrame.listbox}
\alias{getRcmdr}
\alias{getSelection}
\alias{getSelection.listbox}
\alias{gettextRcmdr}
\alias{glmP}
\alias{GrabFocus}
\alias{groupsBox}
\alias{groupsLabel}
\alias{hclustSolutionsP}
\alias{initializeDialog}
\alias{is.valid.name}
\alias{justDoIt}
\alias{Library}
\alias{listAllModels}
\alias{listAOVModels}
\alias{listDataSets}
\alias{listGeneralizedLinearModels}
\alias{listFactors}
\alias{listLinearModels}
\alias{listMultinomialLogitModels}
\alias{listNumeric}
\alias{listPlugins}
\alias{listProportionalOddsModels}
\alias{listTwoLevelFactors}
\alias{listVariables}
\alias{lmP}
\alias{logger}
\alias{LogWindow}
\alias{MacOSXP}
\alias{Message}
\alias{MessagesWindow}
\alias{modelFormula}
\alias{modelsP}
\alias{multinomP}
\alias{Numeric}
\alias{numericP}
\alias{OKCancelHelp}
\alias{OutputWindow}
\alias{packageAvailable}
\alias{polrP}
\alias{popCommand}
\alias{popOutput}
\alias{putDialog}
\alias{putRcmdr}
\alias{radioButtons}
\alias{RcmdrTclSet}
\alias{RcmdrTkmessageBox}
\alias{RExcelSupported}
\alias{rglLoaded}
\alias{sortVarNames}
\alias{subOKCancelHelp}
\alias{subsetBox}
\alias{tclvalue}
\alias{trim.blanks}
\alias{TwoLevelFactors}
\alias{twoLevelFactorsP}
\alias{UpdateModelNumber}
\alias{variableListBox}
\alias{Variables}
\alias{varPosn}

\alias{buttonRcmdr}
\alias{labelRcmdr}
\alias{ttkentry}
\alias{ttkframe}
\alias{ttkradiobutton}
\alias{ttkscrollbar}

\title{Rcmdr Utility Functions}
\description{
  These functions support writing additions to the Rcmdr package. Additional R code
  can be placed in files with file type \code{.R} in the \code{etc} subdirectory
  of the package. Add menus, submenus, and menu items by editing the file
  \code{Rcmdr-menus.txt} in the same directory.
}

\usage{
activateMenus()
activeDataSet(dsname, flushModel=TRUE, flushDialogMemory=TRUE)
ActiveDataSet(name)
activeDataSetP()
activeModel(model)
ActiveModel(name)
activeModelP()
aicP()
checkActiveDataSet()
checkActiveModel()
checkBoxes(window=top, frame, boxes, initialValues=NULL, labels, title=NULL)  # macro
checkClass(object, class, message=NULL)  # macro
checkFactors(n=1)
checkMethod(generic, object, message=NULL, default=FALSE, strict=FALSE, 
    reportError=TRUE)  # macro
checkNumeric(n=1)
checkReplace(name, type=gettextRcmdr("Variable"))
checkTwoLevelFactors(n=1)
checkVariables(n=1)
closeCommander(ask=TRUE, ask.save=ask)
closeDialog(window, release=TRUE)  # macro
CommanderWindow()
dataSetsP(n=1)
defmacro(..., expr)
dialogSuffix(window=top, onOK=onOK, onCancel=onCancel, rows=1, columns=1, 
	focus=top, bindReturn=TRUE, 
    preventGrabFocus=FALSE, preventDoubleClick=FALSE, preventCrisp=FALSE)  # macro
doItAndPrint(command, log=TRUE)
errorCondition(window=top, recall=NULL, message, model=FALSE)  # macro
exists.method(generic, object, default=TRUE, strict=FALSE)
Factors(names)
factorsP(n=1)
formulaFields(model, hasLhs=TRUE, glm=FALSE)
flushDialogMemory(what)
getDialog(dialog, defaults=NULL)
\method{getFrame}{listbox}(object)
\method{getSelection}{listbox}(object)
getRcmdr(x, mode="any")
gettextRcmdr(...)
glmP()
GrabFocus(value)
groupsBox(recall=NULL, label=gettextRcmdr("Plot by:"), 
    initialLabel=gettextRcmdr("Plot by groups"),
    plotLinesByGroup=FALSE, positionLegend=FALSE, 
    plotLinesByGroupsText=gettextRcmdr("Plot lines by group"), 
    initialGroup=NULL, initialLinesByGroup=1)  # macro
groupsLabel(frame=top, groupsBox=groupsBox, columnspan=1, initialText=NULL)  # macro
hclustSolutionsP()
initializeDialog(window=top, title="", offset=10, preventCrisp=FALSE)  # macro
is.valid.name(x)
justDoIt(command)
Library(package, pos=4)
listAllModels(envir=.GlobalEnv, ...)
listAOVModels(envir=.GlobalEnv, ...)
listDataSets(envir=.GlobalEnv, ...)
listFactors(dataSet=ActiveDataSet())
listGeneralizedLinearModels(envir=.GlobalEnv, ...)
listLinearModels(envir=.GlobalEnv, ...)
listMultinomialLogitModels(envir=.GlobalEnv, ...)
listNumeric(dataSet=ActiveDataSet())
listPlugins(loaded=FALSE)
listProportionalOddsModels(envir=.GlobalEnv, ...)
listTwoLevelFactors(dataSet=ActiveDataSet())
listVariables(dataSet=ActiveDataSet())
lmP()
logger(command)
LogWindow()
MacOSXP()
Message(message, type=c("note", "error", "warning"))
MessagesWindow()
modelFormula(frame=top, hasLhs=TRUE)  # macro
modelsP(n=1)
multinomP()
Numeric(names)
numericP(n=1)
OKCancelHelp(window=top, helpSubject=NULL, model=FALSE, reset=NULL)  # macro
OutputWindow()
packageAvailable(name)
polrP()
popCommand()
popOutput()
putDialog(dialog, values=NULL, resettable=TRUE)
putRcmdr(x, value)
radioButtons(window=top, name, buttons, values=NULL, 
	initialValue=..values[1], labels, 
	title="", title.color="blue", right.buttons=TRUE)  # macro
RcmdrTclSet(name, value)
RcmdrTkmessageBox(message, icon=c("info", "question", "warning",
    "error"), type=c("okcancel", "yesno", "ok"), default, title="")
RExcelSupported()
rglLoaded()
sortVarNames(x)
subOKCancelHelp(window=subdialog, helpSubject=NULL)  # macro
subsetBox(window = top, subset.expression = NULL, model = FALSE)  # macro
tclvalue(x)
trim.blanks(text)
TwoLevelFactors(names)
twoLevelFactorsP(n=1)
UpdateModelNumber(increment=1)
variableListBox(parentWindow, variableList=Variables(), bg="white",
    selectmode="single", export="FALSE", initialSelection=NULL, 
    listHeight=getRcmdr("variable.list.height"), title)
Variables(names)
varPosn(variables, type=c("all", "factor", "numeric", "nonfactor", "twoLevelFactor"))

# the following function is exported for technical reasons,
# but is not meant to be called directly

commanderPosition()
}

\arguments{
  \item{ask}{ask for confirmation.}
  \item{ask.save}{ask whether to save contents of script and output windows.}
  \item{bg}{background color.}
  \item{bindReturn}{if \code{TRUE}, the \emph{Return} key is bound to the \code{onOK} function in the dialog.}
  \item{boxes}{vector of quoted names for check boxes, used to generate each box and its associated variable.}
  \item{buttons}{vector of quoted names for buttons in a set of related radio buttons.}
  \item{class}{quoted name of class.}
  \item{columnspan}{number of dialog-box columns to be spanned by frame.}
  \item{command}{a character string that evaluates to an R command.}
  \item{dataSet, dsname}{the quoted name of a data frame in memory.}
  \item{default}{default button: if not specified, "ok" for "okcancel", "yes" for "yesno", and "ok" for "ok";
    or look for a default method; for \code{putDialog}, a list of defaults for the dialog box if there are no
    stored previous values.}
  \item{defaults}{a list of named default values for options in a dialog if no previous selections are stored.}
  \item{dialog}{the quoted name of a dialog box under which previous selections are stored.}
  \item{envir}{the environment to be searched; should generally be left at the default.}
  \item{export}{export selection?}
  \item{expr}{expression constituting the body of the macro; typically a compound expression.}
  \item{flushDialogMemory}{remove saved values of dialog options so that \code{getDialog} returns \code{NULL} for all dialogs.}
  \item{flushModel}{set (or reset) the active model to NULL? Should normally be \code{TRUE} when the active 
    data set is changed; an exception is when variables are simply added to, deleted from, or modified in 
    the data set set.}
  \item{focus}{Tk window to get the focus.}
  \item{frame}{frame or quoted name for frame depending upon the function.}
  \item{generic}{quoted name of generic function.}
  \item{glm}{\code{TRUE} if the model is a \code{glm} object, \code{FALSE} otherwise.}
  \item{groupsBox}{listbox object for selecting groups variable.}
  \item{initialText}{initial text to display in the groups label; if \code{NULL}, \code{"<No groups selected>"}
    will be displayed.}
  \item{hasLhs}{does the model formula have a left-hand side?}
  \item{helpSubject}{the quoted name of a help subject, to be called as \code{help(helpSubject)}
    when the dialog \emph{Help} button is pressed.}
  \item{icon}{Message-box icon.}
  \item{increment}{increment to model number; -1 to set back after error.}
  \item{initialGroup}{quoted name of variable to define groups, set as initial selection in Groups variable list; 
  	\code{NULL} (the default) for no initial selection.}
  \item{initialLinesByGroup}{if 1, the lines-by-groups check box is initially checked; 0 to uncheck.}
  \item{initialLabel}{label for groups button before a selection is made.}
  \item{initialSelection}{index of item initially selected, 0-base indexing.}
  \item{initialValue}{for a set of related radio buttons.}
  \item{initialValues}{for a set of related check boxes.}
  \item{label}{label prefix for groups button after a selection is made.}
  \item{labels}{a vector of character strings to label a set of radio buttons or check boxes.}
  \item{listHeight}{Maximum number of elements displayed simultaneously in list box.}
  \item{loaded}{if \code{TRUE}, plug-in packages that are loaded are included in the vector of names
    returned.}
  \item{log}{echo command to the log window, as well as executing it and printing its output.}
  \item{message}{error (or other) message.}
  \item{mode}{mode of object to retrieve.}
  \item{model}{the name of a model, as a character string, or a model object, 
  	or \code{TRUE} or \code{FALSE}, depending upon the function.}
  \item{name}{quoted name.}
  \item{names}{optional names to be stored.}
  \item{n}{number of items to check for.}
  \item{object}{an object (depends on context).}
  \item{offset}{in pixels, from top-left of Commander window.}
  \item{onOK}{function to execute when the \emph{OK} button is pressed.}
  \item{onCancel}{function to execute when the \emph{Cancel} button or \emph{Esc} key is pressed.}
  \item{package}{quoted name of package to load.}
  \item{plotLinesByGroup}{include a check box for plotting lines by group?}
  \item{plotLinesByGroupsText}{the label for the plot-lines-by-group check box.}
  \item{pos}{position on search path at which to load package; default is \code{4}.}
  \item{positionLegend}{include a check box for a legend?}
  \item{preventGrabFocus}{prevent the dialog box from grabbing the focus.}
  \item{preventDoubleClick}{prevent double-clicking from pressing the OK button, even when the
    double.click option is set; necessary for statistical modelling dialogs, which use double-clicking
    to build the model formula.}
  \item{preventCrisp}{prevent call to \code{tclServiceMode}, which (rarely) causes
    problems with some dialogs.}
  \item{recall}{function to call after error --- usually the function that initiates the dialog.}
  \item{release}{release the focus if the grab.focus option has been set.}
  \item{reset}{quoted name of dialog function, to be invoked with all defaults by Reset button.}
  \item{resettable}{should dialog state be reset when the data set changes? The default is 
  code{TRUE}.}
  \item{reportError}{if \code{TRUE}, report an error message.}
  \item{right.buttons}{radio button placed to right of button-labels; defaults to \code{TRUE}.}
  \item{rows, columns}{numbers of rows and columns of widgets in the dialog box.}
  \item{selectmode}{\code{"single"} or \code{"multiple"}.}
  \item{strict}{if \code{TRUE}, only use first element of class vector.}
  \item{subset.expression}{a quoted expression to subset the data set.}
  \item{text}{a text string.}
  \item{title}{Window or dialog-box-element title.}
  \item{title.color}{color for title above radio buttons; defaults to \code{"blue"}.} 
  \item{type}{quoted type of object to check; used to generate check-replace dialog box;
    or type of message to print in Message window. For \code{varPosn}, type of variable list.}
  \item{value}{an object to be stored.}
  \item{values}{vector of quoted values associated with radio buttons or check boxes; for \code{putDialog},
  	a list of current selections to be stored in support of dialog memory.}
  \item{variableList}{a vector of variable names.}
  \item{variables}{a vector of one or more variable names.}
  \item{what}{optional character vector of one or more dialog names for which the memory is to be flushed; if not 
    specified, all dialog memory will be flushed.}
  \item{window, parentWindow}{a Tk window.}
  \item{x}{an R object name, as a character string, or a tcl variable or object, or a vector of
    variable names to be sorted.}
  \item{\dots}{For \code{gettextRcmdr}, text string or vector of text strings
    to translate; for \code{defmacro}, arguments for the macro; otherwise disregard.}
}

\details{

  There are several groups of functions exported by the \code{Rcmdr} package and documented briefly here.
  To see how these functions work, it is simplest to examine the dialog-generating functions in the \code{Rcmdr} package.

  \emph{Executing and logging commands: }
  The functions \code{doItAndPrint}, \code{justDoIt}, and \code{logger} control the 
  execution, logging, and printing of commands generated by menus and dialogs. 
  \code{logger(command)} adds \code{command} to the log/script window and to the output window. 
  \code{justDoIt(command)} causes \code{command}
  to be executed. \code{doItAndPrint(command)} does both of these operations, and also prints the
  output produced by the command.
  The R Commander maintains a list of output objects, by default including the last
  10 outputs. \code{popOutput()} ``pops'' (i.e., returns and removes) the first entry
  of the output stack. Note that, as a stack, the queue is LIFO (``last in, first out'').
  There is also a stack of commands, which is accessed similarly by \code{popCommand()}.
  
  \emph{Checking for errors: }
  The function \code{is.valid.name} checks whether a character string specifies a valid name
  for an R object. The functions \code{checkActiveDataSet}, \code{checkActiveModel},
  \code{checkFactors}, \code{checkNumeric}, \code{checkTwoLevelFactors}, and \code{checkVariables}
  check for the existence of objects and write an error message to the log if they are absent
  (or insufficiently numerous, in the case of different kinds of variables). The function 
  \code{checkReplace} opens a dialog to query whether an existing object should be replaced.
  The function \code{checkMethod}, checks whether a method exists for a particular generic that is
  appropriate for a particular object. The function \code{checkClass} checks whether an object is of
  a specific class. Both of these functions write error messages to the log if the condition fails.
  The function \code{errorCondition} reports an error to the user and (optionally) re-starts
  a dialog.
  
  \emph{Information: }
  Several functions return vectors of object names: \code{listAllModels}, \cr
  \code{listAOVModels}, \code{listDataSets}, \code{listGeneralizedLinearModels},
  \code{listFactors}, \code{listLinearModels}, \code{listMultinomialLogitModels},
  \code{listNumeric}, \code{listProportionalOddsModels},
  \code{listTwoLevelFactors},
  \code{listVariables}. The functions \code{activeDataSet} and \code{activeModel} respectively report or set the active
  data set and model.
  The function \code{packageAvailable} reports whether the named package is 
  available to be loaded (or has possibly already been loaded).
  The function \code{exists.method} checks whether a method exists for a particular generic that is
  appropriate for a particular object, and returns \code{TRUE} or \code{FALSE}.
  
  \emph{Building dialog boxes: }
  Several functions simplify the process of constructing Tk dialogs: 
  initializing a dialog box, \code{initializeDialog}, and completing the definition of a dialog box, \code{dialogSuffix};
  a set of check boxes, \code{checkBoxes};
  a set of radio buttons, \code{radioButtons};
  a list box with associated scrollbars and state variable, \code{variableListBox}
  (and the associated functions \code{getFrame} and \code{getSelection});
  a button and subdialog for selecting a "grouping" variable, \code{groupsBox}; 
  displaying the currently defined groups in a dialog, \code{groupsLabel};
  a dialog-box structure for entering a model formula, \code{modelFormula};
  a text box for entering a subsetting expression, \code{subsetBox};
  \emph{OK}, \emph{Cancel}, and \emph{Help} buttons for dialogs, \code{OKCancelHelp}, and subdialogs, \code{subOKCancelHelp}.
  The functions \code{putDialog}, \code{getDialog}, and \code{varPosn} support dialog-box memory---i.e., retaining
  selections across invocations of a dialog.
  
  \emph{``Themed'' Tk widgets:}
  Tk 8.5 introduced so-called ``themed'' widgets, which look better than the traditional Tk widgets. 
  Several functions, contributed by Brian Ripley, are written to access the new widgets by switching automatically 
  between the new and old widget sets depending upon the availability of the former: \code{buttonRcmdr}, 
  to access either \code{\link{ttkbutton}} or \code{\link{tkbutton}}; \code{labelRcmdr}, 
  to access either \code{\link{ttklabel}} or \code{\link{tklabel}}; \code{ttkentry}, 
  to access either \code{\link{ttkentry}} or \code{\link{tkentry}}; \code{ttkframe}, 
  to access either \code{\link{ttkframe}} or \code{\link{tkframe}}; \code{ttkradiobutton}, 
  to access either \code{\link{ttkradiobutton}} or \code{\link{tkradiobutton}}; and \code{ttkscrollbar}, 
  to access either \code{\link{ttkscrollbar}} or \code{\link{tkscrollbar}}. 
  Note that the last four functions mask functions of the same names in the \pkg{tcltk} package.
  
  \emph{`Predicate' functions:}
  A number of functions of the form \emph{name}\code{P} are `predicate' functions, which return
  \code{TRUE} or \code{FALSE} depending upon whether some condition obtains. For example, \code{lmP()} 
  returns \code{TRUE} if there is an active model that is a linear model; and \code{factorsP(2)}
  returns \code{TRUE} if there are at least two factors in the active data set.
  
  \emph{Translating text:}
  The \code{gettextRcmdr} function simply passes its argument(s) to
   \code{\link[base]{gettext}}, adding the argument \code{domain="R-Rcmdr"}.
   
  \emph{Miscellaneous:}
  The function \code{trim.blanks} removes spaces from the beginning and end of a character string. The function
  \code{installPlugin} installs an Rcmdr plug-in from a ZIP file or directory; this function may be used to
  create self-installing plug-ins in the form of packages. The function \code{nobs} returns the number of 
  observations on which a statistical model is based. The function \code{formulaFields} returns information
  about the left-hand side, right-hand side, data, subject, and (for GLMs) family and link, of a model object.
  The function \code{sortVarNames} sorts variable names, including those containing numerals, into a more
  "natural" order than does the standard \code{sort} function. The function \code{Library} may be used to
  load packages; it checks whether a package is already loaded, and if not by default puts it in position
  4 on the search path.
  
  The function \code{RExcelSupported} is used for the RExcel interface.
  
  Some of these functions, marked \code{# macro} under \emph{Usage}, are "macro-like" in their behaviour, in that they 
  execute in the environment from which they are called. These were defined with an adaptation (used with permission) of
  Thomas Lumley's \code{defmacro} function, described in Lumley (2001).
  
}

\references{
    T. Lumley (2001) Programmer's niche: Macros in R.
    \emph{R News}, \bold{1(3)}, 11--13.
    }

\author{John Fox \email{jfox@mcmaster.ca}}

\keyword{misc}
