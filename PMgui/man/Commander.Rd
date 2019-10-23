\name{Commander}
\alias{Commander}

\title{R Commander}
\description{
  Start the R Commander GUI (graphical user interface)
}
\usage{
Commander()
}

\details{

\bold{Getting Started}

The default R Commander interface consists of (from top to bottom) a menu bar, a toolbar,
a script window, an output window, and a messages window.

Commands to read, write, transform, and analyze data are entered using the menus in the
menu bar at the top of the \emph{Commander} window. Most menu items lead to dialog boxes
requesting further specification. I suggest that you explore the menus to see
what is available.

Below the menu bar is a toolbar with (from left to right) an information field
displaying the name of the active data set; buttons for editing and displaying the active
data set; and an information field showing the active statistical model. There is also
a \emph{Submit} button for re-executing commands in the script window.
The information fields for the active data set and active model are actually 
buttons that can be used to select the active data set and model from among, respectively,
data frames or suitable model objects in memory.

Almost all commands require an active data set. When the Commander starts, there is no active
data set, as indicated in the data set information field. A data set becomes the active data set
when it is read into memory from an R package or imported from a text file, SPSS data set,
Minitab data set,  STATA data set, or an Excel, Access, or dBase data set. 
In addition, the active data set can be selected from among R data frames
resident in memory. You can therefore switch among data sets during a session.

By default, commands are logged to the script window (the
initially empty text window immediately below the toolbar), and 
commands and output appear in the output window (the initially empty text window below the
script window). To alter these and
other defaults, see the information below on configuration.
 
Some \code{Rcmdr} dialogs (those in the \emph{Statistics -> Fit models} menu) 
produce linear, generalized
linear, or other models. When a model is fit, it becomes the active model, as indicated in the information
field in the R Commander toolbar.
Items in the \emph{Models} menu apply to the active model. Initially, there is no active model. 
If there are several models in memory, you can select the active model from among them.

If command logging in turned on, R commands that are generated from the menus and dialog boxes are
entered into the script window in the Commander. You can edit these commands in the normal
manner and can also type new commands into the script window. Individual commands
can be continued over more than one line, the several lines of a multi-line command
must be submitted simultaneously. (It is not necessary, as in earlier versions of the
R Commander, to begin continuation lines with white space.) The contents of the script
window can be saved during or at the end of the session, and a saved script can be loaded into the
script window. The contents of the output window can also be edited or saved to a text file. Finally,
editing operations also work in the messages window.

To re-execute a command or set of commands, select the lines to be executed using
the mouse and press the \emph{Submit} button at the right of the toolbar
(or \emph{Control-R}, for "run", or \emph{Control-Tab}). If no text is selected, 
the \emph{Submit} button (or \emph{Control-R} or \emph{Control-Tab}) 
submits the line containing the text-insertion cursor. Note that an error will be generated if the submitted
command or commands are incomplete.

Pressing \emph{Control-F} brings up a find-text dialog box (which can also be accessed via 
\emph{Edit -> Find}) to search for text in the script, output, or messages window. Edit functions such as
search are performed in the script window unless you first click in the output or messages window to make it the active
window.

Pressing \emph{Control-S} will save the script or output window.

Pressing \emph{Control-A} selects all of the text in the script, output, or messages window.

In addition, the following Control-key combinations work in the script, output, and messages windows: \emph{Control-X}, cut;
\emph{Control-C}, copy; \emph{Control-V}, insert; \emph{Control-Z} or \emph{Alt-Backspace}, undo; and \emph{Control-W}, redo.

Right-clicking the mouse (clicking button 3 on a three-button mouse, or \emph{Control}-left-clicking) in the script, output, or message window brings up
a "context" menu with the \emph{Edit}-menu items, plus (in the script window) a \emph{Submit} item.

When you
execute commands from the \emph{Commander} window, you must ensure that the sequence of commands
is logical. For example, it makes no sense to fit a statistical model to a data set that has
not been read into memory.

Pressing a letter key (e.g., "a") in a list box will scroll the list box to bring the next entry 
starting with that letter to the top of the box. 

You can cancel an R Commander dialog box by pressing the \emph{Esc} key.

Exit from the Commander via the \emph{File -> Exit} menu or by closing the \emph{Commander} window.

\bold{Customization and Configuration}

The preferred way of customizing the R Commander is to write a plug-in package:
see \code{help("\link{Plugins}")}.

Alternatively, configuration files reside in the \code{etc} subdirectory of the package, 
or in the locations given by the \code{etc} and \code{etcMenus} options (see below).

The \code{Rcmdr} menus can be customized by editing the file \code{Rcmdr-menus.txt}.

You can add R code to the package, e.g., for creating additional dialogs, by placing files with
file type \code{.R} in the \code{etc} directory, also editing \code{Rcmdr-menus.txt} to provide
additional menus, sub-menus, or menu-items.  Alternatively, you can edit 
the source package and recompile it. 

To reiterate, however, the preferred procedure is to write an R Commander plug-in package.

A number of functions are provided to assist in writing dialogs, and \code{Rcmdr} state information
is stored in a separate environment. See \code{help("\link{Rcmdr.Utilities}")}
and the manual supplied in the \code{doc} directory of the \code{Rcmdr} package for more information.

In addition, several features are controlled by run-time options, set via the \code{options("Rcmdr")}
command. These options should be set before the package is loaded. If the options are unset, which is
the usual situation, defaults are used. Specify options as a list of \emph{name=value} pairs. You can
set none, one, several, or all options. The available options are as follows:

\describe{
	\item{\code{ask.to.exit}}{if \code{TRUE} (the default), then the user is asked whether he or she wants to exit the \code{Rcmdr};
	if this option is set to \code{FALSE}, then the subsequent option is also set to \code{FALSE}.}
	
    \item{\code{ask.on.exit}}{if \code{TRUE} (the default), then the user is asked whether to save
    the script file and the output file when the \code{Rcmdr} exits.}

    \item{\code{attach.data.set}}{if \code{TRUE} (the default is \code{FALSE}), the active data set is attached to the
    search path.}

    \item{\code{check.packages}}{if \code{TRUE} (the default), on start-up, the presence of all of the \code{Rcmdr}
    recommended packages will be checked, and if any are absent, the \code{Rcmdr} will offer to install them.}

    \item{\code{command.text.color}}{Color for commands in the output window; the default is \code{"red"}.}
       
    \item{\code{console.output}}{If \code{TRUE}, output is directed to the \emph{R Console}, and the 
    \emph{R Commander} output window is not displayed. The default is \code{FALSE}, unless the R Commander is running under RStudio, 
    in which case the default is \code{TRUE}.}
    
    \item{\code{crisp.dialogs}}{If \code{TRUE}, dialogs should appear on the screen fully drawn, rather than built up
    widget by widget. Prior to R 2.6.1, this option only works on the Windows version of R, but should in any event be harmless.
    The default is \code{TRUE}. If you encounter stability problems, try setting this option to
    \code{FALSE}.}
    
    \item{\code{default.contrasts}}{Serves the same function as the general \code{contrasts} option; the default is\cr
    \code{c("contr.Treatment", "contr.poly")}. When the Commander exits, the \code{contrasts}
    option is returned to its pre-existing value. Note that \code{contr.Treatment} is from the \code{car} package.}
    
    \item{\code{default.font}}{The default font for GUI elements such as menus and text labels, in the form of a 
    Tk font specification, given in a character string. 
    For example,  \code{"-family Helvetica -size 12"} specifies a 12-point Helvetica font.
    If specified, this value takes precedence over the default font size (below). Absent a compelling reason for
    changing the font, it is generally more convenient simply to change the default font size.}

    \item{\code{default.font.size}}{The size, in points, of the default font. The default is 10. 
    To set the font size for R input and output, see the \code{log.font.size} option.
    It is recommended that Windows users set the \code{Rcmdr} \code{scale factor} option to
    control font size.}
    
    \item{\code{retain.selections}}{If \code{TRUE} (the default), dialogs remember their previous state, where appropriate,
     as long as the data set isn't changed; some dialogs, e.g., for probabilities, retain selections even when the data set chanages.}
    
    \item{\code{double.click}}{Set to \code{TRUE} if you want a double-click of the left mouse
    button to press the default button in all dialogs. The default is \code{FALSE}.}
    
    \item{\code{error.text.color}}{Color for error messages; the default is \code{"red"}.}
    
    \item{\code{etc}}{Set to the path of the directory containing the \code{Rcmdr}
    configuration files; defaults to the \code{etc} subdirectory of the installed \code{Rcmdr} package.}
    
    \item{\code{grab.focus}}{Set to \code{TRUE} for the current Tk window to 
    "grab" the focus --- that is, to prevent the focus from being changed to another Tk window.
    On some systems, grabbing the focus in this manner apparently causes problems. The default is \code{TRUE}. 
    If you experience focus problems, try setting this option to \code{FALSE}.}
    
    \item{\code{iconify.commander}}{If \code{TRUE}, the \emph{Commander} window is minimized on startup;
    the default is \code{FALSE}.}
    
    \item{\code{length.output.stack}}{The R Commander maintains a list of output objects, by default including the last
    several outputs; the default length of the output stack is 10. \code{popOutput()} ``pops'' (i.e., returns and removes) the first entry
    of the output stack. Note that, as a stack, the queue is LIFO (``last in, first out'').}
    
    \item{\code{length.command.stack}}{The R Commander also maintains a list of commands that is managed similarly; the default length
    of this stack is also 10.}
    
%%    \item{\code{load.at.startup}}{A character vector of names of packages to be loaded 
%%    when the \code{Rcmdr} package is loaded; the default is to load only the \code{car} package. 
%%    Other required packages will be loaded as needed. If it is available, the 
%%    \code{car} package will be loaded at when the Commander starts in any event.}
      
    \item{\code{log.commands}}{If \code{TRUE} (the default), commands are echoed to the script window;
    if \code{FALSE}, the script window is not displayed.}

    \item{\code{log.font.size}}{The font size, in points, to be used in the script window,
    in the output window,
    in recode dialogs, and in compute expressions --- that is, where a monospaced font is used. The default is 10.
    It is recommended that Windows users set the \code{Rcmdr} \code{scale factor} option to
    control font size.}
    
    \item{\code{log.height}}{The height of the script window, in lines. The default is 10. Setting \code{log.height}
    to 0 has the same effect as setting \code{log.commands} to \code{FALSE}.}
        
    \item{\code{log.text.color}}{Color for text in the script window; the default is \code{"black"}.}
        
    \item{\code{log.width}}{The width of the script and output windows, in characters. The default is 80.}
    
    \item{\code{messages.height}}{The height of the messages window, in lines. The default is 3.} 
    
    \item{\code{multiple.select.mode}}{Affects the way multiple variables are selected in variable-list boxes.
    If set to \code{"extended"} (the default), left-clicking on a variable selects it and deselects any other
    variables that are selected; Control-left-click toggles
    the selection (and may be used to select additional variables); Shift-left-click extends the selection.
    This is the standard Windows convention. If set
    to \code{"multiple"}, left-clicking toggles the selection of a variable and may be used to select more than one
    variable. This is the behaviour in the \code{Rcmdr} prior to version 1.9-10.}
    
    \item{\code{number.messages}}{If \code{TRUE}, the default, messages in the messages window are numbered.}
    
    \item{\code{output.height}}{The height of the output window, in lines. The default is twice the height
    of the script window, or 20 if the script window is suppressed. Setting \code{output.height}
    to 0 has the same effect as setting \code{console.output} to \code{TRUE}.}
        
    \item{\code{output.text.color}}{Color for output in the output window; the default is \code{"blue"}.}
        
    \item{\code{placement}}{Placement of the \emph{R Commander} window, in pixels; the default is
    \code{"-40+20"}, which puts the window near the upper-right corner of the screen.}
    
    \item{\code{plugins}}{A character vector giving the names of \code{Rcmdr} plug-in packages to load when the 
    Commander starts up. Plug-in packages can also be loaded from the \emph{Tools -> Load Rcmdr plug-in(s)} menu.
    See \link{Plugins}.}
    
    \item{\code{prefixes}}{A four-item character vector to specify the prefixes used when output is directed
    to the R console; the default is \code{c("Rcmdr> ", "Rcmdr+ ", "RcmdrMsg: ", "RcmdrMsg+ ")}.}
    
    \item{\code{suppress.menus}}{if \code{TRUE}, the Commander menu bar and tool bar are suppressed, allowing another
    program (such as Excel) to take over these functions. The default (of course) is \code{FALSE}.}
            
    \item{\code{suppress.X11.warnings}}{On (some?) Linux and Mac OS X systems, multiple X11 warnings are generated by \code{Rcmdr}
    commands after a graphics-device window has been opened. Set this option to \code{TRUE} (the default when running
    interactively under X11) to suppress reporting of these warnings. An undesirable side effect
    is that then \emph{all} warnings and error messages are intercepted by the \code{Rcmdr},
    even those for commands entered at the R command prompt. Messages produced by such commands will be printed
    in the Commander Messages window after the next \code{Rcmdr}-generated command.
    Some X11 warnings may be printed when you exit from the Commander.}
    
    \item{\code{retain.messages}}{If \code{TRUE} (the default), the contents of the message window
    are not erased between messages. In any event, a "NOTE" message will not erase a preceding "WARNING" or "ERROR".}
    
    \item{\code{RExcelSupport}}{If \code{TRUE} (the default is \code{FALSE}), menus and output are handled by Excel.}
    
    \item{\code{scale.factor}}{A scaling factor to be applied to all Tk elements, such as fonts. This works well
    only in Windows. The default is \code{NULL}.}

    \item{\code{showData.threshold}}{If the number of variables in the active data set exceeds this value (default, 100),
    then \code{View()} rather than \code{showData()} is used to display the data set. The reason for the option is that \code{showData()} is very slow
    when the number of variables is large; setting the threshold to 0 suppresses the use of showData altogether.}
         
    \item{\code{show.edit.button}}{Set to \code{TRUE} (the default) if you want an \emph{Edit} button in the Commander
    window, permitting you to edit the active data set. Windows users may wish to set this option to \code{FALSE}
    to suppress the \emph{Edit} button because changing variable names in the data editor can cause R to crash
    (though I believe that this problem as been solved).}
    
    \item{\code{sort.names}}{Set to \code{TRUE} (the default) if you want variable names to be sorted
    alphabetically in variable lists.}
        
    \item{\code{tkwait}}{This option addresses a problem that, to my knowledge, is rare, 
    and may occur on some non-Windows systems. If the Commander causes R to hang, 
    then set the \code{tkwait} option to
    \code{TRUE}; otherwise set the option to \code{FALSE} or ignore it. 
    An undesirable side effect of setting
    the \code{tkwait} option to \code{TRUE} is that the R session command prompt is suppressed until the Commander exits.
    One can still enter commands via the script window, however. In particular, there is no reason to use this option
    under Windows, and it should not be used with the Windows R GUI with buffered output when output is directed to the
    R console.}
    
    \item{\code{use.rgl}}{If \code{TRUE} (the default), the \code{rgl} package will be loaded if it is present in an
    accessible library; if \code{FALSE}, the \code{rgl} package will be ignored even if it is available. 
    The \code{rgl} package can sometimes cause problems when running R under X11.}
    
    \item{\code{variable.list.height}}{the number of items (typically variables) to display in list boxes; longer lists may be viewed by scrolling. 
    The default is 4.} 
    
    \item{\code{variable.list.width}}{a two-item vector controlling the width of list boxes, in characters, giving the minimum and maximum
    width to display; the default is \code{c(20, Inf)}. If the widest item name falls in this range, then its number of characters
    determines the width of the box. \emph{Note:} This specification works only approximately.}
    
    \item{\code{warning.text.color}}{Color for warning messages; the default is \code{"darkgreen"}.}
    }
    
Some options can also be set via the \emph{File -> Options} menu, which will restart the Commander after options are set.

If you want always to launch the R Commander when R starts up, you can include the following code in one of R's start-up 
files (e.g., in the \code{Rprofile.site} file in R's \code{etc} subdirectory):

\preformatted{
local({
old <- getOption("defaultPackages")
options(defaultPackages = c(old, "Rcmdr"))
})
}

R Commander options can also be permanently set in the same manner.
For more information about R initialization, see \code{?Startup}.
}

\section{Warning}{
    The R Commander Script window does not provide a true console to R, and may have certain limitations.
    I don't recommend using the R Commander for serious programming or for data analysis that relies
    primarily on scripts --- use a programming editor instead. If you encounter any problems with the
    Script window, however, I'd appreciate it if you brought them to my attention.
    }

\section{Known Problems}{
  Occasionally, under Windows, after typing some text into a dialog box (e.g., a subsetting expression in the Subset  Data Set dialog), 
  buttons in the dialog (e.g., the OK button) will have no effect when they are pressed. Clicking anywhere inside or outside of the dialog 
  box should restore the function of the buttons. As far as I have been able to ascertain, this is a problem with Tcl/Tk for Windows.
  I have not seen this behavior in some time and the problem may have been solved.
}


\note{
  This version may be compatible with SciViews:
   \url{http://www.sciviews.org/SciViews-R}; see \code{\link{Rcmdr.sciviews-specific}}.
  This version should be compatiable with the \pkg{RExcel} package, which can use the R Commander
  menus.
  
  Under Windows, the \code{Rcmdr} package can be run under the \emph{Rgui} in the SDI (single-document
  interface) mode, or under \code{rterm.exe}. You might experience problems running the \code{Rcmdr} under
  \code{ESS} with NTEmacs or XEmacs; under other R consoles; or under the Rgui in the MDI (multiple-document interface) mode.
}

\references{
Fox, J. (2005) 
The R Commander: A Basic Statistics Graphical User Interface to R. 
\emph{Journal of Statistical Software}, \bold{14(9)}: 1--42.

Fox, J. (2007)
Extending the R Commander by "plug in" packages. 
\emph{R News}, \bold{7(3)}: 46--52.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\link{Plugins}, \link{Rcmdr.Utilities}}

\examples{
    options(Rcmdr=list(log.font.size=12, default.contrasts=c("contr.Sum", "contr.poly")))
    }

\keyword{misc}
