#' \code{PMFortranConfig} will read or define the installed Fortran compiler and generate
#' a command line template appropriate to the compiler. 
#'
#' Command line templates are defined for the following compilers: \bold{gfortran}, \bold{g95},
#' \bold{Intel Visual}, and \bold{Lahey}.  Additionally, users may specify a custom command line
#' template for any other compiler.  Within the template \emph{<exec>} is used as a placeholder
#' for the filename of the executable file, and \emph{<files>} as a placeholder for the files to
#' compile and link, both of which will be defined at run time by the appropriate Pmetrics functions.
#' The Pmetrics functions which use a Fortran compiler are \code{\link{NPrun}}, \code{\link{ITrun}},
#' \code{\link{ERRrun}}, and \code{\link{SIMrun}}.
#' @title Read or define the Fortran compiler and command line template
#' @param reconfig Default is \code{False}.  If \code{True}, will allow user to change
#' the previously specified compiler and template.
#' @return \code{PMFortranConfig} returns the compile command template specific to the chosen
#' compiler.  
#' @author Michael Neely
#' @seealso \code{\link{NPrun}}, \code{\link{ITrun}},\code{\link{ERRrun}}, and
#' \code{\link{SIMrun}}
#' @export

PMFortranConfig <- function(reconfig = F) {
  #figure out the OS
  OS <- getOS()
  #get the fortran path
  configFilename <- switch(OS, "~/.config/Pmetrics/FortConfig.txt",
                           paste(Sys.getenv("APPDATA"), "\\Pmetrics\\FortConfig.txt", sep = ""),
                           "~/.config/Pmetrics/FortConfig.txt")
  #read the configuration file
  if (file.exists(configFilename)) {
    compiler <- readLines(configFilename)
    #check for non-parallel gfortran, and if present, update for 64 bit only (doesn't work in 32 bit)
    if (getBits() == "64" & length(compiler) == 1 & length(grep("^gfortran", compiler) > 0)) {
      compiler <- c(compiler, "gfortran -O3 -w -fopenmp -fmax-stack-var-size=32768 -o <exec> <files>")
      writeLines(compiler, configFilename)
      cat("Your gfortran compiler statement has been updated to include parallel processing.\nYou can use PMFortranConfig() to see the new statement.\n\n")
      flush.console()
    }
  } else {
    compiler <- NA
    dir.create(switch(OS, "~/.config/Pmetrics",
                      paste(Sys.getenv("APPDATA"), "\\Pmetrics", sep = ""),
                      "~/.config/Pmetrics"), showWarnings = T, recursive = T)
  }
  #if compiler is missing or user wants to reconfigure, execute the following block
  if (is.na(compiler[1]) | reconfig) {
    cat("\nPmetrics needs to know which Fortran compiler you are using.\n")
    cat("You only have to specify this once.\n")
    cat("However, you can reconfigure if your compiler changes\n")
    cat("by using the command PMFortranConfig(reconfig=T).\n\n")
    cat("In each of the following <exec> is a place holder for the executable file name\n")
    cat("and <files> is a placeholder for the files to be compiled.  Both are required.\n")
    cat("When applicable serial and parallel compile statements in Pmetrics are listed in that order.\n\n")
    cat(paste("1. gfortran -m", getBits(), " -w -O3 -o <exec> <files>\ngfortran -O3 -w -fopenmp -fmax-stack-var-size=32768 -o <exec> <files>\n", sep = ""))
    cat("2. g95: g95 -o -fstatic <exec> <files>\n")
    cat("3. Intel Visual: ifort -o <exec> <files>\n")
    cat("4. Lahey: lf90  <files> -fix -out <exec>\n")
    cat("5. Other (define custom command)\n")
    cat("6. Help, I don't have a Fortran compiler!\n")
    choice <- NA
    #ensure valid input
    while (is.na(choice)) {
      # choice <- readline("\nEnter the number of your compiler: ")
      choice <- "1" #make gfortran only choice for now
      if (length(grep("[[:digit:]]", choice)) == 0) { choice <- NA; next }
      if ((OS == 1 | OS == 3) & (choice == "2" | choice == "4")) {
        cat("\n g95 and Lahey compilers are not available in the Mac/Unix/Linux environment.\n")
        choice <- NA
        next
      }

      if (as.numeric(choice) < 5) {
        compiler <- switch(as.numeric(choice),
                           paste("gfortran -m", getBits(), " -w -O3 -march=native -std=legacy -o <exec> <files>\ngfortran -O3 -w -march=native -std=legacy -fopenmp -fmax-stack-var-size=32768 -o <exec> <files>", sep = ""),
                           "g95 -o -fstatic <exec> <files> ",
                           "ifort -o <exec> <files>",
                           "lf90  <files> -fix -out <exec>")
      } else {
        if (choice == "5") {
          cat("\nCustom compile commands should be entered using '<exec>' as a placeholder\n")
          cat("for the executable filename, and '<files>' as a placeholder for the files\n")
          cat("to be linked and compiled.\n")
          cat("\nExample: gfortran -O3 -o <exec> <files>\n")
          compiler <- readline("\nEnter your custom compile statement: ")
        } else {
          #get system-appropriate gfortran
          compiler <- gfortranCheck()
        }
      }
      writeLines(compiler, configFilename)
    }
  }
  return(compiler)
}

