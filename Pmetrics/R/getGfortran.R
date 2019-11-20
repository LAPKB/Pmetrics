.getGfortran <- function() {
  OS <- getOS()
  if (OS == 1) {
    cat("Checking for brew \n")
    if (system("which -s brew") != 0) {
      cat("Brew not found - Installing... \n")
      system("ruby -e \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)\"")
    } else {
      cat("Brew found - Updating... \n")
      system("brew update")
    }

    cat("Checking for gcc \n")
    if (system("brew ls --versions gcc") != 0) {
      cat("GCC not found - Installing... \n")
      system("brew install gcc")
    } else {
      cat("GCC found\n")
    }

    cat("Checking for gfortran \n")
    if (system("which -s gfortran") != 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else if (OS == 2) {
    return = system2("C:/Windows/System32/WindowsPowerShell/v1.0/powershell", args = c("-file", paste(system.file("", package = "Pmetrics"), "win/exec.ps1", sep = "/")), wait = T)
    readline(prompt = "Background installation of gfortran started, please wait until the process finishes and press RETURN to continue.")
    return(TRUE)
  }
  else if (OS == 3) {
    cat("Checking for build essentials \n")
    if (system("which -s gcc") != 0) {
      cat("Build essentials not found - Installing... \n")
      system("sudo apt get install build-essentials")
    } else {
      cat("Build essentials found\n")
    }

    cat("Checking for gfortran \n")
    if (system("which -s gfortran") != 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }



}