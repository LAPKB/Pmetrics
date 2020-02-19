.getGfortran <- function() {
  OS <- getOS()
  if (OS == 1) {
    cat("Checking for Brew \n")
    if (system("which -s brew") != 0) {
      cat("Pmetrics did not find the Homebrew Package Manager and will attempt to download and install it.\n")
      input <- tolower(readline("Do you wish to proceed? (Y/N)"))
      if (substr(input,1,1) == "n") {
        return(FALSE)
      }
      script = paste(system.file("", package = "Pmetrics"), "mac/install_homebrew.sh", sep = "/")
      system(paste("chmod +x ", script))
      system(paste0("open -a Terminal.app '", script, "'"))
      #system("ruby -e \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)\"")
      cat("A new terminal will open and ask for permission to install.  Enter your computer password.\n")
      input <- readline(prompt =  "Press any key when Homebrew installation is complete.\n")
    } else {
      cat("Pmetrics found Homebrew found and will ensure latest version is installed.\n")
      system("brew update")
    }
    if (system("brew ls --versions gcc") != 0) {
      cat("Pmetrics did not find GCC and will install it.\n")
      system("brew install gcc")
    } else {
      cat("Pmetrics found GCC.\n")
    }

    if (system("which -s gfortran") != 0) {
      cat("ERROR: Pmetrics did not install gfortan automatically.\nPlease install gfortran manually and then run PMbuild().\nGo to http://www.lapk.org/Pmetrics_install.php for help.\n")
      return(FALSE)
    } else {
      cat("Pmetrics installed gfortran successfully.\n")
      return(TRUE)
    }
  } else if (OS == 2) {
    return = system2("C:/Windows/System32/WindowsPowerShell/v1.0/powershell", args = c("-file", paste(system.file("", package = "Pmetrics"), "win/exec.ps1", sep = "/")), wait = T)
    cat("Background installation of gfortran started.\n")
    readline(prompt = "After installation is complete, press RETURN to continue.\n")
    return(TRUE)
  }
  else if (OS == 3) {
    cat("Pmetrics is checking for build essentials...\n")
    if (system("which gcc") != 0) {
      cat("Pmetrics did not find build essentials - Installing... \n")
      system("sudo apt-get install build-essential")
    } else {
      cat("Pmetrics found build essentials.\n")
    }

    cat("Pmetrics is checking for gfortran...\n")
    if (system("which gfortran") != 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }



}