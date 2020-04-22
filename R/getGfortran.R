#' Download and install Gfortran updates
#'
#' @title Download and install Gfortran updates
#' @return A boolean that represents if the latest version of Gfortran for your OS is installed.
#' @author Michael Neely
#' @export

updateGfortran <- function() {
  OS <- getOS()
  if (OS == 1) {
    .installOrUpdateGCC()
  } else {
    cat("This functionality is currently only working in MacOS, if you are using another OS\nremove your current gfortran installation and run PMbuild()")
  }
}

.installOrUpdateGfortran <- function() {
  OS <- getOS()
  if (OS == 1) {
    cat("Checking for Brew \n")
    if(!.installOrUpdateBrew()){
      return(F)
    }
    .installOrUpdateGCC()

    if (system("which -s gfortran") != 0) {
      cat("ERROR: Pmetrics did not install gfortan automatically.\nPlease install gfortran manually and then run PMbuild().\nGo to http://www.lapk.org/Pmetrics_install.php for help.\n")
      return(FALSE)
    } else {
      cat("Pmetrics installed gfortran successfully.\n")
      return(TRUE)
    }
  } else if (OS == 2) {
    return = system2("C:/Windows/System32/WindowsPowerShell/v1.0/powershell", args = c("-ExecutionPolicy","ByPass","-file", paste(system.file("", package = "Pmetrics"), "win/exec.ps1", sep = "/")), wait = T)
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
      system("sudo apt-get update build-essential")
    }

    cat("Pmetrics is checking for gfortran...\n")
    if (system("which gfortran") != 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

.installOrUpdateBrew <- function(){
  if (system("which -s brew") != 0) {
    #Brew is not installed
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
    #brew is installed
    cat("Pmetrics found Homebrew found and will ensure latest version is installed.\n")
    system("brew update")
  }
  return(T)
}

.installOrUpdateGCC <- function(){
  if (system("brew ls --versions gcc") != 0) {
      cat("Pmetrics did not find GCC and will install it.\n")
      system("brew install gcc")
    } else {
      cat("Pmetrics found GCC.\n")
      if(system("brew outdated | grep gcc")==0){
        #there is a new version of GCC
        cat("Pmetrics found a new version of GCC and will update it.\n")
        system("brew upgrade gcc")
      }
    }
}