#' Download and install Gfortran updates
#' IMPORTANT: Only use this function if you are using either brew on Mac or chocolatey on windows to install gfortran.
#'
#' @title Download and install Gfortran updates
#' @return A boolean that represents if the latest version of Gfortran for your OS is installed.
#' @author Michael Neely
#' @export

update_gfortran <- function() {
  OS <- getOS()
  if (OS == 1 || OS == 2) {
    .installOrUpdateGCC(OS)
  } else {
    cat("This functionality is currently only working in MacOS, if you are using another OS\nremove your current gfortran installation and run PMbuild()")
  }
}

.installOrUpdateGfortran <- function() {
  OS <- getOS()
  if (OS == 1) {
    cat("Checking for Brew \n")
    if (!.installOrUpdateBrew()) {
      return(F)
    }
    .installOrUpdateGCC(OS)
    if (isM1()) {
      sch_str <- "which -s /opt/homebrew/bin/gfortran"
    } else {
      sch_str <- "which -s gfortran"
    }
    if (system(sch_str) != 0) {
      cat("ERROR: Pmetrics did not install gfortan automatically.\nPlease install gfortran manually and then run PMbuild().\nGo to http://www.lapk.org/Pmetrics_install.php for help.\n")
      return(FALSE)
    } else {
      cat("Pmetrics installed gfortran successfully.\n")
      return(TRUE)
    }
  } else if (OS == 2) {
    return <- system2("C:/Windows/System32/WindowsPowerShell/v1.0/powershell", args = c("-ExecutionPolicy", "ByPass", "-file", paste(system.file("", package = "Pmetrics"), "win/exec.ps1", sep = "/")), wait = T)
    cat("Background installation of gfortran started.\n")
    readline(prompt = "After installation is complete, press RETURN to continue.\n")
    return(TRUE)
  } else if (OS == 3) {
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

.installOrUpdateBrew <- function() {
  if (isM1()) {
    #gfortran_path <- "/opt/R/arm64/gfortran/bin"
    gfortran_path <- "/opt/homebrew/bin/gfortran"
    
    brew_path <- "/opt/homebrew/bin/brew"
  } else {
    gfortran_path <- "gfortran"
    brew_path <- "brew"
  }
  if (system(paste0("which -s ", gfortran_path)) != 0) {
    # Brew is not installed
    cat("Pmetrics did not find the Homebrew Package Manager and will attempt to download and install it.\n")
    input <- tolower(readline("Do you wish to proceed? (Y/N)"))
    if (substr(input, 1, 1) == "n") {
      return(FALSE)
    }
    script <- paste(system.file("", package = "Pmetrics"), "mac/install_homebrew.sh", sep = "/")
    system(paste("chmod +x ", script))
    system(paste0("open -a Terminal.app '", script, "'"))
    cat("A new terminal will open and ask for permission to install.  Enter your computer password.\n")
    input <- readline(prompt = "Press any key when Homebrew installation is complete.\n")
  } else {
    # brew is installed
    cat("Pmetrics found Homebrew found and will ensure latest version is installed.\n")
    system(paste0(brew_path, " update"))
  }
  return(TRUE)
}

.installOrUpdateGCC <- function(OS) {
  if (OS == 1) {
    if (isM1()) {
      brew_path <- "/opt/homebrew/bin/brew"
    } else {
      brew_path <- "brew"
    }
    if (system(paste0(brew_path, " ls --versions gcc")) != 0) {
      cat("Pmetrics did not find GCC and will install it.\n")
      system(paste0(brew_path, " install gcc"))
    } else {
      cat("Pmetrics found GCC.\n")
      if (system(paste0(brew_path, " outdated | grep gcc")) == 0) {
        # there is a new version of GCC
        cat("Pmetrics found a newer version of GCC and will update it.\n")
        system(paste0(brew_path, " upgrade gcc"))
      }
    }
  } else if (OS == 2) {
    if (system2("C:/Windows/System32/WindowsPowerShell/v1.0/powershell", "choco outdated | grep mingw") == 0) {
      # there is a new version of mingw
      cat("Pmetrics found a newer version of GCC and will update it.\n")
      system("choco upgrade mingw -y")
    }
  }
}