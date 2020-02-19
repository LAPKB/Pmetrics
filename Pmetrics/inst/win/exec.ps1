#Julian Otalvaro Nov 2019

function Install-Choco {
  Write-Output "Pmetrics is trying to install Chocolatey..."
  Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
  Write-Output "Checking installation..."
  If (Is-Choco-Installed) {
    Write-Output "Chocolatey installed successfully"
  }
  Else {
    Write-Output "Pmetrics cannot install Chocolatey automatically. Go to https://chocolatey.org/install/ to install it and re-run this script."
    exit
  }
}


# function Elevate-Rights {
#     Start-Process powershell.exe "-File",('"{0}"' -f $MyInvocation.MyCommand.Path) -Verb RunAs
#     exit
# }

# function Is-Elevated {
#     If (([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)){
#         return $True
#     } else {
#         return $False
#     }
# }

function Is-Choco-Installed {
  return Get-Command choco.exe -ErrorAction SilentlyContinue
}

function Install-Gfortran {
  choco install mingw -y
}


function Is-Gfortran-Installed {
  return Get-Command gfortran.exe -ErrorAction SilentlyContinue
}


# function Main {
  
# }

If (-NOT ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
  Write-Output "Not elevated"
  # Relaunch as an elevated process:
  Start-Process powershell.exe "-File", ('"{0}"' -f $MyInvocation.MyCommand.Path) -Verb RunAs
  exit
}
else {
    
  Write-Output "elevated"
  if (Is-Gfortran-Installed) {
    Write-Output "Gfortran found."
    Pause
    return 1
  }
  else {
    If (Is-Choco-Installed) {
      Write-Output "Chocolatey installed"
      # Uninstall-Choco
    }
    Else {
      Write-Output "Chocolatey is not installed"
      # if (-NOT(Is-Elevated)) {
      #     Elevate-Rights
      # }
      Install-Choco
    }
    Install-Gfortran
    
    if (Is-Gfortran-Installed) {
      Write-Output "Pmetrics installed gfortran successfully."
      Write-Output "You can now close this window and continue Pmetrics setup in R."
      # Rscript -e "library(Pmetrics);PMbuild()"
      Pause
      return 1
    }
    else {
      Write-Output "Pmetrics could not automatically install gfortran. Please install it manually to continue.  For help, visit http://www.lapk.org/Pmetrics_install.php."
      Pause
      return 0
    } 
  }
}


# Main
