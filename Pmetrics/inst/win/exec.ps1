#Julian Otalvaro Nov 2019

function Install-Choco {
  Write-Output "Trying to install Chocolatey"
  Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
  Write-Output "Checking installation"
  If (Is-Choco-Installed) {
    Write-Output "Chocolatey Installed successfully"
  }
  Else {
    Write-Output "Chocolatey Cannot be installed automatically, go to https://chocolatey.org/install/ install it and re-run this script"
    Pause
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


function Main {
    
  Pause
}
# Main
If (-NOT ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
  Write-Output "Not elevated"
  # Relaunch as an elevated process:
  Start-Process powershell.exe "-File", ('"{0}"' -f $MyInvocation.MyCommand.Path) -Verb RunAs
  exit
}
else {
    
  Write-Output "elevated"
  if (Is-Gfortran-Installed) {
    Write-Output "Gfortran found, nothing to do..."
  }
  else {
    If (Is-Choco-Installed) {
      Write-Output "Choco installed"
      # Uninstall-Choco
    }
    Else {
      Write-Output "Choco is not installed"
      # if (-NOT(Is-Elevated)) {
      #     Elevate-Rights
      # }
      Install-Choco
    }
    Install-Gfortran
    
    if (Is-Gfortran-Installed) {
      Write-Output "Gfortran Installed successfully"
    }
    else {
      Write-Output "There were and error installing gfortran, please install it manually to continue"
    } 
  }
}
Pause
