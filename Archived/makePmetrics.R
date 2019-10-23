
makePmetrics <- function(fortranChange=F,build=T,pdf=F,check=F,ITver,NPver,SIMver){
  
  require(devtools)
  OS <- switch(.Platform$OS.type,unix=1,windows=2)
  if(OS==1){
    #do this only if fortran files have changed
    if(fortranChange==T){
      setwd("~/LAPK/PmetricsSource/Source")
      
      system("rm ./catenated/*")
      system("rm ./compiled/Mac/32/*")
      system("rm ./compiled/Mac/64/*")
      system(paste("cat ./IT2B/prep/*.* > ./catenated/ITprep_",ITver,".f",sep=""))
      system(paste("cat ./IT2B/error/*.* > ./catenated/ITerr_",ITver,".f",sep=""))
      system(paste("cat ./IT2B/engine/*.* > ./catenated/ITeng_",ITver,".f",sep=""))
      system(paste("gfortran -m32 -O3 -o ./compiled/Mac/32/ITprepGF_",ITver,".o -c ./catenated/ITprep_",ITver,".f",sep=""))
      system(paste("gfortran -m32 -O3 -o ./compiled/Mac/32/ITerrGF_",ITver,".o -c ./catenated/ITerr_",ITver,".f",sep=""))
      system(paste("gfortran -m32 -O3 -o ./compiled/Mac/32/ITengGF_",ITver,".o -c ./catenated/ITeng_",ITver,".f",sep=""))
      system(paste("gfortran -m64 -O3 -o ./compiled/Mac/64/ITprepGF_",ITver,".o -c ./catenated/ITprep_",ITver,".f",sep=""))
      system(paste("gfortran -m64 -O3 -o ./compiled/Mac/64/ITerrGF_",ITver,".o -c ./catenated/ITerr_",ITver,".f",sep=""))
      system(paste("gfortran -m64 -O3 -o ./compiled/Mac/64/ITengGF_",ITver,".o -c ./catenated/ITeng_",ITver,".f",sep=""))
      
      system(paste("cat ./NPAG/prep/*.* > ./catenated/NPprep_",NPver,".f",sep=""))
      system(paste("cat ./NPAG/engine/*.* > ./catenated/NPeng_",NPver,".f",sep=""))
      system(paste("gfortran -m32 -O3 -o ./compiled/Mac/32/NPprepGF_",NPver,".o -c ./catenated/NPprep_",NPver,".f",sep=""))
      system(paste("gfortran -m32 -O3 -o ./compiled/Mac/32/NPengGF_",NPver,".o -c ./catenated/NPeng_",NPver,".f",sep=""))
      system(paste("gfortran -m64 -O3 -o ./compiled/Mac/64/NPprepGF_",NPver,".o -c ./catenated/NPprep_",NPver,".f",sep=""))
      system(paste("gfortran -m64 -O3 -o ./compiled/Mac/64/NPengGF_",NPver,".o -c ./catenated/NPeng_",NPver,".f",sep=""))
      
      system(paste("gfortran -m32 -O3 -o ./compiled/Mac/32/SIMengGF_",SIMver,".o -c ./Simulator/engine/MONT",SIMver,".for",sep=""))
      system(paste("gfortran -m64 -O3 -o ./compiled/Mac/64/SIMengGF_",SIMver,".o -c ./Simulator/engine/MONT",SIMver,".for",sep=""))
      

    }
    #do this for all changes
    setwd("~/LAPK/PmetricsSource/Source")
    
    system("rm ../Pmetrics/inst/IT2B/32/prep/*")
    system("rm ../Pmetrics/inst/IT2B/32/engine/*")
    system("rm ../Pmetrics/inst/IT2B/32/error/*")
    system("rm ../Pmetrics/inst/IT2B/64/prep/*")
    system("rm ../Pmetrics/inst/IT2B/64/engine/*")
    system("rm ../Pmetrics/inst/IT2B/64/error/*")
    system("rm ../Pmetrics/inst/NPAG/32/prep/*")
    system("rm ../Pmetrics/inst/NPAG/32/engine/*")
    system("rm ../Pmetrics/inst/NPAG/64/prep/*")
    system("rm ../Pmetrics/inst/NPAG/64/engine/*")
    system("rm ../Pmetrics/inst/Sim/32/engine/*")
    system("rm ../Pmetrics/inst/Sim/64/engine/*")
    
    
    file.copy(from=paste("./compiled/Mac/32/ITprepGF_",ITver,".o",sep=""),to="../Pmetrics/inst/IT2B/32/prep/")
    file.copy(from=paste("./compiled/Mac/32/ITerrGF_",ITver,".o",sep=""),to="../Pmetrics/inst/IT2B/32/error/")
    file.copy(from=paste("./compiled/Mac/32/ITengGF_",ITver,".o",sep=""),to="../Pmetrics/inst/IT2B/32/engine/")
    file.copy(from=paste("./compiled/Mac//32/NPprepGF_",NPver,".o",sep=""),to="../Pmetrics/inst/NPAG/32/prep/")
    file.copy(from=paste("./compiled/Mac/32/NPengGF_",NPver,".o",sep=""),to="../Pmetrics/inst/NPAG/32/engine/")
    file.copy(from=paste("./compiled/Mac/32/SIMengGF_",SIMver,".o",sep=""),to="../Pmetrics/inst/Sim/32/engine/")
    
    file.copy(from=paste("./compiled/Mac/64/ITprepGF_",ITver,".o",sep=""),to="../Pmetrics/inst/IT2B/64/prep/")
    file.copy(from=paste("./compiled/Mac/64/ITerrGF_",ITver,".o",sep=""),to="../Pmetrics/inst/IT2B/64/error/")
    file.copy(from=paste("./compiled/Mac/64/ITengGF_",ITver,".o",sep=""),to="../Pmetrics/inst/IT2B/64/engine/")
    file.copy(from=paste("./compiled/Mac//64/NPprepGF_",NPver,".o",sep=""),to="../Pmetrics/inst/NPAG/64/prep/")
    file.copy(from=paste("./compiled/Mac/64/NPengGF_",NPver,".o",sep=""),to="../Pmetrics/inst/NPAG/64/engine/")
    file.copy(from=paste("./compiled/Mac/64/SIMengGF_",SIMver,".o",sep=""),to="../Pmetrics/inst/Sim/64/engine/")
    
    setwd("~/LAPK/PmetricsSource")
    document("Pmetrics")
    if(pdf){
      file.remove("./Pmetrics/inst/doc/Pmetrics-manual.pdf")
      system("R CMD Rd2pdf --output=./Pmetrics/inst/doc/Pmetrics-manual.pdf --no-preview ./Pmetrics/man")}
    if(check) check("Pmetrics")
    if(build) build("Pmetrics",binary=T)
    install("Pmetrics")
    
  } else {  #this is for Windows
    #do this only if fortran files have changed
    if(fortranChange==T){
      setwd("Z:/LAPK/PmetricsSource/Source")
      
      shell("erase /Q .\\compiled\\Windows\\32\\*")
      shell("erase /Q .\\compiled\\Windows\\64\\*")
      
      shell(paste("gfortran -m32 -O3 -o .\\compiled\\Windows\\32\\ITprepGF_",ITver,".o -c .\\catenated\\ITprep_",ITver,".f",sep=""))
      shell(paste("gfortran -m32 -O3 -o .\\compiled\\Windows\\32\\ITerrGF_",ITver,".o -c .\\catenated\\ITerr_",ITver,".f",sep=""))
      shell(paste("gfortran -m32 -O3 -o .\\compiled\\Windows\\32\\ITengGF_",ITver,".o -c .\\catenated\\ITeng_",ITver,".f",sep=""))
      shell(paste("gfortran -m64 -O3 -o .\\compiled\\Windows\\64\\ITprepGF_",ITver,".o -c .\\catenated\\ITprep_",ITver,".f",sep=""))
      shell(paste("gfortran -m64 -O3 -o .\\compiled\\Windows\\64\\ITerrGF_",ITver,".o -c .\\catenated\\ITerr_",ITver,".f",sep=""))
      shell(paste("gfortran -m64 -O3 -o .\\compiled\\Windows\\64\\ITengGF_",ITver,".o -c .\\catenated\\ITeng_",ITver,".f",sep=""))
      
      shell(paste("gfortran -m32 -O3 -o .\\compiled\\Windows\\32\\NPprepGF_",NPver,".o -c .\\catenated\\NPprep_",NPver,".f",sep=""))
      shell(paste("gfortran -m32 -O3 -o .\\compiled\\Windows\\32\\NPengGF_",NPver,".o -c .\\catenated\\NPeng_",NPver,".f",sep=""))
      shell(paste("gfortran -m64 -O3 -o .\\compiled\\Windows\\64\\NPprepGF_",NPver,".o -c .\\catenated\\NPprep_",NPver,".f",sep=""))
      shell(paste("gfortran -m64 -O3 -o .\\compiled\\Windows\\64\\NPengGF_",NPver,".o -c .\\catenated\\NPeng_",NPver,".f",sep=""))
      
      shell(paste("gfortran -m32 -O3 -o .\\compiled\\Windows\\32\\SIMengGF_",SIMver,".o -c .\\Simulator\\engine\\MONT",SIMver,".for",sep=""))
      shell(paste("gfortran -m64 -O3 -o .\\compiled\\Windows\\64\\SIMengGF_",SIMver,".o -c .\\Simulator\\engine\\MONT",SIMver,".for",sep=""))
      
 
      
    }
    #do this for all changes
    setwd("Z:/LAPK/PmetricsSource/Source")
    
    shell("erase /Q  ..\\Pmetrics\\inst\\IT2B\\32\\prep\\*")
    shell("erase /Q  ..\\Pmetrics\\inst\\IT2B\\32\\engine\\*")
    shell("erase /Q  ..\\Pmetrics\\inst\\IT2B\\32\\error\\*")
    shell("erase /Q  ..\\Pmetrics\\inst\\IT2B\\64\\prep\\*")
    shell("erase /Q  ..\\Pmetrics\\inst\\IT2B\\64\\engine\\*")
    shell("erase /Q  ..\\Pmetrics\\inst\\IT2B\\64\\error\\*")
    shell("erase /Q ..\\Pmetrics\\inst\\NPAG\\32\\prep\\*")
    shell("erase /Q ..\\Pmetrics\\inst\\NPAG\\32\\engine\\*")
    shell("erase /Q ..\\Pmetrics\\inst\\NPAG\\64\\prep\\*")
    shell("erase /Q ..\\Pmetrics\\inst\\NPAG\\64\\engine\\*")
    shell("erase /Q ..\\Pmetrics\\inst\\Sim\\32\\engine\\*")
    shell("erase /Q ..\\Pmetrics\\inst\\Sim\\64\\engine\\*")
    
    file.copy(from=paste(".\\compiled\\Windows\\32\\ITprepGF_",ITver,".o",sep=""),to="..\\Pmetrics\\inst\\IT2B\\32\\prep")
    file.copy(from=paste(".\\compiled\\Windows\\32\\ITerrGF_",ITver,".o",sep=""),to="..\\Pmetrics\\inst\\IT2B\\32\\error")
    file.copy(from=paste(".\\compiled\\Windows\\32\\ITengGF_",ITver,".o",sep=""),to="..\\Pmetrics\\inst\\IT2B\\32\\engine")
    file.copy(from=paste(".\\compiled\\Windows\\32\\NPprepGF_",NPver,".o",sep=""),to="..\\Pmetrics\\inst\\NPAG\\32\\prep")
    file.copy(from=paste(".\\compiled\\Windows\\32\\NPengGF_",NPver,".o",sep=""),to="..\\Pmetrics\\inst\\NPAG\\32\\engine")
    file.copy(from=paste(".\\compiled\\Windows\\32\\SIMengGF_",SIMver,".o",sep=""),to="..\\Pmetrics\\inst\\Sim\\32\\engine")
    file.copy(from=paste(".\\compiled\\Windows\\64\\ITprepGF_",ITver,".o",sep=""),to="..\\Pmetrics\\inst\\IT2B\\64\\prep")
    file.copy(from=paste(".\\compiled\\Windows\\64\\ITerrGF_",ITver,".o",sep=""),to="..\\Pmetrics\\inst\\IT2B\\64\\error")
    file.copy(from=paste(".\\compiled\\Windows\\64\\ITengGF_",ITver,".o",sep=""),to="..\\Pmetrics\\inst\\IT2B\\64\\engine")
    file.copy(from=paste(".\\compiled\\Windows\\64\\NPprepGF_",NPver,".o",sep=""),to="..\\Pmetrics\\inst\\NPAG\\64\\prep")
    file.copy(from=paste(".\\compiled\\Windows\\64\\NPengGF_",NPver,".o",sep=""),to="..\\Pmetrics\\inst\\NPAG\\64\\engine")
    file.copy(from=paste(".\\compiled\\Windows\\64\\SIMengGF_",SIMver,".o",sep=""),to="..\\Pmetrics\\inst\\Sim\\64\\engine")
    
    setwd("Z:/LAPK/PmetricsSource")
    if(build) build("Pmetrics",binary=T)
  }
  
}

makePmetrics(fortranChange=F,build=T,pdf=F,check=F,ITver=22,NPver=22,SIMver="106")

