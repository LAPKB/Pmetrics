choose.folder <- function(prompt="Choose folder:") {
  system(paste("osascript -e 'tell app \"Rstudio\" to POSIX path of (choose folder with prompt \"",prompt,"\")' > /tmp/R_folder",sep=""),
         intern = FALSE, ignore.stderr = TRUE)
  p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  return(ifelse(length(p), p, NA))
}



setPM <- function(newfolder){
  if(missing(newfolder)) newfolder <- choose.folder(prompt="Set the current Pmetrics project folder")
  if(!is.na(newfolder)) assign("PMproject",newfolder,envir=PMenv)
  cat(paste("Pmetrics current project folder now set to:\n",newfolder,"\n",sep=""))
}


getPM <- function(folder){
  PM <- tryCatch(get("PMproject",envir=PMenv), error=function(e) NA)
  if(is.na(PM)) PM <- setPM()
  if(!missing(folder)){
    PM2 <- paste(PM,folder,sep="")
    if(file.exists(PM2)){return(PM2)}else{return(NA)}
  }
  return(PM)
}
