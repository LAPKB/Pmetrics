#Returns environment variables used throughout Pmetrics

getPMpath <- function() {
  #checkRequiredPackages("purrr")
  #return Pmetrics installation folder 
  paste(.libPaths(), "Pmetrics", sep = "/") %>%
    keep(file.exists) %>%
    pluck(1) #ensure only one path returned (should be unnecessary)
}

getBits <- function() {
  # figure out 32 or 64 bit
  Rver <- numeric_version(paste(R.version$major, R.version$minor,sep = "."))
  if(Rver < numeric_version("4.4.0")){
    if (length(grep("64-bit", utils::sessionInfo())) > 0) {
      return(64)
    } else {
      return(32)
    }
  } else {
    if (length(grep("32-bit", utils::sessionInfo())) > 0) {
      return(32)
    } else {
      return(64)
    }
  }
}

getFixedColNames <- function() {
  #set current names of fixed columns in data file

  c("id", "evid", "time", "dur", "dose", "addl",
    "ii", "input", "out", "outeq", "c0", "c1", "c2", "c3")
}

getFixedColNum <- function() {
  #set current number of fixed columns in data file
  length(getFixedColNames())
}

.getApiKey <- function() {
  return("qoc+7YRUCCK7BmOJrkzNRY6gKzXIGzPPR6IoefaZpOtPkEsKwO48vkCPM18G97Y9")
}


