#Returns environment variables used throughout Pmetrics

getPMpath <- function() {
  require(purrr)
  #return Pmetrics installation folder 
  unlist(
    .libPaths() %>%
    map(~paste(., "Pmetrics", sep = "/")) %>%
    map(~.[file.exists(.)])
    )
}

getBits <- function() {
  #figure out 32 or 64 bit
  if (length(grep("64-bit", utils::sessionInfo())) > 0) { return(64) } else { return(32) }
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

