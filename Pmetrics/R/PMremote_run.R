.PMremote_run <- function(model, data, server_address) {

  library(httr)
  library(purrr)
  api_url <- paste(server_address, "/api", sep = "")
  model_txt <- readChar(model, file.info(model)$size)
  data_txt <- readChar(data, file.info(data)$size)
  r <- POST(
      paste(api_url, "/analysis/new", sep = ""),
      body = list(
        model_txt = model_txt,
        data_txt = data_txt,
        name = "prueba"),
    encode = "json"
    )
  if (!exists("remoteRuns")) { remoteRuns <<- c() }
  remoteRuns <<- c(remoteRuns, content(r, "parsed")$id)
  nRuns <- length(remoteRuns)
  sprintf("Remote run #%d started successfuly, You can access this run's id using: remoteRuns(%d).\n", nRuns, nRuns) %>%
  cat()
  remoteRuns[nRuns] %>%
  return()
}

.PMremote_check <- function(rid, server_address) {
  library(httr)
  api_url <- paste0(server_address, "/api")
  r <- GET(paste0(api_url, "/analysis/", rid, "/status"))
  return(content(r, "parsed")$status)
}

PMremote_outdata <- function(rid, server_address) {
  library(httr)
  wd <- getwd()
  setwd(tempdir())
  api_url <- paste0(server_address, "/api")
  r <- GET(paste0(api_url, "/analysis/", rid, "/outdata"))
  fileConn <- file("enc_outdata.txt")
  writeLines(content(r, "parsed")$outdata, fileConn)
  close(fileConn)
  system("base64 --decode -i enc_outdata.txt -o outdata.Rdata")
  load("outdata.Rdata", .GlobalEnv)
  setwd(wd)
}
