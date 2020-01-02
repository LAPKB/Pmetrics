register_user <- function(email, password, password_confirmation, server_address = "http://localhost:5000") {
  library(httr)
  library(purrr)
  api_url <- paste0(server_address, "/api")
  r <- POST(
      paste0(api_url, "/user/new"),
      body = list(
        email = email,
        password = password,
        password_confirmation = password_confirmation),
    encode = "json",
    add_headers(api_key = .getApiKey())
    )
  content(r)
}

login_user <- function(email, password, server_address = "http://localhost:5000") {
  library(httr)
  library(purrr)
  api_url <- paste0(server_address, "/api")
  r <- POST(
      paste0(api_url, "/session/new"),
      body = list(
        email = email,
        password = password),
    encode = "json",
    add_headers(api_key = .getApiKey())
    )
  if (r$status == 200) {
    print("authorized")
    .setupPMremote()
    PMremote$session_info <<- cookies(r)$value
  } else {
    print("unauthorized")
  }
  r
}
r <- login_user("juliandavid347@gmail.com", "prueba1234")

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
    encode = "json",
    add_headers(api_key = .getApiKey())
    )
  .setupPMremote()
  PMremote$runs <<- c(PMremote$runs, content(r, "parsed")$id)
  nRuns <- length(PMremote$runs)
  sprintf("Remote run #%d started successfuly, You can access this run's id using: PMremote$runs(%d).\n", nRuns, nRuns) %>%
  cat()
  PMremote$runs[nRuns] %>%
  return()
}

.PMremote_check <- function(rid, server_address) {
  library(httr)
  api_url <- paste0(server_address, "/api")
  r <- GET(paste0(api_url, "/analysis/", rid, "/status"), add_headers(api_key = .getApiKey()))
  return(content(r, "parsed")$status)
}

PMremote_outdata <- function(rid, server_address) {
  library(httr)
  wd <- getwd()
  setwd(tempdir())
  api_url <- paste0(server_address, "/api")
  r <- GET(paste0(api_url, "/analysis/", rid, "/outdata"), add_headers(api_key = .getApiKey()))
  fileConn <- file("enc_outdata.txt")
  writeLines(content(r, "parsed")$outdata, fileConn)
  close(fileConn)
  system("base64 --decode -i enc_outdata.txt -o outdata.Rdata")
  load("outdata.Rdata", .GlobalEnv)
  setwd(wd)
}
