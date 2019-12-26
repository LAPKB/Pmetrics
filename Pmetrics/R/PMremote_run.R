.PMremote_run <- function(model, data, server_address) {

  library(httr)
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
  return(content(r, "parsed")$id)
}

.PMremote_check <- function(rid, server_address) {
  library(httr)
  api_url <- paste(server_address, "/api", sep = "")
  r <- GET(paste(api_url, paste0("/analysis/", rid, "/status"), sep = ""))
  return(content(r, "parsed")$status)
}
