register_user <- function(email, password, server_address = "http://localhost:5000") {
  library(httr)
  library(purrr)
  api_url <- paste0(server_address, "/api")
  r <- POST(
      paste(api_url, "/user/new", sep = ""),
      body = list(
        email = email,
        password = password),
    encode = "json",
    add_headers(api_key = "qoc+7YRUCCK7BmOJrkzNRY6gKzXIGzPPR6IoefaZpOtPkEsKwO48vkCPM18G97Y9")
    )
}