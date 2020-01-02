register_user <- function(email, password, password_confirmation, server_address = "http://localhost:5000") {
  library(httr)
  library(purrr)
  api_url <- paste0(server_address, "/api")
  r <- POST(
      paste(api_url, "/user/new", sep = ""),
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
      paste(api_url, "/session/new", sep = ""),
      body = list(
        email = email,
        password = password),
    encode = "json",
    add_headers(api_key = .getApiKey())
    )
  content(r)
}