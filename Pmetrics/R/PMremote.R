PMregister <- function(email, server_address = "http://localhost:5000") {
  if (length(grep("askpass", installed.packages()[, 1])) == 0) {
    install.packages("askpass", repos = "http://cran.cnr.Berkeley.edu", dependencies = T)
  }
  askpass.installed <- require(askpass)
  password <- askpass("Password: ")
  password_confirmation <- askpass("Password Confirmation: ")
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

PMlogin <- function(email, server_address = "http://localhost:5000") {
  if (length(grep("askpass", installed.packages()[, 1])) == 0) {
    install.packages("askpass", repos = "http://cran.cnr.Berkeley.edu", dependencies = T)
  }
  askpass.installed <- require(askpass)
  library(httr)
  library(purrr)
  password <- askpass("Password: ")
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
    cat("Authorized\n")
  } else {
    cat("Unauthorized\n")
  }
}

PMlogout <- function(server_address = "http://localhost:5000") {
  library(httr)
  library(purrr)
  api_url <- paste(server_address, "/api", sep = "")
  r <- DELETE(
    paste0(api_url, "/session")
  )
  handle_reset(server_address)
  if (r$status == 200) {
    cat("Logged out.\n")
  } else {
    cat("Error.\n")
    print(str(content(r)))
  }
}
# r <- login_user("juliandavid347@gmail.com", "prueba1234")

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
  if (r$status == 200) {
    .setupPMremote()
    #TODO: Check r, is it possible to the user is not logged in, return the right message
    PMremote$runs <<- c(PMremote$runs, content(r, "parsed")$id)
    nRuns <- length(PMremote$runs)
    sprintf("Remote run #%d started successfuly, You can access this run's id using: PMremote$runs(%d).\n", nRuns, nRuns) %>%
    cat()
    PMremote$runs[nRuns] %>%
    return()
  } else {
    cat("You need to be logged in to perform this operation.\n")
    return("Authentication error")
  }

}

.PMremote_check <- function(rid, server_address) {
  library(httr)
  api_url <- paste0(server_address, "/api")
  r <- GET(paste0(api_url, "/analysis/", rid, "/status"), add_headers(api_key = .getApiKey()))
  if (r$status == 200) {
    status <- content(r, "parsed")$status
    if (status == "finished") {
      cat("The run finished, fetching results from server...\n")
      .PMremote_outdata(rid, server_address)
    } else {
      status
    }
  } else {
    cat("You need to be logged in to perform this operation.\n")
    return("Authentication error")
  }

}

.PMremote_outdata <- function(rid, server_address) {
  if (length(grep("base64enc", installed.packages()[, 1])) == 0) {
    install.packages("base64enc", repos = "http://cran.cnr.Berkeley.edu", dependencies = T)
  }
  base64enc.installed <- require(base64enc)
  library(httr)
  wd <- getwd()
  setwd(tempdir())
  api_url <- paste0(server_address, "/api")
  r <- GET(paste0(api_url, "/analysis/", rid, "/outdata"), add_headers(api_key = .getApiKey()))
  if (r$status == 200) {
    cat("Results fetched, parsing...\n")
    #fileConn <- file("enc_outdata.txt")
    #writeLines(content(r, "parsed")$outdata, fileConn)
    #close(fileConn)
    #system("base64 --decode -i enc_outdata.txt -o outdata.Rdata")
    #Windows : https://stackoverflow.com/questions/16945780/decoding-base64-in-batch
    # Works!
    out <- file("outdata.Rdata", "wb")
    content(r, "parsed")$outdata %>%
    base64decode(output = out)
    close(out)

    load("outdata.Rdata", .GlobalEnv)
    setwd(wd)
    cat("Parsed! NPAGout object created.\n")
  } else {
    cat("You need to be logged in to perform this operation.\n")
  }
}
