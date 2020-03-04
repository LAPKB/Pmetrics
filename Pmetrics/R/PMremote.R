PMregister <- function(email, server_address) {
  if (missing(server_address)) server_address <- getPMoptions("server_address")
  password <- askpass::askpass("Password: ")
  password_confirmation <-askpass::askpass("Password Confirmation: ")
  api_url <- paste0(server_address, "/api")
  r <- httr::POST(
      paste0(api_url, "/user/new"),
      body = list(
        email = email,
        password = password,
        password_confirmation = password_confirmation),
    encode = "json",
    add_headers(api_key = .getApiKey())
    )
  httr::content(r)
}

PMlogin <- function(email, server_address) {
  if (missing(email)) email <- readline("please type your email: ")
  if (missing(server_address)) server_address <- getPMoptions("server_address")
  password <- askpass::askpass("Password: ")
  api_url <- paste0(server_address, "/api")
  r <- httr::POST(
      paste0(api_url, "/session/new"),
      body = list(
        email = email,
        password = password),
    encode = "json",
    add_headers(api_key = .getApiKey())
    )
  if (r$status == 200) {
    cat("Authorized\n")
    return(T)
  } else {
    cat("Unauthorized\n")
    return(F)
  }
}

PMlogout <- function() {
  if (missing(server_address)) server_address <- getPMoptions("server_address")
  api_url <- paste(server_address, "/api", sep = "")
  r <- httr::DELETE(
    paste0(api_url, "/session")
  )
  httr::handle_reset(server_address)
  if (r$status == 200) {
    cat("Logged out.\n")
  } else {
    cat("Error.\n")
    print(str(content(r)))
  }
}
# r <- login_user("juliandavid347@gmail.com", "prueba1234")

.PMremote_run <- function(model, data, server_address, run) {
  api_url <- paste(server_address, "/api", sep = "")
  model_txt <- readChar(model, file.info(model)$size)
  data_txt <- readChar(data, file.info(data)$size)
  r <- htrr::POST(
      paste(api_url, "/analysis/new", sep = ""),
      body = list(
        model_txt = model_txt,
        data_txt = data_txt,
        name = "prueba"),
    encode = "json",
    add_headers(api_key = .getApiKey())
    )
  if (r$status == 200) {
    #The same code in PMrun
    currwd <- getwd()
    if (is.null(run)) {
      olddir <- list.dirs(recursive = F)
      olddir <- olddir[grep("^\\./[[:digit:]]+", olddir)]
      olddir <- sub("^\\./", "", olddir)
      if (length(olddir) > 0) {
        newdir <- as.character(max(as.numeric(olddir)) + 1)
      } else { newdir <- "1" }
    } else {
      if (!is.numeric(run)) { endNicely("'run' must be numeric.\n") } else { newdir <- as.character(run) }
    }
    if (file.exists(newdir)) {
      if (overwrite) { unlink(newdir, recursive = T) } else { endNicely(paste("\n", newdir, " exists already.  Set overwrite=T to overwrite.\n")) }
    }
    dir.create(newdir)
    dir.create(paste(newdir, "inputs", sep = "/"))
    dir.create(paste(newdir, "outputs", sep = "/"))
    inputFiles <- c(model, data) #list.files(getwd(), "txt|csv")
    file.copy(inputFiles, paste(newdir, "inputs", sep = "/"))
    file.remove(inputFiles)
    setwd(paste(newdir, "inputs", sep = "/"))
    #END same code PMrun
    sprintf("Remote run #%s started successfuly, You can access this run's id using: PMload(id).\n Id can be the full id string or the run number.\n", newdir, newdir) %>%
    cat()
    fileConn <- file("id.txt")
    write(content(r, "parsed")$id, fileConn)
    close(fileConn)
    setwd(currwd)
    httr::content(r, "parsed")$id %>%
    return()
  } else {
    cat("You need to be logged in to perform this operation.\n")

    if (PMlogin()) {
      .PMremote_run(model, data, server_address, run)
    } else {
      cat("Authentication error\n")
    }

  }

}

.PMremote_check <- function(rid, server_address) {
  if (missing(server_address)) server_address <- getPMoptions("server_address")
  api_url <- paste0(server_address, "/api")
  request_url <- paste0(api_url, "/analysis/", rid, "/status")
  r <- httr::GET(request_url, add_headers(api_key = .getApiKey()))
  if (r$status == 200) {
    status <- httr::content(r, "parsed")$status
    return(status)
  } else {
    cat("You need to be logged in to perform this operation.\n")
    stop("Authentication error")
  }

}

.PMremote_outdata <- function(run, server_address) {
  #checkRequiredPackages("base64enc")
  if (missing(server_address)) server_address <- getPMoptions("server_address")

  rid <- .getRemoteId(run)
  wd <- getwd()
  setwd(paste(run, "outputs", sep = "/"))
  api_url <- paste0(server_address, "/api")
  r <- httr::GET(paste0(api_url, "/analysis/", rid, "/outdata"), add_headers(api_key = .getApiKey()))
  if (r$status == 200) {
    cat("Results fetched, parsing...\n")
    #fileConn <- file("enc_outdata.txt")
    #writeLines(content(r, "parsed")$outdata, fileConn)
    #close(fileConn)
    #system("base64 --decode -i enc_outdata.txt -o NPAGout.Rdata")
    #Windows : https://stackoverflow.com/questions/16945780/decoding-base64-in-batch
    # Works!
    if (file.exists("NPAGout.Rdata")) { system("rm NPAGout.Rdata") }
    out <- file("NPAGout.Rdata", "wb")
    httr::content(r, "parsed")$outdata %>%
    base64encode::base64decode(output = out)
    httr::close(out)
    load("NPAGout.Rdata", .GlobalEnv)
    PMreport(getwd(), rdata = NPAGout) #TODO: check if this works with multiple PMload inputs
    OS <- getOS() #1 Mac, 2 Windows, 3 Linux
    command <- c("open", "start", "xdg-open")[OS]
    system(paste(command, "NPAGreport.html"))
    cat("Parsed! NPAGout object created.\n")
  } else {
    cat("You need to be logged in to perform this operation.\nUse PMlogin() and try again.")
  }
  setwd(wd)
}
