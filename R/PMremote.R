#' Creates a new user in the remote server
#'
#' This function receives an unused email and optionally the server adress.  
#'
#' @title Register a new user in the remote server
#' @param email User's email. 
#' @param server_address remote server address. If omitted,  Pmetrics will use the the default one. See \code{\link{setPMoptions}}.
#' @return A message that specifies if the user was registered or if some error happened.
#' @author Michael Neely
#' @export

PMregister <- function(email, server_address) {
  if (missing(server_address)) server_address <- getPMoptions("server_address")
  password <- askpass::askpass("Password: ")
  password_confirmation <- askpass::askpass("Password Confirmation: ")
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

#' Login a existent user onto the server.
#'
#' This function receives an email of a registered user and optionally the server adress.  
#' The function will prompt the user for the password.
#' The server session will last until the R session finishes or by using \code{PMlogout}.
#'
#' @title Login a user in the remote server
#' @param email User's email. 
#' @param server_address remote server address. If omitted,  Pmetrics will use the the default one. See \code{\link{setPMoptions}}.
#' @return A message that specifies if the user was successfuly logged in, or an explanation of the error.
#' @author Michael Neely
#' @export

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

#' Logout a logged in user onto the server.
#'
#' If no server_address is provided Pmetrics will use the the default one. See \code{\link{setPMoptions}}.
#'
#' @title Login a user in the remote server
#' @param server_address remote server address. If omitted,  Pmetrics will use the the default one. See \code{\link{setPMoptions}}.
#' @return A message that specifies if the user was successfuly logged out, or an explanation of the error.
#' @author Michael Neely
#' @export

PMlogout <- function(server_address) {
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
    print(str(httr::content(r)))
  }
}

.PMremote_run <- function(model, data, server_address, run, overwrite) {
  api_url <- paste(server_address, "/api", sep = "")
  model_txt <- readChar(model, file.info(model)$size)
  data_txt <- readChar(data, file.info(data)$size)
  r <- httr::POST(
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
    write(httr::content(r, "parsed")$id, fileConn)
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
    base64enc::base64decode(output = out)
    close(out)
    #declare variable to avoid R CMD Check flag
    NPAGout <- NULL
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

.PMremote_registerNewInstallation <- function() {
  if (Sys.getenv("env") != "Development") {
  current_version <- packageVersion("Pmetrics")
  api_url <-"http://50.18.143.118:4000/api/v0/count"
  r <- httr::POST(
      api_url,
      body = list(version = paste0("v", current_version)),
    encode = "json",
    httr::content_type_json()
    )
  httr::content(r)
  } else {
    cat("You are inside the development folder, skipping the registration of the current installation. ")
  }
}
