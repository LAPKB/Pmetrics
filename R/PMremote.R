#' @export
PM_remote <- R6Class("PM_remote", 
  public = list(
    run_id = NULL,
    status = NULL,
    token = NULL,
    run_folder = NULL,
    server_address = NULL,

    #' @description
    #' Login a existent user onto the server.
    #'
    #' This function receives an email of a registered user and optionally the server adress.  
    #' The function will prompt the user for the password.
    #' The server session will last until the R session finishes or by using PM_remote$logout().
    #'
    #' @param email User's email. 
    #' @param server_address remote server address. If omitted,  Pmetrics will use the the default one. See \code{\link{setPMoptions}}.
    #' @return A message that specifies if the user was successfuly logged in, or an explanation of the error.
    initialize = function(email, server_address){
      if (missing(email)) email <- readline("please type your email: ")
      if (missing(server_address)) server_address <- getPMoptions("server_address")
      password <- askpass::askpass("Password: ")
      api_url <- paste0(server_address, "/api")
      r <- httr::POST(
            paste0(api_url, "/users/log_in"),
            body = list(
              user = list(
                email = email,
                password = password)),
            encode = "json")
      if (r$status == 200) {
        cat("Authorized\n")
        self$token <- httr::content(r)$data$token
        self$server_address <- server_address
        self$status <- "Logged_in"
      } else {
        cat("Unauthorized\n")
      }
    },
    get_updates = function(server_address){
      if (missing(server_address)) server_address <- getPMoptions("server_address")
      api_url <- paste0(server_address, "/api")
      request_url <- paste0(api_url, "/runs/", self$run_id, "/status")
      r <- httr::GET(request_url, add_headers(Authorization = sprintf("Bearer %s",remote$token)))
      if (r$status == 200) {
        self$status <- httr::content(r, "parsed")$status
        return(self$status)
      } else {
        stop(sprintf("error code: %i", r$status))
      }

    },
    logout = function(){

    }
  )
)

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

.remote_run <- function(model, data, server_address, remote, run=1, overwrite=T) {
  api_url <- paste(remote$server_address, "/api", sep = "")

  r <- httr::POST(
        paste(api_url, "/runs/new", sep = ""),
        body = list(
          run = list(
            model_txt = model,
            data_txt = data)),
        encode = "json",
        add_headers(Authorization = sprintf("Bearer %s",remote$token))
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
    # inputFiles <- c(model, data) #list.files(getwd(), "txt|csv")
    # file.copy(inputFiles, paste(newdir, "inputs", sep = "/"))
    # file.remove(inputFiles)
    setwd(paste(newdir, "inputs", sep = "/"))
    #END same code PMrun
    sprintf("Remote run #%s started successfuly.\n", newdir) %>%
    cat()
    fileConn <- file("id.txt")
    write(httr::content(r, "parsed")$id, fileConn)
    close(fileConn)
    setwd(currwd)
    remote$run_folder <- paste(currwd,newdir,sep = "/")
    remote$run_id <- httr::content(r, "parsed")$id 
    remote$status <- "Queued"
    return(remote)
  } else {
    cat("You need to be logged in to perform this operation.\n")
  }

}

.PMremote_check <- function(rid, server_address) {
  

}

.PMremote_outdata <- function(remote, server_address) {
  #checkRequiredPackages("base64enc")
  if (missing(server_address)) server_address <- getPMoptions("server_address")

  rid <- remote$run_id
  wd <- getwd()
  setwd(paste(remote$run_folder, "outputs", sep = "/"))
  api_url <- paste0(server_address, "/api")
  r <- httr::GET(paste0(api_url, "/runs/", rid, "/outdata"), httr::add_headers(Authorization = sprintf("Bearer %s",remote$token)))
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
    load("NPAGout.Rdata")
    PMreport(getwd(), rdata = NPAGout) #TODO: check if this works with multiple PMload inputs
    OS <- getOS() #1 Mac, 2 Windows, 3 Linux
    command <- c("open", "start", "xdg-open")[OS]
    system(paste(command, "NPAGreport.html"))
    cat("Parsed! NPAGout object created.\n")
  } else {
    cat("You need to be logged in to perform this operation.\nUse PMlogin() and try again.")
  }
  setwd(wd)
  return(NPAGout)
}

.PMremote_registerNewInstallation = function() {
  if (Sys.getenv("env") == "Developmet") {
    cat("You are inside the development folder, skipping the registration of the current installation.")
    return()
  }
  
  current_version <- packageVersion("Pmetrics")
  api_url <-"https://pmcount.siel.com.co/api/v0/count"
  
  safe_POST = purrr::safely(httr::POST)
  r <- safe_POST(
    api_url,
    body = list(version = paste0("v", current_version)),
    encode = "json",
    httr::content_type_json()
  )
  
  if (!is.null(r$error)) {
    cat("Pmetrics was unable to register your installation.")
  }
}
