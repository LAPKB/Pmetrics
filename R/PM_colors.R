# Centralize Pmetrics colors for tables

make_color <- function(hex, alpha = 1) {
  if(alpha > 1) alpha <- 1
  if(alpha < 0) alpha <- 0
  if(!grepl("^#?[A-Fa-f0-9]{6}$", hex)){
    rgb_vals <- tryCatch(grDevices::col2rgb(hex), error = function(e) {-1})
    if (any(rgb_vals == -1)) {
      cli::cli_abort("Invalid color name or hex code provided.")
    }
    hex <- grDevices::rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
  }
  alpha_hex <- ifelse(nchar(hex) == 9, toupper(sprintf("%02X", round(alpha * 255))), "")
  paste0(gsub("^#", "#", hex), alpha_hex)
}

blue <- function(alpha = 1) {
  make_color(hex = "dodgerblue", alpha = alpha)
}

navy <- function(alpha = 1) {
  make_color(hex = "#003366", alpha = alpha)
}

green <- function(alpha = 1) {
  make_color(hex = "#009933", alpha = alpha)
}

red <- function(alpha = 1) {
  make_color(hex = "#FF9999", alpha = alpha)
}

yellow <- function(alpha = 1) {
  make_color(hex = "#FFCC33", alpha = alpha)
}

gold <- function(alpha = 1) {
  make_color(hex = "#FFCC00", alpha = alpha)
}

orange <- function(alpha = 1) {
  make_color(hex = "#FF9900", alpha = alpha)
}

purple <- function(alpha = 1) {
  make_color(hex = "#CC99FF", alpha = alpha)
}

gray <- function(alpha = 1) {
  make_color(hex = "grey60", alpha = alpha)
}

black <- function(alpha = 1) {
  make_color(hex = "#000000", alpha = alpha)
}

white <- function(alpha = 1) {
  make_color(hex = "#FFFFFF", alpha = alpha)
}

pink <- function(alpha = 1) {
  make_color(hex = "#FF99CC", alpha = alpha)
}

brown <- function(alpha = 1) {
  make_color(hex = "#996633", alpha = alpha)
}



