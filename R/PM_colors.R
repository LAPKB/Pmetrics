# Centralize Pmetrics colors for tables

make_color <- function(hex, alpha = 1) {
  if(alpha > 1) alpha <- 1
  if(alpha < 0) alpha <- 0
  stopifnot(grepl("^#?[A-Fa-f0-9]{6}$", hex))
  alpha_hex <- toupper(sprintf("%02X", round(alpha * 255)))
  paste0(gsub("^#", "#", hex), alpha_hex)
}

blue <- function(alpha = 1) {
  make_color(hex = "#99CCFF", alpha = alpha)
}

navy <- function(alpha = 1) {
  make_color(hex = "#003366", alpha = alpha)
}

green <- function(alpha = 1) {
  make_color(hex = "#99FF99", alpha = alpha)
}

red <- function(alpha = 1) {
  make_color(hex = "#FF9999", alpha = alpha)
}

yellow <- function(alpha = 1) {
  make_color(hex = "#FFFF99", alpha = alpha)
}

orange <- function(alpha = 1) {
  make_color(hex = "#FFCC99", alpha = alpha)
}

purple <- function(alpha = 1) {
  make_color(hex = "#CC99FF", alpha = alpha)
}

gray <- function(alpha = 1) {
  make_color(hex = "#CCCCCC", alpha = alpha)
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



