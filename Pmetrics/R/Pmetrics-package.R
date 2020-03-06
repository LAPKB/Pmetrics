#' Parametric and non-parametric modeling and simulation of pharmacokinetic-pharmacodynamic systems.
#'
#' This package contains functions to run and analyze the output from all three components
#' of the Pmetrics software suite for population pharmacometric data analysis:
#' 1) IT2B (Iterative Two-Stage Bayesian) for parametric models; 2) NPAG (Non-parametric
#' Adaptive Grid) for non-parametric models; 3) Simulator for semi-parametric Monte-Carlo
#' simulations.
#'
#' @name Pmetrics-package
#' @aliases Pmetrics
#' @docType package
#' @author Michael Neely, MD
#' \url{http://www.lapk.org}
#' @keywords package
#'
#' @importFrom dplyr select arrange as.tbl_cube filter mutate
#' @importFrom foreach %dopar%
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_polygon 
#' scale_x_log10 scale_x_continuous scale_y_log10 scale_y_continuous xlab ylab
#' @importFrom purrr map map_chr keep pluck %>% 
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider unnest
#' @importFrom mclust MClust
NULL

