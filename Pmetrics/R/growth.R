#' Centers for Disease Control Pediatric and Adolescent Growth Data Table
#'
#' Coefficients to calculate sex-specific percentiles of length, weight and head cicumference data 
#' in children from 0 to 18 years.  Downloaded and combined from http://www.cdc.gov/growthcharts/data_tables.htm.
#' Used with the \code{qgrowth} function to generate height and weight percentiles for the purposes of simulation.
#'
#' @name growth
#' @docType data
#' @title CDC Pediatric and Adolescent Growth Data Table
#' @usage growth
#' @format A data frame with the following 9 columns: KNOT (integer age in months); A, B1, B2, B3 (coefficients for calculating
#' percentiles), SEX, AGE, PERCENTILE, and CHART (length x age, wt x age, wt x length, hc x age, or ht x age).
#' @author Michael Neely
#' @keywords datasets
NULL

