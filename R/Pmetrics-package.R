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
#' @author Michael Neely MD
#' \url{http://www.lapk.org}
#' @keywords package
#'
#' @importFrom dplyr select arrange filter mutate transmute group_by row_number
#' group_map ungroup bind_cols bind_rows nest_by rowwise rename inner_join
#' slice_tail slice_head slice across tibble as_tibble .data
#' @importFrom foreach %dopar%
#' @importFrom readr read_file write_file
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_polygon geom_hline
#' scale_x_log10 scale_x_continuous scale_y_log10 scale_y_continuous xlab ylab
#' theme ggtitle element_blank geom_segment aes_string aes_string theme_bw theme_grey
#' @importFrom purrr map map2 reduce map_chr keep pluck
#' @importFrom magrittr %>%
# #' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider nest unnest extract separate fill crossing
#' @importFrom stringr str_replace regex
#' @importFrom mclust Mclust mclustBIC
#' @importFrom grDevices col2rgb dev.off devAskNewPage gray.colors jpeg 
#' pdf png postscript rgb setEPS
#' @importFrom graphics abline arrows axTicks axis boxplot hist legend lines 
#' par plot points polygon rect rug segments text
#' @importFrom stats aggregate anova approx as.formula binom.test coef 
#' complete.cases confint cor cor.test cov cov.wt cov2cor density dnorm 
#' get_all_vars glm kmeans kruskal.test ks.test
#' lm median model.frame pchisq pnorm predict
#' pt qchisq qnorm qqline qqnorm qqplot qt
#' quantile rnorm runif sd shapiro.test step
#' t.test terms time var weighted.mean wilcox.test
#' @importFrom utils compareVersion data flush.console glob2rx head 
#' install.packages modifyList news packageVersion read.table setTxtProgressBar str
#' tail txtProgressBar write.csv write.table
#' @importFrom httr add_headers content GET POST DELETE
# #' @importFrom base64enc base64decode
#' @importFrom R6 R6Class
#' @importFrom plotly filter mutate plot_ly add_markers add_lines layout 
#' ggplotly subplot add_annotations add_bars
#' @importFrom shiny fluidPage titlePanel tags navlistPanel tabPanel fluidRow
#' column h2 h3 h4 h5 fileInput selectInput numericInput radioButtons checkboxInput
#' textInput uiOutput actionButton hr textAreaInput reactiveVal reactive
#' observeEvent updateSelectizeInput updateTextAreaInput updateNumericInput
#' updateSelectInput renderUI markdown plotOutput div HTML htmlOutput
#' showModal modalDialog icon shinyApp textOutput
#' @importFrom trelliscopejs trelliscope map_plot 
## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL


