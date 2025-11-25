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
#' @author Michael Neely MD
#' \url{http://www.lapk.org}
#' @keywords internal
#' "_PACKAGE"
#'
#' @importFrom dplyr select arrange filter mutate group_by row_number
#' group_map ungroup bind_cols bind_rows nest_by rowwise relocate rename reframe inner_join
#' slice_tail slice_head slice across tibble as_tibble quo pull n everything 
#' case_match case_when starts_with all_of where distinct summarize if_else
#' @importFrom readr read_file write_file
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_polygon geom_hline geom_rect
#' geom_label scale_x_log10 scale_x_continuous scale_y_log10 scale_y_continuous xlab ylab
#' theme ggtitle element_blank element_text geom_segment aes_string aes_string theme_bw theme_grey
#' coord_fixed facet_wrap labs geom_smooth xlim ylim theme_void
#' scale_color_identity scale_fill_identity
#' @importFrom purrr map map2 reduce map_chr keep pluck map_lgl map_df list_rbind
#' @importFrom magrittr %>%
# #' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider nest unnest extract separate fill
#' crossing separate_wider_delim
#' @importFrom stringr str_replace regex
# #' @importFrom mclust Mclust
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
#' tail txtProgressBar write.csv write.table getTxtProgressBar
# #' @importFrom base64enc base64decode
#' @importFrom R6 R6Class
#' @importFrom rlang .data set_names :=
#' @importFrom plotly filter mutate plot_ly add_markers add_lines layout
#' ggplotly subplot add_annotations add_bars renderPlotly add_trace
#' @importFrom shiny fluidPage titlePanel tags navlistPanel tabPanel fluidRow
#' column h2 h3 h4 h5 fileInput selectInput numericInput radioButtons checkboxInput
#' textInput uiOutput actionButton hr textAreaInput reactiveVal reactive
#' observeEvent updateSelectizeInput updateTextAreaInput updateNumericInput
#' updateSelectInput renderUI markdown plotOutput div HTML htmlOutput
#' showModal modalDialog icon shinyApp textOutput helpText conditionalPanel br
#' @importFrom bslib accordion accordion_panel card navset_card_tab
#' nav_panel
# #' @importFrom trelliscopejs trelliscope map_plot
## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom DT datatable
#' @importFrom shiny fluidPage titlePanel tags navlistPanel tabPanel fluidRow
#' column h2 h3 h4 h5 fileInput selectInput numericInput radioButtons checkboxInput
#' textInput uiOutput actionButton hr textAreaInput reactiveVal reactive
#' observeEvent updateSelectizeInput updateTextAreaInput updateNumericInput
#' updateSelectInput renderUI markdown plotOutput div HTML htmlOutput
#' showModal modalDialog icon shinyApp textOutput helpText conditionalPanel
## usethis namespace: end
NULL
