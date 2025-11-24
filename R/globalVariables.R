utils::globalVariables(
  ".groups A AGE Agemos B1 B2 B3 CHART HTML KNOT PERCENTILE SEX Sex
    actionButton ageyrs aic algebraic annotate arrow assignInNamespace
    bic block bmi bolus browseURL browseVignettes cdc_bmi checkboxInput
    ci cmt code colorRampPalette column combn corr_age cyc cycle d dcBin
    div dose elim evid fileInput final fitted fluidPage fluidRow found from
    gamma_lambda ger_bmi getFromNamespace group h2 h3 h4 h5 hr ht
    htmlOutput icen icon input is isS3method IT2bout key label locales lowerCI
    m_two_ll map2 map_lgl markdown measure modalDialog
    na.exclude name navlistPanel ncomp NPAGout node numericInput obs obsSD
    observe observeEvent out outeq parameter percentile pcObs pk plotOutput
    point popMean popMedian position postMean postMedian pred pred.type
    prob prop_success radioButtons rateiv reactive reactiveVal reactiveValues
    renderPlot renderText renderUI route selectInput selectizeInput
    shinyApp showModal showlegend simnum sim_num statistic success_ratio
    tabPanel tabsetPanel
    tadBin tadBinMedian tadBinNum tags target tau textAreaInput textInput
    textOutput thisobs thisrun thissim timeBin timeBinMedian timeBinNum
    titlePanel to type uiOutput unit updateNumericInput updateSelectInput
    updateSelectizeInput updateTextAreaInput upperCI value wd withMathJax
    E ESS ESS_cor Name P Parameters Run Scr1 Scr2 Value above
     alpha_val alt bias cens color compartment contains converged
     coord_fixed crcl crcl_cg crcl_jelliffe cross default_line fill_col
     first geom_label geom_rect gfr_mdrd group_split growth id id2 imp int
     lag last_col line_col loess.control map_vec metric modelFor n_out
     neg2ll nsim nvar pair pdi plot.new primary pval r2 reg_num run
     scale_color_identity scale_fill_identity scrAve setNames sl src
     str_split sym symbol theme_void time1 time2 title updateCheckboxInput
     val xend xlim yend ylim
    wt x y ." %>%
    stringr::str_replace_all("\n\\s+", " ") %>%
    stringr::str_split(" ") %>%
    unlist()
)
