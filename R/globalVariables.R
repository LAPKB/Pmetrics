utils::globalVariables(
  ".groups A AGE Agemos B1 B2 B3 CHART HTML KNOT PERCENTILE SEX Sex
    actionButton ageyrs aic algebraic annotate arrow assignInNamespace
    bic block bmi bolus browseURL browseVignettes cdc_bmi checkboxInput
    ci cmt code colorRampPalette column combn corr_age cyc cycle d dcBin
    div dose elim evid fileInput final fitted fluidPage fluidRow from
    gamma_lambda ger_bmi getFromNamespace group h2 h3 h4 h5 hr ht
    htmlOutput icen icon input is isS3method key label locales lowerCI
    m_two_ll map2 map_lgl markdown measure modalDialog modelLibrary
    na.exclude name navlistPanel ncomp node numericInput obs obsSD
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
    wt x y ." %>%
    stringr::str_replace_all("\n\\s+", " ") %>%
    stringr::str_split(" ") %>%
    unlist()
)
