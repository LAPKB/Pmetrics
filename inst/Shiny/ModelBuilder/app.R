library(shiny)
library(Pmetrics)
library(tidyverse)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  
  # Application title
  titlePanel("Pmetrics Model Builder App"),
  #Layout
  
  #Model components
  #  #Formatting
  tags$head(
    tags$style(".alert {padding: 20px; background-color: #F44336; color: white;}",
               ".success {padding: 20px; background-color: #04AA6D; color: white;}",
               ".closebtn {margin-left: 12px; color: white; font-weight: bold; float: right; font-size: 22px; line-height: 20px; cursor: pointer; transition: 0.3s;}",
               ".closebtn:hover {color: black;}"
               
    )
  ),
  #uiOutput("msg"),
  
  
  navlistPanel(
    "Model Components",
    widths = c(3, 9),
    #tabPanel: Model Library
    tabPanel("Model Library",
             markdown("INSTRUCTIONS: Choose a model from the library or load 
             one of your own previously created models to populate
             model fields. Using components to the left, you can edit the
             fields, or define them yourself for any level of customization.
             It's best to define relevant components in the menu order from
                      top to bottom."),
             markdown("If you don't wish to use a model from the library, and you
                      don't have a prior model file, start with the PRImary tab."),
             hr(style = "border-top: 1px solid #FFFFFF;"),
             h2("Previous Model"),
             fileInput(
               "model_file",
               "Choose prior model file",
               accept = c(".txt", "text/plain")
             ),
             h2("Model Library"),
             fluidRow(
               column(6,
                      h3("Filters"),
                      selectInput(
                        "mod_route",
                        "Dosing route(s):",
                        list("","Oral", "Intravenous"),
                        multiple = TRUE
                      ),
                      numericInput(
                        "mod_ncomp",
                        "Number of compartments (including oral bolus):",
                        value = NA,
                        min = 1
                      ),
                      selectInput(
                        "mod_elim",
                        "Elimination from:",
                        "",
                        multiple = TRUE
                      ),
                      radioButtons(
                        "mod_kecl",
                        "Parameterized as:",
                        c("Rate constants", "Clearances"),
                        selected = character(0)
                      )
               ),
               column(6,
                      h3("Description Search"),
                      textInput(
                        inputId = "searchme",
                        label = ""
                      )
               )
             ), #end fluid Row
             uiOutput("bottom_library")
    ), #end tabPanel: Model Library
    #tabPanel: Primary
    tabPanel("PRImary",
             markdown("Pmetrics will estimate distributions for these parameters alone.
                      Choose the number of parameters, whether to parameterize as ranges
                      or means/SDs, and enter names."),
             markdown("Parameters are assumed to be random
                      (unknown mean/variance). If a value is entered for either *Min* or *Mean*
                      but *Max*/*SD* is blank (delete the contents) or contains `NA`, the parameter will be considered
                      as *Fixed*, which means the same but unknown value in the population
                      (unknown mean, zero variance). The entered value will be the starting
                      value for the optimization. When a parameter is fixed, checking
                      *Constant* will make the entered value the same for the population
                      and it will not be optimized (known mean, zero variance)."),
             markdown("Parameters can also
                      be marked as *GTZ* or greater than zero, if they are to be kept positive.
                      This is only relevant for parametric estimation, as nonparametric estimation
                      will strictly respect boundaries."),
             hr(style = "border-top: 1px solid #FFFFFF;"),
             fluidRow(
               column(4, h5("Number of primary parameters:")),
               column(4, h5("Specify as:"))
             ),
             
             fluidRow(
               column(4,
                      numericInput("nvar",
                                   "",
                                   value = 1,
                                   min = 1
                      )
               ),
               column(5,
                      radioButtons("ab_msd",
                                   "",
                                   choices = c("Range", "Mean/SD"),
                                   inline = TRUE,
                                   selected = "Range"
                      )
               )
             ),
             hr(style = "border-top: 1px solid #FFFFFF;"),
             
             uiOutput("pri_var"),
             uiOutput("bottom_pri")
             
    ), #end tabPanel: Primary
    #tabPanel: Covariates
    
    tabPanel("COVariates",
             htmlOutput("cov_instructions"),
             uiOutput("cov"),
             uiOutput("bottom_cov"),
             
             
    ), #end tabPanel: Covariates
    #tabPanel: Secondary
    tabPanel("SECondary",
             textAreaInput("secVar",
                           "Secondary variable definitions:",
                           rows = NULL),
             uiOutput("bottom_sec")
             
    ), #end tabPanel: Secondary
    #tabPanel: Bolus
    tabPanel("BOLus",
             textAreaInput("bolComp",
                           "Bolus compartments:",
                           rows = NULL),
             uiOutput("bottom_bol")
             
    ), #end tabPanel: Bolus
    #tabPanel: Initial Conditions
    tabPanel("INItial Conditions",
             markdown("If any Primary or Secondary parameters are initial conditions,
                      choose them here and edit the code if needed. You can only
                      select as many parameters as you have model compartments (e.g. equations)."),
             hr(style = "border-top: 1px solid #FFFFFF;"),
             fluidRow(
               uiOutput("ini"),
               textAreaInput("iniCond",
                             "Initial conditions. Edit as necessary.",
                             rows = NULL),
               actionButton("reset_ini","Reset", icon("trash"))
               
             ),
             uiOutput("bottom_ini")
             
    ), #end tabPanel: Initial Conditions
    #tabPanel: FA (bioavailability)
    tabPanel("FA (bioavailability)",
             markdown("If any Primary or Secondary parameters are bioavailability,
                      choose them here and edit the code if needed. You can only
                      select as many parameters as you have inputs (e.g. drugs)."),
             hr(style = "border-top: 1px solid #FFFFFF;"),
             fluidRow(
               uiOutput("fa"),
               textAreaInput("FA",
                             "Bioavailability Code. Edit as necessary.",
                             rows = NULL),
               actionButton("reset_fa","Reset", icon("trash"))
               
             ),
             uiOutput("bottom_fa")
    ), #end tabPanel: F
    #tabPanel: Lag
    
    tabPanel("LAG time",
             markdown("If any Primary or Secondary parameters are lag times,
                      choose them here and edit the code if needed. You can only
                      select as many parameters as you have inputs (e.g. drugs)."),
             hr(style = "border-top: 1px solid #FFFFFF;"),
             fluidRow(
               uiOutput("lag"),
               textAreaInput("lagTime",
                             "Lag Time Code. Edit as necessary.",
                             rows = NULL),
               actionButton("reset_lag","Reset", icon("trash"))
             ),
             uiOutput("bottom_lag")
             
             
    ), #end tabPanel: Lag
    #tabPanel: Equations
    tabPanel("EQUations",
             markdown(c("Write the differential equations for your model here.",
                        "Use `dX[i]` for change in compartment amounts, where i is the compartment number, e.g. dX[1] or dX[2].",
                        "Compartment amounts are referred to as `X[i]`, e.g. X[1] or X[2].",
                        "Use `BOLUS[j]` for bolus input j and `RATEIV[k]` for infusion k.",
                        "j and k correspond to the INPUT column in the data file, which is usually omitted and assumed to be 1 for all doses.",
                        "The DUR column in the data file determines whether a dose is treated as a BOLUS (DUR = 0) or RATEIV (DUR > 0).",
                        "Any variable defined in PRI, COV, or SEC may be used.",
                        "Example: `dX[1] = RATEIV[1] * WT - Ke * X[1]`")),
             hr(style = "border-top: 1px solid #FFFFFF;"),
             textAreaInput("modEq",
                           "Model Equations:",
                           rows = NULL),
             uiOutput("bottom_eqn")
    ), #end tabPanel: Equations
    #tabPanel: Outputs
    tabPanel("OUTputs",
             markdown("Outputs are referred to as *Y[i]*, where *i* is the output equation number, e.g. Y[1]."),
             markdown("Compartments are referred to as *X[j]*, where *j* is the compartment number, e.g. X[1]."),
             markdown("Any variable defined in PRI, COV, or SEC may be used, e.g. Y[1] = X[1]/V."),
             hr(style = "border-top: 1px solid #FFFFFF;"),
             numericInput("nout",
                          "Number of output equations:",
                          value = 1,
                          width = "30%"),
             uiOutput("outputs"),
             uiOutput("bottom_out")
    ) #end tabPanel: Outputs
    
    
  ) #end navlistPanel
  
  
  
  
  
) #end ui

server <- function(input, output, session) {
  
  #does object exist?
  exist_obj <- function(obj){
    tryCatch(length(obj), error = function(e) FALSE)
  }
  
  #note "data" can be passed as an argument in build_model()
  #variables
  npar <- reactive({as.integer(input$nvar)})
  if(exist_obj(data)){
    cov_names <- Pmetrics:::getCov(data$standard_data)$covnames
    ncov <- Pmetrics:::getCov(data$standard_data)$ncov
    ninput <- max(data$standard_data$input, na.rm = TRUE)
  } else {
    cov_names <- NULL
    ncov <- 0
    ninput <- 100 #set to something ridiculous
  }
  
  numeqt <- reactive({as.integer(input$nout)})
  blockNames <- c("pri","cov","sec","bol","ini","fa","lag","eqn","out")
  
  #set initial values for stored variables
  store <- reactiveValues(
    var_name_1 = "Ke",
    var_a_1 = 0,
    var_b_1 = 5,
    # var_mean_1 = 2.5,
    # var_sd_1 = 0.833,
    var_constant_1 = FALSE,
    var_gtz_1 = FALSE
  )
  
  #create model
  new_model <- reactiveVal()
  #copy model
  copy_list <- reactiveVal()
  
  
  
  #extract secondary variables
  extract_sec <- reactive({
    if(input$secVar != ""){
      map(input$secVar,function(x){
        eqn <- parse(text = x)
        dplyr::case_when(
          class(eqn[[1]]) == "=" ~ deparse(eqn[[1]][[2]]),
          class(eqn[[1]]) == "if" ~ deparse(eqn[[1]][[3]][[2]])
        )
      })
    } else {
      ""
    }
  })
  
  #process reset buttons
  observeEvent(input$reset_ini, {
    updateSelectizeInput(
      inputId = "special_ini",
      selected = ""
    )
  })
  observeEvent(input$reset_ini, {
    updateTextAreaInput(
      inputId = "iniCond",
      value = ""
    )
  })
  
  observeEvent(input$reset_fa, {
    updateSelectizeInput(
      inputId = "special_fa",
      selected = ""
    )
  })
  observeEvent(input$reset_fa, {
    updateTextAreaInput(
      inputId = "FA",
      value = ""
    )
  })
  
  observeEvent(input$reset_lag, {
    updateSelectizeInput(
      inputId = "special_lag",
      selected = ""
    )
  })
  observeEvent(input$reset_lag, {
    updateTextAreaInput(
      inputId = "lagTime",
      value = ""
    )
  })
  
  
  
  
  
  
  
  # MODEL LIBRARY COMPONENT -------------------------------------------------
  
  
  mods <- modelLibrary
  
  #create the global filter results
  mods_filter <- reactiveVal()
  
  #make combined filter trigger
  filter_obj <- reactive({
    list(input$mod_route, input$mod_ncomp, input$mod_elim, input$mod_kecl, input$searchme)
  })
  
  #set compartments for elimination
  observeEvent(input$mod_ncomp,{
    if(!is.na(input$mod_ncomp)){
      updateSelectInput(
        inputId = "mod_elim",
        choices = 1:input$mod_ncomp,
        selected = NULL
      )
    }
    
  }
  )
  
  #function to aid filtering
  find_x_in_y <- function(x, y){
    any(stringr::str_detect(y, stringr::regex(x,ignore_case = T)))
  }
  
  
  
  #if filter inputs change, execute this
  observeEvent(filter_obj(),{
    this_filter <- mods
    if(!is.null(input$mod_route) ){
      this_filter <- this_filter %>%
        filter(map_lgl(route, ~find_x_in_y(input$mod_route, .x)))
    }
    if(!is.na(input$mod_ncomp)){
      this_filter <- this_filter %>%
        filter(map_lgl(ncomp, ~input$mod_ncomp == .x))
    }
    if(!is.null(input$mod_elim) && input$mod_elim != ""){
      this_filter <- this_filter %>%
        filter(map_lgl(elim, ~find_x_in_y(input$mod_elim, .x)))
      
    }
    if(length(input$mod_kecl)>0){
      if(input$mod_kecl == "Rate constants"){key <- "K" } else {key <- "C"}
      this_filter <- this_filter %>%
        filter(par == key)
      
    }
    
    #if search box changes, execute this
    if(input$searchme != ""){
      #browser()
      terms <- stringr::str_split(input$searchme, ",|;|\\s+") %>% unlist() %>% stringi::stri_remove_empty()
      this_filter <- this_filter %>%
        filter(map_lgl(name, ~find_x_in_y(terms, .x)))

    }
    
    #update the global filter results
    if(nrow(this_filter)==0){
      this_filter <- tibble(name = "None")
    }
    mods_filter(this_filter)
    
  })
  
  
  #BOTTOM PANEL: Library
  
  output$bottom_library <- renderUI({
    list(hr(style = "border-top: 1px solid #FFFFFF;"),
         fluidRow(
           column(6,
                  h4("Matching Models"),
                  selectInput(
                    "mod_list",
                    "",
                    choices = mods_filter()$name,
                    selectize = FALSE,
                    multiple = FALSE,
                    size = 10
                  ),
                  actionButton("select_model","Select", icon("check"))
                  
           ),
           column(6,
                  h4("Model Snapshot"),
                  markdown("B = Bolus, R = infusion Rate, Y = observation"),
                  plotOutput("model_snapshot")
           )
         ) #end fluidRow
    ) #end list
  }) #end renderUI
  
  #render the model snapshot
  observeEvent(input$mod_list,
               ignoreInit = TRUE,
               ignoreNULL = TRUE,
               {
                 output$model_snapshot <- renderPlot({
                   model <- tryCatch(modelLibrary$mod[which(modelLibrary$name == input$mod_list)][[1]], error = function(e) NA)
                   class(model) <- "PM_model"
                   #browser()
                   tryCatch(plot(model),
                            error = function(e) {
                              ggplot2::ggplot(data = data.frame(x = c(0,1), y = c(0,1)),
                                              aes(x, y)) +
                                annotate("label", x = 0.5, y = 0.5, label = "Unable to display model diagram...") +
                                ggraph::theme_graph()
                            }
                   )
                 })
               })
  
  
  #create reactive model object
  model <- reactiveVal()
  
  #update model if select model updates
  observeEvent(input$select_model,
               ignoreInit = TRUE,
               {
                 #reset store
                 #store <- purrr::map(store, ~NULL)
                 model(modelLibrary$mod[which(modelLibrary$name == input$mod_list)][[1]]$model_list)
               })
  
  #update model if previous model loaded
  observeEvent(input$model_file,
               ignoreInit = TRUE,
               {
                 loaded <- tryCatch(PM_model$new(input$model_file$datapath), error = function(e) {print("Error loading model.");return()})
                 model(loaded$model_list)
               }
  )
  
  
  observeEvent(model(),
               {
                 #grab model
                 model <- model()
                 
                 npar <- length(model$pri)
                 updateNumericInput(inputId = "nvar", value = npar)
                 purrr::walk(1:npar,
                             ~{
                               store[[paste0("var_name_",.x)]] <- names(model$pri)[.x]
                               store[[paste0("var_a_",.x)]] <- ifelse(abmsd()=="Range", model$pri[[.x]]$min, model$pri[[.x]]$mean)
                               store[[paste0("var_b_",.x)]] <- ifelse(abmsd()=="Range", model$pri[[.x]]$max, model$pri[[.x]]$sd)
                               store[[paste0("var_constant_",.x)]] <- model$pri[[.x]]$constant
                               store[[paste0("var_gtz_",.x)]] <- model$pri[[.x]]$gtz
                             })
                 updateTextAreaInput(inputId = "secVar", value = paste(model$sec, collapse = "\n"))
                 updateTextAreaInput(inputId = "lagTime", value = paste(model$lag, collapse = "\n"))
                 updateTextAreaInput(inputId = "iniCond", value = paste(model$ini, collapse = "\n"))
                 updateTextAreaInput(inputId = "FA", value = paste(model$fa, collapse = "\n"))
                 updateTextAreaInput(inputId = "modEq", value = paste(model$eqn, collapse = "\n"))
                 numeqt <- length(model$out)
                 updateNumericInput(inputId = "nout", value = numeqt)
                 purrr::walk(1:numeqt,
                             ~{
                               store[[paste0("out_eqn_",.x)]] <- model$out[[.x]]$val
                               store[[paste0("out_assay_err_",.x)]] <- paste0(model$out[[.x]]$err$assay$coefficients, collapse = ", ")
                               store[[paste0("out_assay_err_always_",.x)]] <- model$out[[.x]]$err$assay$constant
                               store[[paste0("out_model_err_type_",.x)]] <-
                                 dplyr::case_when(
                                   !is.null(model$out[[.x]]$err$model$additive) && !model$out[[.x]]$err$model$constant ~ "Additive (lambda)",
                                   !is.null(model$out[[.x]]$err$model$additive) && model$out[[.x]]$err$model$constant ~ "Additive Fixed",
                                   !is.null(model$out[[.x]]$err$model$proportional) && !model$out[[.x]]$err$model$constant ~ "Proportional (gamma)",
                                   !is.null(model$out[[.x]]$err$model$proportional) && model$out[[.x]]$err$model$constant ~ "Proportional Fixed"
                                 )
                               store[[paste0("out_model_err_val_",.x)]] <-
                                 ifelse(
                                   !is.null(model$out[[.x]]$err$model$additive),
                                   model$out[[.x]]$err$model$additive,
                                   model$out[[.x]]$err$model$proportional
                                 )
                               
                             })
                 
               })
  
  
  
  
  
  
  #PRIMARY PARAMETERS COMPONENT
  
  #get user choice for ab vs msd
  abmsd <- reactive({
    input$ab_msd
  })
  
  #save primary values
  observeEvent(npar(), {
    map(1:npar(),
        ~{
          if(exist_obj(input[[paste0("var_name_",.x)]])) store[[paste0("var_name_",.x)]] <- input[[paste0("var_name_",.x)]]
          if(exist_obj(input[[paste0("var_a_",.x)]])) store[[paste0("var_a_",.x)]] <- input[[paste0("var_a_",.x)]]
          if(exist_obj(input[[paste0("var_b_",.x)]])) store[[paste0("var_b_",.x)]] <- input[[paste0("var_b_",.x)]]
          if(exist_obj(input[[paste0("var_constant_",.x)]])) store[[paste0("var_constant_",.x)]] <- input[[paste0("var_constant_",.x)]]
          if(exist_obj(input[[paste0("var_gtz_",.x)]])) store[[paste0("var_gtz_",.x)]] <- input[[paste0("var_gtz_",.x)]]
        })
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  #render variable primary parameters in UI
  output$pri_var <- renderUI({
    purrr::map(1:npar(),
               ~{
                 fluidRow(
                   column(3,
                          textInput(
                            paste0("var_name_",.x),
                            label = paste0("Name ",.x,":"),
                            value = store[[paste0("var_name_",.x)]]
                          )
                   ),
                   column(2,
                          numericInput(
                            paste0("var_a_",.x),
                            ifelse(abmsd()=="Range","Min:","Mean:"),
                            value = store[[paste0("var_a_",.x)]]
                          )
                   ),
                   column(2,
                          numericInput(
                            paste0("var_b_",.x),
                            ifelse(abmsd()=="Range","Max:","SD:"),
                            value = store[[paste0("var_b_",.x)]]
                          )
                   ),
                   column(2,
                          div(style = "display: inline-block; margin-left: 10px; margin-right: 10px; vertical-align: -30px;",
                              checkboxInput(
                                paste0("var_constant_",.x),
                                "Constant?",
                                value = store[[paste0("var_constant_",.x)]]
                              )
                          )
                   ),
                   column(2,
                          div(style = "display: inline-block; margin-left: 10px; margin-right: 10px; vertical-align: -30px;",
                              checkboxInput(
                                paste0("var_gtz_",.x),
                                "GTZ?",
                                value = store[[paste0("var_gtz_",.x)]]
                              )
                          )
                   ) #end columns
                 ) #end row
               } #end ~ function
    ) #end map
  }) #end renderUI
  
  
  #COVARIATE COMPONENT
  
  output$cov_instructions <- renderUI({
    if(ncov > 0) {
      tagList(p("Covariates obtained from data. To modify, edit the data.",
                "Check any covariates which are constant between measurements.",
                "Leave unchecked if covariate is linearly interpolated between measurements."),
              hr(style = "border-top: 1px solid #FFFFFF;")
      )
    } else {
      p("No data were loaded so there are no covariates available.",
        "You can still type any covariate name you want in your model equations for building purposes,
        but it will not generate a functional model with an appropriate cov block. For that, start
        with the data you want to model, and the covariates will be loaded automatically.")
    }
  })
  
  #set default covariate values based on data
  output$cov <- renderUI({
    purrr::map(cov_names, function(x){
      checkboxInput(
        inputId = paste0(x,"_constant"),
        label = paste0(x),
        value = FALSE
      )
    })
  })
  
  
  #INI COMPONENT
  output$ini <- renderUI({
    selectizeInput("special_ini",
                   "Select the following parameters to be intial conditions:",
                   choices = list(`Primary` = map(1:npar(), ~input[[paste0("var_name_",.x)]]),
                                  `Secondary` = extract_sec(),
                                  `Covariates` = as.list(cov_names)),
                   multiple = TRUE,
                   options = list(maxItems = numeqt())
    )
    
  })
  
  observe({
    nini <- length(input$special_ini)
    if(nini > 0){
      updateTextAreaInput(
        inputId = "iniCond",
        value = map(1:nini,
                    ~paste0("X[",.x,"] = ",input$special_ini[.x])) %>%
          unlist() %>% paste(collapse = "\n")
        
      )
    }
  })
  
  #FA COMPONENT
  output$fa <- renderUI({
    selectizeInput("special_fa",
                   "Select the following parameters to be bioavailability:",
                   choices = list(`Primary` = map(1:npar(), ~input[[paste0("var_name_",.x)]]),
                                  `Secondary` = extract_sec(),
                                  `Covariates` = as.list(cov_names)),
                   multiple = TRUE,
                   options = list(maxItems = ninput)
    )
    
  })
  
  observe({
    nfa <- length(input$special_fa)
    if(nfa > 0){
      updateTextAreaInput(
        inputId = "FA",
        value = map(1:nfa,
                    ~paste0("FA[",.x,"] = ",input$special_fa[.x])) %>%
          unlist() %>% paste(collapse = "\n")
        
      )
    }
  })
  
  #LAG COMPONENT
  output$lag <- renderUI({
    selectizeInput("special_lag",
                   "Select the following parameters to be lag times:",
                   choices = list(`Primary` = map(1:npar(), ~input[[paste0("var_name_",.x)]]),
                                  `Secondary` = extract_sec(),
                                  `Covariates` = as.list(cov_names)),
                   multiple = TRUE,
                   options = list(maxItems = ninput)
    )
    
  })
  
  observe({
    nlag <- length(input$special_lag)
    if(nlag > 0){
      updateTextAreaInput(
        inputId = "lagTime",
        value = map(1:nlag,
                    ~paste0("LAG[",.x,"] = ",input$special_lag[.x])) %>%
          unlist() %>% paste(collapse = "\n")
        
      )
    }
  })
  
  
  #OUTPUTS COMPONENT
  
  ##save outputs values
  observeEvent(numeqt(), {
    map(1:numeqt(),
        ~{
          if(exist_obj(input[[paste0("out_eqn_",.x)]])) store[[paste0("out_eqn_",.x)]] <- input[[paste0("out_eqn_",.x)]]
          if(exist_obj(input[[paste0("out_assay_err_",.x)]])) store[[paste0("out_assay_err_",.x)]] <- input[[paste0("out_assay_err_",.x)]]
          if(exist_obj(input[[paste0("out_assay_err_always_",.x)]])) store[[paste0("out_assay_err_always_",.x)]] <- input[[paste0("out_assay_err_always_",.x)]]
          if(exist_obj(input[[paste0("out_model_err_type_",.x)]])) store[[paste0("out_model_err_type_",.x)]] <- input[[paste0("out_model_err_type_",.x)]]
          if(exist_obj(input[[paste0("out_model_err_val_",.x)]])) store[[paste0("out_model_err_val_",.x)]] <- input[[paste0("out_model_err_val_",.x)]]
        })
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  #render output parameters in UI
  output$outputs <- renderUI({
    purrr::map(1:numeqt(),
               ~{
                 fluidPage(
                   fluidRow(
                     h3(paste0("Output ",.x)),
                     column(4,
                            textInput(
                              paste0("out_eqn_",.x),
                              label = "Equation:",
                              value = store[[paste0("out_eqn_",.x)]]
                            )
                     )
                   ), #end fluidRow
                   fluidRow(
                     column(6, h4("Assay Error")),
                     if(.x == 1) {column(6, h4("Model Error"))}
                   ),
                   fluidRow(
                     column(4,
                            textInput(
                              paste0("out_assay_err_",.x),
                              "Coefficients: ",
                              value = store[[paste0("out_assay_err_",.x)]]
                            )
                     ),
                     column(2,
                            div(style = "display: inline-block; margin-left: 10px; margin-right: 10px; vertical-align: -30px;",
                                checkboxInput(
                                  paste0("out_assay_err_always_",.x),
                                  "Use always?",
                                  value = store[[paste0("out_assay_err_always_",.x)]]
                                )
                            )
                     ),
                     if(.x == 1) {
                       column(4,
                              selectInput(
                                paste0("out_model_err_type_",.x),
                                "Type:",
                                choices = c("Additive (lambda)", "Additive Fixed", "Proportional (gamma)", "Proportional Fixed"),
                                selected = store[[paste0("out_model_err_type_",.x)]]
                              )
                       )
                     },
                     if(.x == 1) {column(2,
                                         textInput(
                                           paste0("out_model_err_val_",.x),
                                           "Value:",
                                           value = store[[paste0("out_model_err_val_",.x)]]
                                         )
                     )}
                   ) #end fluidRow
                 )#end fluidPage
               } #end ~ function
    ) #end map
  }) #end renderUI
  
  
  
  #BOTTOM PANEL: Components
  
  tab <- function(n){
    return(paste0(rep(" ",n),collapse=""))
  }
  
  purrr::map(blockNames, function(x) {
    output[[paste0("model_list_",x)]] <- renderUI({
      
      all_blocks <- list() #this is for printing
      
      #primary
      all_blocks[[1]] <-
        paste0(
          tab(4), "pri = list(<br>",
          paste0(purrr::map(1:npar(), ~paste0(tab(6),
                                              input[[paste0("var_name_",.x)]],
                                              " = ",
                                              dplyr::case_when(
                                                is.na(input[[paste0("var_b_",.x)]])  ~ paste0("fixed(", input[[paste0("var_a_",.x)]]),
                                                abmsd() == "Range" ~ paste0("ab(",input[[paste0("var_a_",.x)]], ", ", input[[paste0("var_b_",.x)]]),
                                                abmsd() == "Mean/SD" ~ paste0("msd(",input[[paste0("var_a_",.x)]], ", ", input[[paste0("var_b_",.x)]])
                                              ),
                                              if(!is.null(input[[paste0("var_b_",.x)]]) &&
                                                 is.na(input[[paste0("var_b_",.x)]]) &&
                                                 input[[paste0("var_constant_",.x)]]) {", constant = TRUE"},
                                              if(!is.null(input[[paste0("var_gtz_",.x)]]) && input[[paste0("var_gtz_",.x)]])
                                              {", gtz = TRUE"},
                                              ")"))) %>%
            unlist() %>% paste(collapse = ",<br>"),"<br>",
          tab(4), ")"
        )
      
      #covariates
      if (ncov > 0){
        all_blocks[[2]] <-
          paste0(
            tab(4), "cov = list(<br>",
            paste0(purrr::map(cov_names, ~paste0(tab(6),
                                                 "covariate(\"",
                                                 .x,
                                                 "\"",
                                                 ifelse(
                                                   !is.null(input[[paste0(.x, "_constant")]]) && input[[paste0(.x, "_constant")]],
                                                   ", constant = TRUE)",
                                                   ")"
                                                 )
            ))) %>%
              unlist() %>% paste(collapse = ",<br>"),"<br>",
            tab(4), ")"
          )
      } else {
        all_blocks[[2]] <- NULL
      }
      
      
      #secondary
      if (input$secVar != ""){
        all_blocks[[3]] <-
          paste0(
            tab(4), "sec = c(<br>",
            paste0(tab(6), "\"", stringr::str_split_1(input$secVar, "\n"),
                   "\"", collapse = ",<br>"),
            "<br>", tab(4), ")"
          )
      } else {
        all_blocks[[3]] <- NULL
      }
      
      #bolus compartments
      if (input$bolComp != ""){
        all_blocks[[4]] <-
          paste0(
            tab(4), "bol = c(<br>",
            paste0(tab(6), "\"", stringr::str_split_1(input$bolComp, "\n"), "\"", collapse = ",<br>"),
            "<br>", tab(4), ")"
          )
      } else {
        all_blocks[[4]] <- NULL
      }
      
      #initial conditions
      if (input$iniCond != ""){
        all_blocks[[5]] <-
          paste0(
            tab(4), "ini = c(<br>",
            paste0(tab(6), "\"", input$iniCond, "\"", collapse = ",<br>"),
            "<br>", tab(4), ")"
          )
      } else {
        all_blocks[[5]] <- NULL
      }
      
      #bioavailability
      if (input$FA != ""){
        all_blocks[[6]] <-
          paste0(
            tab(4), "fa = c(<br>",
            paste0(tab(6), "\"", input$FA, "\"", collapse = ",<br>"),
            "<br>", tab(4), ")"
          )
      } else {
        all_blocks[[6]] <- NULL
      }
      
      #lag time
      if (input$lagTime != ""){
        all_blocks[[7]] <-
          paste0(
            tab(4), "lag = c(<br>",
            paste0(tab(6), "\"", input$lagTime, "\"", collapse = ",<br>"),
            "<br>", tab(4), ")"
          )
      } else {
        all_blocks[[7]] <- NULL
      }
      
      #equations
      if (input$modEq != ""){
        all_blocks[[8]] <-
          paste0(
            tab(4), "eqn = c(<br>",
            paste0(tab(6), "\"", stringr::str_split_1(input$modEq, "\n"),
                   "\"", collapse = ",<br>"),
            "<br>", tab(4), ")"
          )
      } else {
        all_blocks[[8]] <- NULL
      }
      
      #outputs
      all_blocks[[9]] <-
        paste0(
          tab(4), "out = list(<br>",
          paste0(purrr::map(1:numeqt(), ~paste0(tab(6),
                                                "Y",.x," = list(<br>",
                                                tab(8),"val = \"",input[[paste0("out_eqn_",.x)]],"\",<br>",
                                                tab(8),"err = list(<br>",
                                                paste0(tab(10),"model = ",
                                                       dplyr::case_when(
                                                         input$out_model_err_type_1 == "Additive (lambda)" ~ paste0("additive(",input$out_model_err_val_1,"),<br>"),
                                                         input$out_model_err_type_1 == "Additive Constant" ~ paste0("additive(",input$out_model_err_val_1,", constant = TRUE),<br>"),
                                                         input$out_model_err_type_1  == "Proportional (gamma)" ~ paste0("proportional(",input$out_model_err_val_1,"),<br>"),
                                                         input$out_model_err_type_1 == "Proportional Constant" ~ paste0("proportional(",input$out_model_err_val_1,", constant = TRUE),<br>"),
                                                       )),
                                                paste0(tab(10), "assay = ",
                                                       dplyr::case_when(
                                                         input[[paste0("out_assay_err_always_",.x)]] == TRUE ~ paste0("errorPoly(c(",input[[paste0("out_assay_err_",.x)]],"), constant = TRUE)<br>"),
                                                         input[[paste0("out_assay_err_always_",.x)]] == FALSE ~ paste0("errorPoly(c(",input[[paste0("out_assay_err_",.x)]],"))<br>"),
                                                       )),
                                                tab(8), ")<br>", #close err list
                                                tab(6), ")" #close output Y list
          ) #end paste0
          ) #end map
          %>%
            unlist() %>% paste(collapse = ",<br>"),"<br>",
          tab(4), ")<br>" #close out list
          ) #end paste0
        ) #end paste0
      
      #create object suitable for copying to clipboard
      copy_list(
        paste0("PM_model$new(\n",
               tab(2), "list(\n",
               paste0(purrr::compact(
                 #tidy up html characters
                 purrr::map(all_blocks,
                            ~{
                              stringr::str_replace_all(.x, "<br>","\n") #%>%
                              #stringr::str_replace_all("\\\"","'") %>%
                            }
                 )
               ), collapse = ",\n"),
               tab(2),")\n",")"
        )
      )
      
      #model_list to create a PM_model before all_blocks compacted
      model_list <- all_blocks %>%
        #tidy up html characters
        purrr::map(
          ~{
            stringr::str_replace_all(.x, "<br>\\s*","") %>%
              stringr::str_replace_all("\\\"","'") %>%
              stringr::str_replace_all("^\\s+","")
          }
        ) %>%
        #parse
        
        purrr::map(~tryCatch(eval(parse(text = .x)), error = function(e) "Error")) %>%
        #browser()
        rlang::set_names(blockNames) %>%
        #remove empty
        purrr::compact()
      
      #update reactiveVal
      new_model(model_list)
      
      #put it all together to display all_blocks in app
      HTML(paste0("<pre>",
                  "PM_model$new(<br>",
                  tab(2), "list(<br>",
                  paste0(purrr::compact(all_blocks), collapse = ",<br>"),
                  tab(2), ")<br>",
                  ")<br>",
                  "</pre>"
      )
      )
      
    }) # end output
  } # end function
  ) #end map
  
  
  #render the model diagrams
  purrr::map(blockNames, function(x){
    output[[paste0("model_diagram_",x)]] <- renderPlot({
      mod <- list(model_list = list(eqn = stringr::str_split(input$modEq, "\n") %>% unlist(),
                                    out = rlang::set_names(paste0("Y",1:numeqt())) %>% map2(1:numeqt(), ~list(val = input[[paste0("out_eqn_",.y)]]))))
      class(mod) <- "PM_model"
      #browser()
      tryCatch(plot(mod),
               error = function(e) {
                 ggplot2::ggplot(data = data.frame(x = c(0,1), y = c(0,1)),
                                 aes(x, y)) +
                   annotate("label", x = 0.5, y = 0.5, label = "Model building in progress...") +
                   ggraph::theme_graph()
               }
      )
    })
  })
  
  
  #Create the layout for the bottom panel
  purrr::map(blockNames, function(x){
    output[[paste0("bottom_",x)]] <- renderUI({
      list(hr(style = "border-top: 1px solid #FFFFFF;"),
           fluidRow(
             tabsetPanel(
               tabPanel(
                 type = "pills",
                 "Model Diagram",
                 plotOutput(paste0("model_diagram_",x))
               ),
               type = "pills",
               tabPanel(
                 "Model List",
                 div(
                   style = "width: 75%;",
                   htmlOutput(paste0("model_list_",x))
                 ),
                 column(2,
                        actionButton(paste0("save_model_list_",x),"Save", icon = icon("save"))
                 ),
                 column(2,
                        actionButton(paste0("copy_model_list_",x),"Copy", icon = icon("copy"))
                 )
               )
             ) #end tabsetPanel
           ) #end fluidRow
      ) #end list
    }) #end renderUI
  } #end  function
  ) #end map
  
  
  
  # BUTTON ACTIONS ----------------------------------------------------------
  
  
  # save the model
  observe({
    map(blockNames,
        ~{
          observeEvent(
            input[[paste0("save_model_list_",.x)]],
            ignoreInit = TRUE,
            ignoreNULL = TRUE,
            {
              if(is.null(new_model())){
                return()
              } else {
                #browser()
                incomplete_blocks <- sapply(new_model(), function(x) ifelse(is.list(x), FALSE, stringr::str_detect(x,"^Error")))
                if(any(incomplete_blocks)){
                  incomplete_blocks <- paste0("#",names(new_model())[incomplete_blocks %>% unlist()], collapse = ", ")
                  #browser()
                  message(paste("Enter information for", incomplete_blocks, "block", c("","s")[1+(length(incomplete_blocks)>1)], "first, then save."))
                  alert_count(alert_count() + 1) #trigger message popup
                  
                } else {
                  #browser()
                  model_save <- tryCatch(PM_model$new(new_model())$write("model.txt"), error = function(e) "Fail")
                  if(model_save != "Fail"){
                    message("Model saved as 'model.txt' in current working directory.")
                    success_count(success_count() + 1) #trigger message popup
                    
                  } else {
                    message("Model not yet complete.")
                    alert_count(alert_count() + 1) #trigger message popup
                    
                  }
                }
              }
            })
          
        }) #end map
    
  }) #end observe
  
  
  # copy the model
  observe({
    map(blockNames,
        ~{
          observeEvent(
            input[[paste0("copy_model_list_",.x)]],
            ignoreInit = TRUE,
            ignoreNULL = TRUE,
            {
              if(is.null(new_model())){
                return()
              } else {
                #print(output[[paste0("model_list_",.x)]])
                clipr::write_clip(copy_list())
              } 
            }
          ) #end observeEvent
        }) #end map
  }) #end observe
  
  #message popup
  message <- reactiveVal("")
  alert_count <- reactiveVal(0)
  success_count <- reactiveVal(0)
  
  #alerts
  observeEvent(alert_count(),
               ignoreInit = TRUE,
               {
                 showModal(
                   modalDialog(
                     size = "m",
                     div(
                       class = "alert",
                       message()
                     ) #end div
                   )
                 )
               })
  #success
  observeEvent(success_count(),
               ignoreInit = TRUE,
               {
                 showModal(
                   modalDialog(
                     size = "m",
                     div(
                       class = "success",
                       message()
                     ) #end div
                   )
                 )
               })
  
  
  
} #end server