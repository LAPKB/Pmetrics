#' Launch Model Builder app
#' 
#' Open the shiny model builder app.
#' 
#' @details
#' The app will open in a separate window.
#'
#' @param ... Optional [PM_data] and/or [PM_model] object(s). *PM_data* objects
#' supply covariates. *PM_model* objects supply any other defined model element,
#' and covariates only if there is no *PM_data* object or it has no covariates.
#' If the *PM_model* object contains covariates, they will be superseded by those in
#' the *PM_data* object, if supplied.
#' @return Launches the shiny app.
#' @export
#' @author Michael Neely
#' 
build_model <- function(...) {
  
  require(shiny)
  require(Pmetrics)
  require(tidyverse)
  require(shinythemes)
  
  obj <- list(...)
  data_arg <- purrr::detect(obj, \(x) inherits(x, "PM_data"))
  model_arg <- purrr::detect(obj, \(x) inherits(x, "PM_model"))
  
  
  # Define UI for application that draws a histogram
  shinyApp(
    ui <- fluidPage(
      theme = shinythemes::shinytheme("slate"),
      
      # Application title
      titlePanel("Pmetrics Model Builder App"),
      #Layout
      
      #Model components
      #  #Formatting
      tags$head(
        tags$style(HTML(
          ".alert {padding: 20px; background-color: #F44336; color: white;}",
          ".success {padding: 20px; background-color: #04AA6D; color: white;}",
          ".info {padding: 5px 20px 5px 5px; background-color: #3498db; color: white; font-size: 16px;}",
          ".closebtn {margin-left: 12px; color: white; font-weight: bold; float: right; font-size: 22px; line-height: 20px; cursor: pointer; transition: 0.3s;}",
          ".closebtn:hover {color: black;}",
          ".btn-help {display:block; color: white; padding: 2px 10px 2px 10px; text-align: center; border-radius: 100%; border: 1px solid DodgerBlue; }"
          
        )),
        tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$']], processEscapes: true}
                });
                </script>
                "))
        
      ),
      
      
      navlistPanel(
        "Model Components",
        widths = c(3, 9),
        #tabPanel: Model Library
        tabPanel("Model Library",
                 actionButton(
                   "help_library",
                   "",
                   icon = icon("info"),
                   class = "btn-help"
                 ),
                 fluidRow(
                   column(6,
                          h2("Previous Model"),
                          fileInput(
                            "model_file",
                            "Choose prior model file",
                            accept = c(".txt", "text/plain")
                          ),
                   ),
                   column(6,
                          h2("Data File"),
                          fileInput(
                            "data_file",
                            "Choose prior data file",
                            accept = "text/plain"
                          ),
                   )
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
                          ),
                          checkboxInput(
                            "mod_alg",
                            "Algebraic models only",
                            value = FALSE
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
                 actionButton(
                   "help_pri",
                   "",
                   icon = icon("info"),
                   class = "btn-help"
                 ),
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
                          radioButtons("ab_msd_cv",
                                       "",
                                       choices = c("Range", "Mean/CV%", "Mean/SD"),
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
                 actionButton(
                   "help_cov",
                   "",
                   icon = icon("info"),
                   class = "btn-help"
                 ),
                 uiOutput("cov"),
                 uiOutput("bottom_cov"),
                 
                 
        ), #end tabPanel: Covariates
        #tabPanel: Secondary
        tabPanel("SECondary",
                 actionButton(
                   "help_sec",
                   "",
                   icon = icon("info"),
                   class = "btn-help"
                 ),
                 textAreaInput("secVar",
                               "Secondary variable definitions:",
                               rows = NULL),
                 uiOutput("bottom_sec")
                 
        ), #end tabPanel: Secondary
        #tabPanel: Bolus
        # tabPanel("BOLus",
        #          textAreaInput("bolComp",
        #                        "Bolus compartments:",
        #                        rows = NULL),
        #          uiOutput("bottom_bol")
        #          
        # ), #end tabPanel: Bolus
        #tabPanel: Initial Conditions
        tabPanel("INItial Conditions",
                 actionButton(
                   "help_ini",
                   "",
                   icon = icon("info"),
                   class = "btn-help"
                 ),
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
                 actionButton(
                   "help_fa",
                   "",
                   icon = icon("info"),
                   class = "btn-help"
                 ),
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
                 actionButton(
                   "help_lag",
                   "",
                   icon = icon("info"),
                   class = "btn-help"
                 ),
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
        tabPanel("EQuatioNs",
                 actionButton(
                   "help_eqn",
                   "",
                   icon = icon("info"),
                   class = "btn-help"
                 ),
                 fluidRow(
                   column(4,
                          textAreaInput("modEq",
                                        "Model Equations:",
                                        rows = NULL)
                   ),
                   textOutput("edit_eqn")
                 ),
                 uiOutput("bottom_eqn")
        ), #end tabPanel: Equations
        #tabPanel: Outputs
        tabPanel("OUTputs",
                 actionButton(
                   "help_out",
                   "",
                   icon = icon("info"),
                   class = "btn-help"
                 ),
                 numericInput("nout",
                              "Number of output equations:",
                              value = 1,
                              width = "30%"),
                 uiOutput("outputs"),
                 uiOutput("bottom_out")
        ) #end tabPanel: Outputs
        
        
      ) #end navlistPanel
      
      
      
      
      
    ), #end ui
    
    server <- function(input, output, session) {
      
      #does object exist?
      exist_obj <- function(obj){
        tryCatch(length(obj), error = function(e) FALSE)
      }
      
      #DECLARE GLOBAL REACTIVE VALUES
      
      #reactive value to store model made by user
      new_model <- reactiveVal()
      #a copy of new_model suitable for pasting into R
      copy_list <- reactiveVal()
      
      #create reactive model objects for model loaded as file, 
      #from library, or passed as argument
      #the model itself
      model_obj <- reactiveVal()
      #the type of algebraic model
      alg_mod <- reactiveVal()
      #save the same to check if user edits restored algebraic model
      orig_model <- reactiveVal()
      orig_alg_model <- reactiveVal()
      #reactive object for data loaded from file or passed as argument
      data_obj <- reactiveVal()
      #reactive objects for covariates
      cov_names <- reactiveVal() #supplied by model or data
      cov_list <- reactiveVal() #supplied in app
      ncov <- reactiveVal()
      cov_source <- reactiveVal()
      
      #the model can be passed as an argument in build_model()
      #check if it is so and set
      if(!is.null(model_arg)){
        model_obj(model_arg$model_list) #update global reactive value
      }
      
      #the data can be passed as an argument in build_model()
      #covariates in data will supersede covariates in model
      if(!is.null(data_arg)){
        data_obj(data_arg) #update global 
        cov_data <- Pmetrics:::getCov(data_arg$standard_data)
        if(cov_data$ncov > 0){
          cov_names(cov_data$covnames)
          ncov(cov_data$ncov)
          ninput <- max(data_arg$standard_data$input, na.rm = TRUE)
          cov_source("Covariates obtained from data.")
        }
      } else if(!is.null(model_arg)){ #model supplied, but not data
        if(length(model_arg$model_list$cov)>0){ #model has covariates
          cov_names(purrr::map_chr(model_arg$model_list$cov, \(x) x$covariate))
          ncov(length(model_arg$model_list$cov))
          cov_source("Covariates obtained from model, since none were in the data.")
        } else {
          cov_names(NULL)
          ncov(0)
          cov_source("No covariates available.")
        }
        ninput <- 100 #set to something ridiculous
      } else { #no data or model
        cov_names(NULL)
        ncov(0)
        ninput <- 100 #set to something ridiculous
        cov_source("No covariates available.")
      }
      
      #variables
      npar <- reactive({as.integer(input$nvar)})
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
        list(input$mod_route, input$mod_ncomp, input$mod_elim, input$mod_kecl, 
             input$mod_alg,input$searchme)
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
          if(input$mod_kecl == "Rate constants"){key <- "K" } else {key <- "CL"}
          this_filter <- this_filter %>%
            filter(par == key)
          
        }
        if(input$mod_alg){
          this_filter <- this_filter %>%
            filter(algebraic != "")
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
                       mod_to_plot <- tryCatch(modelLibrary$mod[which(modelLibrary$name == input$mod_list)][[1]], error = function(e) NA)
                       class(mod_to_plot) <- "PM_model"
                       #browser()
                       tryCatch(plot(mod_to_plot),
                                error = function(e) {
                                  ggplot2::ggplot(data = data.frame(x = c(0,1), y = c(0,1)),
                                                  aes(x, y)) +
                                    annotate("label", x = 0.5, y = 0.5, label = "Unable to display model diagram...") +
                                    ggraph::theme_graph()
                                }
                       )
                     })
                   })
      
      #update model if select model updates
      observeEvent(input$select_model,
                   ignoreInit = TRUE,
                   {
                     #update reactive variables
                     model_obj(modelLibrary$mod[which(modelLibrary$name == input$mod_list)][[1]]$model_list) #this model will update
                     alg_mod(modelLibrary$algebraic[which(modelLibrary$name == input$mod_list)]) #indicates algebraic model
                     orig_model(model_obj()$eqn) #keep the original model
                     orig_alg_model(alg_mod()) #keep the original algebraic code
                     
                   })
      
      #update model if previous model loaded from file
      observeEvent(input$model_file,
                   ignoreInit = TRUE,
                   {
                     loaded_mod <- tryCatch(PM_model$new(input$model_file$datapath), error = function(e) {print("Error loading model.");return()})
                     model_obj(loaded_mod$model_list)
                     model_arg <- NULL #zero out
                   }
      )
      
      #update model if previous model loaded from file
      observeEvent(input$data_file,
                   ignoreInit = TRUE,
                   {
                     loaded_dat <- tryCatch(PM_data$new(input$data_file$datapath), error = function(e) {print("Error loading data.");return()})
                     data_obj(loaded_dat)
                     data_arg <- NULL #zero out
                   }
      )
      
      observeEvent(data_obj(), #the data object has changed, update covariates
                   {
                     cov_names(Pmetrics:::getCov(data_obj()$standard_data)$covnames)
                     ncov(Pmetrics:::getCov(data_obj()$standard_data)$ncov)
                     #browser()
                     if(ncov()==0){
                       cov_source("Covariates obtained from data when available, but no covariates in this dataset.")
                     } else {
                       cov_source("Covariates obtained from data.")
                     }
                   }
      )
      
      
      observeEvent(model_obj(), #the model object has changed, update fields
                   {
                     #grab model
                     model <- model_obj()
                     
                     npar <- length(model$pri)
                     updateNumericInput(inputId = "nvar", value = npar)
                     purrr::walk(1:npar,
                                 ~{
                                   store[[paste0("var_name_",.x)]] <- names(model$pri)[.x]
                                   store[[paste0("var_a_",.x)]] <- ifelse(abmsdcv()=="Range" | abmsdcv()=="Mean/CV%", model$pri[[.x]]$min, model$pri[[.x]]$mean)
                                   store[[paste0("var_b_",.x)]] <- ifelse(abmsdcv()=="Range" | abmsdcv()=="Mean/CV%", model$pri[[.x]]$max, model$pri[[.x]]$sd)
                                   store[[paste0("var_constant_",.x)]] <- model$pri[[.x]]$constant
                                   store[[paste0("var_gtz_",.x)]] <- model$pri[[.x]]$gtz
                                 })
                     #browser()
                     if(!is.null(model$cov) && 
                        !is.null(data_obj()) && Pmetrics:::getCov(data_obj()$standard_data)$ncov == 0){ #model has covariates, but none in app
                       cov_names(purrr::map_chr(model$cov, \(x) x$covariate))
                       ncov(length(cov_names()))
                       cov_source("Covariates obtained from model.")
                       
                     }
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
      abmsdcv <- reactive({
        input$ab_msd_cv
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
                                dplyr::case_when(
                                  abmsdcv()=="Range" ~ "Min:",
                                  abmsdcv()=="Mean/SD" ~ "Mean:",
                                  abmsdcv()=="Mean/CV%" ~ "Mean:"),
                                value = store[[paste0("var_a_",.x)]]
                              )
                       ),
                       column(2,
                              numericInput(
                                paste0("var_b_",.x),
                                dplyr::case_when(
                                  abmsdcv()=="Range" ~ "Max:",
                                  abmsdcv()=="Mean/SD" ~ "SD:",
                                  abmsdcv()=="Mean/CV%" ~ "CV%:"),
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
      
      #set default covariate values based on data
      output$cov <- renderUI({
        if(ncov()>0){
          purrr::map(cov_names(), function(x){
            checkboxInput(
              inputId = paste0(x,"_constant"),
              label = paste0(x),
              value = FALSE
            )
          })
        } else {
          textAreaInput("cov_user",
                        "Covariates",)
        }
        
      })
      
      observe({
        if(exist_obj(input$cov_user) && any(input$cov_user != "")){
          cov_list(stringr::str_split(input$cov_user, "\n")[[1]])
        } else {
          cov_list("")
        }
      })
      
      
      #INI COMPONENT
      output$ini <- renderUI({
        selectizeInput("special_ini",
                       "Select the following parameters to be intial conditions:",
                       choices = list(`Primary` = map(1:npar(), ~input[[paste0("var_name_",.x)]]),
                                      `Secondary` = extract_sec(),
                                      `Covariates` = as.list(c(cov_names(), gsub("!","",cov_list())))),
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
                                      `Covariates` = as.list(c(cov_names(), gsub("!","",cov_list())))),
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
                                      `Covariates` = as.list(c(cov_names(), gsub("!","",cov_list())))),
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
      
      
      #EQN COMPONENT
      
      observeEvent(input$modEq,{
        alg <- alg_mod()
        if(!is.null(alg) && alg!=""){
          #compare
          if(!identical(stringr::str_replace_all(input$modEq, "\\s+", ""),
                        stringr::str_replace_all(paste(orig_model(), collapse = "\n"), "\\s+", ""))){
            alg_mod("") #remove algebraic code
            output$edit_eqn <- renderText({"Warning: changing algebraic model equations forces use of ODE solver."})
          } else {
            alg_mod(orig_alg_model()) #restore algebraic code
            output$edit_eqn <- renderText({""})
          }
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
                                                    abmsdcv() == "Range" ~ paste0("ab(",input[[paste0("var_a_",.x)]], ", ", input[[paste0("var_b_",.x)]]),
                                                    abmsdcv() == "Mean/SD" ~ paste0("msd(",input[[paste0("var_a_",.x)]], ", ", input[[paste0("var_b_",.x)]]),
                                                    abmsdcv() == "Mean/CV%" ~ {
                                                      theta <- input[[paste0("var_a_",.x)]]
                                                      omega <- sqrt(log(((as.numeric(input[[paste0("var_b_",.x)]])/100))**2 + 1))
                                                      a <- round(theta * exp(-3*omega),3)
                                                      b <- round(theta * exp(3*omega),3)
                                                      paste0("ab(",a, ", ",b)
                                                    }
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
          if (ncov() > 0){
            all_blocks[[2]] <-
              paste0(
                tab(4), "cov = list(<br>",
                paste0(purrr::map(cov_names(), ~paste0(tab(6),
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
          } else if(any(cov_list() != "")){
            covs <- cov_list()
            fixed_cov <- grepl("!",covs)
            if(any(fixed_cov)){ #found fixed covariates
              covs <- gsub("!","",covs)
            }
            all_blocks[[2]] <-
              paste0(
                tab(4), "cov = list(<br>",
                paste0(purrr::map(1:length(covs), \(x) paste0(tab(6),
                                                       "covariate(\"",
                                                       covs[x],
                                                       "\"",
                                                       ifelse(
                                                         fixed_cov[x],
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
                tab(4), "sec = list(<br>",
                paste0(tab(6), "\"", stringr::str_split_1(input$secVar, "\n"),
                       "\"", collapse = ",<br>"),
                "<br>", tab(4), ")"
              )
          } else {
            all_blocks[[3]] <- NULL
          }
          
          #bolus compartments
          # if (input$bolComp != ""){
          #   all_blocks[[4]] <-
          #     paste0(
          #       tab(4), "bol = c(<br>",
          #       paste0(tab(6), "\"", stringr::str_split_1(input$bolComp, "\n"), "\"", collapse = ",<br>"),
          #       "<br>", tab(4), ")"
          #     )
          # } else {
          #   all_blocks[[4]] <- NULL
          # }
          all_blocks[[4]] <- NULL #no longer need bolus
          
          #initial conditions
          if (input$iniCond != ""){
            all_blocks[[5]] <-
              paste0(
                tab(4), "ini = list(<br>",
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
                tab(4), "fa = list(<br>",
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
                tab(4), "lag = list(<br>",
                paste0(tab(6), "\"", input$lagTime, "\"", collapse = ",<br>"),
                "<br>", tab(4), ")"
              )
          } else {
            all_blocks[[7]] <- NULL
          }
          
          #equations
          if (input$modEq != ""){
            #browser()
            alg <- alg_mod()
            #browser()
            all_blocks[[8]] <-
              paste0(
                tab(4), "eqn = list(<br>",
                if(!is.null(alg) && alg != ""){
                  paste0(tab(6), "\"{algebraic: ", alg, "}\",<br>")
                },
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
      help_count <- reactiveVal(0)
      
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
      
      #help
      observeEvent(help_count(),
                   ignoreInit = TRUE,
                   {
                     showModal(
                       modalDialog(
                         size = "l",
                         div(
                           class = "info",
                           message()
                         ) #end div
                       )
                     )
                   })
      
      #help messages
      observeEvent(input$help_library,{
        message(
          list(
            h4("Model Library"),
            markdown("
             * Choose a model from the library or load 
             one of your own previously created models to populate
             model fields.
             * Using components to the left, you can edit the
             fields, or define them yourself for any level of customization.
             It's best to define relevant components in the menu order from
             top to bottom.
             * If you don't wish to use a model from the library, and you
             don't have a prior model file, start with the PRImary tab.
             * You can also load a data file, which will be used to determine
             the covariates and maximum number of inputs.
             * If you load both model and data files, covariates in the data file
             will be used.
          ")
          )
        )
        help_count(help_count() + 1)
      })
      
      observeEvent(input$help_pri,{
        message(
          list(
            h4("Primary Parameters"),
            markdown("
            * Pmetrics estimates distributions for primary parameters only.
               Choose the number of parameters, whether to parameterize as range,
               mean/CV%, or mean/SD and enter names.
              * Range is the most common way to specify prior model parameter values
                     with a nonparametric statistical approach."),
            HTML("<ul><ul><li>"),
            withMathJax("Mean/CV% is the most common way to specify prior model parameter values
            with a parametric statistical approach. In this model, primary parameters are
            described as a typical (mean) value $\\theta$ in the population, distributed with
            interindividual variability, $\\eta$, assumed to be log-normally distributed with
            variance $\\omega^2$, such that an individual j has a parameter value of 
            $\\theta * e^{\\eta_j}$. 
            When reported as CV%, $\\omega = \\sqrt{log((\\frac{CV%}{100})^2 + 1)}$,
            from the entered $\\theta$ and CV%, Pmetrics will calculate a range 
            for the parameter  as $[\\theta * e^{-3\\omega}, \\theta * e^{3\\omega}]$"),
            HTML("</li><li>"),
            withMathJax("Mean/SD is the least common way to specify prior model parameter values
                        with a parameteric statistical approach because it implies an additive model
                        such that individual j has a parameter value of $\\theta + \\eta_j$, which
                        of course can be negative. For this reason, it is far more common to use
                        the exponential prior, typically reported as mean/CV%. However, if a parameter
                        itself is log transformed, then mean/SD is an appropriate prior specification."),
            HTML("</li></ul></ul>"),
            markdown("
            * Parameters are assumed to be random
            with unknown mean/variance. If a value is entered for either *Min* or *Mean*
            but *Max*/*SD*/*CV%* is blank, the parameter will be considered
            as *Fixed*, which means the same but unknown value in the population
            with unknown mean, zero variance. The entered value will be the starting
            value for the optimization. When a parameter is fixed, checking
            *Constant* will make the entered value the same for the population
            and it will not be optimized, i.e., known mean, zero variance.
            * Parameters can also be marked as *GTZ* or greater than zero, if they are to be kept positive.
            This is only relevant for parametric estimation, as nonparametric estimation will strictly respect boundaries."
                     
            )
          )
        )
        help_count(help_count() + 1)
      })
      
      observeEvent(input$help_cov,{
        if(ncov() > 0) {
          message(
            list(
              h4("Covariates"),
              HTML("<ul><li>"),
              cov_source(),
              HTML("</li></ul>"),
              markdown("
                * Check any covariates which are constant between measurements.
                * Leave unchecked if covariate is linearly interpolated between measurements         
                "
              )
            )
          )
        } else {
          message(
            list(
              h4("Covariates"),
              markdown("
              * No covariates available from model or data file.
              * You can still type any covariate name you want into the box below. 
              Use \"!\" to specify a piece-wise constant covariate, e.g. \"wt!\"."
              )
            )
          )
        }
        help_count(help_count() + 1)
      })
      
      observeEvent(input$help_sec,{
        message(
          list(
            h4("Secondary Parameters"),
            markdown("
            * Secondary variables are those that are defined by equations that are 
            combinations of primary, covariates, and other secondary variables. 
            * It is permissible to have conditional statements, but because expressions in
            this block are translated into variable declarations, expressions other
            than of the form `X = function(Y)` must be on a new line, prefixed by
            `&` and contain only variables which have been previously defined in the
            Primary, Covariate, or Secondary blocks.
            * Examples: 
              * `V = V0 * wt`
              * `& IF(male == 1) CL = CL_m`
            ")
          )
        )
        help_count(help_count() + 1)
      })
      
      observeEvent(input$help_ini,{
        message(
          list(
            h4("Initial Conditions"),
            markdown("
            * If any Primary or Secondary parameters are initial conditions,
            choose them here and edit the code if needed. 
            * You can only select as many parameters as you have model compartments (e.g. equations).
            ")
          )
        )
        help_count(help_count() + 1)
      })
      
      observeEvent(input$help_fa,{
        message(
          list(
            h4("Bioavailability (FA)"),
            markdown("
            * If any Primary or Secondary parameters are bioavailability,
            choose them here and edit the code if needed. 
            * You can only select as many parameters as you have inputs (e.g. drugs).
            "
            )
          )
        )
        help_count(help_count() + 1)
      })
      
      observeEvent(input$help_lag,{
        message(
          list(
            h4("Lag Time"),
            markdown("
            * If any Primary or Secondary parameters are lag times,
            choose them here and edit the code if needed. 
            * You can only select as many parameters as you have inputs (e.g. drugs).
            "
            )
          )
        )
        help_count(help_count() + 1)
      })
      
      observeEvent(input$help_eqn,{
        message(
          list(
            h4("Equations"),
            markdown("
            * Write the differential equations for your model here.
            * Use `dX[i]` for change in compartment amounts, where i is the compartment number, e.g. dX[1] or dX[2].
            * Compartment amounts are referred to as `X[i]`, e.g. X[1] or X[2].
            * Use `BOLUS[j]` for bolus input j and `RATEIV[k]` for infusion k.
            * Indeces j and k correspond to the INPUT column in the data file, 
            which is usually omitted and assumed to be 1 for all doses.
            * The DUR column in the data file determines whether a dose is treated 
            as a BOLUS (DUR = 0) or RATEIV (DUR > 0).
            * Any variable defined in PRI, COV, or SEC may be used.
            * Example: `dX[1] = RATEIV[1] * WT - Ke * X[1]`
            "
            )
          )
        )
        help_count(help_count() + 1)
      })
      
      observeEvent(input$help_out,{
        message(
          list(
            h4("Outputs"),
            markdown("
            * Outputs are referred to as *Y[i]*, where *i* is the output equation number, e.g. Y[1]. 
            * Compartments are referred to as *X[j]*, where *j* is the compartment number, e.g. X[1].
            * Any variable defined in PRI, COV, or SEC may be used, e.g. Y[1] = X[1]/V.
            "
            )
          )
        )
        help_count(help_count() + 1)
      })
      
      
    } #end server
  ) #end shinyApp
} #end build_model
