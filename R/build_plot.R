#' Launch Plot Builder app
#' 
#' Open the shiny plot builder app.
#' 
#' @details
#' The app will open in a separate window.
#' @param x Optional object to plot
#'
#' @return Launches the shiny app.
#' @export
#' @author Michael Neely
#' 

build_plot <- function(x,...) {
  
  requireNamespace("shiny")
  requireNamespace("bslib")
  requireNamespace("plotly")
  
  
  if(!missing(x)){
    choices_user <- tryCatch(deparse(substitute(x)), error = function(e) NULL)
    if(is.null(get(choices_user))){
      choices_user <- NULL
    }
  } else {
    choices_user <- NULL
  }
  
  ClassFilter <- function(x) any(grepl("^PM_result|^PM_model|^PM_data|^PM_sim|^PM_pta|^PM_cov|^PM_final",class(get(x))))
  choices2 <- Filter(ClassFilter,ls(globalenv()))
  
  choices <- unique(c(choices_user, choices2))
  
  if(length(choices)==0) {
    choices <- Filter(ClassFilter,ls("package:Pmetrics")) #load examples if none already loaded
  }


  
  shiny::shinyApp(
    
    ui <- bslib::page_sidebar(
      theme = bslib::bs_theme(bootswatch = "zephyr"),
      title = "Pmetrics Plot",
      
      sidebar = bslib::sidebar(
        width = 400,
        accordion(
          accordion_panel("Data",
                          selectInput("data","Choose a Pmetrics object to plot:",choices = choices),
                          uiOutput("DataControls")
          ),
          bslib::accordion_panel("Formatting",
                                 uiOutput("FormatControls")
          ),
          bslib::accordion_panel("Axes",
                                 uiOutput("AxesControls")
          )
        ) #end accordion Panel
        
      ), #end sidebarPanel
      
      
      h3("Copy and paste the code below into your R script to reproduce the plot:"),
      helpText("Note: If you accepted the default value for an argument,",
               "it is not necessary to include that argument in the call to plot",
               "and it has been omitted here, following standard R practice."),
      helpText(htmlOutput("help")),
      card(textOutput("plotCode"),
           max_height = "100px"),
      card(uiOutput("plotPM")),
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      )
      
    ), #end ui
    
    server <- function(input, output, session) {
      
      #########  HELPER FUNCTIONS #################
      
      getXlim <- function(){
        if(length(input$xmin)>0 & length(input$xmax)>0) {
          if(input$xmin!="" & input$xmax!="") {xlim <- as.numeric(c(input$xmin,input$xmax))} else {xlim <- NULL}
        } else {xlim <- NULL}
        return(xlim)
      }
      
      getYlim <- function(){
        if(length(input$ymin)>0 & length(input$ymax)>0) {
          if(input$ymin!="" & input$ymax!="") {ylim <- as.numeric(c(input$ymin,input$ymax))} else {ylim <- NULL}
        } else {ylim <- NULL}
        return(ylim)
      }
      
      getXlab <- function(){
        if(length(input$xlab)==0 & inherits(get(input$data),"PMmatrix")) return("Time (h)")
        if(length(input$xlab)==0) return(NULL)
        if(input$xlab=="") return(NULL)
        return(input$xlab)
      }
      
      getYlab <- function(){
        if(length(input$ylab)==0 & inherits(get(input$data),"PMmatrix")) return("Observation")
        if(length(input$ylab)==0) return(NULL)
        if(input$ylab=="") return(NULL)
        return(input$ylab)
      }
      
      getFormula <- function(x = "x", y = "y", charac = FALSE, choices){
        if (length(input[[x]])==0 || length(input[[y]])==0) return(NULL)
        if (input[[x]]=="Select" || input[[y]]=="Select") return(NULL)
        if (!input[[x]] %in% choices) return(NULL)
        if (!input[[y]] %in% choices) return(NULL)
        if(charac) {return(paste(input[[y]],input[[x]],sep="~"))
        } else {
          return(as.formula(paste(input[[y]],input[[x]],sep="~")))}
      }
      
      getProbs <- function(){
        if(length(input$probs)==0) {return(c(0.05,0.25,0.5,0.75,0.95))} else {return(as.numeric(input$probs))}
      }
      
      getPred <- function(icen){
        if(length(input$incl_pred)==0) return(NULL)
        if(input$incl_pred == "none") return(NULL)
        if(icen == "post") return(paste0(input$data,"$post"))
        if(icen == "pop") return(paste0(input$data,"$pop"))
      }
      
      getGroup <- function(group){
        x <- get(input$data)
        if(length(group)==0) {return(NULL)}
        colfac <- which(names(x)==group)
        if(length(colfac)>0){return(x[,colfac])}
        return(NULL)
      }
      
      setVal <- function(par, def){
        if(is.null(input[[par]])){
          return(def)
        } else {
          return(input[[par]])
        }
      }
      
      
      ############### Make Data Controls #####################
      
      #can use this for PM_result$data and for PM_data
      recycleDataControl <- function(src){
        if(src == "PM_result"){
          data_obj <- get(input$data)$data$standard_data
        } 
        
        if(src == "PM_data"){
          data_obj <- get(input$data)$standard_data
        }
        
        return(list(
          helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
          radioButtons("data_include","", c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
          selectInput("data_select", "", choices = unique(data_obj$id),
                      selected = unique(data_obj$id),
                      multiple = TRUE, selectize = FALSE),
          selectInput("outeq","Output equation:", choices = 1:max(data_obj$outeq, na.rm = TRUE), selected = 1),
          numericInput("block","Block number:", 1, min = 1, step = 1),
          selectInput("group","Grouping factor",choices = c("None" = "none", getCov(data_obj)$covnames)) #in PMutilities
        ))
      }
      
      
      makeDataControls <- function(){
        
        ############### Data: PM_data #####################
        if(!is.null(input$data) && inherits(get(input$data),"PM_data")){
          return(list(
            recycleDataControl("PM_data")
          ))
        }
        
        
        
        ############### Data: PM_result #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result")){
          return(list(
            radioButtons("res_sub","Plot which field?", 
                         selected = "dat",
                         c("Data" = "dat", 
                           "Model" = "mod",
                           "Obs/Pred" = "op",
                           "Final" = "fin",
                           "Cycle" = "cyc",
                           "Covariate" = "cov",
                           "Validation" = "valid"
                         )),
            
            ############### Data: PM_result$data #####################
            
            conditionalPanel(
              condition = "input.res_sub == 'dat'",
              recycleDataControl("PM_result") #end data conditional panel
            ),
            
            ############### Data: PM_result$op #####################
            
            conditionalPanel(
              condition = "input.res_sub == 'op'",
              radioButtons("pred.type","",c("Posterior Predictions" = "post","Population Predictions" = "pop"), selected = "post"),      
              checkboxInput("resid","Residual Plot",FALSE),
              selectInput("icen","Predictions based on:",choices=c("mean","median"),"median"),
              helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
              radioButtons("op_include","",c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
              selectInput("op_select", "", choices=unique(get(input$data)$op$id), selected=unique(get(input$data)$op$id), 
                          multiple = TRUE, selectize = FALSE),
              selectInput("outeq", "Output equation:", choices = 1:max(get(input$data)$op$outeq), selected = 1),
              selectInput("block", "Block:", choices = c("All",1:max(get(input$data)$op$block)))
            ), #end op conditional panel
            
            ############### Data: PM_result$final #####################
            
            conditionalPanel(
              condition = "input.res_sub == 'fin'",
              radioButtons("ptype","Plot type:",c("Univariate" = "uni", "Bivariate" = "bi")),
              conditionalPanel(
                condition="input.ptype=='bi'",
                selectInput("x","x-axis",choices=c("Select",names(get(input$data)$final$popMean)),selected = "Select"),
                selectInput("y","y-axis",choices=c("Select",names(get(input$data)$final$popMean)),selected = "Select")
              ) #end conditional panel
            ), #end final conditional panel
            
            ############### Data: PM_result$cycle #####################
            
            conditionalPanel(
              condition = "input.res_sub == 'cyc'",
              numericInput("omit","Proportion of burn-in cycles to omit:", 0.2, min = 0, max = 1, step = 0.1)
            ), #end cycle conditional panel
            
            ############### Data: PM_result$cov #####################
            
            conditionalPanel(
              condition = "input.res_sub == 'cov'",
              selectInput("covY", "Y-axis", 
                          choices=c("Select",names(get(input$data)$cov$data)[names(get(input$data)$cov$data) != "icen"]), 
                          selected = "Select"),
              selectInput("covX", "X-axis", 
                          choices=c("Select",names(get(input$data)$cov$data)[names(get(input$data)$cov$data) != "icen"]), 
                          selected = "Select"),      
              helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
              radioButtons("cov_include", "", c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
              selectInput("cov_select","", choices = unique(get(input$data)$cov$data$id),
                          selected = unique(get(input$data)$cov$data$id), multiple = T, selectize = FALSE),
              selectInput("icen","Summary method for changing covariates:",
                          choices = c("mean", "median", "mode", "none"), "mean")
            ) #end cov conditional panel
            
            ############### Data: PM_result$model #####################
            
            # not needed
            
            
          )) #end return list
        } #end PM_result
        
        ############### Data: PM_sim #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_sim")){
          x <- get(input$data)$data
          if(inherits(x, "PM_simlist")){
            simlist_sims <- length(x)
          } else {
            simlist_sims <- 0
          }
          OPFilter <- function(x) any(grepl("^PM_result",class(get(x))))
          OPchoices <- Filter(OPFilter,ls(globalenv()))
          if(length(OPchoices)==0){OPchoices <- "None"} else {OPchoices <- c("None", OPchoices)}
          
          
          return(list(
            conditionalPanel(
              condition = "input.simlist_sims > 0",
              numericInput("simChooser","Which simulation?", value = 1, 
                           min = 1, max = simlist_sims,
                           step = 1)
            ),
            selectInput("outeq","Output equation:",choices=1:max(x[[1]]$obs$outeq,na.rm=T),selected=1),
            selectInput("sim_obs","Observed (for VPC)",choices = OPchoices)
          )) #end list
        } #end PMsim
        
        ############### Data: PM_model #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_model")){
          
          return(NULL)
        }
        
      } #end makeDataControls function
      
      ############### Make Format Controls #####################
      makeFormatControls <- function(){
        
        ############### Format: PM_result$cov #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") 
           && !is.null(input$res_sub) && input$res_sub == "cov"){
          return(list(
            accordion(
              accordion_panel(
                "Line Options",
                bslib::navset_card_tab(
                  bslib::nav_panel(
                    "Linear",
                    checkboxInput("cov_lm", "Include?", FALSE),
                    checkboxInput("def_lm_fmt","Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_lm_fmt",
                      numericInput("lm_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      textInput("lm_col", "Color:", "dodgerblue"),
                      numericInput("lm_lwd", "Line width", 1, step = 0.5),
                      selectInput("lm_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  ),
                  bslib::nav_panel(
                    "Loess",
                    checkboxInput("cov_loess", "Include?", TRUE),
                    checkboxInput("def_loess_fmt","Use default formatting",TRUE),
                    conditionalPanel(
                      condition = "!input.def_loess_fmt",
                      numericInput("loess_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      textInput("loess_col","Color:", "dodgerblue"),
                      numericInput("loess_lwd", "Line width", 1, step = 0.5),
                      selectInput("loess_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash")
                    )
                  ),
                  bslib::nav_panel(
                    "Reference",
                    checkboxInput("cov_ref", "Include?", FALSE),
                    checkboxInput("def_ref_fmt","Use default formatting",TRUE),
                    conditionalPanel(
                      condition = "!input.def_ref_fmt",
                      textInput("ref_col","Color:", "dodgerblue"),
                      numericInput("ref_lwd","Line width", 1, step = 0.5),
                      selectInput("ref_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  )
                )
              ), #end accordion panel
              accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt","Use default formatting",TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  textInput("mrk_col","Color:", "orange"),
                  textInput("mrk_symbol","Symbol","circle"),
                  numericInput("mrk_size","Size", 10, step = 1),
                  numericInput("mrk_opacity","Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd","Outline width", 1, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                )
              ), #end accordion_panel
              accordion_panel(
                "Plot Options",
                checkboxInput("log","Log-log plot", FALSE),
                checkboxInput("grid","Grid", TRUE),
                helpText("Only relevant for linear regression"),
                checkboxInput("def_stats_fmt","Use default statistics formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_stats_fmt",
                  textInput("stats_col","Color:","black"),
                  numericInput("stats_size","Size", 14, step = 1),
                  checkboxInput("stats_bold","Bold?", FALSE),
                  numericInput("stats_x","Horizontal pos", 0.8, step = 0.1),
                  numericInput("stats_y","Vertical pos", 0.1, step = 0.1)
                ),
                checkboxInput("def_title_fmt","Omit title", TRUE),
                conditionalPanel(
                  condition = "!input.def_title_fmt",
                  textInput("title_text", "Title:", ""),
                  textInput("title_col","Color:", "black"),
                  numericInput("title_size","Size", 20, step=1),
                  checkboxInput("title_bold","Bold?", TRUE)
                )
                
              ) #end accordion_panel
            ) #end accordion
          )) #end list
        } #end PMcov
        
        ############### Format: PM_result$final (NPAG) #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") 
           && !is.null(input$res_sub) && input$res_sub == "fin" & inherits(get(input$data)$final,"NPAG")){
          return(list(
            conditionalPanel(
              condition="input.ptype == 'uni'",
              bslib::navset_card_tab(
                bslib::nav_panel(
                  "Bars",
                  checkboxInput("def_bar_fmt", "Use default formatting", TRUE),
                  conditionalPanel(
                    condition = "!input.def_bar_fmt",
                    textInput("bar_col","Color:","dodgerblue"),
                    numericInput("bar_width","Width:", value = 0.02, min = 0, step = 0.01),
                    numericInput("bar_opacity", "Opacity:", value = 0.5, min = 0, max = 1, step = 0.1),
                    textInput("bar_lcol", "Outline Color:", "black"),
                    numericInput("bar_lwd", "Outline Width:", value = 1, min = 0)
                  )
                ),
                bslib::nav_panel(
                  "Density Line",
                  checkboxInput("incl_line","Include?", FALSE),
                  checkboxInput("def_line_fmt","Use default formatting", TRUE),
                  conditionalPanel(
                    condition = "!input.def_line_fmt",
                    textInput("line_col","Color:","black"),
                    numericInput("line_width","Width:", value = 1),
                    selectInput("line_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.ptype == 'bi'",
              bslib::navset_card_tab(
                bslib::nav_panel(
                  "Markers",
                  checkboxInput("def_mrk_fmt","Use default formatting", TRUE),
                  conditionalPanel(
                    condition = "!input.def_mrk_fmt",
                    textInput("mrk_col","Color:", "dodgerblue"),
                    textInput("mrk_symbol","Symbol","circle"),
                    numericInput("mrk_size","Size", 5, step = 1),
                    numericInput("mrk_opacity","Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                    numericInput("mrk_lwd","Outline width", 1, step = 0.5),
                    textInput("mrk_lcol", "Outline color:", "black")
                  )
                ),
                bslib::nav_panel(
                  "Drop Lines",
                  checkboxInput("incl_drop", "Include?", TRUE),
                  checkboxInput("def_drop_fmt","Use default formatting", TRUE),
                  conditionalPanel(
                    condition = "!input.def_drop_fmt",
                    textInput("drop_col","Color:","black"),
                    numericInput("drop_width","Width:", value = 1),
                    selectInput("drop_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash")
                  )
                )
              )
            ) #end conditional panel
          )) #end list
        } #end PM_final, NPAG
        
        ############### Format: PM_result$final IT2B #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") 
           && !is.null(input$res_sub) && input$res_sub == "fin" & inherits(get(input$data)$final,"IT2B")){
          return(list(
            conditionalPanel(
              condition="input.ptype=='uni'",
              checkboxInput("standard","Standardize IT2B marginals",FALSE),
              numericInput("lwd1","Line width:",4),
              textInput("col1","Color:","red")
            ),
            conditionalPanel(
              condition="input.ptype=='bi'",
              checkboxInput("grid","Grid",TRUE),
              checkboxInput("legend","Legend",TRUE),
              numericInput("lwd2","Line width:",1),
              textInput("col2","Color:","white"),
              numericInput("cex","Point size:",1,step=0.1),
              numericInput("pch","Plotting character:",3,min=1,step=1),
              selectInput("probs","Quantiles",choices=c(0.01,0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.975,0.99),selected=c(0.05,0.25,0.5,0.75,0.95),multiple=T)
            ) #end conditional panel
          )) #end list
        } #end PM_final, IT2B
        
        ############### Format: PM_result$op #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") 
           && !is.null(input$res_sub) && input$res_sub == "op"){
          return(list(
            accordion(
              accordion_panel(
                "Line Options",
                # h4("Line Options:"),
                bslib::navset_card_tab(
                  bslib::nav_panel(
                    "Linear",
                    checkboxInput("op_lm", "Include?", TRUE),
                    checkboxInput("def_lm_fmt","Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_lm_fmt",
                      numericInput("lm_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      textInput("lm_col", "Color:", "dodgerblue"),
                      numericInput("lm_lwd", "Line width", 1, step = 0.5),
                      selectInput("lm_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  ),
                  bslib::nav_panel(
                    "Loess",
                    checkboxInput("op_loess", "Include?",FALSE),
                    checkboxInput("def_loess_fmt","Use default formatting",TRUE),
                    conditionalPanel(
                      condition = "!input.def_loess_fmt",
                      numericInput("loess_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      textInput("loess_col","Color:", "dodgerblue"),
                      numericInput("loess_lwd", "Line width", 1, step = 0.5),
                      selectInput("loess_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash")
                    )
                  ),
                  bslib::nav_panel(
                    "Reference",
                    checkboxInput("op_ref", "Include?",TRUE),
                    checkboxInput("def_ref_fmt","Use default formatting",TRUE),
                    conditionalPanel(
                      condition = "!input.def_ref_fmt",
                      textInput("ref_col","Color:", "dodgerblue"),
                      numericInput("ref_lwd","Line width", 1, step = 0.5),
                      selectInput("ref_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  )
                )
              ), #end accordion panel
              accordion_panel(
                "Marker Options",
                # h4("Marker Options:"),
                checkboxInput("def_marker_fmt","Use default formatting",TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  textInput("mrk_col","Color:", "orange"),
                  textInput("mrk_symbol","Symbol","circle"),
                  numericInput("mrk_size","Size", 10, step = 1),
                  numericInput("mrk_opacity","Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd","Outline width", 1, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                )
              ), #end accordion_panel
              accordion_panel(
                "Plot Options",
                numericInput("mult","Multiplication factor for axes:","1"),
                checkboxInput("log","Log-log plot", FALSE),
                checkboxInput("grid","Grid", TRUE),
                helpText("Only relevant for linear regression"),
                checkboxInput("def_stats_fmt","Use default statistics formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_stats_fmt",
                  textInput("stats_col","Color:","black"),
                  numericInput("stats_size","Size", 14, step = 1),
                  checkboxInput("stats_bold","Bold?", FALSE),
                  numericInput("stats_x","Horizontal pos", 0.8, step = 0.1),
                  numericInput("stats_y","Vertical pos", 0.1, step = 0.1)
                ),
                checkboxInput("def_title_fmt","Omit title", TRUE),
                conditionalPanel(
                  condition = "!input.def_title_fmt",
                  textInput("title_text", "Title:", ""),
                  textInput("title_col","Color:", "black"),
                  numericInput("title_size","Size", 20, step=1),
                  checkboxInput("title_bold","Bold?", TRUE)
                )
                
              ) #end accordion_panel
            ) #end accordion
          )) #end list
        } #end PM_op 
        
        ############### Format: PM_result$cycle #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") 
           && !is.null(input$res_sub) && input$res_sub == "cyc"){
          
          return(list(
            accordion(
              accordion_panel(
                "Line Options",
                checkboxInput("def_ab_fmt","Use default formatting",TRUE),
                conditionalPanel(
                  condition = "!input.def_ab_fmt",
                  bslib::navset_card_tab(
                    bslib::nav_panel(
                      "Both Rows",
                      helpText("Applies to all plots"),
                      numericInput("ab_lwd", "Line width", 1, step = 0.5),
                    ),
                    bslib::nav_panel(
                      "Row A",
                      helpText("Applies to LL, AIC, gamma/lambda"),
                      textInput("a_col", "Color:", "dodgerblue"),
                      selectInput("a_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    ),
                    bslib::nav_panel(
                      "Row B",
                      helpText("Applies to normalized parameter statistics"),
                      helpText("Choose colors for each parameter trace. Select 'Other' for custom."),
                      selectInput("b_col", "Color palette:",
                                  choices = c(row.names(RColorBrewer::brewer.pal.info), "Other"), selected = "Spectral"),
                      conditionalPanel(
                        condition = "input.b_col == 'Other'",
                        helpText("Enter color names separated by commas. Values will be recycled as needed."),
                        textInput("b_custom_colors", "Custom Colors:", value = "")
                      ),
                      helpText("Choose dash styles for each parameter trace. Values will be recycled if needed."),
                      selectInput("b_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash", multiple = TRUE)
                    ) #end nav_panel
                  ) #end navset_card_tab
                ) #end conditional panel
              ), #end accordion panel
              accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt","Use default formatting",TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  textInput("mrk_col","Color:", "dodgerblue"),
                  textInput("mrk_symbol","Symbol","circle"),
                  numericInput("mrk_size","Size", 4, step = 1),
                  numericInput("mrk_opacity","Opacity", value = 1, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd","Outline width", 0, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                ),
              ), #end accordion_panel
              accordion_panel(
                "Plot Options",
                checkboxInput("grid","Grid", TRUE),
              ) #end accordion_panel
            ) #end accordion
          )) #end list  
        } #end PM_cycle
        
        ############### Format: PM_result$data #####################
        
        if(!is.null(input$data) && 
           (inherits(get(input$data),"PM_data") |
            (inherits(get(input$data),"PM_result") 
             && !is.null(input$res_sub) && input$res_sub == "dat"))){
          
          return(list(
            accordion(
              accordion_panel(
                "Line Options",
                # h4("Line Options:"),
                bslib::navset_card_tab(
                  bslib::nav_panel(
                    "Join",
                    checkboxInput("join", "Include?", TRUE),
                    checkboxInput("def_join_fmt","Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_join_fmt",
                      textInput("join_col", "Color:", "dodgerblue"),
                      numericInput("join_lwd", "Line width", 1, step = 0.5),
                      selectInput("join_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  ),
                  bslib::nav_panel(
                    "Pred",
                    radioButtons("incl_pred","Include Predictions?",c("None" = "none", "Population" = "pop", "Posterior" = "post")),
                    checkboxInput("def_pred_fmt","Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_pred_fmt",
                      textInput("pred_col","Color:", ""),
                      numericInput("pred_lwd", "Line width", 1, step = 0.5),
                      selectInput("pred_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash")
                    )
                  ) #end nav_panel
                ) #end navset_card_tab
              ), #end accordion panel
              accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt","Use default formatting",TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  textInput("mrk_col","Color:", "red"),
                  textInput("mrk_symbol","Symbol","circle"),
                  numericInput("mrk_size","Size", 10, step = 1),
                  numericInput("mrk_opacity","Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd","Outline width", 1, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                ),
              ), #end accordion_panel
              accordion_panel(
                "Plot Options",
                checkboxInput("tad", "Use time after dose", FALSE),
                numericInput("mult","Multiplication factor for axes:", value = 1, min = 0, step = 1),
                checkboxInput("log","Semi-log plot", FALSE),
                checkboxInput("grid","Grid", FALSE),
                checkboxInput("def_title_fmt","Omit title", TRUE),
                conditionalPanel(
                  condition = "!input.def_title_fmt",
                  textInput("title_text", "Title:", ""),
                  textInput("title_col","Color:", "black"),
                  numericInput("title_size","Size", 20, step=1),
                  checkboxInput("title_bold","Bold?", TRUE)
                )
              ), #end accordion_panel
              accordion_panel(
                "Group Options",
                conditionalPanel(
                  condition = "input.group !== 'none'",
                  helpText("One name per group, separate by commas"),
                  textInput("group_names","Group Names:",
                            value = paste(unique(get(input$data)$data$data[[input$group]]), collapse = ", ")
                  ),
                  checkboxInput("legend","Legend", value = ifelse(input$group != 'none', TRUE, FALSE)),
                  selectInput("group_col", "Color palette for groups:",
                              choices = row.names(RColorBrewer::brewer.pal.info), selected = "Set1")
                ),
                conditionalPanel(
                  condition = "input.group == 'none'",
                  helpText("Define groups in data")
                )
              ) #end accordion panel
            ) #end accordion
          )) #end list  
        } #end PM_data
        
        ############### Format: PM_sim #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_sim")){
          return(
            list(
              accordion(
                accordion_panel(
                  "Line Options",
                  bslib::nav_panel(
                    "Line",
                    checkboxInput("sim_line", "Include?", TRUE),
                    checkboxInput("def_sim_line_fmt","Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_sim_line_fmt",
                      numericInput("sim_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      selectInput("sim_probs","Quantiles",
                                  choices=c(NA,0.01,0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.975,0.99),
                                  selected=c(0.05,0.25,0.5,0.75,0.95), multiple = TRUE),
                      textInput("sim_col", "Color:", "dodgerblue"),
                      numericInput("sim_lwd", "Line width", 1, step = 0.5),
                      selectInput("sim_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  )
                ), #end accordion panel
                accordion_panel(
                  "Marker Options",
                  checkboxInput("def_sim_marker_fmt","Use default formatting",TRUE),
                  conditionalPanel(
                    condition = "!input.def_sim_marker_fmt",
                    textInput("mrk_col","Color:", "black"),
                    textInput("mrk_symbol","Symbol","circle-open"),
                    numericInput("mrk_size","Size", 8, step = 1),
                    numericInput("mrk_opacity","Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                    numericInput("mrk_lwd","Outline width", 1, step = 0.5),
                    textInput("mrk_lcol", "Outline color:", "black")
                  ),
                ), #end accordion_panel
                accordion_panel(
                  "Plot Options",
                  numericInput("binSize","Bin Size", value = 0, min = 0, step = 1),
                  numericInput("mult","Multiplication factor for axes:", value = 1),
                  checkboxInput("log","Semi-log plot", TRUE),
                  checkboxInput("grid","Grid", TRUE),
                  checkboxInput("def_title_fmt","Omit title", TRUE),
                  conditionalPanel(
                    condition = "!input.def_title_fmt",
                    textInput("title_text", "Title:", ""),
                    textInput("title_col","Color:", "black"),
                    numericInput("title_size","Size", 20, step=1),
                    checkboxInput("title_bold","Bold?", TRUE)
                  )
                )
              ) #end accordion
            )) #end list
        } #end PM_sim
        
        ############### Format: PM_model #####################
        
        if(!is.null(input$data) && 
           (inherits(get(input$data),"PM_model") |
            (inherits(get(input$data),"PM_result") &&
             !is.null(input$res_sub) && input$res_sub == "mod"))){
          
          return(list(
            accordion(
              accordion_panel(
                "Line Options",
                checkboxInput("join", "Include?", TRUE),
                checkboxInput("def_join_fmt","Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_join_fmt",
                  textInput("join_col", "Color:", "black"),
                  numericInput("join_lwd", "Line width", 1, step = 0.5),
                  selectInput("join_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                )
                
              ), #end accordion panel
              accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt","Use default formatting",TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  textInput("mrk_col","Color:", "dodgerblue"),
                  numericInput("mrk_size","Size", value = 0.25, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_opacity","Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd","Outline width", 0.5, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                )
              )
            ) #end accordion
          ))
        } #end PM_model
        
      } #end makeFormatControls function
      
      ############### Make Axis Controls #####################
      makeAxisControls <- function(){
        
        ############### Axis: all but PM_data #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_sim") |
           (inherits(get(input$data),"PM_result") && input$res_sub %in% c("cov","op","fin"))){
          # if(inherits(get(input$data),c("PMcov","PMfinal","PMop", "PMsim"))){
          return(list(
            h3("Axes"),
            textInput("xmin","X min"),
            textInput("xmax","X max"),
            textInput("ymin","Y min"),
            textInput("ymax","Y max"),
            textInput("xlab","X label"),
            textInput("ylab","Y label"),
            numericInput("axis_label_size","Axis label size",1.2,step=0.1)
          )) #end list
        } #end PM_cov, PM_final, PM_op, PM_sim
        
        
        ############### Axis: PM_result$data #####################
        
        if(!is.null(input$data) && 
           (inherits(get(input$data),"PM_data") |
            (inherits(get(input$data),"PM_result") && input$res_sub == "dat"))){
          
          return(list(
            h3("Axes"),
            textInput("xmin","X min"),
            textInput("xmax","X max"),
            textInput("ymin","Y min"),
            textInput("ymax","Y max"),
            textInput("xlab","X label","Time (h)"),
            textInput("ylab","Y label","Observation"),
            numericInput("axis_label_size","Axis label size",1.2,step=0.1)
          )) #end list
        } #end PM_data
        
        if(!is.null(input$data) && 
           (inherits(get(input$data),"PM_model") |
            (inherits(get(input$data),"PM_result") && input$res_sub == "mod"))){
          return(NULL)
        }
      } #end makeAxisControls
      
      ############### Make PMplot and Statement #####################
      makePMplot <- function(code = FALSE){
        
        
        if(!is.null(input$data) && 
           (inherits(get(input$data),"PM_data") |
            inherits(get(input$data),"PM_result"))){
          
          ############### Plot and Code: PM_result$cov #####################
          
          if(!is.null(input$res_sub) && input$res_sub == "cov"){
            #x argument
            x <- get(input$data)$cov
            
            #line argument
            cov_lm <- setVal("cov_lm", FALSE)
            def_lm_fmt <- setVal("def_lm_fmt", TRUE)
            def_lm_args <- list(ci = 0.95, color = "dodgerblue", width = 1, dash = "solid")
            lm_ci <- setVal("lm_ci", def_lm_args$ci)
            lm_col <- setVal("lm_col", def_lm_args$color)
            lm_lwd <- setVal("lm_lwd", def_lm_args$width)
            lm_dash <- setVal("lm_dash", def_lm_args$dash)
            
            if(cov_lm){ #yes have lm
              if(!def_lm_fmt){ #not default format?
                line <- list(lm = list(ci = lm_ci, color = lm_col, width = lm_lwd, dash = lm_dash))
                line$lm <- line$lm[!line$lm %in% def_lm_args] #keep only non-defaults
                if(length(line$lm)==0) line$lm <- TRUE
              } else { #default format
                line <- list(lm = TRUE) 
              }
            } else { #don't have lm
              line <- list() #default with cov plot
            }
            
            cov_loess <- setVal("cov_loess", TRUE)
            def_loess_fmt <- setVal("def_loess_fmt", TRUE)
            def_loess_args <- list(ci = 0.95, color = "dodgerblue", width = 1, dash = "dash")
            loess_ci <- setVal("loess_ci", def_loess_args$ci)
            loess_col <- setVal("loess_col", def_loess_args$color)
            loess_lwd <- setVal("loess_lwd", def_loess_args$with)
            loess_dash <- setVal("loess_dash", def_loess_args$dash)
            
            if(cov_loess){
              if(!def_loess_fmt){ #not default format?
                line <- modifyList(line, list(loess = list(ci = loess_ci, color = loess_col, width = loess_lwd, dash = loess_dash)))
                line$loess <- line$loess[!line$loess %in% def_loess_args] #keep only non-defaults
                if(length(line$loess)==0) line$loess <- TRUE
              }
            } else { #don't have loess
              line <- modifyList(line, list(loess = FALSE)) #default for this is TRUE
            }
            
            cov_ref <- setVal("cov_ref", FALSE) 
            def_ref_fmt <- setVal("def_ref_fmt", TRUE)
            def_ref_args <- list(color = "black", width = 1, dash = "dash")
            ref_col <- setVal("ref_col", def_ref_args$color)
            ref_lwd <- setVal("ref_lwd", def_ref_args$width)
            ref_dash <- setVal("ref_dash", def_ref_args$dash)
            
            if(cov_ref){
              if(!def_ref_fmt){
                line <- modifyList(line, list(ref = list(color = ref_col, width = ref_lwd, dash = ref_dash)))
                line$ref <- line$ref[!line$ref %in% def_ref_args] #keep only non-defaults
                if(length(line$ref)==0) line$ref <- TRUE
              } else {
                line <- modifyList(line, list(ref = TRUE))
              }
            }
            
            
            #marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(color = "orange", symbol = "circle", size = 10, opacity = 0.5,
                                    line = list(width = 1, color = "black"))
            
            mrk_col <- setVal("mrk_col", "orange")
            mrk_symbol <- setVal("mrk_symbol", "circle")
            mrk_size <- setVal("mrk_size", 10)
            mrk_opacity <- setVal("mrk_opacity", 0.5)
            mrk_lwd <- setVal("mrk_lwd", 1)
            mrk_lcol <- setVal("mrk_lcol", "black")
            
            if(def_marker_fmt){
              marker <- TRUE
            } else {
              marker <- list(color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                             opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol))
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] #keep only non-defaults
              if(length(marker$line)==0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args] 
              if(length(marker)==0) marker <- NULL
              
            }
            
            #include/exclude arguments
            if(!is.null(input$cov_include)){
              if(input$cov_include=="yes"){
                include <- input$cov_select
                exclude <- NULL
              } else {
                include <- NULL
                exclude <- input$cov_select
              }
            } else {
              include <- exclude <- NULL
            }
            
            #title argument
            def_title_fmt <- setVal("def_title_fmt", TRUE)
            def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
            title_text <- setVal("title_text", "")
            title_col <- setVal("title_col", "black")
            title_bold <- setVal("title_bold", TRUE)
            title_size <- setVal("title_size", 20)
            if(def_title_fmt){
              title <- ""
            } else {
              title <- list(text = title_text, font = list(color = title_col, size = title_size,
                                                           bold = title_bold))
              title$font <- title$font[!title$font %in% def_title_args$font] #keep only non-defaults
              if(length(title$font)==0) title$font <- NULL
              
            }
            
            #stats argument
            def_stats_fmt <- setVal("def_stats_fmt", TRUE)
            def_stats_args <- list(x = 0.8, y = 0.1, font = list(color = "black", size = 14, bold = FALSE))
            stats_col <- setVal("stats_col", "black")
            stats_size <- setVal("stats_size", 14)
            stats_bold <- setVal("stats_bold", FALSE)
            stats_x <- setVal("stats_x", 0.8)
            stats_y <- setVal("stats_y", 0.1)
            if(def_stats_fmt){
              stats <- TRUE
            } else {
              stats <- list(x = stats_x, y = stats_y, font = list(color = stats_col, size = stats_size,
                                                                  bold = stats_bold))
              stats$font <- stats$font[!stats$font %in% def_stats_args$font] #keep only non-defaults
              if(length(stats$font)==0) stats$font <- NULL
              stats <- stats[!stats %in% def_stats_args] 
              if(length(stats)==0) stats <- NULL
            }
            
            #formula
            cov_choices <- names(get(input$data)$cov$data)
            cov_choices <- cov_choices[cov_choices != "icen"]
            
            formula <- getFormula(x = "covX", y = "covY", 
                                  choices = cov_choices)
            
            #Other defaults
            icen <- setVal("icen", "median")
            log <- setVal("log", FALSE)
            grid <- setVal("grid", TRUE)
            
            
            args <- list(x = x, 
                         formula = formula,
                         line = line, 
                         marker = marker, 
                         icen = icen, 
                         include = include, exclude = exclude,
                         log = log, grid = grid,
                         title = title, stats = stats,
                         xlab = getXlab(), ylab = getYlab(),
                         xlim = getXlim(), ylim = getYlim()) 
            args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
            
            #now make the code
            Name <- paste0(input$data,"$cov$plot(")
            #default args
            def_args <- list(
              formula = NULL,
              line = list(lm = FALSE, loess = TRUE, ref = FALSE),
              marker = TRUE,
              icen = "median", 
              include = as.character(unique(get(input$data)$cov$data$id)), exclude = NULL, 
              log = FALSE, 
              grid = TRUE,
              xlab = NULL, ylab = NULL,
              title = "",
              stats = TRUE,
              xlim = NULL, ylim = NULL
            )
            
            arglist <- args[-1] #remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist),names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]) )] #remove args that are in default list
            if(length(arglist)>0){
              arglist <- paste(deparse(arglist), collapse = "") %>% stringr::str_replace("^list\\(","") %>%
                stringr::str_replace_all("(\\d+)L","\\1") %>% stringr::str_replace_all(" +"," ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }
            
            if(!is.null(formula)){
              p <- do.call(plot, args)
              if(code){
                return(codeStatement)
              } else {
                return(p)
              }
            } else {
              return(NULL)
            }
            
          } #end PMcov
          
          
          ############### Plot and Code: PM_result$final, NPAG #####################
          
          if(!is.null(input$res_sub) && 
             (input$res_sub == "fin" & inherits(get(input$data)$final,"NPAG"))){
            
            ptype <- setVal("ptype", "uni")
            
            #x argument
            x <- get(input$data)$final
            
            #marker argument
            def_bar_fmt <- setVal("def_bar_fmt", TRUE)
            def_mrk_fmt <- setVal("def_mrk_fmt", TRUE)
            
            def_bar_args <- list(color = "dodgerblue", 
                                 width = 0.02, opacity = 0.5,
                                 line = list(color = "black", width = 1))
            def_mrk_args <- list(color = "dodgerblue", size = 5, symbol = "circle",
                                 opacity = 0.5,
                                 line = list(color = "black", width = 1))
            
            bar_col <- setVal("bar_col", def_bar_args$color)
            bar_width <- setVal("bar_width", def_bar_args$width)
            bar_size <- setVal("bar_size", def_bar_args$size)
            bar_shape <- setVal("bar_shape", def_bar_args$shape)
            bar_opacity <- setVal("bar_opacity", def_bar_args$opacity)
            bar_lcol <- setVal("bar_lcol", def_bar_args$line$color)
            bar_lwd <- setVal("bar_lwd", def_bar_args$line$width)
            
            mrk_col <- setVal("mrk_col", def_mrk_args$color)
            mrk_width <- setVal("mrk_width", def_mrk_args$width)
            mrk_size <- setVal("mrk_size", def_mrk_args$size)
            mrk_symbol <- setVal("mrk_symbol", def_mrk_args$symbol)
            mrk_opacity <- setVal("mrk_opacity", def_mrk_args$opacity)
            mrk_lcol <- setVal("mrk_lcol", def_mrk_args$line$color)
            mrk_lwd <- setVal("mrk_lwd", def_mrk_args$line$width)
            
            if(ptype == "uni"){
              if(def_bar_fmt){
                marker <- TRUE
              } else {
                marker <- list(color = bar_col, width = bar_width,
                               opacity = bar_opacity,
                               line = list(color = bar_lcol, width = bar_lwd))
                marker$line <- marker$line[!marker$line %in% def_bar_args$line] #keep only non-defaults
                if(length(marker$line)==0) marker$line <- NULL
                marker <- marker[!marker %in% def_bar_args] 
                if(length(marker)==0) marker <- NULL
                
              }
            }
            
            if(ptype == "bi"){
              if(def_mrk_fmt){
                marker <- TRUE
              } else {
                marker <- list(color = mrk_col, size = mrk_size,
                               symbol = mrk_symbol, opacity = mrk_opacity,
                               line = list(color = mrk_lcol, width = mrk_lwd))
                marker$line <- marker$line[!marker$line %in% def_mrk_args$line] #keep only non-defaults
                if(length(marker$line)==0) marker$line <- NULL
                marker <- marker[!marker %in% def_mrk_args] 
                if(length(marker)==0) marker <- NULL
                
              }
            }
            
            #line format
            
            incl_line <- setVal("incl_line", FALSE)
            incl_drop <- setVal("incl_drop", TRUE)
            
            def_line_fmt <- setVal("def_line_fmt", TRUE)
            def_line_args <- list(color = "black", width = 1, 
                                  dash = "solid")
            def_line_color <- setVal("line_col", def_line_args$color)
            def_line_width <- setVal("line_width", def_line_args$width)
            def_line_dash <- setVal("line_dash", def_line_args$dash)
            
            def_drop_fmt <- setVal("def_drop_fmt", TRUE)
            def_drop_args <- list(color = "black", width = 1, 
                                  dash = "dash")
            def_drop_color <- setVal("drop_col", def_drop_args$color)
            def_drop_width <- setVal("drop_width", def_drop_args$width)
            def_drop_dash <- setVal("drop_dash", def_drop_args$dash)
            
            
            if(ptype == "uni"){
              if(incl_line){ #we have a density line
                if(def_line_fmt){ #use default
                  line <- TRUE
                } else {
                  line = list(color = def_line_color, width = def_line_width, dash = def_line_dash)
                }
              } else {
                line <- FALSE
              }
              line <- line[!line %in% def_line_args] 
              if(length(line)==0) line <- NULL
            }
            
            if(ptype == "bi"){
              if(incl_drop){ #we have drop lines
                if(def_drop_fmt){ #use default
                  line <- TRUE
                } else {
                  line = list(color = def_drop_color, width = def_drop_width, dash = def_drop_dash)
                }
              } else {
                line <- FALSE
              }
              
              line <- line[!line %in% def_drop_args] 
              if(length(line)==0) line <- NULL
            }
            
            
            
            args <- list(x = x, 
                         formula = getFormula(choices = names(get(input$data)$final$popMean)),
                         line = line, 
                         marker = marker,
                         xlim = getXlim(), ylim = getYlim(),
                         xlab = getXlab(), ylab = getYlab())
            
            if(ptype == "uni"){args$formula <- NULL} #reset formula
            
            
            args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
            
            #now make the code
            Name <- paste0(input$data,"$final$plot(")
            #default args
            def_args <- list(
              formula = NULL,
              line = ifelse(ptype == "uni", FALSE, TRUE),
              marker = TRUE,
              xlab = NULL, ylab = NULL, zlab = NULL,
              title = "",
              xlim = NULL, ylim = NULL
            )
            
            arglist <- args[-1] #remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist),names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]) )] #remove args that are in default list
            
            if(length(arglist)>0){
              arglist <- paste(deparse(arglist), collapse = "") %>% stringr::str_replace("^list\\(","") %>%
                stringr::str_replace_all("(\\d+)L","\\1") %>% stringr::str_replace_all(" +"," ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }
            
            p <- do.call(plot, args)
            if(code){
              return(codeStatement)
            } else {
              return(p)
            }
            
          } #end PMfinal, NPAG
          
          ############### Plot and Code: PM_result$final, IT2B #####################
          
          if(!is.null(input$res_sub) &&
             (input$res_sub == "fin" & inherits(get(input$data)$final,"IT2B"))){
            
            x <- get(input$data)
            args <- list(x=x,formula=getFormula(choices=names(get(input$data)$popMean)),cex.lab=input$cex.lab,col=getFinalPlotType()$col,
                         pch=input$pch,cex=input$cex,lwd=getFinalPlotType()$lwd,probs=getProbs(),standard=input$standard,legend=input$legend,
                         grid=input$grid,xlim=getXlim(),ylim=getYlim(),xlab=getXlab(),ylab=getYlab())
            if(getFinalPlotType()$ptype=="uni"){args$formula <- NULL} #reset formula
            args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
            do.call(plot.PMfinal,args)
          } #end PMfinal, IT2B
          
          ############### Plot and Code: PM_result$op #####################
          
          if(!is.null(input$res_sub) && input$res_sub == "op"){
            
            #x argument
            x <- get(input$data)$op
            
            #resid sets different defaults for lines to follow
            resid <- setVal("resid", FALSE)
            
            #line argument
            op_lm <- setVal("op_lm", ifelse(resid, FALSE, TRUE))
            def_lm_fmt <- setVal("def_lm_fmt", TRUE)
            def_lm_args <- list(ci = 0.95, color = "dodgerblue", width = 1, dash = "solid")
            lm_ci <- setVal("lm_ci", def_lm_args$ci)
            lm_col <- setVal("lm_col", def_lm_args$color)
            lm_lwd <- setVal("lm_lwd", def_lm_args$width)
            lm_dash <- setVal("lm_dash", def_lm_args$dash)
            
            if(op_lm){ #yes have lm
              if(!def_lm_fmt){ #not default format?
                line <- list(lm = list(ci = lm_ci, color = lm_col, width = lm_lwd, dash = lm_dash))
                line$lm <- line$lm[!line$lm %in% def_lm_args] #keep only non-defaults
                if(length(line$lm)==0) line$lm <- TRUE
              } else { #default format
                if(resid) {
                  line <- list(lm = TRUE) #only need if residual plot, as default for op plot
                } else {
                  line <- list() #we have lm but don't need with default format and op plot
                }
              }
            } else { #don't have lm
              line <- list(lm = FALSE) 
            }
            
            op_loess <- setVal("op_loess", ifelse(resid, TRUE, FALSE))
            def_loess_fmt <- setVal("def_loess_fmt", TRUE)
            def_loess_args <- list(ci = 0.95, color = "dodgerblue", width = 1, dash = "dash")
            loess_ci <- setVal("loess_ci", def_loess_args$ci)
            loess_col <- setVal("loess_col", def_loess_args$color)
            loess_lwd <- setVal("loess_lwd", def_loess_args$with)
            loess_dash <- setVal("loess_dash", def_loess_args$dash)
            
            if(op_loess){
              if(!def_loess_fmt){ #not default format?
                line <- modifyList(line, list(loess = list(ci = loess_ci, color = loess_col, width = loess_lwd, dash = loess_dash)))
                line$loess <- line$loess[!line$loess %in% def_loess_args] #keep only non-defaults
                if(length(line$loess)==0) line$loess <- TRUE
              } else { #default format
                if(!resid) line <- modifyList(line, list(loess = TRUE)) #only need if op plot, as default for resid plot
              }
            } else { #don't have loess
              if(resid) line <- modifyList(line, list(loess = FALSE)) #only add if resid plot, as default for this is TRUE
            }
            
            op_ref <- setVal("op_ref", TRUE) #regardless of resid
            def_ref_fmt <- setVal("def_ref_fmt", TRUE)
            def_ref_args <- list(color = "black", width = 1, dash = "dash")
            ref_col <- setVal("ref_col", def_ref_args$color)
            ref_lwd <- setVal("ref_lwd", def_ref_args$width)
            ref_dash <- setVal("ref_dash", def_ref_args$dash)
            
            if(op_ref){
              if(!def_ref_fmt){
                line <- modifyList(line, list(ref = list(color = ref_col, width = ref_lwd, dash = ref_dash)))
                line$ref <- line$ref[!line$ref %in% def_ref_args] #keep only non-defaults
                if(length(line$ref)==0) line$ref <- TRUE
              } 
            } else {
              line <- modifyList(line, list(ref = FALSE) )
            }
            
            #marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(color = "orange", symbol = "circle", size = 10, opacity = 0.5,
                                    line = list(width = 1, color = "black"))
            
            mrk_col <- setVal("mrk_col", "orange")
            mrk_symbol <- setVal("mrk_symbol", "circle")
            mrk_size <- setVal("mrk_size", 10)
            mrk_opacity <- setVal("mrk_opacity", 0.5)
            mrk_lwd <- setVal("mrk_lwd", 1)
            mrk_lcol <- setVal("mrk_lcol", "black")
            
            if(def_marker_fmt){
              marker <- TRUE
            } else {
              marker <- list(color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                             opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol))
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] #keep only non-defaults
              if(length(marker$line)==0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args] 
              if(length(marker)==0) marker <- NULL
              
            }
            
            #include/exclude arguments
            if(!is.null(input$op_include)){
              if(input$op_include=="yes"){
                include <- input$op_select
                exclude <- NULL
              } else {
                include <- NULL
                exclude <- input$op_select
              }
            } else {
              include <- exclude <- NULL
            }
            
            #title argument
            def_title_fmt <- setVal("def_title_fmt", TRUE)
            def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
            title_text <- setVal("title_text", "")
            title_col <- setVal("title_col", "black")
            title_bold <- setVal("title_bold", TRUE)
            title_size <- setVal("title_size", 20)
            if(def_title_fmt){
              title <- ""
            } else {
              title <- list(text = title_text, font = list(color = title_col, size = title_size,
                                                           bold = title_bold))
              title$font <- title$font[!title$font %in% def_title_args$font] #keep only non-defaults
              if(length(title$font)==0) title$font <- NULL
              
            }
            
            #stats argument
            def_stats_fmt <- setVal("def_stats_fmt", TRUE)
            def_stats_args <- list(x = 0.8, y = 0.1, font = list(color = "black", size = 14, bold = FALSE))
            stats_col <- setVal("stats_col", "black")
            stats_size <- setVal("stats_size", 14)
            stats_bold <- setVal("stats_bold", FALSE)
            stats_x <- setVal("stats_x", 0.8)
            stats_y <- setVal("stats_y", 0.1)
            if(def_stats_fmt){
              stats <- TRUE
            } else {
              stats <- list(x = stats_x, y = stats_y, font = list(color = stats_col, size = stats_size,
                                                                  bold = stats_bold))
              stats$font <- stats$font[!stats$font %in% def_stats_args$font] #keep only non-defaults
              if(length(stats$font)==0) stats$font <- NULL
              stats <- stats[!stats %in% def_stats_args] 
              if(length(stats)==0) stats <- NULL
            }
            
            #Other defaults
            icen <- setVal("icen", "median")
            pred.type <- setVal("pred.type", "post")
            outeq <- setVal("outeq", 1)
            block <- setVal("block", "All")
            mult <- setVal("mult", 1)
            log <- setVal("log", FALSE)
            grid <- setVal("grid", TRUE)
            
            if(block == "All") block <- NULL
            
            
            args <- list(x = x, line = line, marker = marker, resid = resid,
                         icen = icen, pred.type = pred.type, outeq = as.numeric(outeq),
                         block = block, include = include, exclude = exclude,
                         mult = as.numeric(mult), log = log, grid = grid,
                         title = title, stats = stats,
                         xlab = getXlab(), ylab = getYlab(),
                         xlim = getXlim(), ylim = getYlim()) 
            args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
            
            #now make the code
            Name <- paste0(input$data,"$op$plot(")
            #default args
            def_args <- list(
              line = list(lm = TRUE, loess = FALSE, ref = TRUE),
              marker = TRUE,
              resid = FALSE,                      
              icen = "median", pred.type = "post", outeq = 1, block = NULL,
              include = as.character(unique(get(input$data)$op$id)), exclude = NULL, 
              mult = 1,
              log = FALSE, 
              grid = TRUE,
              xlab = NULL, ylab = NULL,
              title = "",
              stats = TRUE,
              xlim = NULL, ylim = NULL
            )
            
            if(resid) modifyList(def_args$line, list(lm = FALSE, loess = TRUE, ref = TRUE)) #modify default
            
            arglist <- args[-1] #remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist),names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]) )] #remove args that are in default list
            if(length(arglist)>0){
              arglist <- paste(deparse(arglist), collapse = "") %>% stringr::str_replace("^list\\(","") %>%
                stringr::str_replace_all("(\\d+)L","\\1") %>% stringr::str_replace_all(" +"," ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }
            
            p <- do.call(plot, args)
            if(code){
              return(codeStatement)
            } else {
              return(p)
            }
            
          } #end PM_op
          
          ############### Plot and Code: PM_result$cycle #####################
          
          if(!is.null(input$res_sub) && input$res_sub == "cyc"){
            
            x <- get(input$data)$cycle
            
            if(length(input$omit)==0) {omit <- 0.2} else {omit <- input$omit}
            
            #line argument
            def_ab_fmt <- setVal("def_ab_fmt", TRUE)
            def_a_args <- list(color = "dodgerblue", width = 1, dash = "solid")
            def_b_args <- list(colors = "Spectral", custom_colors = "", linetypes = "dash")
            a_col <- setVal("a_col", def_a_args$color)
            a_dash <- setVal("a_dash", def_a_args$dash)
            ab_lwd <- setVal("ab_lwd", def_a_args$width)
            b_col <- setVal("b_col", def_b_args$colors)
            b_custom_colors <- setVal("b_custom_colors", def_b_args$custom_colors)
            b_dash <- setVal("b_dash", def_b_args$linetypes)
            
            
            if(!def_ab_fmt){ #not default format?
              line <- list(color = a_col, width = ab_lwd, dash = a_dash)
              line <- line[!line %in% def_a_args] #keep only non-defaults
              if(length(line)==0) line <- TRUE
              
              linetypes <- b_dash
              
              if(b_custom_colors != ""){
                colors <- unlist(stringr::str_split(b_custom_colors, "\\s*,\\s*"))
              } else {
                colors <- b_col #built in palette
              }
              
            } else { #default format
              line <- list() #we have join but don't need with default format and data plot
              colors <- list()
              linetypes <- list()
            }
            
            #marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(color = "dodgerblue", symbol = "circle", size = 4, opacity = 1,
                                    line = list(width = 0, color = "black"))
            
            mrk_col <- setVal("mrk_col", def_marker_args$col)
            mrk_symbol <- setVal("mrk_symbol", def_marker_args$shape)
            mrk_size <- setVal("mrk_size", def_marker_args$size)
            mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
            mrk_lwd <- setVal("mrk_lwd", def_marker_args$line$width)
            mrk_lcol <- setVal("mrk_lcol", def_marker_args$line$color)
            
            if(def_marker_fmt){
              marker <- TRUE
            } else {
              marker <- list(color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                             opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol))
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] #keep only non-defaults
              if(length(marker$line)==0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args] 
              if(length(marker)==0) marker <- NULL
            }
            
            #Other defaults
            grid <- setVal("grid", TRUE)
            
            args <- list(
              x = x,
              line = line,
              marker = marker,
              colors = colors,
              linetypes = linetypes,
              omit = omit,
              grid = grid,
              xlab = getXlab(), ylab = getYlab()
            )
            
            args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
            #now make the code
            Name <- paste0(input$data,"$cycle$plot(")
            #default args
            def_args <- list(
              line = TRUE,
              marker = TRUE,
              colors = NULL,
              linetypes = NULL,
              omit = 0.2, 
              grid = TRUE,
              xlab = NULL, ylab = NULL
            )
            
            arglist <- args[-1] #remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist),names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]) )] #remove args that are in default list
            if(length(arglist)>0){
              arglist <- paste(deparse(arglist), collapse = "") %>% stringr::str_replace("^list\\(","") %>%
                stringr::str_replace_all("(\\d+)L","\\1") %>% stringr::str_replace_all(" +"," ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }
            
            p <- do.call(plot, args)
            if(code){
              return(codeStatement)
            } else {
              return(p)
            }
          } #end PMcycle
          
          
          ############### Plot and Code: PM_data & PM_result$data #####################
          
          if(inherits(get(input$data),"PM_data") ||
             (!is.null(input$res_sub) && input$res_sub == "dat")){
            
            #x argument
            if(inherits(get(input$data),"PM_result")){
              x <- get(input$data)$data
            }
            
            if(inherits(get(input$data),"PM_data")){
              x <- get(input$data)
            }
            
            #line argument
            join <- setVal("join", TRUE)
            def_join_fmt <- setVal("def_join_fmt", TRUE)
            def_join_args <- list(color = "dodgerblue", width = 1, dash = "solid")
            join_col <- setVal("join_col", def_join_args$color)
            join_lwd <- setVal("join_lwd", def_join_args$width)
            join_dash <- setVal("join_dash", def_join_args$dash)
            
            if(join){ #yes have join
              if(!def_join_fmt){ #not default format?
                line <- list(join = list(color = join_col, width = join_lwd, dash = join_dash))
                line$join <- line$join[!line$join %in% def_join_args] #keep only non-defaults
                if(length(line$join)==0) line$join <- TRUE
              } else { #default format
                line <- list() #we have join but don't need with default format and data plot
              }
            } else { #don't have join
              line <- list(join = FALSE) 
            }
            
            incl_pred <- setVal("incl_pred", "none")
            def_pred_fmt <- setVal("def_pred_fmt", TRUE)
            def_pred_args <- list(color = "", width = 1, dash = "dash")
            pred_col <- setVal("pred_col", def_pred_args$color)
            pred_lwd <- setVal("pred_lwd", def_pred_args$width)
            pred_dash <- setVal("pred_dash", def_pred_args$dash)
            
            if(incl_pred != "none"){
              pred_obj <- get(input$data)[[incl_pred]]
              if(!def_pred_fmt){ #not default format?
                pred = list(color = pred_col, width = pred_lwd, dash = pred_dash)
                pred <- pred[!pred %in% def_pred_args] #keep only non-defaults
                if(length(pred)==0) {
                  line$pred <- pred_obj
                } else {
                  line$pred <- c(pred_obj, pred)
                }
              } else { #default format
                line$pred <- pred_obj
              }
            } 
            
            #marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(color = "red", symbol = "circle", size = 10, opacity = 0.5,
                                    line = list(width = 1, color = "black"))
            
            mrk_col <- setVal("mrk_col", def_marker_args$col)
            mrk_symbol <- setVal("mrk_symbol", def_marker_args$shape)
            mrk_size <- setVal("mrk_size", def_marker_args$size)
            mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
            mrk_lwd <- setVal("mrk_lwd", def_marker_args$line$width)
            mrk_lcol <- setVal("mrk_lcol", def_marker_args$line$color)
            
            if(def_marker_fmt){
              marker <- TRUE
            } else {
              marker <- list(color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                             opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol))
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] #keep only non-defaults
              if(length(marker$line)==0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args] 
              if(length(marker)==0) marker <- NULL
              
            }
            
            #include/exclude arguments
            if(!is.null(input$data_include)){
              if(input$data_include=="yes"){
                include <- input$data_select
                exclude <- NULL
              } else {
                include <- NULL
                exclude <- input$data_select
              }
            } else {
              include <- exclude <- NULL
            }
            
            #title argument
            def_title_fmt <- setVal("def_title_fmt", TRUE)
            def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
            title_text <- setVal("title_text", "")
            title_col <- setVal("title_col", "black")
            title_bold <- setVal("title_bold", TRUE)
            title_size <- setVal("title_size", 20)
            if(def_title_fmt){
              title <- ""
            } else {
              title <- list(text = title_text, font = list(color = title_col, size = title_size,
                                                           bold = title_bold))
              title$font <- title$font[!title$font %in% def_title_args$font] #keep only non-defaults
              if(length(title$font)==0) title$font <- NULL
              
            }
            
            #group names
            names <- setVal("group_names", "")
            if(!is.null(names)){
              names <- unlist(stringr::str_split(names, "\\s*,\\s*"))
            }
            
            #legend
            if(!is.null(input$group) && input$group != "none"){
              legend <- setVal("legend", TRUE)
            } else {
              legend <- setVal("legend", FALSE)
            }
            
            
            #Other defaults
            color <- setVal("group", "none")
            colors <- setVal("group_col", "Set1")
            tad <- setVal("tad", FALSE)
            outeq <- setVal("outeq", 1)
            block <- setVal("block", 1)
            mult <- setVal("mult", 1)
            log <- setVal("log", FALSE)
            grid <- setVal("grid", FALSE)
            
            if(color == "none") color <- NULL
            if(block == "All") block <- 1
            
            args <- list(x = x, 
                         include = include, exclude = exclude,
                         line = line, marker = marker,
                         color = color,
                         colors = colors,
                         names = names,
                         outeq = as.numeric(outeq),
                         block = as.numeric(block), 
                         tad = tad,
                         #overlay = FALSE,
                         legend = legend,
                         mult = as.numeric(mult), log = log, grid = grid,
                         title = title,
                         xlab = getXlab(), ylab = getYlab(),
                         xlim = getXlim(), ylim = getYlim()) 
            
            args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
            
            
            
            #now make the code
            if(inherits(get(input$data),"PM_result")){
              Name <- paste0(input$data,"$data$plot(")
              id_obj <- as.character(unique(get(input$data)$data$data$id))
            }
            
            if(inherits(get(input$data),"PM_data")){
              Name <- paste0(input$data,"$plot(")
              id_obj <- as.character(unique(get(input$data)$data$id))
              
            }
            
            #default args
            
            def_args <- list(
              include = id_obj, exclude = NULL, 
              line = list(join = TRUE, pred = FALSE),
              marker = TRUE,
              color = NULL,
              colors = "Set1",
              names = "", 
              mult = 1,
              outeq = 1, block = 1,
              tad = FALSE,
              legend = ifelse(!is.null(input$group) && input$group != "none", TRUE, FALSE),
              log = FALSE, 
              grid = FALSE,
              xlab = NULL, ylab = NULL,
              title = "",
              stats = TRUE,
              xlim = NULL, ylim = NULL
            )
            
            arglist <- args[-1] #remove the data object
            if(incl_pred != "none"){ #replace pred object with its name if there
              if(inherits(arglist$line$pred, c("PM_pop", "PM_post"))){
                arglist$line$pred <- paste(input$data, incl_pred, sep = "$")
              } else {
                arglist$line$pred[[1]] <- paste(input$data, incl_pred, sep = "$")
              }
            }
            arglist <- arglist[map_lgl(intersect(names(arglist),names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]) )] #remove args that are in default list
            if(length(arglist)>0){
              arglist <- paste(deparse(arglist), collapse = "") %>% stringr::str_replace("^list\\(","") %>%
                stringr::str_replace_all("(\\d+)L","\\1") %>% stringr::str_replace_all(" +"," ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }
            
            codeStatement <- stringr::str_replace(codeStatement,
                                                  "pred = \"(\\S+)\"",
                                                  "pred = \\1")
            
            codeStatement <- stringr::str_replace(codeStatement,
                                                  "pred = list\\(\"(\\S+)\"",
                                                  "pred = list\\(\\1")
            
            p <- do.call(plot, args)
            if(code){
              return(codeStatement)
            } else {
              return(p)
            }
            
          } #end PM_data
          
        } #end check for PM_result
        
        ############### Plot and Code: PM_sim #####################
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_sim")){
          #x argument
          x <- get(input$data)$data
          
          #line argument
          sim_line <- setVal("sim_line", TRUE)
          def_sim_line_fmt <- setVal("def_sim_line_fmt", TRUE)
          def_sim_args <- list(ci = 0.95, 
                               sim_probs = c(0.05,0.25,0.5,0.75,0.95),
                               color = "dodgerblue", width = 1, dash = "solid")
          sim_ci <- setVal("sim_ci", def_sim_args$ci)
          sim_probs <- setVal("sim_probs", def_sim_args$sim_probs)
          sim_col <- setVal("sim_col", def_sim_args$color)
          sim_lwd <- setVal("sim_lwd", def_sim_args$width)
          sim_dash <- setVal("sim_dash", def_sim_args$dash)
          
          if(sim_line){ #yes have line
            if(!def_sim_line_fmt){ #not default format?
              line <- list(probs = sim_probs, color = sim_col, width = sim_lwd, dash = sim_dash)
              line <- line[!line %in% def_sim_args] #keep only non-defaults
              if(length(line)==0) line <- TRUE
            } else { #default format
              line <- list() 
            }
          } else { #don't have line
            line <- FALSE 
          }
          
          
          #marker argument
          sim_obs <- setVal("sim_obs", NULL)
          obs_name <- ""
          if(!is.null(sim_obs)){
            if(sim_obs == "None"){
              sim_obs <- NULL
            } else {
              obs_name <- sim_obs
              sim_obs <- get(sim_obs)
            }
          }
          
          
          def_sim_marker_fmt <- setVal("def_sim_marker_fmt", TRUE)
          def_marker_args <- list(color = "black", symbol = "circle-open", size = 8, opacity = 0.5,
                                  line = list(width = 1, color = "black"))
          
          mrk_col <- setVal("mrk_col", def_marker_args$color)
          mrk_symbol <- setVal("mrk_symbol", def_marker_args$symbol)
          mrk_size <- setVal("mrk_size", def_marker_args$size)
          mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
          mrk_lwd <- setVal("mrk_lwd", def_marker_args$lwd)
          mrk_lcol <- setVal("mrk_lcol", def_marker_args$lcol)
          
          if(!is.null(sim_obs)){
            if(def_sim_marker_fmt){
              marker <- TRUE
            } else {
              marker <- list(color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                             opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol))
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] #keep only non-defaults
              if(length(marker$line)==0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args] 
              if(length(marker)==0) marker <- NULL
            }
          } else {
            marker <- FALSE
          }
          
          #title argument
          def_title_fmt <- setVal("def_title_fmt", TRUE)
          def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
          title_text <- setVal("title_text", "")
          title_col <- setVal("title_col", "black")
          title_bold <- setVal("title_bold", TRUE)
          title_size <- setVal("title_size", 20)
          if(def_title_fmt){
            title <- ""
          } else {
            title <- list(text = title_text, font = list(color = title_col, size = title_size,
                                                         bold = title_bold))
            title$font <- title$font[!title$font %in% def_title_args$font] #keep only non-defaults
            if(length(title$font)==0) title$font <- NULL
            
          }
          
          #Other defaults
          log <- setVal("log", TRUE)
          grid <- setVal("grid", TRUE)
          mult <- setVal("mult", 1)
          outeq <- setVal("outeq", 1)
          binSize <- setVal("binSize", 0)
          simnum <- setVal("simchooser", 1)
          
          
          
          args <- list(x = x, 
                       mult = as.numeric(mult),
                       ci = as.numeric(sim_ci),
                       binSize = as.numeric(binSize),
                       outeq = as.numeric(outeq),
                       line = line, 
                       marker = marker, 
                       obs = sim_obs,
                       log = log, 
                       grid = grid,
                       title = title,
                       xlab = getXlab(), ylab = getYlab(),
                       xlim = getXlim(), ylim = getYlim(),
                       simnum = simnum) 
          
          
          
          args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
          
          #now make the code
          Name <- paste0(input$data,"$plot(")
          #default args
          def_args <- list(
            mult = 1,
            ci = 0.95,
            binSize = 0,
            outeq = 1,
            line = TRUE,
            marker = ifelse(is.null(sim_obs), FALSE, TRUE),
            obs = NULL,
            log = TRUE, 
            grid = TRUE,
            xlab = NULL, ylab = NULL,
            title = "",
            xlim = NULL, ylim = NULL,
            simnum = 1
          )
          
          arglist <- args[-1] #remove the data object
          if(!is.null(arglist$obs)){
            arglist$obs <- obs_name
          }
          arglist <- arglist[map_lgl(intersect(names(arglist),names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]) )] #remove args that are in default list
          
          if(length(arglist)>0){
            arglist <- paste(deparse(arglist), collapse = "") %>% stringr::str_replace("^list\\(","") %>%
              stringr::str_replace_all("(\\d+)L","\\1") %>% stringr::str_replace_all(" +"," ")
            codeStatement <- paste0(Name, arglist)
          } else {
            codeStatement <- paste0(Name, ")")
          }
          
          
          
          codeStatement <- stringr::str_replace(codeStatement,
                                                "obs = \"(\\S+)\"",
                                                "obs = \\1")
          
          
          p <- do.call(plot, args)
          if(code){
            return(codeStatement)
          } else {
            return(p$p)
          }
          
        } #end PM_sim
        
        ############### Plot and Code: PM_model & PM_result$model #####################
        
        if(!is.null(input$data) && 
           (inherits(get(input$data),"PM_model") |
            (inherits(get(input$data),"PM_result") 
             && !is.null(input$res_sub) && input$res_sub == "mod"))){
          
          #x argument
          if(inherits(get(input$data),"PM_result")){
            x <- get(input$data)$model
          }
          
          if(inherits(get(input$data),"PM_model")){
            x <- get(input$data)
          }
          
          #line argument
          join <- setVal("join", TRUE)
          def_join_fmt <- setVal("def_join_fmt", TRUE)
          def_join_args <- list(color = "black", width = 1, dash = "solid")
          join_col <- setVal("join_col", def_join_args$color)
          join_lwd <- setVal("join_lwd", def_join_args$width)
          join_dash <- setVal("join_dash", def_join_args$dash)
          
          if(join){ #yes have join
            if(!def_join_fmt){ #not default format?
              line <- list(color = join_col, width = join_lwd, dash = join_dash)
              line <- line[!line %in% def_join_args] #keep only non-defaults
              if(length(line)==0) line <- TRUE
            } else { #default format
              line <- list() #we have line but don't need with default format and model plot
            }
          } else { #don't have join
            line <- FALSE 
          }
          
          #marker argument
          def_marker_fmt <- setVal("def_marker_fmt", TRUE)
          def_marker_args <- list(color = "dodgerblue", size = 0.25, opacity = 0.5,
                                  line = list(width = 1, color = "black"))
          
          mrk_col <- setVal("mrk_col", def_marker_args$col)
          mrk_size <- setVal("mrk_size", def_marker_args$size)
          mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
          mrk_lwd <- setVal("mrk_lwd", def_marker_args$line$width)
          mrk_lcol <- setVal("mrk_lcol", def_marker_args$line$color)
          
          if(def_marker_fmt){
            marker <- TRUE
          } else {
            marker <- list(color = mrk_col, size = mrk_size,
                           opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol))
            marker$line <- marker$line[!marker$line %in% def_marker_args$line] #keep only non-defaults
            if(length(marker$line)==0) marker$line <- NULL
            marker <- marker[!marker %in% def_marker_args] 
            if(length(marker)==0) marker <- NULL
            
          }
          
          #Other defaults
          args <- list(x = x, 
                       line = line, marker = marker)
          
          
          args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
          
          
          
          #now make the code
          if(inherits(get(input$data),"PM_result")){
            Name <- paste0(input$data,"$model$plot(")
          }
          
          if(inherits(get(input$data),"PM_model")){
            Name <- paste0(input$data,"$plot(")
          }
          
          
          #default args
          
          def_args <- list(
            line = TRUE,
            marker = TRUE
          )
          
          arglist <- args[-1] #remove the data object
          arglist <- arglist[map_lgl(intersect(names(arglist),names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]) )] #remove args that are in default list
          if(length(arglist)>0){
            arglist <- paste(deparse(arglist), collapse = "") %>% stringr::str_replace("^list\\(","") %>%
              stringr::str_replace_all("(\\d+)L","\\1") %>% stringr::str_replace_all(" +"," ")
            codeStatement <- paste0(Name, arglist)
          } else {
            codeStatement <- paste0(Name, ")")
          }
          
          p <- do.call(plot, args)
          if(code){
            return(codeStatement)
          } else {
            return(p)
          }
          
        } #end PM_model
        
        return(NULL) #plot method not defined
        
      } #end makePMplot
      
      
      
      #################################################### 
      ############### Build the Page #####################
      #################################################### 
      
      output$help <- renderText({
        if(inherits(get(input$data),"PM_cov")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMcov</span>.")}
        if(inherits(get(input$data),"PM_final")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMfinal</span>.")}
        if(inherits(get(input$data),"PM_op")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMop</span>.")}
        if(inherits(get(input$data),"PMcycle")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMcycle</span>.")}
        if(inherits(get(input$data),"PMmatrix")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMmatrix</span>.")}
        if(inherits(get(input$data),"PMsim")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMsim</span>.")}
        
      })
      
      #Set up the inputs
      output$DataControls <- renderUI({makeDataControls()}) 
      output$FormatControls <- renderUI({makeFormatControls()})
      output$AxesControls <- renderUI({makeAxisControls()})
      
      
      #Build the Pmetrics plot statements
      output$plotCode <- renderText({makePMplot(code = TRUE)})
      
      #Make the plot call
      output$plotPM <- renderUI({
        if(!is.null(input$data) &&
           (inherits(get(input$data),"PM_model") ||
            (inherits(get(input$data),"PM_result") &&
             !is.null(input$res_sub) && input$res_sub == "mod"))){
          renderPlot({makePMplot()})
        } else {
          p <- makePMplot()
          if(inherits(p, "plotly")){ #need this to avoid warning message
            renderPlotly({p})
          }
        }
      })
      
      
      
      
      
    } #end shinyServer
    
  ) #end shiny App
  
} #end function

#shinyApp(ui, server)
