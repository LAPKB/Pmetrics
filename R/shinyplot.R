

#build_plot <- function(...) {
  
  requireNamespace("shiny")
  
  ClassFilter <- function(x) any(grepl("^PM_result|^PM_model|^PM_data|^PM_sim|^PM_pta|^PM_cov|^PM_final",class(get(x))))
  choices <- Filter(ClassFilter,ls(globalenv()))
  if(length(choices)==0) {
    data("NPex","dataEx","modEx") #load examples if none already loaded
  }
  
  shiny::shinyApp(
    
    ui <- bslib::page_sidebar(
      theme = bslib::bs_theme(bootswatch = "zephyr"),
      title = "Pmetrics Plot",
      
      sidebar = sidebar(
        width = 400,
        accordion(
          accordion_panel("Data",
                          selectInput("data","Choose a Pmetrics object to plot:",choices = choices),
                          uiOutput("DataControls")
          ),
          accordion_panel("Formatting",
                          uiOutput("FormatControls")
          ),
          accordion_panel("Axes",
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
      card(plotly::plotlyOutput("PMplot"))
      
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
      
      getFormula <- function(charac = FALSE, choices){
        if (length(input$x)==0 || length(input$y)==0) return(NULL)
        if (input$x=="Select" || input$y=="Select") return(NULL)
        if (!input$x %in% choices) return(NULL)
        if (!input$y %in% choices) return(NULL)
        if(charac) {return(paste(input$y,input$x,sep="~"))} else {return(as.formula(paste(input$y,input$x,sep="~")))}
      }
      
      # getFinalPlotType <- function(){
      #   if(length(input$ptype)==0) {return(list(ptype = "uni",lwd = input$line_lwd, col = input$line_col))} 
      #   if(input$ptype=="uni") {return(list(ptype="uni",lwd=input$lwd1,col=input$col1))} 
      #   if(input$ptype=="bi") {return(list(ptype="bi",lwd=input$lwd2,col=input$col2))} 
      # }
      
      getProbs <- function(){
        if(length(input$probs)==0) {return(c(0.05,0.25,0.5,0.75,0.95))} else {return(as.numeric(input$probs))}
      }
      
      getPred <- function(){
        if(length(input$pred)==0) return(NULL)
        if(input$pred=="") return(NULL)
        return(get(input$pred))
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
      makeDataControls <- function(){
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result")){
          return(list(
            radioButtons("res_sub","Plot which field?", 
                         selected = character(0),
                         c("Data" = "dat", 
                           "Model" = "mod",
                           "Obs/Pred" = "op",
                           "Final" = "fin",
                           "Cycle" = "cyc",
                           "Covariate" = "cov",
                           "Validation" = "valid"
                         )),
            conditionalPanel(
              condition = "input.res_sub == 'dat'",
              helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
              radioButtons("include","",c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
              selectInput("select","",choices=unique(get(input$data)$data$standard_data$id),selected = unique(get(input$data)$data$standard_data$id), multiple = T),
              selectInput("outeq","Output equation:",choices=1:max(get(input$data)$data$standard_data$outeq,na.rm=T),selected=1),
              numericInput("block","Block number:",1,min=1,step=1),
              selectInput("group","Grouping factor",choices = c("None", Pmetrics:::getCov(get(input$data)$data$standard_data)$covnames)),
              radioButtons("pred_obj","Include Predictions?",c("None" = "", "Population" = "pop", "Posterior" = "post"))
            ), #end data conditional panel
            
            conditionalPanel(
              condition = "input.res_sub == 'op'",
              radioButtons("pred.type","",c("Posterior Predictions" = "post","Population Predictions" = "pop"), selected = "post"),      
              checkboxInput("resid","Residual Plot",FALSE),
              selectInput("icen","Predictions based on:",choices=c("mean","median"),"median"),
              helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
              radioButtons("include","",c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
              selectInput("select", "", choices=unique(get(input$data)$op$id), selected=unique(get(input$data)$op$id), multiple = T, selectize = F),
              selectInput("outeq", "Output equation:", choices = 1:max(get(input$data)$op$outeq), selected = 1),
              selectInput("block", "Block:", choices = c("All",1:max(get(input$data)$op$block)))
              
            ), #end op conditional panel
            conditionalPanel(
              condition = "input.res_sub == 'fin'",
              radioButtons("ptype","Plot type:",c("Univariate" = "uni", "Bivariate" = "bi")),
              conditionalPanel(
                condition="input.ptype=='bi'",
                selectInput("x","x-axis",choices=c("Select",names(get(input$data)$final$popMean)),selected = "Select"),
                selectInput("y","y-axis",choices=c("Select",names(get(input$data)$final$popMean)),selected = "Select")
              ) #end conditional panel
            ), #end final conditional panel
            conditionalPanel(
              condition = "input.res_sub == 'cyc'",
              numericInput("omit","Proportion of burn-in cycles to omit:", 0.2, min = 0, max = 1, step = 0.1)
            ), #end cycle conditional panel
            conditionalPanel(
              condition = "input.res_sub == 'cov'",
              selectInput("y", "Y-axis", choices=c("Select",names(get(input$data)$cov$data)), selected = "Select"),
              selectInput("x", "X-axis", choices=c("Select",names(get(input$data)$cov$data)), selected = "Select"),      
              helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
              radioButtons("include", "", c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
              selectInput("select","",choices=unique(get(input$data)$cov$data$id),selected=unique(get(input$data)$cov$data$id),multiple=T,selectize=F),
              selectInput("icen","Summary method for changing covariates:",choices=c("mean","median","mode","none"),"mean")
            ) #end cov conditional panel
          )) #end return list
        } #end PM_result
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_sim")){
          x <- get(input$data)
          OPFilter <- function(x) any(grepl("^PM_result",class(get(x))))
          OPchoices <- Filter(OPFilter,ls(globalenv()))
          if(length(OPchoices)==0){OPchoices <- "None"} else {OPchoices <- c("None",OPchoices)}
          
          return(list(
            selectInput("outeq","Output equation:",choices=1:max(x$obs[[1]]$outeq,na.rm=T),selected=1),
            selectInput("obs","Observed (for VPC)",choices=OPchoices)
          )) #end list
        } #end PMsim
        
      } #end makeDataControls function
      
      ############### Make Format Controls #####################
      makeFormatControls <- function(){
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") && input$res_sub == "cov"){
          return(list(
            h4("Regression Options:"),
            checkboxGroupInput("regression","",
                               choices=c("Linear"="reg",
                                         "Lowess"="lowess",
                                         "Unity line"="ref")),
            numericInput("ci","Confidence Interval",0.95,min=0.1,max=0.99,step=0.05),
            h4("Formatting Options:"),
            checkboxInput("grid","Grid",FALSE),
            checkboxInput("ident","Identify points with subject IDs",FALSE),
            checkboxInput("square","Square",FALSE),
            checkboxInput("log","Log-log plot",FALSE),
            textInput("col","Color:","red"),
            textInput("col.stat","Color of stat text:","black"),
            numericInput("cex","Point size",1,step=0.5),
            numericInput("cex.stat","Stat text size",0.8,step=0.1),
            numericInput("x.stat","Stat horizontal pos",0.6,step=0.1),
            numericInput("y.stat","Stat vertical pos",0.1,step=0.1),
            numericInput("lwd","Line width",2,step=0.5)
          )) #end list
        } #end PMcov
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") && input$res_sub == "fin" & inherits(get(input$data)$final,"NPAG")){
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
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") && input$res_sub == "fin" & inherits(get(input$data)$final,"IT2B")){
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
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") && input$res_sub == "op"){
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
                ),
              ), #end accordion_panel
              accordion_panel(
                "Plot Options",
                textInput("mult","Multiplication factor for axes:","1"),
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
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") && input$res_sub == "cyc"){
          return(list(
            numericInput("x.leg","Legend horizontal pos:",0,min=0,max=1,step=0.1),
            numericInput("y.leg","Legend vertical pos:",1,min=0,max=1,step=0.1),
            numericInput("cex.leg","Legend text size",1.2,step=0.1)
          )) #end list
        } #end PM_cycle
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") && input$res_sub == "dat"){
          return(list(
            helpText("Note that all plots are overlayed in this GUI"),
            textInput("mult","Multiplication factor for y-axis:","1"),
            helpText("Note that all plots are overlayed in this GUI"),
            checkboxInput("log","Log scale Y-axis",FALSE),
            checkboxInput("grid","Grid",FALSE),
            checkboxInput("ident","Identify points with subject IDs",FALSE),
            checkboxInput("legend","Legend for groups",FALSE),
            numericInput("pch","Plotting character",NA,min=0,step=1),
            checkboxInput("errbar","Observation error bars",FALSE),
            checkboxInput("join","Join observations",TRUE),
            numericInput("cex","Point size",1,step=0.5)
          )) #end list
        } #end PM_data
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_sim")){
          return(list(
            textInput("mult","Multiplication factor for y-axis","1"),
            checkboxInput("log","Log scale Y-axis",FALSE),
            selectInput("probs","Quantiles",choices=c(0.01,0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.975,0.99),selected=c(0.05,0.25,0.5,0.75,0.95),multiple=T),
            textInput("binSize","Bin Size",0),
            numericInput("pch","Plotting character",1,min=1,step=1),
            checkboxInput("join","Join observations",TRUE),
            numericInput("cex.qlab","Quantile label size",0.8,step=0.1),
            numericInput("x.qlab","Quantile label horizontal pos",0.4,step=0.1),
            numericInput("ci","Confidence interval",0.95,min=0,max=1,step=0.05),
            checkboxInput("grid","Grid",TRUE),
            textInput("ocol","Observation Color","white")
          )) #end list
        } #end PM_sim
        
      } #end makeFormatControls function
      
      ############### Make Axis Controls #####################
      makeAxisControls <- function(){
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
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result") && input$res_sub == "dat"){
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
      } #end makeAxisControls
      
      ############### Make PMplot and Statement #####################
      makePMplot <- function(code = FALSE){
        
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_result")){ #check for PM_result
          
          ##### PM_cov ##### 
          
          if(!is.null(input$res_sub) && input$res_sub == "cov"){
            if (length(input$x)==0 || length(input$y)==0) return(NULL)
            if (input$x=="Select" || input$y=="Select") return(NULL)
            x <- get(input$data)
            if(input$include=="yes"){
              include <- input$select
              exclude <- NULL
            } else {
              include <- NULL
              exclude <- input$select
            }
            
            if(length(input$regression>0)){
              reg <- "reg" %in% unlist(input$regression)
              lowess <- "lowess" %in% unlist(input$regression)
              ref <- "ref" %in% unlist(input$regression)
            } else {
              reg <- F
              lowess <- F
              ref <- F
            }
            args <- list(x=x,formula=getFormula(choices=names(get(input$data))),include=include,exclude=exclude,icen=input$icen,reg=reg,lowess=lowess,ref=ref,grid=input$grid,cex=input$cex,col=input$col,
                         log=input$log,square=input$square,ident=input$ident,ci=input$ci,x.stat=input$x.stat,y.stat=input$y.stat,
                         col.stat=input$col.stat,cex.stat=input$cex.stat,cex.lab=input$cex.lab,lwd=input$lwd,xlim=getXlim(),ylim=getYlim(),xlab=getXlab(),ylab=getYlab()) 
            args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
            do.call(plot.PMcov,args)  
          } #end PMcov
          
          
          ##### PM_final ##### 
          
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
            
            
            
            args <- list(x = x, formula = getFormula(choices = names(get(input$data)$final$popMean)),
                         line = line, marker = marker,
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
          
          ##### PM_op ##### 
          
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
            def_loess_args <- list(color = "dodgerblue", width = 1, dash = "dash")
            loess_col <- setVal("loess_col", def_loess_args$color)
            loess_lwd <- setVal("loess_lwd", def_loess_args$with)
            loess_dash <- setVal("loess_dash", def_loess_args$dash)
            
            if(op_loess){
              if(!def_loess_fmt){ #not default format?
                line <- modifyList(line, list(loess = list(color = loess_col, width = loess_lwd, dash = loess_dash)))
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
            if(input$include=="yes"){
              include <- input$select
              exclude <- NULL
            } else {
              include <- NULL
              exclude <- input$select
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
          
          ##### PM_cycle ##### 
          
          if(!is.null(input$res_sub) && input$res_sub == "cyc"){
            x <- get(input$data)
            if(length(input$omit)==0) {omit <- 0.2} else {omit <- input$omit}
            args <- list(x=x,x.leg=input$x.leg,y.leg=input$y.leg,cex.leg=input$cex.leg,omit=omit)
            args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
            do.call(plot.PMcycle,args)
          } #end PMcycle
          
          
          ##### PM_data ##### 
          
          if(!is.null(input$res_sub) && input$res_sub == "dat"){
            x <- get(input$data)
            if(input$include=="yes"){
              include <- input$select
              exclude <- NULL
            } else {
              include <- NULL
              exclude <- input$select
            }   
            if(length(input$group)==0 || input$group=="None"){group <- NULL} else {group <- input$group}
            
            args <- list(x=x,include=include,exclude=exclude,pred=getPred(),outeq=as.numeric(input$outeq),group=group,
                         block=as.numeric(input$block),mult=as.numeric(input$mult),grid=input$grid,cex=input$cex,
                         log=input$log,ident=input$ident,join=input$join,errbar=input$errbar,pch=input$pch,legend=input$legend,
                         cex.lab=input$cex.lab,xlim=getXlim(),ylim=getYlim(),
                         xlab=getXlab(),ylab=getYlab()) 
            args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
            
          } #end PMmatrix
          
        } #end check for PM_result
        
        ##### PM_sim ##### 
        
        if(!is.null(input$data) && inherits(get(input$data),"PM_sim")){
          x <- get(input$data)
          args <- list(x=x,mult=as.numeric(input$mult),log=input$log,
                       probs=getProbs(),
                       binSize=as.numeric(input$binSize),
                       outeq=as.numeric(input$outeq),pch=input$pch,join=input$join,
                       x.qlab=input$x.qlab,cex.qlab=input$cex.qlab,
                       pos.qlab=as.numeric(input$pos.qlab),ci=input$ci,cex.lab=input$cex.lab,
                       xlab=getXlab(),ylab=getYlab(),xlim=getXlim(),ylim=getYlim(),
                       obs=input$obs,grid=input$grid,ocol=input$ocol) 
          args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
          do.call(plot.PMop,args)  
        } #end PMop
        
        return(NULL) #plot method not defined
        
      } #end makePMplot
      
      
      ############### Make PMstatement #####################
      makePMstatement <- function(){
        FormatArg <- function(argname,argval,argdef,quote=F,showargname=T){
          if(length(argval)>0 & !identical(as.character(argval),as.character(argdef))){
            if(length(argval)>1) argval=paste("c(",paste(argval,collapse=","),")",sep="")
            if(showargname){
              argstr <- paste(argname,"=",ifelse(quote,"\"",""),argval,ifelse(quote,"\"",""),sep="")
            } else {argstr <- paste(ifelse(quote,"\"",""),argval,ifelse(quote,"\"",""),sep="")}
          } else {argstr <- ""}
          return(argstr)
        } #end FormatArg
        
        if(inherits(get(input$data),"PMcov")){
          if (length(input$x)==0 || length(input$y)==0) return("plot(...)")
          if (input$x=="Select" || input$y=="Select") return("plot(...)")
          if(length(input$include)!=0){
            if(input$include=="yes"){
              incdef <- unique(get(input$data)$id)
              excdef <- input$select
            } else {
              incdef <- input$select
              excdef <- unique(get(input$data)$id)
            }
          } else {
            incdef <- unique(get(input$data)$id)
            excdef <- input$select
          }
          arglist <- c(
            FormatArg("x",input$data,"",showargname=F),
            FormatArg("formula",getFormula(charac=T,choices=names(get(input$data))),"",showargname=F),
            FormatArg("icen",input$icen,"median",T),
            FormatArg("include",input$select,incdef),      
            FormatArg("exclude",input$select,excdef),
            FormatArg("log",input$log,F),
            FormatArg("square",input$square,F),
            FormatArg("ref","ref" %in% unlist(input$regression),F),
            FormatArg("lowess","lowess" %in% unlist(input$regression),F),
            FormatArg("grid",input$grid,F),
            FormatArg("ident",input$ident,F),
            FormatArg("reg","reg" %in% unlist(input$regression),F),
            FormatArg("ci",input$ci,0.95),
            FormatArg("cex",input$cex,1),
            FormatArg("cex.lab",input$cex.lab,1.2),
            FormatArg("x.stat",input$x.stat,0.6),
            FormatArg("y.stat",input$y.stat,0.1),
            FormatArg("col.stat",input$col.stat,"black",T),
            FormatArg("cex.stat",input$cex.stat,0.8),
            FormatArg("lwd",input$lwd,2),
            FormatArg("col",input$col,"red",T),
            FormatArg("xlim",getXlim(),NULL),
            FormatArg("ylim",getYlim(),NULL),
            FormatArg("xlab",getXlab(),NULL,T),
            FormatArg("ylab",getYlab(),NULL,T)
          ) #end arglist for PMcov
          if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
          arglist <- paste(arglist,collapse=", ")
          return(paste("plot(",arglist,")",sep=""))
        } #end PMcov
        
        if(identical(inherits(get(input$data),c("PMfinal","NPAG"),which=T),as.integer(1:2))){
          arglist <- c(
            FormatArg("x",input$data,"",showargname=F),
            ifelse(getFinalPlotType()$ptype=="uni","",FormatArg("formula",getFormula(charac=T,choices=names(get(input$data)$popMean)),"",showargname=F)),
            FormatArg("cex.lab",input$cex.lab,1.2),
            FormatArg("col",getFinalPlotType()$col,ifelse(getFinalPlotType()$ptype=="uni","red","white"),T),
            FormatArg("pch",input$pch,3),
            FormatArg("cex",input$cex,1),
            FormatArg("lwd",getFinalPlotType()$lwd,ifelse(getFinalPlotType()$ptype=="uni",4,1)),
            FormatArg("density",input$density,FALSE),
            FormatArg("scale",input$scale,100),
            FormatArg("bg",input$bg,"gray50",TRUE),
            FormatArg("grid",input$grid,TRUE),
            FormatArg("xlim",getXlim(),NULL),
            FormatArg("ylim",getYlim(),NULL),
            FormatArg("xlab",getXlab(),NULL,T),
            FormatArg("ylab",getYlab(),NULL,T)
          ) #end arglist for PMfinal
          if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
          arglist <- paste(arglist,collapse=", ")
          return(paste("plot(",arglist,")",sep=""))
        } #end PMfinal, NPAG
        
        if(identical(inherits(get(input$data),c("PMfinal","IT2B"),which=T),as.integer(1:2))){
          arglist <- c(
            FormatArg("x",input$data,"",showargname=F),
            ifelse(getFinalPlotType()$ptype=="uni","",FormatArg("formula",getFormula(charac=T,choices=names(get(input$data)$popMean)),"",showargname=F)),
            FormatArg("cex.lab",input$cex.lab,1.2),
            FormatArg("col",getFinalPlotType()$col,ifelse(getFinalPlotType()$ptype=="uni","red","white"),T),
            FormatArg("pch",input$pch,3),
            FormatArg("cex",input$cex,1),
            FormatArg("lwd",getFinalPlotType()$lwd,ifelse(getFinalPlotType()$ptype=="uni",4,1)),
            FormatArg("legend",input$legend,TRUE),
            FormatArg("probs",getFinalprobs(),c(0.05,0.25,0.5,0.75,0.95)),
            FormatArg("standard",input$standard,FALSE),
            FormatArg("grid",input$grid,TRUE),
            FormatArg("xlim",getXlim(),NULL),
            FormatArg("ylim",getYlim(),NULL),
            FormatArg("xlab",getXlab(),NULL,T),
            FormatArg("ylab",getYlab(),NULL,T)
          ) #end arglist for PMfinal
          if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
          arglist <- paste(arglist,collapse=", ")
          return(paste("plot(",arglist,")",sep=""))
        } #end PMfinal, IT2B
        
        if(inherits(get(input$data),"PMop")){
          if(length(input$include)!=0){
            if(input$include=="yes"){
              incdef <- unique(get(input$data)$id)
              excdef <- input$select
            } else {
              incdef <- input$select
              excdef <- unique(get(input$data)$id)
            }
          } else {
            incdef <- unique(get(input$data)$id)
            excdef <- input$select
          }
          if(length(input$regression)>0){
            reg <- "reg" %in% unlist(input$regression)
            lowess <- "lowess" %in% unlist(input$regression)
            ref <- "ref" %in% unlist(input$regression)
          } else {
            reg <- T
            lowess <- F
            ref <- T
          }
          if(length(input$square)>0){
            square <- input$square
          } else {square <- T}
          arglist <- c(
            FormatArg("x",input$data,"",showargname=F),
            FormatArg("include",input$select,incdef),      
            FormatArg("exclude",input$select,excdef),
            FormatArg("pred.type",input$pred.type,"post",T),
            FormatArg("icen",input$icen,"median",T),
            FormatArg("outeq",input$outeq,1),
            FormatArg("mult",as.numeric(input$mult),1),
            FormatArg("resid",input$resid,F),
            FormatArg("log",input$log,F),
            FormatArg("square",square,T),
            FormatArg("ref",ref,T),
            FormatArg("lowess",lowess,F),
            FormatArg("grid",input$grid,F),
            FormatArg("ident",input$ident,F),
            FormatArg("reg",reg,T),
            FormatArg("ci",input$ci,0.95),
            FormatArg("cex",input$cex,1),
            FormatArg("cex.lab",input$cex.lab,1.2),
            FormatArg("x.stat",input$x.stat,0.6),
            FormatArg("y.stat",input$y.stat,0.1),
            FormatArg("col.stat",input$col.stat,"black",T),
            FormatArg("cex.stat",input$cex.stat,0.8),
            FormatArg("lwd",input$lwd,2),
            FormatArg("col",input$col,"red",T),
            FormatArg("xlim",getXlim(),NULL),
            FormatArg("ylim",getYlim(),NULL),
            FormatArg("xlab",getXlab(),NULL,T),
            FormatArg("ylab",getYlab(),NULL,T)
          ) #end arglist for PMop
          if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
          arglist <- paste(arglist,collapse=", ")
          return(paste("plot(",arglist,")",sep=""))
        } #end PMop
        
        if(inherits(get(input$data),"PMcycle")){
          arglist <- c(
            FormatArg("x",input$data,"",showargname=F),
            FormatArg("x.leg",input$x.leg,0),
            FormatArg("y.leg",input$y.leg,1),
            FormatArg("cex.leg",input$cex.leg,1.2),
            FormatArg("omit",input$omit,0.2)
          ) #end arglist for PMcycle
          if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
          arglist <- paste(arglist,collapse=", ")
          return(paste("plot(",arglist,")",sep=""))
        } #end PMcycle
        
        if(inherits(get(input$data),"PMmatrix")){
          if(length(input$include)!=0){
            if(input$include=="yes"){
              incdef <- unique(get(input$data)$id)
              excdef <- input$select
            } else {
              incdef <- input$select
              excdef <- unique(get(input$data)$id)
            }
          } else {
            incdef <- unique(get(input$data)$id)
            excdef <- input$select
          }
          if(length(input$group)==0 || input$group=="None"){group <- NULL} else {group <- input$group}
          
          arglist <- c(
            FormatArg("x",input$data,"",showargname=F),
            FormatArg("include",input$select,incdef),      
            FormatArg("exclude",input$select,excdef),
            FormatArg("pred",input$pred,NULL),
            FormatArg("outeq",as.numeric(input$outeq),1),
            FormatArg("group",group,NULL,T),
            FormatArg("block",as.numeric(input$block),1),
            FormatArg("mult",as.numeric(input$mult),1),
            FormatArg("log",input$log,F),
            FormatArg("grid",input$grid,F),
            FormatArg("join",input$join,T),
            FormatArg("pch",input$pch,NA),
            FormatArg("ident",input$ident,F),
            FormatArg("cex",input$cex,1),
            FormatArg("cex.lab",input$cex.lab,1.2),        
            FormatArg("legend",input$legend,F),
            FormatArg("xlim",getXlim(),NULL),
            FormatArg("ylim",getYlim(),NULL),
            FormatArg("xlab",getXlab(),"Time (h)",T),
            FormatArg("ylab",getYlab(),"Observation",T)
          ) #end arglist for PMmatrix
          if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
          arglist <- paste(arglist,collapse=", ")
          return(paste("plot(",arglist,")",sep=""))
        } #end PMmatrix
        
        if(inherits(get(input$data),"PMsim")){
          arglist <- c(
            FormatArg("x",input$data,"",showargname=F),
            FormatArg("mult",as.numeric(input$mult),1),
            FormatArg("log",input$log,F),
            FormatArg("probs",getFinalprobs(),c(0.05,0.25,0.5,0.75,0.95)),
            FormatArg("binSize",as.numeric(input$binSize),0),
            FormatArg("outeq",as.numeric(input$outeq),1),
            FormatArg("pch",input$pch,3),
            FormatArg("join",input$join,T),
            FormatArg("x.qlab",input$x.qlab,0.4),
            FormatArg("cex.qlab",input$cex.qlab,0.8),
            FormatArg("pos.qlab",as.numeric(input$pos.qlab),1),
            FormatArg("ci",input$ci,0.95),
            FormatArg("cex.lab",input$cex.lab,1.2),
            FormatArg("xlab",getXlab(),NULL,T),
            FormatArg("ylab",getYlab(),NULL,T),
            
            FormatArg("xlim",getXlim(),NULL),
            FormatArg("ylim",getYlim(),NULL),
            FormatArg("obs",input$obs,showargname=T),
            FormatArg("grid",input$grid,TRUE),        
            FormatArg("ocol",input$ocol,"blue")
          ) #end arglist for PMsim
          if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
          arglist <- paste(arglist,collapse=", ")
          return(paste("plot(",arglist,")",sep=""))
        } #end PMsim
        
      } #end makePMstatement
      
      
      #################################################### 
      ############### Build the Page #####################
      #################################################### 
      
      output$help <- renderText({
        if(inherits(get(input$data),"PMcov")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMcov</span>.")}
        if(inherits(get(input$data),"PMfinal")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMfinal</span>.")}
        if(inherits(get(input$data),"PMop")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMop</span>.")}
        if(inherits(get(input$data),"PMcycle")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMcycle</span>.")}
        if(inherits(get(input$data),"PMmatrix")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMmatrix</span>.")}
        if(inherits(get(input$data),"PMsim")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMsim</span>.")}
        
      })
      
      #Set up the inputs
      output$DataControls <- renderUI({makeDataControls()}) 
      output$FormatControls <- renderUI({makeFormatControls()})
      output$AxesControls <- renderUI({makeAxisControls()})
      
      #Make the plot call
      output$PMplot <- plotly::renderPlotly({makePMplot()})
      
      #Build the Pmetrics plot statements
      output$plotCode <- renderText({makePMplot(code = TRUE)})
      
      
    } #end shinyServer
    
  ) #end shiny App
  
#} #end function
#
shinyApp(ui, server)
