library(shiny)
ruleExplorer <- function(medata, parameter = NULL) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package shiny is required to run this method.", call. = FALSE)
  }
  roundUp <- function(medata,digits = 3) round(medata+.5*10^-digits, digits)
  roundDown <- function(medata,digits = 3) round(medata-.5*10^-digits, digits)
  
  ### dataset can be rules or transactions
  dataset <- medata
  aparameter <- as(parameter,'APparameter')
  supp <- aparameter@support
  conf <- aparameter@confidence
  
  ### make sure we have transactions or rules
  if(is(dataset, "data.frame")) {
    dataset <- discretizeDF(dataset)
    dataset <- as(dataset, "transactions")
  }}
  
  ### default measures to use
  xIndexCached <- "support"
  yIndexCached <- "confidence"
  zIndexCached <- "lift"
  
  #logOutput <- shiny::reactiveVal('Output log')
  
  ### js cannot handle very large arrays
  #itemLabels <- itemLabels(dataset)
  #if(length(itemLabels) > 10000) 
    #itemLabels <- list('Disabled because of excessive number of items (>10,000)'= c(""))
  
  if(is(dataset, "rules")) {
    if(length(dataset) < 1) stop("Zero rules provided!")
    
    minSupp <- roundDown(min(quality(dataset)$support), 3)
    maxSupp <- roundUp(max(quality(dataset)$support), 3)
    minConf <- roundDown(min(quality(dataset)$confidence), 3)
    maxConf <- roundUp(max(quality(dataset)$confidence), 3)
    minLift <- floor(min(quality(dataset)$lift))
    maxLift <- ceiling(max(quality(dataset)$lift))
    
    supp <- minSupp
    conf <- minConf
    lift <- minLift
  } else {  
    ### transactions
    minSupp <- 0
    maxSupp <- 1
    minConf <- 0
    maxConf <- 1
    minLift <- 0
    maxLift <- 25
    lift <- 0
    
  }
shiny::shinyUI(ui = shiny::shinyUI(shiny::fluidPage(
  #shiny::titlePanel("Association Rules"),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      shiny::htmlOutput('numRulesOutput'),
      shiny::br(),
      shiny::uiOutput("kSelectInput"),
      shiny::uiOutput("xAxisSelectInput"),
      shiny::uiOutput("yAxisSelectInput"),
      shiny::uiOutput("cAxisSelectInput"),
      shiny::br(),
      shiny::sliderInput("supp", "Minimum Support:", min = minSupp, max = maxSupp, value = supp , step = (maxSupp-minSupp)/10000, sep =""),
      shiny::sliderInput("conf", "Minimum Confidence:", min = minConf, max = maxConf, value = conf , step =  (maxConf-minConf)/1000, sep = ""), 
      shiny::sliderInput("lift", "Minimum Lift:", min = minLift, max = maxLift, value = lift , step =  (maxLift-minLift)/1000, sep = ""), 
      shiny::numericInput("minL", "Min. items in rule:", 2), 
      shiny::numericInput("maxL", "Max. items in rule:", 10), 
      shiny::br(),
      shiny::HTML('<b>Filter rules by items:</b>'),
      shiny::br(),
      shiny::selectInput('colsType',NULL,
                         c('Exclude items:'='rem','Require items:'='req')),
      shiny::uiOutput("choose_columns"), 
      shiny::selectInput('colsLHSType',NULL,
                         c('Exclude items from LHS:'='rem','Require items in LHS:'='req')),
      shiny::uiOutput("choose_lhs"), 
      shiny::selectInput('colsRHSType',NULL,
                         c('Exclude items from RHS:'='rem','Require items in RHS:'='req')),
      shiny::uiOutput("choose_rhs"), 
      shiny::br(),
      shiny::downloadButton('rules.csv', 'Download Rules as CSV')
      #,
      #  shiny::br(),
      #  shiny::verbatimTextOutput('logOutput')
    ),
    
    shiny::mainPanel(
      shiny::tabsetPanel(id='mytab',
                           shiny::tabPanel('Data Table', value='datatable', 
                                           shiny::dataTableOutput("rulesDataTable")),
                           shiny::tabPanel('Scatter', value='scatter', 
                                           plotlyOutput("scatterPlot", 
                                                        width='100%', height='100%')),
                           shiny::tabPanel('Matrix', value='matrix', 
                                           plotlyOutput("matrixPlot", width='100%', height='100%')),
                           shiny::tabPanel('Grouped', value='grouped', 
                                           shiny::plotOutput("groupedPlot", width='100%', height='100%')),
                           shiny::tabPanel('Graph', value='graph', 
                                           visNetworkOutput("graphPlot", width='100%', height='800px'))
        )
    )
  ))))
  
