# Loading libraries
library(shiny)
library(DT)
library(plyr)
library(dplyr)
library(leaflet)
library(INLA)

################################################################################################################
# Shiny App for Annual Species Temporal Abundance Models 
################################################################################################################

# First the user needs to upload the data csv file into the application and 
# then select whether normalize the numerical predictors or not.
#         The data file should include only:
#         1. Species - Different species
#         2. Year - Detected Year
#         3. Count - Species count
#         with or without predictor variables (numeric/factor).
#         The above names are case sensitive."),
# A sample format of the data can be found in https://github.com/uwijewardhana/UDMTA.

### Shiny User Interface ###

ui <- fluidPage(
  
titlePanel(strong("UDM - Shiny App for Annual Species Temporal Abundance Models", titleWidth = 350)),
  
# Loading the data file
div(style="display: inline-block;vertical-align:top; width: 300px;", fileInput("file", "Choose data CSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
div(style="display: inline-block;vertical-align:top; width: 300px;", selectInput("prednorm", "Numeric predictors normalization:", choices=c("rnorm", "stand", "none"), selected = "none")),
  
tabsetPanel(
    
tabPanel("Data",
         fluidRow(style = "margin-top: 25px;",
         column(8, p(tags$b('Annual Numeric Data', style = 'font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;'))),
         column(4, p(tags$b('Summary of Numeric Predictors', style = "font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;")))),
         fluidRow(column(8, DT::dataTableOutput("contents")),
         column(4, verbatimTextOutput("datasummary")))
),
    
tabPanel("Species Distribution Model",
         sidebarLayout(
         sidebarPanel(div(style='height:950px; overflow: scroll',
         selectInput("type", "Model type (Multi-species or Single species", choices=c("Multi Species", "Single Species"), selected = "Multi Species"),
         selectInput("distribution", "Distribution:", choices=c("Poisson", "Negative Binomial",
                                                      "Zeroinflated Poisson", "Zeroinflated Negative Binomial",
                                                      "Poisson Hurdle", "Negative Binomial Hurdle"), selected = "Poisson"),
         selectInput("tempeffect", "temporal random effect model:", choices=c("'ar1'", "'iid'", "'rw1'", "'rw2'"), selected = "'ar1'"),
         selectInput("controlgroup", "control group model (multispecies):", choices=c("'iid'", "'exchangeable'"), selected = "'exchangeable'"),
         h5('Generate Interaction Variables Here (if applicable)'),
         uiOutput("independent"),
         uiOutput("makeInteract1"), uiOutput("makeInteract2"),
         uiOutput("uiAdded"), actionButton("actionBtnAdd", "Create Interaction Term"),
         hr(),
         actionButton("summary", "Summary"))),
               
mainPanel(fluidRow(column(12, p(tags$b('Summary results of species distribution model:', style = "font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;")))),
          fluidRow(column(12, verbatimTextOutput("summary"))))

))))

### Shiny Server ###

server <- function(input, output, session){
  
# Read Data CSV file
  
filedata1 <- reactive({
    inFile <- input$file
    if (is.null(inFile)){return(NULL)}
    
    x <- as.data.frame(read.csv(inFile$datapath, fileEncoding="UTF-8-BOM"))
    x$Count <- as.character(x$Count)
    x$Count <- as.numeric(x$Count)

    y = dplyr::select_if(x, is.numeric)
    z = cbind(Species = x[ , (names(x) %in% c("Species"))], y)
    Final <- unique(z)
})
  
# Subset possible numeric predictor variables
  
filedata2 <- reactive({
    req(input$file)
    x <- filedata1()
    
    y = dplyr::select_if(x, is.numeric)
    if(ncol(y)>2){
    p = subset(y, select = -c(Count))
    p <- unique(p)
    p = subset(p, select = -c(Year))
    }else {p = NULL}
    
    if(!is.null(p)){
    for(i in 1:ncol(p)){
    if(input$prednorm == "rnorm"){p[,i] <- round(rnorm(p[,i]), digits = 4)
    } else if(input$prednorm == "stand"){p[,i] <- round(scale(p[,i]), digits = 4)
    } else {p[,i] <- round(p[,i], digits = 4)}}
    } 
    return(p)
})
  
# Output of the data table
  
output$contents <- DT::renderDataTable({
req(input$file)
df <- filedata1()
return(DT::datatable(df, options = list(scrollX = TRUE)))
})
  
# Output of the numeric predictors summary table
  
output$datasummary <- renderPrint({
    req(input$file)
    df <- filedata2()
    if (is.null(df)){return(NULL)}
    return(summary(df))
})
  
# Rendering the list to the ui
  
output$uiAdded <- renderUI({checkboxGroupInput('added', 'List of combinations', choices = names(interacts))})
  
# The main named list that will be used in other tasks
interacts <- reactiveValues()
makeReactiveBinding("interacts")
  
observe({
    input$actionBtnAdd # Trigger Add actions
    isolate({
    a <- c(input$makeInteract1,input$makeInteract2)
    b <- a %>% paste(collapse = "*")
    if(b != "")
    interacts[[b]] <- a
})})

all <- reactive({
  inFile <- input$file
  if (is.null(inFile)){return(NULL)}
  x <- as.data.frame(read.csv(inFile$datapath, fileEncoding="UTF-8-BOM"))
  
  y = dplyr::select_if(x, is.numeric)
  if(ncol(y)>2){
    p = subset(y, select = -c(Count))
    p <- unique(p)
    p = subset(p, select = -c(Year))
  }else {p = NULL}

  if(!is.null(p)){
  for(i in 1:ncol(p)){
  if(input$prednorm == "rnorm"){p[,i] <- round(rnorm(p[,i]), digits = 4)
  }else if(input$prednorm == "stand") {p[,i] <- round(scale(p[,i]), digits = 4)
  }else {p[,i] <- p[,i]}}}
  
  xx = cbind(Year = unique(x$Year), p)
  
  if(is.null(p)){
    Final = x
  }else {
    z = dplyr::select_if(x, is.factor)
    q <- x[ , (names(x) %in% c("Species", "Year", "Count"))]
    n = nrow(x)/length(unique(x$Year))
    p <- xx[rep(seq_len(nrow(xx)), n), ]
    
    Final = cbind(q, subset(p, select = -c(Year)), subset(z, select = -c(Species)))
  }
  return(Final)
})

# Checkbox list of all numeric variables to use 
independent <- reactive({
    if(!is.null(input$file)){
    df = all()[ , !(names(all()) %in% c("Count"))]
    return(names(df))
    }
})
  
output$independent <- renderUI({checkboxGroupInput("independent", "Independent (Predictor) Variables:", independent())})
  
# Variables to Add to the List of Combinations  
makeInteract <- reactive({
    if(!is.null(input$file)){
    df = all()[ , !(names(all()) %in% c("Count"))]
    return(names(df))}
})
  
output$makeInteract1 <- renderUI({selectInput("makeInteract1", "Variable1 For Interaction:", makeInteract())})
output$makeInteract2 <- renderUI({selectInput("makeInteract2", "Variable2 For Interaction:", makeInteract())})

# Fit SDM using R-INLA
  
fitsummary <- reactive({
    
    if(input$type == "Multi Species"){
      
    if(!is.null(input$independent)){
      df <- unique(all()[ , (names(all()) %in% c(as.vector(paste(input$independent, sep = ",")), "Species", "Count", "Year"))])
    }
    
    # "Count" for the specific distribution
    count = df$Count
      
    if(input$distribution == "Poisson Hurdle" | input$distribution == "Negative Binomial Hurdle"){
    count[count == 0] <- NA
    } else {count = count}
      
    df$Count <- count
    df$effect <- df$Year
    df$group <- rep(c(1:length(unique(df$Species))), each = nrow(df)/length(unique(df$Species)))
      
    # distribution
    if(input$distribution == "Poisson"){distribution = "poisson"
    } else if(input$distribution == "Negative Binomial"){distribution = "nbinomial"
    } else if(input$distribution == "Zeroinflated Poisson") {distribution = "zeroinflatedpoisson1"
    } else if(input$distribution == "Zeroinflated Negative Binomial") {distribution = "zeroinflatednbinomial1"
    } else if(input$distribution == "Poisson Hurdle") {distribution = "zeroinflatedpoisson0"
    } else {distribution = "zeroinflatednbinomial0"}
    
    if(!is.null(input$added)){
    formula = as.formula(paste("Count ~ 1 +", paste(input$independent, collapse = "+"), 
              paste("+", paste(input$added, collapse = "+")), 
              paste("+", "f(effect, model = ", input$tempeffect, ", group = group",
              ", control.group = list(model = ", input$controlgroup,"))")))
    
    }else{
    formula = as.formula(paste("Count ~ 1 +", paste(input$independent, collapse = "+"), 
              paste("+", "f(effect, model = ", input$tempeffect, ", group = group",
              ", control.group = list(model = ", input$controlgroup,"))")))
    }
    
    model <- INLA::inla(formula, data = df, family = distribution,
                        control.family = list(link = "log"),
                        control.compute = list(dic = TRUE, cpo = TRUE, po = TRUE), verbose = TRUE)
    return(model$summary.fixed[,c(1:3,5)])
    
    }else {
      
    if(!is.null(input$independent)){
      df <- unique(all()[ , (names(all()) %in% c(as.vector(paste(input$independent, sep = ",")), "Species", "Count", "Year"))])
    }      
    
    # "Count" for the specific distribution
    count = df$Count
      
    if(input$distribution == "Poisson Hurdle" | input$distribution == "Negative Binomial Hurdle"){
    count[count == 0] <- NA
    } else {count = count}
      
    df$Count <- count
    df$effect <- df$Year
      
    # distribution
    if(input$distribution == "Poisson"){distribution = "poisson"
    } else if(input$distribution == "Negative Binomial"){distribution = "nbinomial"
    } else if(input$distribution == "Zeroinflated Poisson") {distribution = "zeroinflatedpoisson1"
    } else if(input$distribution == "Zeroinflated Negative Binomial") {distribution = "zeroinflatednbinomial1"
    } else if(input$distribution == "Poisson Hurdle") {distribution = "zeroinflatedpoisson0"
    } else {distribution = "zeroinflatednbinomial0"}
      
    lst <- split(df, df$Species) 
      
    if(!is.null(input$added)){
    
    formula = as.formula(paste("Count ~ 1 +", paste(input$independent, collapse = "+"), 
              paste("+", paste(input$added, collapse = "+")), 
              paste("+", "f(effect, model = ", input$tempeffect, ")")))
    }else {
    formula = as.formula(paste("Count ~ 1 + ", paste(input$independent, collapse = "+"), 
              paste("+", "f(effect, model = ", input$tempeffect, ")")))
    }
    
    model <- list()
    results <- list()

    for (i in 1:length(unique(df$Species))){
      
    model[[i]] <- INLA::inla(formula, data = lst[[i]], family = distribution,
                  control.family = list(link = "log"),
                  control.compute = list(dic = TRUE,cpo = TRUE, po = TRUE), verbose = TRUE)
    
    results[[i]] <- model[[i]]$summary.fixed[,c(1:3,5)]
    
    }
    return(results)
    }
})
  
# Summary output of SDM
  
fitsum <- eventReactive(input$summary, {fitsummary()})
output$summary <- renderPrint({return(fitsum())})

url <- a("Definition", href="https://rdrr.io/github/andrewzm/INLA/man/inla.mesh.2d.html")
output$tab <- renderUI({tagList("URL link:", url)})
  
}

shinyApp(ui, server)
