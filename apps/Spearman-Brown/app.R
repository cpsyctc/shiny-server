### Spearman-Brown

suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(CECPfuns))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload)) # from remotes::install_github("keithnewman/shinyDownload")
suppressMessages(library(DT))

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Spearman-Brown",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Use Spearman-Brown formula to predict reliability\nby number of items\n\n")),
  
  p("This app simply uses the Spearman-Brown formulat to predict changes in internal reliability ",
    "if changing the length (number of items) in a multi-item measure."),
  p("You input the existing reliability and length of the measure and define the range of other lengths ",
    "you want and the y-axis range for the plot."),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("currRel",
                   "Reliability of current form: .1 < rel < 1",
                   value = .65,
                   min = -.1,
                   max = .999999,
                   width = "100%"),
      helpText("In theory reliability can be negative but I have assumed you don't want to model that!"),
      numericInput("currK",
                   "k: number of items in existing measure with reliability you put in previous box",
                   value = 10,
                   min = 3,
                   max = 200,
                   width = "100%"),
      helpText("No point in modelling k < 3 and k > 200 unrealistic"),
      numericInput("minK",
                   "Smallest k you want to model",
                   value = 3,
                   min = 3,
                   max = 199,
                   width = "100%"),
      helpText("No point in modelling k < 3 and k > 200 unrealistic"),
      numericInput("maxK",
                   "Largest k you want to model",
                   value = 50,
                   min = 4,
                   max = 200,
                   width = "100%"),
      helpText("No point in modelling k < 3 and k > 200 unrealistic"),
      numericInput("step",
                   "Step size to plot (optional)",
                   value = 1,
                   min = 1,
                   max = 10,
                   width = "100%"),
      numericInput("minY",
                   "Smallest reliability value on the y axis you want plotted",
                   value = 0,
                   min = 0,
                   max = .8,
                   width = "100%"),
      numericInput("maxY",
                   "Largest reliability value on the y axis you want plotted",
                   value = 1,
                   min = .5,
                   max = 1,
                   width = "100%"),
      numericInput("dp",
                   "Number of decimal places for the computed reliabilities",
                   value = 3,
                   min = 1,
                   max = 7,
                   width="100%"),
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot", 
                           p("This shows the plot."),
                           p("You can use the dialogue below the plot to download it chosing the name and the format of the plot"),
                           p(" "),
                           uiOutput("title"),
                           # uiOutput("xLab"),
                           # uiOutput("yLab"),
                           plotOutput("relPlot"),
                           downloadGGPlotButtonUI("plotDownload", "reliabilitiies"),
                           p(" "),
                           p("Thanks to Keith Newman for the download handler: ",
                             a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file"))),
                  
                  tabPanel("Reliabilities", 
                           p(" "),
                           p("This shows all the data for the selected variable.  Buttons at the bottom allow you to export the data."),
                           p(" "),
                           DT::dataTableOutput("reliabilities")),
                  
                  tabPanel("Explanation",
                           p("This is ancient psychometrics but still of some use.  For more information, see:"),
                           a("https://www.psyctc.org/Rblog/posts/2021-04-09-spearman-brown-formula", 
                             href = "https://www.psyctc.org/Rblog/posts/2021-04-09-spearman-brown-formula"),
                           p(" "),
                           p("The formula is simple:"),
                           p(" "),
                           withMathJax("$$ \\rho^{*}=\\frac{n\\rho}{1 + (n-1)\\rho} $$"),
                           p(paste0("The short summary is that any multi-item measure will have overall internal reliability/consistency ",
                                    "that is a function of the mean inter-item correlations and the number of items and, for any mean ",
                                    "inter-item correlation, a longer measure will have a higher reliability.  The formula for the ",
                                    "relationship was published separately by both Spearman in 1910 and in the same year by Brown, ",
                                    "who was working for Karl Pearson, who had a running feud with Spearman.  See:")),
                           a("https://en.wikipedia.org/wiki/Spearman%E2%80%93Brown_prediction_formula#History",
                              href = "https://en.wikipedia.org/wiki/Spearman%E2%80%93Brown_prediction_formula#History"),
                           p(paste0("That also gives some arguments that the formula should really be termed the Brown-Spearman formula, ",
                                    "but I am bowing to historical precedent here."))),
                  
                  tabPanel("Background", 
                           p("App created 2.v.24 by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 5.iv.25 to fix download not downloading all rows of the reliabilities."),
                           p("Licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           includeHTML("https://shiny.psyctc.org/boilerplate.html"))
      ),
    )
  )
)

# Define server logic required
server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # 3. Track basics and inputs and input values
  
  ### 
  ### start with validation functions
  ###
  checkKvalues <- function(minK, maxK){
    if (maxK <= minK) {
      return(FALSE)
    }
    return(TRUE)
  }
  checkYvalues <- function(minY, maxY){
    if (maxY <= minY) {
      return(FALSE)
    }
    return(TRUE)
  }
  ### 
  ###
  
  relDat <- reactive({
    tibble(k = seq(input$minK, input$maxK, input$step)) %>%
      rowwise() %>%
      mutate(kRatio = k / input$currK,
             rel = getRelBySpearmanBrown(input$currRel,
                                         kRatio,
                                         verbose = FALSE)) %>%
      ungroup()
  })
  
  plotSpearmanBrown <- function(tibDat, currRel, currK, minK, maxK, minY, maxY) {
    # relPlot <- reactive({
    suppressWarnings(suppressMessages(ggplot(data = tibDat,
                                             aes(x = k, y = rel)) +
                                        geom_point() +
                                        geom_line() +
                                        ylim(c(minY, maxY)) +
                                        ylab("Reliability") +
                                        xlab("k: number of items in new meausure") -> p ))
    if(minK <= currK & currK <= maxK) {
      p +
        geom_vline(xintercept = currK,
                   linetype = 3) +
        geom_hline(yintercept = currRel,
                   linetype = 3) +
        ggtitle("Predicted reliability against measure length",
                subtitle = "Reference lines mark existing measure") -> p
    } else {
      p +
        ggtitle("Predicted reliability against measure length") -> p
    }
    return(p)
  }
  
  relPlot <- reactive({
    plotSpearmanBrown(tibDat(), input$currRel, input$currK, input$minK, maxK, minY, maxY)
  })
  
  output$titleText <- renderText({input$title})
  
  output$relPlot <- renderPlot({
    req(input$currRel)
    req(input$currK)
    req(input$minK)
    req(input$maxK)
    req(input$minY)
    req(input$maxY)
    validate(
      need(checkKvalues(input$minK, input$maxK), 
           "maxK must be greater than minK"),
      need(checkYvalues(input$minY, input$maxY),
           "maxY must be greater than minY"),
    )
    plotSpearmanBrown(relDat(), input$currRel, input$currK, input$minK, input$maxK, input$minY, input$maxY)
  })
  
  downloadGGPlotButtonServer(
    id = "plotDownload", # <= this should match the ID used in the UI module
    ggplotObject = relPlot # No parentheses here to pass *expression*
  )
  
  relDat2 <- reactive({
    req(input$dp)
    relDat() %>%
      mutate(rel = round(rel, input$dp)) 
  })
  
  output$reliabilities <- DT::renderDataTable(
    DT::datatable({relDat2()},
                  extensions = "Buttons",
                  options = list(                                                     
                    fixedColumns = TRUE,
                    pageLength = -1,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'frtipB',
                    editable = FALSE,
                    searching = FALSE,
                    buttons = c('copy', 'csv', 'excel', "pdf")
                  ),
    )
  )
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
