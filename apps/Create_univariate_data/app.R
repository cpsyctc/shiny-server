### Create_univariate_data
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(tidyverse))

### telemetry 1. Initialize telemetry with default options (store to a local sqlite database)
telemetry <- Telemetry$new(app_name = "RCI1",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # telemetry 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Create a sample of data from a known population"), 
     align = "center"),
  
  p("This app creates samples from distributions you set.  For the moment I am only doing Gaussian and continuous uniform distributions.",
    "There are situations in which you may need these data for use outside these apps but I created this app mainly to allow you to generate",
    "data that you might want to plug into other apps here."),
  p("You input data using the sidebar on the left to select the distribution you want and then the sample and population characteristics",
    "you want.  The options for those depend on the distribution you want.  Hit the 'generate' button when you have the specifications you want.",
    "The data come up in the tabs to the right, one gives them as a vector you can copy, the next as a single variable table that can be",
    "downloaded in various file formats."),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h4("Define what you want and hit the generate button", align="center"),
      
      radioButtons("dist", "Distribution type:",
                   c("Gaussian ('Normal')" = "Gauss",
                     "Continous uniform" = "contUnif"),
                   selected = "Gauss"),
      
      conditionalPanel(condition = "input.dist == 'Gauss'",
                       helpText("Values defining your sample from a Gaussian distribution"),
                       
                       numericInput("seed", "Set the seed for the random number generator",
                                    value = 12345,
                                    min = 1,
                                    max = 9999,
                                    step = 1,
                                    width = "100%"),
                       helpText(paste0("The same seed will generate the same, i.e. replicable, sample",
                                       "\nMust be between 1 and 9999 inclusive")),
                       
                       numericInput("n", "Size of sample you want",
                                    value = 10,
                                    min = 3,
                                    max = 99999,
                                    step = 1,
                                    width = "100%"),
                       helpText("Must be between 3 and 99999 inclusive"),
                       
                       numericInput("mean", "Population mean you want",
                                    value = 0,
                                    min = -999,
                                    max = 999,
                                    width = "100%"),
                       helpText(paste0("Replace '0' with whatever population mean you want",
                                       "\nMust be between -999 and 999 inclusive")),
                       
                       numericInput("sd", "Population SD you want",
                                    value = 1,
                                    min = -999,
                                    max = 999,
                                    width = "100%"),
                       helpText(paste0("Replace '1' with whatever population SD you want",
                                       "\nMust be between -999 and 999 inclusive")),
      ),
      
      conditionalPanel(condition = "input.dist == 'contUnif'",
                       helpText("Values defining your sample from a continous uniform distribution"),
                       
                       numericInput("seed", "Set the seed for the random number generator",
                                    value = 12345,
                                    min = 1,
                                    max = 9999,
                                    step = 1,
                                    width = "100%"),
                       helpText(paste0("The same seed will generate the same, i.e. replicable, sample",
                                       "\nMust be between 1 and 9999 inclusive")),
                       
                       numericInput("n", "Size of sample you want",
                                    value = 10,
                                    min = 3,
                                    max = 99999,
                                    step = 1,
                                    width = "100%"),
                       helpText("Must be between 3 and 99999 inclusive"),
                       
                       numericInput("min", "Popuation minimum value you want",
                                    value = 10,
                                    min = -999,
                                    max = 999,
                                    step = 1,
                                    width = "100%"),
                       helpText("Replace '10' with whatever you want. Must be between -999 and 999 inclusive and smaller than the maximum you set."),
                       
                       numericInput("max", "Popuation maximum value you want",
                                    value = 11,
                                    min = -999,
                                    max = 999,
                                    step = 1,
                                    width = "100%"),
                       helpText("Replace '11' with whatever you want. Must be between -999 and 999 inclusive and larger than the maximum you set."),
      ),
      
      numericInput("dp", "Number of decimal places",
                   value = NA,
                   min = 0,
                   max = 9,
                   step = 1,
                   width = "100%"),
      helpText("Must be NA or between 0 and 9 inclusive, setting 0 rounds to nearest integer (simple way to get discrete integer data)"),
      
      actionButton("generate", "Generate the data!"),
      
      helpText("You can update all the inputs above and hit the button again to get a new set of data.")
    ),
    
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Data vector", 
                           verbatimTextOutput("rawDat")
                           # verbatimTextOutput("outputMessage"),
                           # p(" "),
                           # p("As raw numbers that you can copy and paste elsewhere, first space separated:"),
                           # verbatimTextOutput("resSpace"),
                           # p("As raw numbers that you can copy and paste elsewhere, next comma separated:"),
                           # verbatimTextOutput("resComma"),
                           # p("As raw numbers that you can copy and paste elsewhere, semicolon separated:"),
                           # verbatimTextOutput("resSemicolon"),
                           # p("As raw numbers that you can copy and paste elsewhere, finally tab separated:"),
                           # verbatimTextOutput("resTab"),
                  ), 
                  
                  tabPanel("Data table",
                           p("If you have generated your sample this shows the same as a long table with two variables, the row number and then the values.  ",
                           "The buttons at the bottom allow you to export the data."),
                           p(" "),
                           DT::dataTableOutput("dataTable")),
                  
                  tabPanel("Explanation/information", 
                           h3("Computation background"),
                           p("This uses the built in random number generation functions,",
                             "currently rnorm() and runif() that are in the standard, i.e. attached by default,",
                             "package 'stats'.  This run used R version."),
                           verbatimTextOutput("version"),
                           p("Rather than putting more about them here where they might get out of date,",
                             "I suspect that if you want more detail, you are geeky enough to find the information",
                             "by getting your own copy of R and using that or by searching on the internet."),
                           
                           h3("Next please ..."),
                           p("Unless you are very familiar with it, please now go to the 'Background' tab and read the information there.")
                  ),
                  
                  tabPanel("Background", 
                           p("App created 7.iv.24 by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/"),
                             p("Last updated 7.iv.24."),
                             "Licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           includeHTML("https://shiny.psyctc.org/boilerplate.html"))
      ),
    )
  )
)



# Define server logic required
### this is the standard shiny server constructor
server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # telemetry 3. Track basics and inputs and input values
  
  data <- eventReactive(input$generate, {
    # validate(
    #   need(input$dist == "contUnif" & (input$max > input$min), "Your population maximum must be bigger than the population minimum!")
    # )
    if (input$dist == "contUnif" & (input$max <= input$min)) {
      validate("If chosen distribution is continuous uniform then the population maximum must be bigger than the population minimum ")
    }
    set.seed <- input$seed
    if (input$dist == "Gauss") {
      vecDat <- rnorm(input$n, input$mean, input$sd)
    } else {
      vecDat <- runif(input$n, input$min, input$max)
    }
    if (!is.na(input$dp)) {
      vecDat <- round(vecDat, input$dp)
    }
    vecDat
  })
  
  nRowData <- reactive({
    length(data())
  })
  
  output$rawDat <- renderText({
    paste0("You have ",
           nRowData(),
           " rows of data as follows.\n",
           "   As a space separated vector:\n",
           paste(data(), collapse = " "),
           "\n\n   As a comma separated vector:\n",
           paste(data(), collapse = ", "),
           "\n\n\   As a semicolon separated vector:\n",
           paste(data(), collapse = "; "),
           "\n\n   And finally as a tab separated vector:\n",
           paste(data(), collapse = "\t ")
    )
  })
  
  output$dataTable <- DT::renderDataTable(
    DT::datatable({as_tibble(data())},
                  extensions = "Buttons",
                  options = list(                                                     
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'frtipB',
                    editable = FALSE,
                    searching = FALSE,
                    buttons = c('copy', 'csv', 'excel', "pdf"))
    )
  )
  
  output$version <- renderText({
    str_remove(R.version.string, fixed("R version "))
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
