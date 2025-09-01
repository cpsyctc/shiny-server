### Get s value from p value
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(shiny.telemetry))

### function
getSvalFromPval <- function(pVal) {
  sVal <- -log(pVal, base = 2)
}


### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "get_Sval_from_Pval",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Get s value from p value\n\n")),
  
  p("This app uses the very simple formula that takes a p value (or any probability of an observed event) ",
  "and gives you an s value (surprisal or self-information value or Shannon information/entropy score) for ",
  "that p value."),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("pVal",
                   "p value: 0 < rel < 1",
                   value = .05,
                   min = 10^-10,
                   max = .999999,
                   width = "100%"),
      helpText("Must be a valid probability so greater than zero and less than or equal to 1.0"),
      numericInput("dp",
                   "Number of decimal places",
                   value = 2,
                   min = 0,
                   max = 5,
                   width="100%")
      ),
    
    mainPanel(
      h3("Getting s value from p value", align="center"),
      verbatimTextOutput("res"),
      p("This incredibly basic shiny app uses the simple equation for the conversion."),
      p(" "),
      p("The formula for standardised alpha is:"),
      p(" "),
      withMathJax("$$s=-log_{2}{p}$$"),
      p(" "),
      p("where p is the p value."),
      p("  "),
      p("App created by Chris Evans",
        a("PSYCTC.org",
          href="https://www.psyctc.org/psyctc/about-me/"),
        "1.ix.25 Licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href="http://creativecommons.org/licenses/by-sa/1.0/"),
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
      hr(),
      includeHTML("https://shiny.psyctc.org/boilerplate.html")
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
  ### 
  ###
  

  output$titleText <- renderText({input$title})
  
  sVal <- reactive({
    req(input$pVal)
    req(input$dp)
    sVal <- round(getSvalFromPval(input$pVal),
                  input$dp)
    paste0("The s value corresponding to that p value is ",
           sVal)
   })
  
  output$res <- renderText(sVal())
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
