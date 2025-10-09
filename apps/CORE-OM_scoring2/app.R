### App that might help SystmOne (sic) users of the CORE-OM
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(shiny.telemetry))

### function
getSvalFromPval <- function(pVal) {
  sVal <- -log(pVal, base = 2)
}


### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "CORE-OM_scoring2",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("CORE-OM scores for SystmOne users\n\n")),
  
  p("I have just learned that the ways that",
    a("SystmOne",
      href="https://en.wikipedia.org/wiki/SystmOne"),
    "scores CORE-OM data re pretty unhelpful.  This app might help.  ",
    "For now it assumes that the SystmOne screen returns domain scores as means ",
    "and this gives the correct total score."),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput(inputId = "Func",
                   label = "Functioning",
                   value = .0,
                   min = 0,
                   max = 4,
                   width = "100%"),
      helpText("Must be a  number between zero and 4."),
      numericInput(inputId = "Prob",
                   label = "Problems",
                   value = .0,
                   min = 0,
                   max = 4,
                   width = "100%"),
      helpText("Must be a  number between zero and 4."),
      numericInput(inputId = "Risk",
                   label = "Risk",
                   value = .0,
                   min = 0,
                   max = 4,
                   width = "100%"),
      helpText("Must be a  number between zero and 4."),
      numericInput(inputId = "WB",
                   label = "Well being",
                   value = .0,
                   min = 0,
                   max = 4,
                   width = "100%"),
      helpText("Must be a  number between zero and 4."),
      radioButtons( 
        inputId = "MeanClin", 
        label = "Do you want the total score in mean format (range 0 to 4) or 'clinical' (range 0 to 40)?", 
        choices = list( 
          "Mean" = 1, 
          "'Clinical'" = 2
        ) 
      ), 
    ),
    
    mainPanel(
      h3("Getting CORE total score from domain mean scores", align="center"),
      verbatimTextOutput("res"),
      p("This incredibly basic shiny app ...."),
      p(" "),
      p("App created by Chris Evans",
        a("PSYCTC.org",
          href="https://www.psyctc.org/psyctc/about-me/"),
        "9.x.25 Licenced under a ",
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
  isValidMeanScore <- function(x) {
    if(x < 0 | x > 4 | is.na(x)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  
  output$titleText <- renderText({input$title})
  
  Scores <- reactive({
    req(input$Func)
    req(input$Prob)
    req(input$WB)
    req(input$Risk)
    req(input$MeanClin)
    validate(
      need(
        isValidMeanScore(input$Func),
        "value must be between zero and four (inclusive), i.e. 0 <= value <= 4"),
      need(
        isValidMeanScore(input$Prob),
        "value must be between zero and four (inclusive), i.e. 0 <= value <= 4"),
      need(
        isValidMeanScore(input$WB),
        "value must be between zero and four (inclusive), i.e. 0 <= value <= 4"),
      need(
        isValidMeanScore(input$Risk),
        "value must be between zero and four (inclusive), i.e. 0 <= value <= 4")
    )
    totF <- 12 * input$Func
    totP <- 12 * input$Prob
    totW <- 4 * input$WB
    totR <- 6 * input$Risk
    totAll <- totF + totP + totW + totR
    totNR <- totF + totP + totW
    meanAll <- round(totAll / 34, 2)
    meanNR <- round(totNR / 28, 2)
    if (input$MeanClin == 1) {
      retVal <- paste0("The mean total score for those values is ",
                       meanAll,
                       "\nand the mean NR (non-risk) score is ",
                       meanNR)
    } else {
      retVal <- paste0("The 'clinical' total score for those values is ",
                       meanAll,
                       "\nand 'clinical' NR (non-risk) score is ",
                       meanNR)
    }
    retVal
  })
  
  output$res <- renderText(Scores())
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
