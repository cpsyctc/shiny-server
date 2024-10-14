### getCorrectedR: correction of correlation for unreliability

suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(CECPfuns))
suppressMessages(library(shiny.telemetry))

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "getCorrectedR",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

### function that gets the attenuated R
getCorrectedR <- function(obsR, rel1, rel2) {
  ### function that takes an unattenuated correlation 
  ### and the reliabilities of the two variables
  ### and returns the attenuated correlation
  obsR / sqrt(rel1 * rel2)
}

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("'Correction' of an observed correlation for unreliability\n\n")),
  
  p("This app simply uses the standard formula for attenuation of correlation by unreliability given an 'corrected' correlation",
    "from an observed correlation and reliabilities for the two variables."),
  HTML("<center"),
  p(" "),
  a("https://shiny.psyctc.org/apps/getCorrectedR/", 
    href = "https://shiny.psyctc.org/apps/getCorrectedR/"),
  HTML("</center>"),
  p(" "),
  p(" "),
  HTML("<hr>"),
  p(" "),
  p(" "),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("obsR",
                   "Observed correlation you are considering: -1 < R < 1",
                   value = .65,
                   min = -.1,
                   max = .999999,
                   width = "100%"),
      numericInput("rel1",
                   "Reliability of the first variable",
                   value = .7,
                   min = .2,
                   max = .999,
                   width = "100%"),
      helpText("In theory reliability can be negative but I've set the lowest value you might use to .2"),
      numericInput("rel2",
                   "Reliability of the second variable",
                   value = .7,
                   min = .2,
                   max = .999,
                   width = "100%"),
      helpText("In theory reliability can be negative but I've set the lowest value you might use to .2"),
      numericInput("dp",
                   "Number of decimal places for the computed value",
                   value = 3,
                   min = 1,
                   max = 7,
                   width="100%"),
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Corrected_correlation",
                           p("The 'corrected' correlation for the inputs you gave is:"),
                           verbatimTextOutput("res"),
                           p("Please see the 'Explanation' tab for my health warning about this 'correction'.")
                  ),
                  
                  tabPanel("Explanation",
                           p("This is ancient psychometrics but still of some use.  For more information, see:"),
                           a("https://www.psyctc.org/psyctc/glossary2/attenuation-by-unreliability-of-measurement/", 
                             href = "https://www.psyctc.org/psyctc/glossary2/attenuation-by-unreliability-of-measurement/"),
                           p(" "),
                           p("The formula is simple:"),
                           p(" "),
                           withMathJax("$$correctedR = \\frac{obsR}{\\sqrt{rel_{x} * rel_{y}}}$$"),
                           p(paste0("The short summary is that unreliability in the measurement of both variables involved in a correlation ",
                                    "always reduces the observed correlation between the variables from what it would have been had the ",
                                    "variables been measured with no unreliability (which is essentially impossible for any self-report measures ",
                                    "and pretty much any measures used in our fields.)")),
                           p(paste0("I am putting 'scare quotes' round 'corrected' throughout this app because I think it's important to be very",
                                    " cautious about this 'correction' as it can return a correlation above 1.0 where you have a reasonable observed",
                                    " correlation and low reliabilities and as for any observed correlation, the lower your reliabilities the higher",
                                    " the 'corrected' correlation which feels uncomfortable to me for all I see the logic!"))),
                  
                  tabPanel("Background", 
                           p("App created 13.x.24 by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 13.x.24."),
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
  
  checkAltB <- function(A, B){
    if (B <= A) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  transR <- reactive({
    req(input$obsR)
    req(input$rel1)
    req(input$rel2)
    
    validate(
      need(checkAltB(input$obsR, 1), 
           "obsR must be smaller than 1"),
      need(checkAltB(-1, input$obsR), 
           "obsR must be greater than -1"),
      need(checkAltB(.2, input$rel1), 
           "rel1 must be greater than .2"),
      need(checkAltB(input$rel1, .99999), 
           "rel1 must be less than .99999"),
      need(checkAltB(.2, input$rel2), 
           "rel2 must be greater than .2"),
      need(checkAltB(input$rel2, .99999), 
           "rel2 must be less than .99999"),      
      
    )
    
    getCorrectedR(input$obsR, input$rel1, input$rel2)
  })
  
  # output$titleText <- renderText({input$title})
  
  output$res <- renderText({
    if (transR() < 1) {
      round(transR(), input$dp)
    } else {
      paste0("The 'corrected' correlation is ",
             round(transR(), input$dp),
             " which, as you can see, is over 1.0\n which is impossible.\n",
             " This illustrates that the 'correction' can sometimes be misleading.")
    }
  })
  
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
