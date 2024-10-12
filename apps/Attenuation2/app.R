### Attentuation of correlation by unreliability (2)

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
telemetry <- Telemetry$new(app_name = "Attenuation2",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

### function that gets the attenuated R
getAttenuatedR <- function(unattR, rel1, rel2) {
  ### function that takes an unattenuated correlation 
  ### and the reliabilities of the two variables
  ### and returns the attenuated correlation
  unattR * sqrt(rel1 * rel2)
}

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Attentuation of correlation by unreliability (2)\n\n")),
  
  p("This app simply uses the standard formula to show how correlation is attenuated by unreliability given an unattenuated correlation",
    "and two reliabilities of the two variables."),
  p("I have created another more complicated app that allows you to put in a range of reliabilities and to get a plot of the attenuated correlations",
    " for a range of unattenuated correlations with a plot and a downloadable table of the correlations: "),
  HTML("<center"),
  p(" "),
  a("https://shiny.psyctc.org/apps/Attenuation/", 
    href = "https://shiny.psyctc.org/apps/Attenuation/"),
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
      numericInput("unattR",
                   "Unattenuated correlation you are considering: -1 < R < 1",
                   value = .65,
                   min = -.1,
                   max = .999999,
                   width = "100%"),
      helpText("Remember this is unknowable correlation in the model, not an observed correlation."),
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
                  
                  tabPanel("Transformed_correlation",
                           p("The attenuated correlation for the inputs you gave is:"),
                           verbatimTextOutput("res"),
                  ),
                  
                  tabPanel("Explanation",
                           p("This is ancient psychometrics but still of some use.  For more information, see:"),
                           a("https://www.psyctc.org/psyctc/glossary2/attenuation-by-unreliability-of-measurement/", 
                             href = "https://www.psyctc.org/psyctc/glossary2/attenuation-by-unreliability-of-measurement/"),
                           p(" "),
                           p("The formula is simple:"),
                           p(" "),
                           withMathJax("$$correctedCorr = unattenuatedCorr * \\sqrt{rel_{x} * rel_{y}}$$"),
                           p(paste0("The short summary is that unreliability in the measurement of both variables involved in a correlation ",
                                    "always reduces the observed correlation between the variables from what it would have been had the ",
                                    "variables been measured with no unreliability (which is essentially impossible for any self-report measures ",
                                    "and pretty much any measures used in our fields.)"))),
                  
                  tabPanel("Background", 
                           p("App created 10.x.24 by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 12.x.24."),
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
    req(input$unattR)
    req(input$rel1)
    req(input$rel2)
    
    validate(
      need(checkAltB(input$unattR, 1), 
           "unattR must be smaller than 1"),
      need(checkAltB(-1, input$unattR), 
           "unattR must be greater than -1"),
      need(checkAltB(.2, input$rel1), 
           "rel1 must be greater than .2"),
      need(checkAltB(input$rel1, .99999), 
           "rel1 must be less than .99999"),
      need(checkAltB(.2, input$rel2), 
           "rel2 must be greater than .2"),
      need(checkAltB(input$rel2, .99999), 
           "rel2 must be less than .99999"),      
      
    )
    
    getAttenuatedR(input$unattR, input$rel1, input$rel2)
  })
  
  # output$titleText <- renderText({input$title})
  
  output$res <- renderText({
    round(transR(), input$dp)
  })
  
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
