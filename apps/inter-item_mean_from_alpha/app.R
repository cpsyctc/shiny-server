### Mean i-i-corr from alpha
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
# suppressMessages(library(CECPfuns))
suppressMessages(library(shiny.telemetry))
# suppressMessages(library(shinyDownload)) # from remotes::install_github("keithnewman/shinyDownload")
# suppressMessages(library(DT))

### function
getAvRfromAlphaAndK <- function(a, k) {
  1 / (1 + k/a - k)
}

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Mean i-i-corr from alpha",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Mean inter-item correlation for alpha and k\n\n")),
  
  p("This app simply uses the formula for the standardised Cronbach alpha coefficient to get back to a ",
    "mean inter-item correlation from that alpha and k.  You input the alpha and k it gives you the mean ",
    "inter-item correlation that would have given you that alpha for that k."),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("alpha",
                   "Cronback alpha value: .1 < rel < 1",
                   value = .65,
                   min = -.1,
                   max = .999999,
                   width = "100%"),
      helpText("In theory reliability can be negative but I have assumed you don't want to model that!"),
      numericInput("k",
                   "k: number of items in existing measure with alpha you put in previous box",
                   value = 10,
                   min = 3,
                   max = 200,
                   width = "100%"),
      numericInput("dp",
                   "Number of decimal places",
                   value = 2,
                   min = 0,
                   max = 5,
                   width="100%")
      ),
    
    mainPanel(
      h3("Mean inter-item correlation for that alpha and k", align="center"),
      verbatimTextOutput("res"),
      p("This incredibly basic shiny app uses the equation for a standardised Cronbach alpha",
        "to get a mean inter-item correlation that would have given that alpha given the k for the measure."),
      p(" "),
      p("The formula is standardised alpha is:"),
      p(" "),
      withMathJax("$$\\alpha = \\frac{kr}{1 +(k-1)r}$$"),
      p(" "),
      p("So by rearranging that (boy am I getting rusty at elementary algebra!) the formula to get that r from k and alpha is: "),
      withMathJax("$$\\frac{1}{(1 + \\frac{k}{\\alpha}-\\alpha)}$$"),
      p("App created by Chris Evans",
        a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/"),
        "26.vi.25 and licenced under a ",
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
  

  avR <- reactive({
    req(input$alpha)
    req(input$k)
    req(input$dp)
    round(getAvRfromAlphaAndK(input$alpha, input$k),
          input$dp)
   })
  
  output$res <- renderText(avR())
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
