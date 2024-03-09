### RCI1
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))

### telemetry 1. Initialize telemetry with default options (store to a local sqlite database)
telemetry <- Telemetry$new(app_name = "RCI1",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # telemetry 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  # h1(HTML("<center>Compute RCI from SD and reliability (and inclusion interval if not 95%)</center>")),
  h1(HTML("Compute RCI from SD and reliability<br>(and inclusion interval if not 95%)"), align = "center"),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Replace the values here with your own", align="center"),
    numericInput("SD",
                 "Your baseline score standard deviation (SD)",
                 value = 11.1,
                 min = 0,
                 max = 10^9,
                 width="100%"),
    helpText("A standard deviation must be numeric and positive!"),
    numericInput("rel",
                 "Reliability",
                 value = .9,
                 min = 0,
                 max = 1,
                 width="100%"),
    helpText("Generally this will be the Cronbach's alpha from your data but may be a referential value."),
    numericInput("ci",
                 "Inclusion interval",
                 value = .95,
                 min = .699999,
                 max = .999,
                 width="100%"),
    helpText("This is usually .95, i.e. 95% inclusion/coverage."),
    numericInput("dp",
                 "Number of decimal places",
                 value = 2,
                 min = 0,
                 max = 5,
                 width="100%"),
    helpText("When you have filled in the above, hit this next button to get the RCI!"),
    actionButton("compute", "Compute RCI")
  ),
  
  mainPanel(
    
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Result", 
                         verbatimTextOutput("res")
                ),
                
                tabPanel("Explanation/information", 
                         h3("Computation background"),
                         p(HTML("This uses the function <em>getRCIfromSDandAlpha()</em> in my "),
                           a("CECPfuns R package",
                             href = 'https://cecpfuns.psyctc.org/'),
                           "but the maths is trivial and\n
       straight out of the original work on the RCI."),
                         h3("Maths of the RCI "),
                         p("Ignore this next bit if you don't like equations!"),
                         p("The full equation is:"),
                         withMathJax("$$SD*\\sqrt{2}*\\sqrt{1-rel}*qnorm(1 - \\frac{(1 - ci)}{2})$$"),
                         p("where 'SD' is the SD you input, 'rel' the reliability you input and 'ci' the inclusion interval you wanted, usually .95, i.e. 95% and 'qnorm' is the quantile of the standard Gaussian/Normal distribution for that ci."),
                         p("So if you asked for the usual 95% interval that simplifies to this:"),
                         withMathJax("$$SD*\\sqrt{2}*\\sqrt{1-rel}*1.96$$"),
                         p("because 1.96 is the value of the standard Gaussian that maps to 95% inclusion"),
                         h3("Recommendations"),
                         p("If you have the raw item data and n > 20 I recommend you\n
       compute the Cronbach alpha internal reliability for your own data. I will put up an app to compute that\n
       for raw item data when I can.\n\n"),
                         h3("Next please ..."),
                         p("Unless you are very familiar with it, please now go to the 'Background' tab and read the information there.")
                ),
                         
                tabPanel("Background", 
                         p("App created by Chris Evans",
                           a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/"),
                           "licenced under a ",
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
  
  ### 
  ### now the functions adapted from CECPfuns plotCIcorrelation
  ###
  getRCI <- function(SD, rel, ci = 0.95, dp = 2) {
    RCI <- CECPfuns::getRCIfromSDandAlpha(SD, rel, ci)
    ci.perc <- round(100 * ci)
    retText <- paste0("Given:\n",
                      "   SD = ", round(SD, dp), "\n",
                      "   Reliability = ", round(rel, dp), " and ", ci.perc, "% inclusion interval gives\n",
                      "   RCI = ", round(RCI, dp), "\n\n",
                      "So given that SD, and reliability, you would expect ",
                      ci.perc,
                      "% of your data to have shown absolute change\n (increase or decrease), of less than or equal to ",
                      round(RCI, dp), 
                      " were change simply down to measurement unreliability\n i.e. there being no 'real' change happening.\n\n",
                      "Go to 'Explanation/information' tab for more explanation about the RCI.")
    return(retText)
  }
  
  RCI <- eventReactive(input$compute, {
    validate(
      need(input$SD, "You must give a usable value for SD"),
      need(input$rel, "You must give a usable value for reliability"),
      need(input$ci, "You must give a usable value for ci (usually .95)"),
      need(input$dp, "You must give a usable value for dp (decimal places)")
    )
    getRCI(input$SD,
           input$rel,
           input$ci,
           input$dp)
  })
  
  output$res <- renderText({
    RCI()
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
