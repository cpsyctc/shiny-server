### CIcorrelation
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry with default options (store to a local logfile)
# telemetry <- Telemetry$new(app_name = "CIcorrelation",
#                            data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  # use_telemetry(), # 2. Add necessary Javascript to Shiny
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css for head blocks
  tags$head(
    tags$style(
      ".title {margin: auto; align: center}"
    )
  ),
  tags$div(class="title", titlePanel("Confidence interval for a Pearson correlation\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones", align="center"),
    numericInput("n",
                 "Total n, (zero or positive integer)",
                 value = 100,
                 min = 0,
                 max = 10^9,
                 width="100%"),
    numericInput("R",
                 "Observed correlation",
                 value = .7,
                 min = -1,
                 max = 1,
                 width="100%"),
    numericInput("ci",
                 "Width of CI (usually .95, i.e. 95% CI, <=.99)",
                 value = .95,
                 min = .699999,
                 max = .999,
                 width="100%"),
    numericInput("dp",
                 "Number of decimal places",
                 value = 2,
                 min = 0,
                 max = 5,
                 width="100%")
  ),
  
  mainPanel(
    h3("Your input and results",align="center"),
    verbatimTextOutput("res"),
    p("This uses parametric assumptions, i.e. that distributions of the variables in the population are Gaussiann\n
        always dodgy.  If distributions are not Gaussian the CI can have coverage considerably off from what you want\n
        but if all you have is the observed correlations and the n it's as good as you can get.\n
        If you have the raw data I recommend you use the bootstrap CI of the Pearson correlation.\n
      I'll put up an app to do that when I can.\n\n"),
    p("App created by Chris Evans",
      a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/"),
      "licenced under a ",
      a("Creative Commons, Attribution Licence-ShareAlike",
        href="http://creativecommons.org/licenses/by-sa/1.0/"),
      " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
    hr(),
    includeHTML("https://shiny.psyctc.org/boilerplate.html")
  )
)
)


# Define server logic required
### this is the standard shiny server constructor
server <- function(input, output, session) {
  # telemetry$start_session(track_inputs = FALSE, track_values = FALSE) # 3. Minimal setup to track events, not even inputs
  ### 
  ### start with validation functions
  ### I don't think I actually use any these as I've now used numericInput() to set the ranges

  ### 
  ### now the functions adapted from CECPfuns plotCIcorrelation
  ###
  getCI <- function(R, n, ci = 0.95, dp = 2) {
    z <- atanh(R)
    norm <- qnorm((1 - ci)/2)
    den <- sqrt(n - 3)
    zl <- z + norm/den
    zu <- z - norm/den
    rl <- tanh(zl)
    ru <- tanh(zu)
    ci.perc <- round(100 * ci)
    retText <- paste0("Given:\n",
                      "   R = ", R,"\n",
                      "   n = ", n,"\n",
                      "   observed correlation = ", round(R, dp),
                      "\n",
                      "   ", ci.perc, "% confidence interval from ", round(rl, dp),
                      " to ", round(ru, dp),"\n\n")
    return(retText)
  }
  
  output$res <- renderText({
    # validate(
    #   need(checkForPosInt(input$n, minInt = 0), 
    #        "n must be a positive integer > 10 and < 10^9"),
    #   need(checkNumRange(input$R, minx = -1, maxx = 1, incEq = TRUE),
    #        "R must be a value >= -1.0 and <= 1.0"),
    #   need(checkNumRange(input$ci, minx = .69999, maxx = 1, incEq = FALSE),
    #        "ci must be a value > .7 and < .99"),
    #   need(checkForPosInt(input$dp, minInt = 1, maxInt = 5),
    #        "dp must be a value between 1 and 5")
    # )
    getCI(input$R,
          input$n,
          input$ci,
          input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
