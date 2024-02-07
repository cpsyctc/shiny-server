### CISD
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "CISD",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css for head blocks
  tags$head(
    tags$style(
      ".title {margin: auto; align: center}"
    )
  ),
  tags$div(class="title", titlePanel("Compute confidence interval around an observed SD\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones", align="center"),
    numericInput("SD",
                 "This should be the SD based on the n - 1 denominator, i.e. the unbiased estimator of the population SD (must be positive))",
                 value = 1,
                 min = 0,
                 max = 10^9,
                 width="100%"),
    numericInput("n",
                 "Dataset size (n: positive integer)",
                 value = 5000,
                 min = 0,
                 max = 1,
                 width="100%"),
    numericInput("ci",
                 "Width of inclusion interval of the RCI (usually .95, i.e. 95%)",
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
    p("This uses the function getCIaroundSD() going into my CECPfuns R package.  It's based on the excellent page at:",
      a("https://en.wikipedia.org/wiki/Standard_deviation",
        href="https://en.wikipedia.org/wiki/Standard_deviation"),
      " and cross-checked with ",
      a("https://www.statology.org/confidence-interval-standard-deviation/", 
      href="https://www.statology.org/confidence-interval-standard-deviation/")),
    p("This assumes Gaussian population distribution.  There are bootstrap ways to get a robust CI for the SD for",
      "non-Gaussian distributions in various R packages.  I may add an app for them later.\n\n"),
    p("This is a bit of a niche CI but it's important when thinking about the RCI (Reliable Change Index)\n\n"),
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
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # 3. Track basics and inputs and input values
  
  getCIaroundSD <- function(SD, n, ci, dp) {
    k <- n - 1 # df for the chisq values for the probabilities at the ends of the CI
    ### confidence limits are just the SD multiplied by those values
    UCL <- SD * sqrt(k) / sqrt(qchisq((1 - ci) / 2, k))
    LCL <- SD * sqrt(k) / sqrt(qchisq(1 - ((1 - ci) / 2), k))
    ci.perc <- round(100 * ci)
    retText <- paste0("Given:\n",
                      "   SD = ", round(SD, dp), "\n",
                      "   n = ", n, " and \n",
                      "   ", ci.perc, "% inclusion interval gives\n",
                      "   the confidence interval is from ", round(LCL, dp), " to ", round(UCL, dp), "\n\n")
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
    getCIaroundSD(input$SD,
          input$n,
          input$ci,
          input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
