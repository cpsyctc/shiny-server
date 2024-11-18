### CIcorrelation
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry storing, I hope, to sqlite db in shiny server root 
telemetry <- Telemetry$new(app_name = "CIcorrelation",
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
      p("This uses parametric assumptions, i.e. that distributions of the variables in the population are Gaussian.\n
        That's always dodgy!  If distributions are not Gaussian the CI can have coverage considerably off from what you want\n
        but if all you have is the observed correlations and the n it's as good as you can get.\n
        If you have the raw data I recommend you use the bootstrap CI of the Pearson correlation.\n
      I'll put up an app to do that when I can.\n\n"),
      p("App created by Chris Evans ",
        a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/"),
        " 27.xi.23 and last updated (adding SE calculations) 18.xi.24.",
        "\nIt is licenced under a ",
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
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # 3. Minimal setup to track events, not even inputs
  ### 
  ### start with validation functions
  checkIsInteger <- function(x) {
    isTRUE(all.equal(x, round(x), tolerance = sqrt(.Machine$double.eps)))
  }
  checkIsGtrThan <- function(x, y) {
    x > y
  }
  checkIsLessThan <- function(x, y) {
    x < y
  }
  checkIsGtrOrEq <- function(x, y) {
    x >= y
  }
  checkIsLessThanOrEq <- function(x, y) {
    x >= y
  }
  checkIsInRange <- function(x, lwr, upr, inclusive = TRUE) {
    if (inclusive) {
      (x >= lwr) & (x <= upr)
    } else {
      (x > lwr) & (x < upr)
    }
  }
  
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
    SE1 <- sqrt((1 - R^2) / (n - 2))
    z <- atanh(.975)
    SEz <- tanh(1 / sqrt(n - 3))
    retText <- paste0("Given:\n",
                      "   R = ", R,"\n",
                      "   n = ", n,"\n",
                      "   observed correlation = ", round(R, dp),
                      "\n",
                      "   ", ci.perc, "% confidence interval from ", round(rl, dp),
                      " to ", round(ru, dp),
                      "\n\n",
                      "A nice little sideline here is that there are two ways to compute the SE",
                      "\n (Standard Error) of a Pearson correlation.",
                      "\nOne is based on raw correlations, the other, like the calculation ",
                      "\nof the CI, is based on the z transform.  The first only works well ",
                      "\nwhen correlations are near zero, the other is more stable across all ",
                      "\ncorrelation values.  For your values of R and n the first method gives ",
                      "\n an SE of: ",
                      round(SE1, dp),
                      " and the other gives an SE of ",
                      round(SEz, dp),
                      ". \n\nIt seems clear that if you really want the SE, use the second!")
    return(retText)
  }
  
  output$res <- renderText({
    validate(
      need(checkIsInteger(input$n),
           "n must be a positive integer > 10 and < 10^9"),
      need(checkIsInRange(input$n, 10, 10^9),
           "n must be a positive integer > 10 and < 10^9"),
      need(checkIsInteger(input$dp),
           "n must be a positive integer > 1 and < 8"),
      need(checkIsInRange(input$dp, 1, 8),
           "n must be a positive integer > 1 and < 8"),
      need(checkIsInRange(input$R, -1, 1),
           "R must be between -1 and +1"),
      need(checkIsInRange(input$ci, .7, .999),
           "ci must be between .7 and .999"),
    )
    getCI(input$R,
          input$n,
          input$ci,
          input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
