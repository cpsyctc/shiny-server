### CISpearman
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(CECPfuns))
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "CISpearman",
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
  tags$div(class="title", titlePanel("Confidence interval for Spearman correlation\n\n")),
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones", align="center"),
      numericInput("n",
                   "Total n, (zero or positive integer)",
                   value = 50,
                   min = 0,
                   max = 10^9,
                   width="100%"),
      numericInput("rs",
                   "Observed correlation",
                   value = .5,
                   min = -.9,
                   max = .9,
                   width="100%"),
      numericInput("ci",
                   "Width of CI (usually .95, i.e. 95% CI, <=.99)",
                   value = .95,
                   min = .699999,
                   max = .999,
                   width="100%"),
      radioButtons("method", "Method to use:",
                   c("Bonett & White" = "B",
                     "Fieller, Hartley & Pearson" = "F")),
      radioButtons("Gaussian", "CI lookup distribution to use:",
                   c("Gaussian distribution" = "G",
                     "t distribution" = "t")),
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
      p("This gives you all four recognised ways of getting a confidence interval around an observed Spearman correlation.\n\n
    The four options come from two binary choices.\n\n
    The first is whether to look up the coverage quantile from the t distribution (with n - 2 degrees of freedom) which is the default,\n
    or from the Gaussian distribution."),
      p("The other option is whether to use the equation of Bonett & White (2000) for the SE of the correlation or to use the\n 
    equation of Fieller, Hartley & Pearson (1957)"),
      p("I have set the defaults here to use the Bonett & White method with the t distribution, matching the defaults in the\n
    getCISpearman() function in the",
        a("CECPfuns package", 
          href="https://cecpfuns.psyctc.org/")),
      p("Both methods are approximations and all four approaches are known give poor coverage for n < 25\n 
    or for very strong correlations (i.e. absolute correlation > .9) Having said that, the methods are moderately robust for\n
    n > 25 and abs(rs) < .9 and if all you have is the observed correlation and the n it's as good as you can get.\n
    If you have the raw data I recommend you use a bootstrap CI of the correlation.  I'll put up an app to do that when I can.\n\n"),
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
  
  ### 
  ### start with validation functions
  ### I don't think I actually use any these as I've now used numericInput() to set the ranges
  
  ### 
  ### now the functions adapted from CECPfuns plotCIcorrelation
  ###
  getCISpearman <- function(rs, n, ci = .95, Gaussian = FALSE, FHP = FALSE, dp = 2){
    ### cut down variant of version in CECPfuns without warnings and sanity checking
    alpha <- 1 - ci
    if (Gaussian) {
      ### If Gaussian, use the Gaussian distribution
      if (!FHP){
        ### Not using the Fieller, Hartley & Pearson (1957) SE approximation so ...
        ### This is the Bonett & Wright (2000) SE approximation
        CI <- sort(tanh(atanh(rs) + c(-1,1) * sqrt((1 + rs^2 / 2)/(n - 3)) * qnorm(p = alpha / 2)))
      } else {
        ### using the Fieller, Hartley & Pearson (1957) SE approximation
        ### uses sqrt(1.06 / (n - 3)) as in Bishara & Hittner (2016) as opposed to 1.03 / sqrt(n - 3)
        ### difference is numerically tiny
        CI <- sort(tanh(atanh(rs) + c(-1,1) * sqrt(1.06 / (n - 3)) * qnorm(p = alpha / 2)))
      }
    } else {
      ### Not Gaussian so use the t distribution
      if (!FHP){
        CI <- sort(tanh(atanh(rs) + c(-1,1) * sqrt((1 + rs^2 / 2)/(n - 3)) * qt(p = alpha / 2, df = n - 2)))
      } else {
        CI <- sort(tanh(atanh(rs) + c(-1,1) * sqrt(1.06 / (n - 3)) * qt(p = alpha / 2, df = n - 2)))
      }
    }
    LCL <- round(CI[1], dp)
    UCL <- round(CI[2], dp)
    CIperc <- round(100 * ci, 1)
    paste0("The ",
           CIperc,
           "% confidence interval is from ",
           LCL,
           " to ",
           UCL)
  }
  
  reacGaussian <- reactive({if (input$Gaussian == "G") {
    TRUE
  } else {
    FALSE
  }
  })
  
  reacFHP <- reactive({if (input$method == "B") {
    FALSE
  } else {
    TRUE
  }
  })
  
  
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
    getCISpearman(input$rs, input$n, input$ci, reacGaussian(), reacFHP(), input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
