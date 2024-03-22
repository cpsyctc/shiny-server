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
  h1(HTML("Compute confidence interval around an observed SD or variance")),

  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones", align="center"),
    numericInput("SD",
                 "SD or variance",
                 value = 1,
                 min = 0,
                 max = 10^9,
                 width="100%"),
    helpText("This should be the SD or variance based on the n - 1 denominator, i.e. the unbiased estimator of the population SD and must be positive"),
    radioButtons("SDorVar", 
                 "Are you inputting an observed SD or a variance?",
                 c("SD" = "SD",
                   "Variance" = "Variance"),
                 width="100%"),
    numericInput("n",
                 "Dataset size (n: positive integer)",
                 value = 5000,
                 min = 0,
                 max = 1,
                 width="100%"),
    numericInput("ci",
                 "Width of the confidence interval",
                 value = .95,
                 min = .699999,
                 max = .999,
                 width="100%"),
    helpText("This is typically .95, i.e. 95%"),
    numericInput("dp",
                 "Number of decimal places",
                 value = 2,
                 min = 0,
                 max = 5,
                 width="100%")
  ),
  
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Result", 
                         verbatimTextOutput("res")
                ),
                
                tabPanel("Explanation/information", 
                         h3("Rationale"),
                         p(paste0("It is rare in the literature to see it recognised that a standard deviation, or variance,",
                                  "of values in a dataset is, just as the mean is, an estimate of a population value.",
                                  "It can be salutory to see how wide the confidence interval around these estimates are",
                                  "for small datasets")),
                         
                         h3("Computational background"),
                         p("Ignore this next bit if you don't like equations!"),
                         h4("The CI around an observed SD"),
                         p("The formula for CI around an observed SD in R code is from: "),
                         pre("SD * sqrt(n - 1) / sqrt(qchisq(1 - ((1 - ci) / 2), n - 1))"),
                         p(" to "),
                         pre("SD * sqrt(n - 1) / sqrt(qchisq((1 - ci) / 2, n - 1))"),
                         p("Typesetting that with MathJax is not beautiful but is:"),
                         withMathJax("$$SD * \\sqrt{\\frac{n-1}{qchisq((1 - (1 - ci) / 2), (n-1)}}$$"),
                         p(" to "),
                         withMathJax("$$SD * \\sqrt{\\frac{n-1}{qchisq((1 - ci) / 2, (n-1)}}$$"),
                         p("where 'qchisq(p, df)' is the quantile of the chisquare function for that df (degrees of freedom, i.e. n - 1)"),
                         
                         h3("Next please ..."),
                         p("Unless you are very familiar with it, please now go to the 'Background' tab and read the information there.")
                ),
                
                tabPanel("Background", 
                         p("App created by Chris Evans",
                           a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                         p("Last updated 21.iii.24."),
                         p("Licenced under a ",
                           a("Creative Commons, Attribution Licence-ShareAlike",
                             href="http://creativecommons.org/licenses/by-sa/1.0/"),
                           " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                         includeHTML("https://shiny.psyctc.org/boilerplate.html"))
    ),
  ),
  ),
)



# Define server logic required
### this is the standard shiny server constructor
server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # 3. Track basics and inputs and input values
  
  getSD <- function(SD, SDorVar){
    if(SDorVar == "Variance") {
      SD <- sqrt(SD)
    } 
    SD
  } 
  
  reactiveSD <- reactive({
    getSD(input$SD, 
          input$SDorVar)
  })
  
  getCIaroundSD <- function(SD, SDorVar, n, ci, dp) {
    req(input$SD,
        input$SDorVar,
        input$n,
        input$ci,
        input$dp)
    k <- n - 1 # df for the chisq values for the probabilities at the ends of the CI
    ### confidence limits are just the SD multiplied by those values
    UCL <- SD * sqrt(k) / sqrt(qchisq((1 - ci) / 2, k))
    LCL <- SD * sqrt(k) / sqrt(qchisq(1 - ((1 - ci) / 2), k))
    ci.perc <- round(100 * ci)
    
    if (input$SDorVar == "Variance") {
      SDorVarTxt1 <- "variance"
      SDorVarText <- paste0("   That you input a ",
                            SDorVarTxt1,
                            " of ",
                            input$SD,
                            " i.e. an SD of ",
                            round(reactiveSD(), dp),
                            " and\n")
    } else {
      SDorVarText <- paste0("   SD = ",
                            round(reactiveSD(), dp),
                            "\n")
    }
                          
    retText <- paste0("Given:\n",
                      SDorVarText,
                      "   n = ", 
                      n,
                      " and you asked for a\n",
                      "   ", 
                      ci.perc, 
                      "% inclusion interval gives\n",
                      "   the confidence interval for the SD is from ",
                      round(LCL, dp),
                      " to ", 
                      round(UCL, dp),
                      "\n   and the interval for the variance is from ",
                      round(LCL^2, dp),
                      " to ",
                      round(UCL^2, dp),
                      "\n\n")
    return(retText)
  }
  
  output$res <- renderText({
        getCIaroundSD(reactiveSD(),
                  input$SDorVar,
                  input$n,
                  input$ci,
                  input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
