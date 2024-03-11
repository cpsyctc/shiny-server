### RCI2
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "RCI2",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Compute plausible range for RCI from n, SD and reliability (and inclusion interval if not 95%)"), 
     align = "center"),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Replace the values here with your own", align="center"),
      numericInput("SD",
                   "This is your baseline score standard deviation (must be positive))",
                   value = 11.1,
                   min = 0,
                   max = 10^9,
                   width="100%"),
      helpText("A standard deviation must be numeric and positive!"),
      numericInput("rel",
                   "Appropriate reliability, generally this should be the Cronbach's alpha from your data but may be a referential value.",
                   value = .9,
                   min = 0,
                   max = 1,
                   width="100%"),
      helpText("Generally this will be the Cronbach's alpha from your data but may be a referential value."),
      numericInput("n",
                   "Dataset size (n).  Must be positive integer greater than 2.",
                   value = 3,
                   min = 2,
                   max = 10^9,
                   width="100%"),    
      numericInput("ci",
                   "Inclusion interval of the RCI",
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
      helpText("When you have filled in the above, hit this next button to get or update the RCI and interval!"),
      actionButton("compute", "(Re)compute RCI")
    ),
    
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Result", 
                           verbatimTextOutput("res2")
                  ),
                  
                  tabPanel("Explanation/information", 
                           h3("Rationale"),
                           p("All discussion of the RCI that I've seen takes the SD in the derivation (see next) as a fixed value."),
                           p("That's fine if you are using the RCI to tell you what interval for would embrace 95% of change scores were all change only down to measurement unreliability."),
                           p("However, if you are using an RCI from other data then the SD is a sample estimate of a populatiion value you are assuming is the population from which the original RCI was computed and from which your data come.",
                             p(paste0("If so, we should recognise that that RCI you are using as referential has sampling inaccuracy so should really be seen as a plausible range for the RCI",
                                      "as it comes from a sample so the SD feeding into the equation will have a CI around it.",
                                      "  (So will the reliability but that CI is generally very tight for most measures.)  ",
                                      "What I have done to create this 'plausible interval' around the RCI is to compute the CI for the referential SD and use",
                                      " the upper and lower limits for the SD to compute upper and lower limits for the RCI.  ",
                                      "Where n is quite small the interval can be rather large!/n/n")),
                             
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
                             
                             h4("Maths of the RCI "),
                             p("This exactly as in ",
                               a("my shiny app RCI1",href="https://shiny.psyctc.org/apps/RCI1/"),
                               HTML("As in that app, this uses the function <em>getRCIfromSDandAlpha()</em> in my "),
                               a("CECPfuns R package",
                                 href = 'https://cecpfuns.psyctc.org/'),
                               "but the maths is trivial and straight out of the original work on the RCI."),
                             p("The equation is:"),
                             pre("SD * sqrt(2) * sqrt(1 - rel) * qnorm(1 - (1 - ci) / 2)"),
                             p("Again the MathJax typesetting of the equation is a bit ugly:"),
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
    ),
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
  ### now the functions adapted from CECPfuns plotCIcorrelation
  ###
  getRCI <- function(SD, rel, ci, dp) {
    RCI <- CECPfuns::getRCIfromSDandAlpha(SD, rel, ci)
    return(round(RCI, dp))
  }
  RCI <- eventReactive(input$compute, {
    validate(
      need(input$SD, "You must give a usable value for SD"),
      need(input$rel, "You must give a usable value for rel"),
      need(input$n, "You must give a usable value for n"),
      need(input$ci, "You must give a usable value for ci (usually .95)")
    )
    getRCI(input$SD,
           input$rel,
           input$ci,
           input$dp)
  })
  
  getCIaroundSD <- function(SD, n, ci, dp) {
    k <- n - 1 # df for the chisq values for the probabilities at the ends of the CI
    ### confidence limits are just the SD multiplied by those values
    UCL <- SD * sqrt(k) / sqrt(qchisq((1 - ci) / 2, k))
    LCL <- SD * sqrt(k) / sqrt(qchisq(1 - ((1 - ci) / 2), k))
    return(round(c(LCL, UCL), dp))
  }  
  SDinterval <- eventReactive(input$compute, {
    validate(
      need(input$SD, "You must give a usable value for SD"),
      need(input$rel, "You must give a usable value for rel"),
      need(input$n, "You must give a usable value for n"),
      need(input$ci, "You must give a usable value for ci (usually .95)"),
      need(input$dp, "You must give a sensible value for dp"))
    getCIaroundSD(input$SD,
                  input$n,
                  input$ci,
                  input$dp)
  })
  
  getRCIinterval <- function(SD, rel, n, ci) {
    CISD <- getCIaroundSD(SD, n, ci)
    lwrLt <- CECPfuns::getRCIfromSDandAlpha(CISD[1], rel, ci)
    uprLt <- CECPfuns::getRCIfromSDandAlpha(CISD[2], rel, ci)
    return(c(lwrLt, uprLt))
  }  
  RCIinterval <- eventReactive(input$compute, {
    validate(
      need(input$SD, "You must give a usable value for SD"),
      need(input$rel, "You must give a usable value for rel"),
      need(input$n, "You must give a usable value for n"),
      need(input$ci, "You must give a usable value for ci (usually .95)")
    )
    getRCIinterval(input$SD,
                   input$rel,
                   input$n,
                   input$ci)
  })
  
  res2 <- eventReactive(input$compute, {
    validate(
      need(input$n, 'n must be a positive integer > 2 and < 10^9.'),
      need(input$n > 2, 'n must be a positive integer > 2 and < 10^9.')
    )
    ci.perc <- round(100 * input$ci)
    RCI <- RCI()
    SDinterval <- SDinterval()
    tmpInterval <- RCIinterval()
    paste0("Given:\n",
           "   SD = ", round(input$SD, input$dp), " and n = ",
           input$n,
           " the ",
           ci.perc, "% CI for that SD is from ",
           SDinterval()[1],
           " to ",
           SDinterval()[2],
           "\nThe point estimate for the RCI is ", RCI, "\n",
           "However, the plausible interval around that RCI given the n and that CI(SD) is from ",
           round(tmpInterval[1], input$dp),
           " to ",
           round(tmpInterval[2], input$dp),
           ".\n\nPlay around changing your input values, particularly the value you put in for n and hitting the 'Re(compute)' button to see this interval changes.")
  })
  
  
  output$res2 <- renderText({
    res2()
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
