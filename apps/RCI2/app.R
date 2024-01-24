### RCI2
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))

# Define UI for application that does the work
ui <- fluidPage(
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css for head blocks
  tags$head(
    tags$style(
      ".title {margin: auto; align: center}"
    )
  ),
  tags$div(class="title", titlePanel("Compute plausible range for RCI from n, SD and reliability (and inclusion interval if not 95%)\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones", align="center"),
    numericInput("SD",
                 "This is your baseline score standard deviation (must be positive))",
                 value = 11.1,
                 min = 0,
                 max = 10^9,
                 width="100%"),
    numericInput("rel",
                 "Appropriate reliability, generally this should be the Cronbach's alpha from your data but may be a referential value.",
                 value = .9,
                 min = 0,
                 max = 1,
                 width="100%"),
    numericInput("n",
                 "Dataset size (n).  Must be positive integer greater than 2.",
                 value = 3,
                 min = 2,
                 max = 10^9,
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
    verbatimTextOutput("res2"),
    p("This uses the function getRCIfromSDandAlpha() in my CECPfuns R package to get the RCI",
    "but the maths is trivial and straight out of the original work on the RCI.",
    "The code combines that RCI with the standard formula for the Confidence Interval (CI)",
    "to give what I am calling a 'plausible interval' around that RCI based on the width",
    "of the CI you have requested.  Until the n is large, the interval is wide which I think",
    "should help us understand this.  Deciding when your dataset size is too small to give",
    "a usably tight plausible interval for the dataset RCI and hence that it would be better",
    "to use some 'referential' RCI is an interesting challenge.  I'll do some modelling to",
    "address that soon.\n\n"),
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
  ### 
  ### now the functions adapted from CECPfuns plotCIcorrelation
  ###
  getRCI <- function(SD, rel, ci = 0.95, dp = 2) {
    RCI <- CECPfuns::getRCIfromSDandAlpha(SD, rel, ci)
    ci.perc <- round(100 * ci)
    retText <- paste0("Given:\n",
                      "   SD = ", round(SD, dp), "\n",
                      "   Reliability = ", round(rel, dp), " and \n",
                      "   ", ci.perc, "% inclusion interval gives\n",
                      "   RCI = ", round(RCI, dp), "\n\n")
    return(retText)
  }
  getCIaroundSD <- function(SD, n, ci) {
    k <- n - 1 # df for the chisq values for the probabilities at the ends of the CI
    ### confidence limits are just the SD multiplied by those values
    UCL <- SD * sqrt(k) / sqrt(qchisq((1 - ci) / 2, k))
    LCL <- SD * sqrt(k) / sqrt(qchisq(1 - ((1 - ci) / 2), k))
    ci.perc <- round(100 * ci)
    return(c(LCL, UCL))
  }  
  getRCIinterval <- function(SD, rel, n, ci) {
    CISD <- getCIaroundSD(SD, n, ci)
    lwrLt <- CECPfuns::getRCIfromSDandAlpha(CISD[1], rel, ci)
    uprLt <- CECPfuns::getRCIfromSDandAlpha(CISD[2], rel, ci)
    return(c(lwrLt, uprLt))
  }  
  
  reactiveCISD <- reactive({
    getCIaroundSD(input$SD, input$n, input$ci)
  })
  
  output$res <- renderText({
    validate(
      need(input$n, 'n must be a positive integer > 2 and < 10^9.'),
      need(input$n > 2, 'n must be a positive integer > 2 and < 10^9.')
    )
    getRCI(input$SD,
          input$rel,
          input$ci,
          input$dp)
  })
  output$res2 <- renderText({
    validate(
      need(input$n, 'n must be a positive integer > 2 and < 10^9.'),
      need(input$n > 2, 'n must be a positive integer > 2 and < 10^9.')
    )
    tmpInterval <- getRCIinterval(input$SD, input$rel, input$n, input$ci)
    paste0("The plausible range for that RCI given the n and hence the CI(SD) is from ",
           round(tmpInterval[1], input$dp),
           " to ",
           round(tmpInterval[2], input$dp),
           ".\nPlay around with the value you put in for n to see how that changes this interval.")
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
