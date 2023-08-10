#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

# Define UI for application that does the work
ui <- fluidPage(
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css
  ### I confess I don't understand why the tweak achieves the centring
  tags$head(
    tags$style(
      ".title {margin: auto; align: center}"
    )
  ),
  tags$div(class="title", titlePanel("Confidence interval for a simple proportion\n\n")),


# ui <- fluidPage(
#   setBackgroundColor("#ffff99"),
#   # Application title
#   titlePanel("Parametric confidence interval for Cronbach alpha"),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      p("This shiny app is part of a number from my shiny server linked with at",
        a("PSYCTC.org",href="https://www.psyctc.org/psyctc/"),
        "There is a form if you want to",
        a("contact me",href="https://www.psyctc.org/psyctc/contact-me/"),
        " so do please use that if you think there is anything wrong here,",
        " or anything that could be improved."),
      h3("Put your values in here, replacing the existing ones",
         align="center"),
      numericInput("n",
                   "Total n, (zero or positive integer)",
                   value=100,
                   width="100%"),
      numericInput("x",
                   "Number of of those n that were counted as positive/important/interesting (positive integer)",
                   value=34,
                   width="100%"),
      numericInput("ci",
                   "Width of CI (usually .95, i.e. 95% CI, <=.99)",
                   value=.95,
                   width="100%"),
      numericInput("dp",
                   "Number of decimal places",
                   value=2,
                   width="100%")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Your input and results",align="center"),
      verbatimTextOutput("res"),
      p("App created by Chris Evans",
        a("PSYCTC.org",href="https://shiny.psyctc.org/CIproportion/"),
        "licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href="http://creativecommons.org/licenses/by-sa/1.0/"),
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here.")
    )
  )
)

# Define server logic required
server <- function(input, output) {
  ### 
  ### start with validation functions
  ###
  checkForPosInt <- function(int, minInt = 0, maxInt = 10^9){
    ### function to check integer input
    if (is.na(int) | is.null(int)) {return(FALSE)}
    if (int < max(0, minInt)) {return(FALSE)}
    if (int > maxInt) {return(FALSE)}
    if (!isTRUE(all.equal(int,round(int)))) {return(FALSE)}
    return(TRUE)
  }
  checkNumRange <- function(x, minx = 1, maxx = 1, incEq = TRUE){
    ### function to check numeric input within range
    if (is.na(x) | is.null(x)) {return(FALSE)}
    if (incEq) {
      if (x < minx) {return(FALSE)}
      if (x > maxx) {return(FALSE)}
    } else {
      if (x <= minx) {return(FALSE)}
      if (x >= maxx) {return(FALSE)}  
    }
    return(TRUE)
  }  
  
  ### 
  ### now the functions from Hmisc
  ###
  getCI <- function(x, n, ci = 0.95, dp = 2) {
    CI <- Hmisc::binconf(x, n, 1 - ci)[1, ]
    ci.perc <- round(100 * ci)
    obs.prop <- CI[1]
    LCL <- CI[2]
    UCL <- CI[3]
    retText <- paste0("Given:\n",
    "   x = ", x,"\n",
    "   n = ", n,"\n",
    "   observed proportion = ", round(CI[1], dp),
    "\n",
    "   ", ci.perc, "% confidence interval from ", round(LCL, dp),
    " to ", round(UCL, dp),"\n\n")
    return(retText)
  }
  
  output$res <- renderText({
    validate(
      need(checkForPosInt(input$n, minInt = 0), 
           "n must be a positive integer > 10 and < 10^9"),
      need(checkForPosInt(input$x, minInt = 3, maxInt = 500), 
           "x must be a positive integer > 2 and < 500"),
      need(checkNumRange(input$ci, minx = .69999, maxx = 1, incEq = FALSE),
           "ci must be a value > .7 and < .99"),
      need(checkForPosInt(input$dp, minInt = 1, maxInt = 5),
           "dp must be a value between 1 and 5")
    )
    getCI(input$x,
           input$n,
           input$ci,
           input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
