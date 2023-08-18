### CIcorrelation
library(shiny)
library(shinyWidgets)

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
  tags$div(class="title", titlePanel("Confidence interval for a Pearson or Spearman correlation\n\n")),
  
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
        always dodgy.  The assumption that this CI is OK for Spearman's rho as well as for the Pearson R because, is\n
        based on the fact that, if there were no ties and underlying Gaussian distributions, then rho is the\n
        Pearson R of the ranks of the two variables.  That's a lot of assumptions but if all you have is the observed\n
        correlations and the <i>n</i> it's as good as you can get.  If you have the raw data I recommend you use the \n
        bootstrap CI of the Pearson correlation.  I'll put up an app to do that when I can.\n\n"),
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
### this is the standard shiny server constructor
server <- function(input, output) {
  ### 
  ### start with validation functions
  ### I don't think I actually need these as I've now used numericInput() to set the ranges
  ###
  checkForPosInt <- function(int, minInt = 0, maxInt = 10^9){
    ### function to check integer input
    if (is.na(int) | is.null(int)) {return(FALSE)}
    if (int < max(0, minInt)) {return(FALSE)}
    if (int > maxInt) {return(FALSE)}
    if (!isTRUE(all.equal(int,round(int)))) {return(FALSE)}
    return(TRUE)
  }
  checkNumRange <- function(x, minx = -1, maxx = 1, incEq = TRUE){
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
    validate(
      need(checkForPosInt(input$n, minInt = 0), 
           "n must be a positive integer > 10 and < 10^9"),
      need(checkNumRange(input$R, minx = -1, maxx = 1, incEq = TRUE),
           "R must be a value >= -1.0 and <= 1.0"),
      need(checkNumRange(input$ci, minx = .69999, maxx = 1, incEq = FALSE),
           "ci must be a value > .7 and < .99"),
      need(checkForPosInt(input$dp, minInt = 1, maxInt = 5),
           "dp must be a value between 1 and 5")
    )
    getCI(input$R,
          input$n,
          input$ci,
          input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
