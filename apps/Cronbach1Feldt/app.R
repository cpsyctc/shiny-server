### Cronbach1Feldt
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
  tags$div(class="title", titlePanel("Parametric confidence interval for Cronbach alpha\n\n")),


# ui <- fluidPage(
#   setBackgroundColor("#ffff99"),
#   # Application title
#   titlePanel("Parametric confidence interval for Cronbach alpha"),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      p("This shiny app is part of a number from my site at",
        a("PSYCTC.org",href="https://www.psyctc.org/psyctc/"),
        "There is a form there to",
        a("contact me",href="https://www.psyctc.org/psyctc/contact-me/"),
        " so do please use that if you think there is anything wrong here,",
        " or anything that could be improved."),
      h3("Put your values in here, replacing the existing ones",
         align="center"),
      numericInput("n",
                   "Sample size (n), positive integer",
                   value = 100,
                   width = "100%"),
      numericInput("k",
                   "Number of items (k), positive integer",
                   value = 34,
                   width = "100%"),
      numericInput("alpha",
                   "Observed/reported Cronbach alpha value (<=1.0)",
                   value = .94,
                   width = "100%"),
      numericInput("altAlpha",
                   "A referential alpha (if you need this)",
                   value = 0,
                   width = "100%"),
      numericInput("ci",
                   "Width of CI (usually .95, i.e. 95% CI, <=.99)",
                   value = .95,
                   width = "100%"),
      numericInput("dp",
                   "Number of decimal places",
                   value = 2,
                   width = "100%")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Your input and results",align = "center"),
      verbatimTextOutput("res"),
      p("App created by Chris Evans",
        a("PSYCTC.org",href = "https://www.psyctc.org/psyctc/"),
        "licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href = "http://creativecommons.org/licenses/by-sa/1.0/"),
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here.")
    )
  )
)

# Define server logic required
server <- function(input, output) {
  ### 
  ### start with validation functions
  ###
  checkForPosInt <- function(int,minInt = 10,maxInt = 10^9){
    ### function to check integer input
    if (is.na(int) | is.null(int)) {return(FALSE)}
    if (int < max(0,minInt)) {return(FALSE)}
    if (int > maxInt) {return(FALSE)}
    if (!isTRUE(all.equal(int,round(int)))) {return(FALSE)}
    return(TRUE)
  }
  checkNumRange <- function(x,minx = 1,maxx = 1,incEq = TRUE){
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
  ### now the functions from Feldt
  ###
  feldt1 <- function(obs.a, n, k, ci = 0.95, null.a = 0, dp=2) {
    #***********************************************************#
    #* program using methods described in Feldt, Woodruff &    *#
    #* Salih (1987) Applied Psychological Measurement 11(1),   *#
    #* pp. 93-103 to carry out omnibus inferential test of     *#
    #* similarity of alpha values from a single sample         *#
    #***********************************************************#
    # obs.a is the observed alpha
    # n is the sample size
    # k is the number of items in the measure or scale
    # ci is the width of the confidence interval about obs.a desired
    ci.perc <- 100 * ci	# purely for printing as a percentage
    # null.a is the null model alpha, usually zero
    # the testing of the observed against the null is a simple F test
    if(obs.a > null.a)
      f <- (1 - obs.a)/(1 - null.a)
    else f <- (1 - null.a)/(1 - obs.a)	# allows for testing against a higher null
    n.den <- (n - 1) * (k - 1)
    n.num <- n - 1
    null.p <- 2*pf(f, n.num, n.den)	# two-tailed as we've allowed either direction
    ### handle formatting of the p value which is likely to be tiny
    if (null.p < 10^(0-dp) & null.p > 10^(0-7)) {
      null.p <- format.pval(null.p, 7)
    } else {
      null.p <- round(null.p, dp)
    }
    # set the upper and lower p values for the desired C.I.
    p1 <- (1 - ci)/2
    p2 <- ci + p1	
    f1 <- qf(p1, n.num, n.den) # corresponding F values
    f2 <- qf(p2, n.num, n.den)	
    lwr <- 1 - (1 - obs.a) * f2 # confidence interval
    upr <- 1 - (1 - obs.a) * f1
    retText <- paste0("Given:\n",
    "   Observed alpha  = ", obs.a, "\n",
    "   n               = ", n, "\n",
    "   Number of items = ", k, "\n",
    "   Gives two-tailed p = ", null.p, "\n",
    "      against alternative alpha of ", null.a, "\n",
    "  ", ci.perc, "% confidence interval from ", round(lwr, dp),
    " to ", round(upr, dp),"\n\n")
    return(retText)
  }
  
  output$res <- renderText({
    validate(
      need(checkForPosInt(input$n, minInt = 10), 
           "n must be a positive integer > 10 and < 10^9"),
      need(checkForPosInt(input$k, minInt = 3, maxInt = 500), 
           "k must be a positive integer > 2 and < 500"),
      need(checkNumRange(input$alpha, minx = -1, maxx = 1, incEq = FALSE),
           "alpha must be a value > -1 and < 1"),
      need(checkNumRange(input$altAlpha, minx=-1, maxx=1, incEq = FALSE),
           "Referential ci must be a value > -1 and < 1"),
      need(checkNumRange(input$ci, minx = .69999, maxx = 1, incEq = FALSE),
           "ci must be a value > .7 and < .99"),
      need(checkForPosInt(input$dp, minInt = 1, maxInt = 5),
           "dp must be a value between 1 and 5")
    )
    feldt1(input$alpha,
           input$n,
           input$k,
           input$ci,
           input$altAlpha,
           input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)

