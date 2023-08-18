### CImean

library(shiny)
library(shinyWidgets)

# Define UI for application that does the work
ui <- fluidPage(
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css
  tags$head(
    tags$style(
      ".title {margin: auto; align: center}"
    )
  ),
  tags$div(class="title", titlePanel("Confidence interval for a mean given only mean, n and SD (or SE)\n\n")),

  # Get input values
  sidebarLayout(
    sidebarPanel(
      p("This shiny app is part of a number from my shiny server linked with at",
        a("PSYCTC.org", href="https://www.psyctc.org/psyctc/"),
        "There is a form if you want to",
        a("contact me", href="https://www.psyctc.org/psyctc/contact-me/"),
        " so do please use that if you think there is anything wrong here,",
        " or anything that could be improved."),
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("n",
                   "Total n, (zero or positive integer)",
                   value = 100,
                   min = 5,
                   max = 10^9,
                   width = "100%"),
      numericInput("mean",
                   "Observed mean",
                   value = .7,
                   width = "100%"),
      numericInput("SD",
                   "Observed standard deviation (SD)\nEnter this or SE",
                   value = .1,
                   width = "100%"),
      numericInput("SE",
                   "Observed standard error (SE),\nput in correct value for one and leave other blank",
                   value = NA,
                   width = "100%"),
      numericInput("ci",
                   "Width of CI (usually .95, i.e. 95% CI, <=.99)",
                   value = .95,
                   min = .7,
                   max = .999,
                   width = "100%"),
      numericInput("dp",
                   "Number of decimal places",
                   value = 2,
                   min = 0,
                   max = 5,
                   width = "100%")
    ),
    
    mainPanel(
      h3("Your input and results", align = "center"),
      verbatimTextOutput("res"),
      p("This uses parametric assumptions, i.e. that the population distribution is Gaussian. That's unlikely to be\n
      the case for typical MH/therapy data but if all you have is the observed mean, dataset size (n) and the SD\n
      (or the SE) then it is as good as you can get.  If you have the raw data I recommend you use the \n
      bootstrap CI for the mean.  I'll put up an app to do that when I can.\n\n"),
      p("App created by Chris Evans",
        a("PSYCTC.org", href = "https://shiny.psyctc.org/CIproportion/"),
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
  checkOneOnly <- function(x, y) {
    if(sum(is.na(c(x, y))) != 1) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  ### 
  ### now the functions from CECPfuns plotCIcorrelation
  ###
  getCI <- function(n, mean, SD, SE, ci = 0.95, dp = 2) {
    # computes CI mean from given n, mean, and s.d.
    # default CI is 95%
    # check data are from p.21 of Gardner & Altman (1989) Statistics with confidence BMA:London
    # (c) Chris Evans <chris@psyctc.org> 2001 but so trite it's not worth saying that!
    # I'm no programmer or statistician so use this entirely at your own risk.
    # you are free to copy and use, let me know if you improve it please!
    ci.perc <- round(100 * ci)
    if (is.na(SE)) {
      SE <- SD / sqrt(n)
    }
    conf.level <- (1 - ci) / 2 # since you need the t value for .975 to get 95% CI
    df <- n - 1
    q <- abs(qt(conf.level, df))
    half.int <- SE * q
    lwr <- mean - half.int
    upr <- mean + half.int
    lwr <- round(lwr, dp)
    upr <- round(upr, dp)
    retText <- paste0("Given:\n",
                      "   n = ", n,"\n",
                      "   mean = ", mean,"\n",
                      "   SE (input or computed) = ", round(SE, dp),
                      "\n",
                      "   ", ci.perc, "% confidence interval from ", round(lwr, dp),
                      " to ", round(upr, dp),"\n\n")
    return(retText)
  }
  
  output$res <- renderText({
    validate(
      need(checkForPosInt(input$n, minInt = 0), 
           "n must be a positive integer > 10 and < 10^9"),
      need(checkOneOnly(input$SE, input$SD),
           "You must enter one of SD or SE, not both. (To avoid confusion!)"),
      need(checkNumRange(input$ci, minx = .69999, maxx = 1, incEq = FALSE),
           "ci must be a value > .7 and < .99"),
      need(checkForPosInt(input$dp, minInt = 1, maxInt = 5),
           "dp must be a value between 1 and 5")
    )
    getCI(input$n,
          input$mean,
          input$SD,
          input$SE,
          input$ci,
          input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
