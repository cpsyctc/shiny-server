### CImean

suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))

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
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
      hr(),
      includeHTML("https://shiny.psyctc.org/boilerplate.html")
    )
  )
)

# Define server logic required
server <- function(input, output, session) {
  ### 
  ### start with validation functions
  ###
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
      need(checkOneOnly(input$SE, input$SD),
           "You must enter one of SD or SE, not both. (To avoid confusion!)"),
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
