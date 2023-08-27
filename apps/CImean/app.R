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
      p("This shiny app is one of a growing number in ",
        a("my shiny server", href = "https://shiny.psyctc.org/"),
        "They complement (1) ",
        a("my Rblog", href = "https://www.psyctc.org/Rblog/index.html"),
        "of posts about using R, (2) the ",
        a("glossary", href = "https://www.psyctc.org/psyctc/book/glossary/"),
        "linked with ",
        a("the OMbook, ", href = "https://www.psyctc.org/psyctc/book/"),
        " and it's all part of the resources of (3)",
        a("PSYCTC.org", href = "https://www.psyctc.org/psyctc/"),
        " and linked with (4)",
        a("the CORE system web site", href = "https://www.coresystemtrust.org.uk"),
        ""),
      p("There is a form if you want to ",
        a("contact me", href = "https://www.psyctc.org/psyctc/contact-me/"),
        " so do please use that if you think there is anything wrong here,",
        " or anything that could be improved."),
      br(),
      p("There is now an Email announcement list, never updating more than monthly, where I will put up developments of new apps here,",
        " a summary of updates to the",
        a("online glossary", href = "https://www.psyctc.org/psyctc/book/glossary/"),
        "and new posts in the ",
        a("Rblog.", href = "https://www.psyctc.org/Rblog/index.html"),
        "You can sign up for that ",
        a("here", href = "https://ombook.psyctc.org/signup")),
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
