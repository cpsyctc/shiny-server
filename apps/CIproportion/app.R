### CIproportion

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
  tags$div(class="title", titlePanel("Confidence interval for a simple proportion\n\n")),
  
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
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("n",
                   "Total n, (zero or positive integer)",
                   value = 100,
                   min = 3,
                   max = 10^9,
                   width="100%"),
      numericInput("x",
                   "Number of of those n that were counted as positive/important/interesting (x, a positive integer)",
                   value = 34,
                   min = 0,
                   max = 10^9,
                   width="100%"),
      numericInput("ci",
                   "Width of CI (usually .95, i.e. 95% CI, <=.99)",
                   value = .95,
                   min = .7,
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
      h3("Your input and results", align="center"),
      verbatimTextOutput("res"),
      p("App created by Chris Evans",
        a("PSYCTC.org", href="https://shiny.psyctc.org/CIproportion/"),
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
      need(checkNumRange(input$x, minx = 0, maxx = input$n), 
           "x must be a positive integer >= 2 and <= n"),
      need(checkNumRange(input$ci, minx = .69999, maxx = 1, incEq = FALSE),
           "ci must be a value > .7 and < .99"),
      need(checkForPosInt(input$dp, minInt = 1, maxInt = 5),
           "dp must be a value between 1 and 5"),
    )
    getCI(input$x,
          input$n,
          input$ci,
          input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
