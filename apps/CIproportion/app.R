### CIproportion

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
  tags$div(class="title", titlePanel("Confidence interval for a simple proportion\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
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
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
      hr(),
      includeHTML("https://shiny.psyctc.org/boilerplate.html")
    )
  )
)

# Define server logic required
server <- function(input, output, session) {
  ### 
  ### start with validation functions: none needed
  ###

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
    getCI(input$x,
          input$n,
          input$ci,
          input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
