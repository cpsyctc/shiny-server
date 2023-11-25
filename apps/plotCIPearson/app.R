### plotCIPearson

suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
# library(plotly)
suppressMessages(library(CECPfuns))

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
  tags$div(class="title", titlePanel("Plot CIs around a given Pearson R for various dataset sizes\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("R",
                   "Pearson R -1 < R < 1",
                   value = .65,
                   min = -.9999,
                   max = .9999,
                   width = "100%"),
      numericInput("minN",
                   "Smallest n you want plotted",
                   value = 10,
                   min = 3,
                   max = 1000,
                   width = "100%"),
      numericInput("maxN",
                   "Largest n you want plotted",
                   value = 100,
                   min = 10,
                   max = 10000,
                   width = "100%"),
      numericInput("step",
                   "Step size to plot (optional)",
                   value = 5,
                   min = 3,
                   max = 1000,
                   width = "100%"),
      numericInput("minY",
                   "Smallest value on the y axis you want plotted",
                   value = -1,
                   min = -1,
                   max = .8,
                   width = "100%"),
      numericInput("maxY",
                   "Largest value on the y axis you want plotted",
                   value = 1,
                   min = .8,
                   max = 1,
                   width = "100%"),
      numericInput("conf",
                   "Width of CI (usually .95, i.e. 95% CI, <=.99)",
                   value = .95,
                   min = .7,
                   max = .999,
                   width = "100%")
    ),
    
    mainPanel(
      h3("Your input and results", align = "center"),
      plotOutput("CIplot", height = 500),
      p("App created by Chris Evans",
        a("PSYCTC.org", href = "https://shiny.psyctc.org/CIproportion/"),
        "licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href = "http://creativecommons.org/licenses/by-sa/1.0/"),
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
  checkNvalues <- function(minN, maxN){
    if (maxN <= minN) {
      return(FALSE)
    }
    return(TRUE)
  }
  checkYvalues <- function(minY, maxY){
    if (maxY <= minY) {
      return(FALSE)
    }
    return(TRUE)
  }
  ### 
  ###
  
  makePlot <- function(R, minN, maxN, step, conf, minY, maxY) {
    suppressWarnings(suppressMessages(plotCIPearson(corr = input$R, 
                                                             minN = input$minN,
                                                             maxN = input$maxN, 
                                                             step = input$step,
                                                             conf = input$conf, 
                                                             minY = input$minY, 
                                                             maxY = input$maxY))) -> p
    return(p)
  }
  
  output$res <- renderText({
    validate(
      need(checkNvalues(input$minN, input$maxN), 
           "maxN must be greater than minN"),
      need(checkYvalues(input$minY, input$maxY), 
           "maxY must be greater than minY"),
    )
    getData(input$R,
            input$minN,
            input$maxN,
            input$step,
            input$conf,
            input$minY,
            input$maxY)
  })
  output$CIplot <- renderPlot({
    makePlot(input$data, input$quartiles, input$ci)
  })
}
# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
