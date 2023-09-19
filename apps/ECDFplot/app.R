### ECDFplot

suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
# library(plotly)
suppressMessages(library(CECPfuns))
set.seed(12345)
vecDat <- round(rnorm(2000), 3)

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
  tags$div(class="title", titlePanel("Plot ECDF with CIs for arbitrary quantiles\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      p("The data starts off with 2000 rounded numbers from a Gaussian distribution, replace that with your data."),
      textInput("data",
                "Your data: as numbers, separated by spaces, commas or semicolons",
                value = vecDat,
                width="100%"),
      textInput("quantiles",
                "The quantiles you want as numbers, separated by spaces, commas or semicolons",
                value = ".05, .1, .25, .5, .75, .9, .95",
                width="100%"),
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
  checkVector <- function(vecChar){
    if (str_detect(vecChar, "[:alpha:]")) {
      return(FALSE)
    }
    return(TRUE)
  }
  stringToNumericVector <- function(vecChar) {
    vecChar <- str_squish(vecChar)
    vecChar <- str_replace_all(vecChar, "\\;", ",")
    vecChar <- str_replace_all(vecChar, "\\,\\,", ", ,")
    vecChar <- str_replace_all(vecChar, ",([:space:])+", ",")
    vecChar <- str_replace_all(vecChar, "([:space:])+", ",")
    vecChar <- unlist(strsplit(vecChar, ","))
    suppressWarnings(as.numeric(vecChar))
  }
  ### 
  ###
  getData <- function(data, quantiles, ci, dp) {
    ci.perc <- paste0(100 * ci, "%")
    vecDat <- stringToNumericVector(data)
    vecQuantiles <- stringToNumericVector(quantiles)
    nTot <- length(vecDat)
    nOK <- getNOK(vecDat)
    nNA <- getNNA(vecDat)
    vecDat <- na.omit(vecDat)
    mean <- round(mean(vecDat), dp)
    min <- round(min(vecDat), dp)
    max <- round(max(vecDat), dp)
    SD <- round(sd(vecDat), dp)
    retText <- paste0("You seem to have input ",
                      nTot,
                      " values in your data.\n Of which ",
                      nNA,
                      " seem to have been missing values and ",
                      nOK,
                      " seem to have been usable values.",
                      "\n\nTheir mean was ",
                      mean,
                      " with range from ",
                      min,
                      " to ",
                      max,
                      " and SD ",
                      SD,
                      "\nYou might want to check those sample statistics if you can to confirm correct data insertion.",
                      "\n\nYou were asking for ",
                      convertVectorToSentence(vecQuantiles),
                      " for the quantiles and ",
                      ci.perc, 
                      " confidence intervals around the quantiles.")
    return(retText)
  }
  
  makePlot <- function(data, quantiles, ci = ci) {
    vecData <- stringToNumericVector(input$data)
    vecQuantiles <- stringToNumericVector(input$quantiles)
    suppressWarnings(suppressMessages(plotQuantileCIsfromDat(vecDat = vecData, 
                                                             vecQuantiles = vecQuantiles, 
                                                             method = "N", 
                                                             type = 4,
                                                             ci = ci))) -> p
    return(p)
  }
  
  output$res <- renderText({
    validate(
      need(checkVector(input$data), 
           "data can only contain numbers, commas, semicolons and spaces"),
      # need(checkVector(input$quantiles), 
      #      "quantiles can only contain numbers, commas, semicolons and spaces"),
    )
    getData(input$data,
            input$quantiles,
            input$ci,
            input$dp)
  })
  output$CIplot <- renderPlot({
    makePlot(input$data, input$quartiles, input$ci)
  })
}
# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
