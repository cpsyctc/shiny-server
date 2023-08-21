### Scattergram and correlation

library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(CECPfuns)
set.seed(12345)
n <- 200
vecDat1 <- round(rnorm(n), 3)
vecDat2 <- vecDat1 + round(rnorm(n), 3)

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
  tags$div(class="title", titlePanel("Plot scattergram for two numeric variables\n\n")),
  
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
      p("The data starts off with 2000 rounded numbers from a Gaussian distribution, replace that with your data."),
      textInput("dataX",
                "Your first variable (for x axis) as numbers, separated by spaces, commas or semicolons",
                value = vecDat1,
                width="100%"),
      textInput("dataY",
                "Your second variable (for y axis) as numbers, separated by spaces, commas or semicolons",
                value = vecDat2,
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
      ### header
      h3("Your input and results", align = "center"),
      verbatimTextOutput("reacTextCI"),
      br(),
      br(),
      p("This next table shows the data you input.  The headers allow you to sort the data by either variable and you can use the search to find a value in the data. The default is just to show the first ten rows but you can change how many rows you see and page through the data using the controls at the bottom of the table."),
      br(),
      DTOutput("DTdat"),
      br(),
      br(),  
      br(),
      tableOutput("stats"),
      br(),
      br(),  
      br(),
      plotOutput("scatterPlot", height = 500),
      br(),
      br(),  
      br(),
      p("App created by Chris Evans",
        a("PSYCTC.org", href = "https://shiny.psyctc.org/CIproportion/"),
        "licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href = "http://creativecommons.org/licenses/by-sa/1.0/"),
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here.")
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
  ###
  ### and this one checks the vectors start off at the same length
  checkEqualLength <- function(vecX, vecY) {
    if (length(vecX) != length(vecY)) {
      return(FALSE)
    }
    return(TRUE)
  }
  ###
  ### end of validation functions
  ###
  
  ### data mangling function
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
  ### OK now get the data and crunch it
  ###
  reacTibDat <- reactive({
    makeTibDat(vecdataX, vecdataY)
  })
  ### that uses this to create the tibble
  makeTibDat <- function(vecdataX, vecdataY) {
    vecDataX <- stringToNumericVector(input$dataX)
    vecDataY <- stringToNumericVector(input$dataY)
    tibble(x = vecDataX,
           y = vecDataY) %>%
      na.omit() -> tibDat
    tibDat
  }
  
  reacTextCI <- reactive({
    ci.perc <- paste0("Confidence intervals are ", 
                      100 * input$ci, 
                      "%")
  })
  reacDP <- reactive({
    input$dp
  })

  reacTibStats <- reactive({
    
    reacTibDat() %>%
      summarise(nUsable = n(),
                mean = round(mean(x), reacDP()),
                min = round(min(x), reacDP()),
                max = round(max(x), reacDP()),
                SD = round(sd(x), reacDP())) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = everything(), names_to = "statistic", values_to = "x") -> tmpTibX
    
    reacTibDat() %>%
      summarise(nUsable = n(),
                mean = round(mean(y), reacDP()),
                min = round(min(y), reacDP()),
                max = round(max(y), reacDP()),
                SD = round(sd(y), reacDP())) %>% 
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = everything(), names_to = "statistic", values_to = "y") -> tmpTibY
    
    tmpTibX %>%
      left_join(tmpTibY, by = "statistic")
  })
  
  makePlot <- function(tibDat) {
    ggplot(data = reacTibDat(),
           aes(x = x, y = y)) +
      geom_point() +
      geom_smooth() -> p
    return(p)
  }
  
  output$res <- renderText({
    validate(
      need(checkVector(input$dataX), 
           "Your X data can only contain numbers, commas, semicolons and spaces"),
      need(checkVector(input$dataY), 
           "Your Y data can only contain numbers, commas, semicolons and spaces"),
      need(checkEqualLength(input$dataX, input$dataY),
           "Your X and Y data must be of the same length")
    )
    getData(input$dataX,
            input$dataY,
            input$ci,
            input$dp)
  })
  ### use a datatable to show the data
  output$DTdat = renderDT(reacTibDat())
  ### and now summary statistics
  output$stats = renderTable(reacTibStats())
  ### and now a plot of the data
  output$scatterPlot <- renderPlot({
    makePlot(reacTibDat)
  })
}

# Finally: run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
