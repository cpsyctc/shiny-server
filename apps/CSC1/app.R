### CSC1
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "CSC1",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css for head blocks
  tags$head(
    tags$style(
      ".title {margin: auto; align: center}"
    )
  ),
  tags$div(class="title", titlePanel("Compute CSC method c from referential SDs and means\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones", align="center"),
      numericInput("meanNHS",
                   "This is your referential non-help-seeking score mean.",
                   value = .76,
                   min = -10^9,
                   max = 10^9,
                   width="100%"),
      numericInput("SDNHS",
                   "This is your referential non-help-seeking score standard deviation (must be positive)).",
                   value = .59,
                   min = 0,
                   max = 10^9,
                   width="100%"),
      numericInput("meanHS",
                   "This is your referential help-seeking score mean.",
                   value = 1.86,
                   min = -10^9,
                   max = 10^9,
                   width="100%"),
      numericInput("SDHS",
                   "This is your referential help-seeking score standard deviation (must be positive)).",
                   value = .75,
                   min = 0,
                   max = 10^9,
                   width="100%"),
      numericInput("minPoss",
                   "This is minimum possible score on the measure.",
                   value = 0,
                   min = -10^9,
                   max = 10^9,
                   width="100%"),
      numericInput("maxPoss",
                   "This is maximum possible score on the measure.",
                   value = 4,
                   min = -10^9,
                   max = 10^9,
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
      verbatimTextOutput("CSC"),
      verbatimTextOutput("missClassHS"),
      plotOutput("plot", height = 500),
      p(" "),
      p("This uses the trivial maths of method c straight out of the original work on the CSC.",
        "The starting values before you put yours in are from:\n"),
      p("Evans, C., Connell, J., Barkham, M., Margison, F., McGrath, G., Mellor-Clark, J., & Audin, K. (2002).\n",
        "Towards a standardised brief outcome measure: Psychometric properties and utility of the CORE-OM.\n",
        "British Journal of Psychiatry, 180(1), 51–60. Scopus.",
        a("https://doi.org/10.1192/bjp.180.1.51", href="https://doi.org/10.1192/bjp.180.1.51"),
        "\n\n"),
      p("App created by Chris Evans",
        a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/"),
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
### this is the standard shiny server constructor
server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # 3. Track basics and inputs and input values
  
  ### 
  ### start with validation functions
  ### I don't think I actually use any these as I've now used numericInput() to set the ranges
  
  ### 
  ### now the functions adapted from CECPfuns plotCIcorrelation
  ###
  getCSC <- function(meanNHS, SDNHS, meanHS, SDHS, dp = 2) {
    CSC <- (SDHS * meanNHS + SDNHS * meanHS) / (SDNHS + SDHS)
    
    retText <- paste0("Given those referential values CSC is ",
                      round(CSC, dp), "\n\n")
    return(retText)
  }
  
  plotCSC <- function(meanNHS, SDNHS, meanHS, SDHS, minPoss, maxPoss) {
    CSC <- (SDHS * meanNHS + SDNHS * meanHS) / (SDNHS + SDHS)
    nPoints <- 500
    xVals <- seq(minPoss, maxPoss, length = nPoints)
    pointsNHS <- dnorm(xVals, meanNHS, SDNHS)
    pointsHS <- dnorm(xVals, meanHS, SDHS)
    tibble(NHS = pointsNHS,
           HS = pointsHS) %>%
      mutate(x = xVals) %>%
      pivot_longer(cols = c(NHS, HS)) -> tmpTib
    vecColours <- c("NHS" = "green", "HS" = "red")
    ggplot(data = tmpTib,
           aes(x = x, y = value, colour = name, group = name)) +
      geom_line() +
      geom_vline(xintercept = CSC) +
      geom_ribbon(data = filter(tmpTib, name == "NHS" & x > CSC),
                  aes(ymin = 0, ymax = value, fill = name),
                  alpha = .5) +
      geom_ribbon(data = filter(tmpTib, name == "HS" & x < CSC),
                  aes(ymin = 0, ymax = value, fill = name),
                  alpha = .5) +
      scale_colour_manual(name = "Referential group",
                          values = vecColours) +
      scale_fill_manual(name = "Referential group",
                        values = vecColours) +
      ylab("Probability of score") +
      xlab("Score") +
      ggtitle("Plot of probabilities of scores for help-seeking and non-help-seeking scores",
              subtitle = "Uses your referential mean and SD and score limits and Gaussian model.") +
      theme_bw() +
      theme(plot.title = element_text(hjust = .5), 
            plot.subtitle = element_text(hjust = .5)) -> p
    return(p)
  }
  
  missClass <- reactive({
    CSC <- (input$SDHS * input$meanNHS + input$SDNHS * input$meanHS) / (input$SDNHS + input$SDHS)
    if (input$meanNHS < input$meanHS) {
      pNHS <- pnorm(CSC, mean = input$meanNHS, sd = input$SDNHS, lower.tail = FALSE)
      pHS <- pnorm(CSC, mean = input$meanHS, sd = input$SDHS, lower.tail = TRUE)
    } else {
      pNHS <- pnorm(CSC, mean = input$meanNHS, sd = input$SDNHS, lower.tail = TRUE)
      pHS <- pnorm(CSC, mean = input$meanHS, sd = input$SDHS, lower.tail = FALSE)
    }
    c(pNHS, pHS)
  })

output$CSC <- renderText({
  ### validation of input logic
  if(input$minPoss > input$maxPoss){
    validate("minimum possible score")
  }
  if(input$meanNHS < input$minPoss | input$meanHS < input$minPoss){
    validate("Mean values must be bigger than minimum possible score")
  }
  if(input$meanNHS > input$maxPoss | input$meanHS > input$maxPoss){
    validate("Mean values must be smaller than maximum possible score")
  }
  
  ### OK now use input
  getCSC(input$meanNHS,
         input$SDNHS,
         input$meanHS,
         input$SDHS,
         input$dp)
})

output$missClassHS <- renderText({paste0("It's not well known that the logic of method c for the CSC is that it balances missclassification.\n",
                                         "For your input, ignoring clipping of the score range and assuming Gaussian distributions,\n",
                                         "missclassification of the non-help-seeking would be: ",
                                         round(missClass()[1], input$dp),
                                         " and of the help-seeking would be: ",
                                         round(missClass()[2], input$dp),
                                         ", the same rates.\n",
                                         "This is shown in the plot below but it will also show you the likely impact of range restriction.")})

### and now a plot of the data
output$plot <- renderPlot({
  plotCSC(input$meanNHS, 
          input$SDNHS, 
          input$meanHS, 
          input$SDHS, 
          input$minPoss, 
          input$maxPoss)
})
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
