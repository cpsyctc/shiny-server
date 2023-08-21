### ECDFplot

library(shiny)
library(shinyWidgets)
library(tidyverse)
# library(plotly)
library(CECPfuns)
set.seed(12345)
vecDat <- round(rnorm(20), 2)

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
      p("This shiny app is part of a number from my shiny server linked with at",
        a("PSYCTC.org", href = "https://www.psyctc.org/psyctc/"),
        "There is a form if you want to",
        a("contact me", href = "https://www.psyctc.org/psyctc/contact-me/"),
        " so do please use that if you think there is anything wrong here,",
        " or anything that could be improved."),
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
      # verbatimTextOutput("res"),
      # plotOutput("CIplot", height = 500),
      
     verbatimTextOutput("res"),
     
     verbatimTextOutput("show_ci"),
      
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
# server <- function(input, output, session) {
#   all_inputs <- reactive({
#     input_df <- NULL
#     df_row <- NULL
#     for(i in 1:length(names(input))){
#       df_row <- as.data.frame(cbind(names(input)[i], input[[names(input)[i]]]))
#       input_df <- as.data.frame(dplyr::bind_rows(input_df, df_row))
#     }
#     names(input_df) <- c("input_id", "input_val")
#     print(str(input_df))
#     input_df
#   })


server <- function(input, output, session) {
  reacCI <- reactive({
    input$ci
  })
  reacDP <- reactive({
    input$dp
  })
  
  output$show_ci <- renderText({
    reacCI()
  })
  output$res <- renderText("sausages")
}
# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
