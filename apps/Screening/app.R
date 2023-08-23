### Screening1

### shiny app to model the basic screening situation

library(tidyverse)
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
  tags$div(class="title", titlePanel("Model of simple screening\n\n")),
  
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
                   "Total n, (suggest between 100 and 10,000)",
                   value = 1000,
                   min = 100,
                   max = 10^5,
                   width="100%"),
      numericInput("prev",
                   "Prevalence, must be a number between (realistically) .001 and .3",
                   value = .1,
                   min = 10^-3,
                   max = .3,
                   width="100%"),
      numericInput("sens",
                   "Sensitivity, (proportion of true positives correctly identified by test), between .1 and .9999",
                   value = .90,
                   min = .1,
                   max = .9999,
                   width="100%"),
      numericInput("spec",
                   "Specificity (proportion of true negatives correctly identified by test), between .1 and .9999",
                   value = .85,
                   min = .1,
                   max = .9999,
                   width="100%"),
      numericInput("dp",
                   "Decimal places you want in the PPV and NPV results (0 to 5)",
                   value = 2,
                   min = 0,
                   max = 5)
    ),
    
    mainPanel(
      # h1("Model of simple screening", align = "center"),
      
      # h2("Input"),
      p("Your input values are"),
      verbatimTextOutput("retInputs"),
      
      h2("Screening table"),
      p("That gives you this screening table ..."),
            tableOutput('table'),
      
      h2("PPV and NPV"),
      p("These are the computed values ..."),
      verbatimTextOutput("reacNHaveProb"),
      
      htmlOutput("retComputed"),
      
     
      br(),
      br(),
      p("App created by Chris Evans",
        a("PSYCTC.org", href="https://shiny.psyctc.org/CIproportion/"),
        "licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href="http://creativecommons.org/licenses/by-sa/1.0/"),
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here.")
    )
  )
)

retInputs <- function(n, prev, sens, spec) {
  retText1 <- paste0("You input:\n",
                    "   n           = ", n,"\n",
                    "   Prevalence  = ", prev,"\n",
                    "   Sensitivity = ", sens, "\n",
                    "   Specitivity = ", spec, "\n\n")
  return(retText1)
}

# Define server logic required
server <- function(input, output, session) {
  ### 
  ### function to make the screening table that will use the reactive values computed below
  makeTib <- function(reacNTestTrueNeg,
                      reacNTestFalsNeg,
                      reacNTestTruePos,
                      reacNTestFalsPos){
    tibble(Population = c("True OK", "True problems", "Total"),
           `Tested negative` = c(reacNTestTrueNeg, reacNTestFalsNeg, reacNTestTrueNeg + reacNTestFalsNeg),
           `Tested positive` = c(reacNTestFalsPos, reacNTestTruePos, reacNTestFalsPos + reacNTestTruePos),
           Total = c(reacNTestTrueNeg + reacNTestFalsPos,
                     reacNTestTrueNeg + reacNTestTruePos,
                     reacNTestTrueNeg + reacNTestFalsNeg + reacNTestFalsPos + reacNTestTruePos)) %>%
      mutate(across(`Tested negative`:Total, round))
    
  }
  
  ### 
  ### now the reactive values
  ###
  reacNHaveProb <- reactive({input$n * input$prev})
  reacNOK <- reactive({input$n - reacNHaveProb()})
  reacNTestTruePos <- reactive(reacNHaveProb() * input$sens)
  reacNTestTrueNeg <- reactive(reacNOK() * input$spec)
  reacNTestFalsPos <- reactive(reacNOK() * (1 - input$spec))
  reacNTestFalsNeg <- reactive(reacNHaveProb() * (1 - input$sens))
  reacNTestPos <- reactive(reacNTestTruePos() + reacNTestFalsPos())
  reacNTestNeg <- reactive(reacNTestTrueNeg() + reacNTestFalsNeg())
  reacNPV <- reactive(round(reacNTestTrueNeg() / reacNTestNeg(), input$dp))
  reacPPV <- reactive(round(reacNTestTruePos() / reacNTestPos(), input$dp))
  
  ### use the reactive values and the table constructor to make the reactive table
  reacTib <- reactive({
    makeTib(reacNTestTrueNeg(),
            reacNTestFalsNeg(),
            reacNTestTruePos(),
            reacNTestFalsPos())
  })

  ###
  ### now the outputs
  ###
  
  ### now the reactive text output of the PPV and NPV
  reacComputed <- reactive({
    paste("The Positive Predictive Value (PPV) i.e. the proportion of those who test positive who are true positives:       ", reacPPV(),
          "<br>The Negative Predictive Value (NPV) i.e. the proportion of those who test negative who are true negatives:       ", reacNPV())
  })
  
  ### outputting the inputs
  output$retInputs <- renderText({
    retInputs(input$n,
              input$prev,
              input$sens,
              input$spec)
  })
  
  ### the screening table
  output$table <- renderTable(reacTib(), digits = 0)
  
  ### the PPV and NPV values
  output$retComputed <- renderText({
    reacComputed()
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
