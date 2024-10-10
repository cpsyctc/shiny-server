### Attentuation of correlation by unreliability

suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(CECPfuns))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload)) # from remotes::install_github("keithnewman/shinyDownload")
suppressMessages(library(DT))

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Attenuation",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

### function that gets the attenuated R
getAttenuatedR <- function(unattR, rel1, rel2) {
  ### function that takes an unattenuated correlation 
  ### and the reliabilities of the two variables
  ### and returns the attenuated correlation
  unattR * sqrt(rel1 * rel2)
}

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Attentuation of correlation by unreliability\n\n")),
  
  p("This app simply uses the standard formula to show how correlation is attenuated by unreliability in the variables correlated"),
  p("You input the unattenuated correlation, the reliabilities of the two variables and you can set the y-axis range for the plot."),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("unattR",
                   "Unattenuated correlation you are considering: -1 < R < 1",
                   value = .65,
                   min = -.1,
                   max = .999999,
                   width = "100%"),
      helpText("Remember this is unknowable correlation in the model, not an observed correlation."),
      numericInput("minUnattR",
                   "Minimum unattenuated correlation to plot: -1 < R < .9",
                   value = .1,
                   min = -1,
                   max = .9,
                   width = "100%"),
      helpText("Again, this is unknowable correlation in the model, not an observed correlation."),
      numericInput("maxUnattR",
                   "Maximum unattenuated correlation to plot: -.2 < R < 1",
                   value = .95,
                   min = -.2,
                   max = 1,
                   width = "100%"),
      helpText("Again, this is unknowable correlation in the model, not an observed correlation."),
      numericInput("stepR",
                   "Step size for unattenated correlations to transform (optional)",
                   value = .05,
                   min = .001,
                   max = .1,
                   width = "100%"),
      helpText("The app allows you to put in several values for the reliability of the first of the variables but only one value for the other.
               The numbering of the variables is arbitrary as we are looking a correlations so both variables are treated equally."),
      helpText("This is where you can enter several values.  Use commas to separate the values, e.g. '.5, .6, .7, .8'"),
      textInput("rel1",
                "Reliability of the first variable",
                value = ".5, .6, .7, .8",
                width = "100%"),
      helpText("In theory reliability can be negative but I've set the lowest value you might use to .2"),
      numericInput("rel2",
                   "Reliability of the second variable",
                   value = .7,
                   min = .2,
                   max = .999,
                   width = "100%"),
      helpText("In theory reliability can be negative but I've set the lowest value you might use to .2"),

      numericInput("minY",
                   "Smallest correlation on the y axis you want plotted",
                   value = 0,
                   min = 0,
                   max = .8,
                   width = "100%"),
      numericInput("maxY",
                   "Largest correlation on the y axis you want plotted",
                   value = 1,
                   min = .5,
                   max = 1,
                   width = "100%"),
      numericInput("dp",
                   "Number of decimal places for the computed reliabilities",
                   value = 3,
                   min = 1,
                   max = 7,
                   width="100%"),
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot",
                           p("The attenuated correlations for the inputs you gave are:"),
                           verbatimTextOutput("res"),
                           p("This shows the plot and you can see a table of all the attenuated correlations for all the inputs you gave in the tab 'All_data'"),
                           p("You can use the dialogue below the plot to download it chosing the name and the format of the plot"),
                           p(" "),
                           uiOutput("title"),
                           plotOutput("corrPlot",
                                      height = "800px"),
                           downloadGGPlotButtonUI("plotDownload", "Unattenuated_correlations"),
                           p(" "),
                           p("Thanks to Keith Newman for the download handler: ",
                             a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file"))),
                  
                  tabPanel("All_data", 
                           p(" "),
                           p("This shows all the data for the selected range of unattended correlations.  Buttons at the bottom allow you to export the data."),
                           p(" "),
                           DT::dataTableOutput("correlations")),
                  
                  tabPanel("Explanation",
                           p("This is ancient psychometrics but still of some use.  For more information, see:"),
                           a("https://www.psyctc.org/psyctc/glossary2/attenuation-by-unreliability-of-measurement/", 
                             href = "https://www.psyctc.org/psyctc/glossary2/attenuation-by-unreliability-of-measurement/"),
                           p(" "),
                           p("The formula is simple:"),
                           p(" "),
                           withMathJax("$$correctedCorr = \\frac{observedCorr}{\\sqrt{rel_{x}rel_{y}}}$$"),
                           p(paste0("The short summary is that unreliability in the measurement of both variables involved in a correlation ",
                                    "always reduces the observed correlation between the variables from what it would have been had the ",
                                    "variables been measured with no unreliability (which is essentially impossible for any self-report measures",
                                    "and pretty much any measures used in our fields."))),
                  
                  tabPanel("Background", 
                           p("App created 9.x.24 by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 10.x.24."),
                           p("Licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           includeHTML("https://shiny.psyctc.org/boilerplate.html"))
      ),
    )
  )
)

# Define server logic required
server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # 3. Track basics and inputs and input values
  
  ### 
  ### start with validation functions
  ###
  checkYvalues <- function(minY, maxY){
    if (maxY <= minY) {
      return(FALSE)
    }
    return(TRUE)
  }
  checkRvalues <- function(minUnattR, maxUnattR){
    if (maxUnattR <= minUnattR) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  ### 
  ### process input$rel1
  ###
  parseRel1 <- function(rel1txt){
    ### bit of a muddle of ordinary R and tidyverse but it works
    rel1txt <- str_squish(rel1txt) # not really necessary but cleaner
    listRel1 <- str_split(rel1txt, fixed(","))
    ### go to tidyverse as I've forgotten how to handle base na.omit()!
    tibble(rel1 = unlist(lapply(listRel1, as.numeric))) %>%
      na.omit() %>%
      ### get back to vector!
      pull()
  }
  
  rel1 <- reactive({
    parseRel1(input$rel1)
  })
  
  transR <- reactive({
    tibble(unattR = input$unattR,
           rel1 = rel1(), 
           rel2 = input$rel2) %>%
      rowwise() %>%
      mutate(transR = getAttenuatedR(unattR, rel1, rel2)) %>%
      ungroup() %>%
      select(transR) %>%
      pull() 
  })
  
  tibDat <- reactive({
    tibble(unattR = seq(input$minUnattR, input$maxUnattR, input$stepR),
           rel1 = list(rel1()),
           rel2 = input$rel2) %>%
      unnest_longer(rel1) %>%
      rowwise() %>%
      ### get the transformed, i.e. attenuated correlations
      mutate(obsR = getAttenuatedR(unattR, rel1, rel2),
             unattR = round(unattR, input$dp),
             obsR = round(obsR, input$dp)) %>% 
      ungroup()
  })
  
  plotTransformedR <- function(tibDat, unattR, minUnattR, maxUnattR, minY, maxY) {
    tibDat %>%
      mutate(rel1f = ordered(rel1)) -> tibDat
    
    suppressWarnings(suppressMessages(ggplot(data = tibDat,
                                             aes(x = unattR, y = obsR,
                                                 colour = rel1f, group = rel1f)) +
                                        geom_point() +
                                        geom_line() +
                                        ylim(c(minY, maxY)) +
                                        ylab("Transformed correlation") +
                                        xlab("Unattenuated R") -> p ))
    ### now put the chosen value in if it's in the range of the plot
    if(minUnattR <= unattR & unattR <= maxUnattR) {
      p +
        geom_vline(xintercept = input$unattR,
                   linetype = 3) +
        geom_hline(yintercept = transR(),
                   linetype = 3) +
        ggtitle("Transformed correlations given reliabilities",
                subtitle = "Reference lines mark unattenuated value input") -> p
    } else {
      p +
        ggtitle("Observed (i.e. transformed) correlations given reliabilities") -> p
    }
    return(p)
  }
  
  corrPlot <- reactive({
    plotTransformedR(tibDat(), input$unattR, input$minUnattR, input$maxUnattR, input$minY, input$maxY)
  })
  
  output$titleText <- renderText({input$title})
  
  output$corrPlot <- renderPlot({
    req(input$unattR)
    req(input$rel1)
    req(input$rel2)
    req(input$minUnattR)
    req(input$maxUnattR)
    req(input$minY)
    req(input$maxY)
    validate(
      need(checkRvalues(input$minUnattR, input$maxUnattR), 
           "maxUnattR must be greater than minUnattR"),
      need(checkRvalues(input$unattR, input$maxUnattR),
           "maxUnattR must be greater than unattR, the unattenuated correlation you entered"),
      need(checkRvalues(input$minUnattR, input$unattR),
           "minUnattR must be smaller than unattR, the unattenuated correlation you entered"),
      need(checkRvalues(input$unattR, input$maxY),
           "maxY must be greater than unattR, the unattenuated correlation you entered"),
      need(checkRvalues(input$minY, input$unattR),
           "minY must be smaller than unattR, the unattenuated correlation you entered"),
      need(checkYvalues(input$minY, input$maxY),
           "maxY must be greater than minY"),
    )
    corrPlot()
  })
  
  downloadGGPlotButtonServer(
    id = "plotDownload", # <= this should match the ID used in the UI module
    ggplotObject = corrPlot # No parentheses here to pass *expression*
  )
  
  
  
  output$res <- renderText({
    req(input$unattR)
    req(input$rel1)
    req(input$rel2)
    req(input$minUnattR)
    req(input$maxUnattR)
    req(input$minY)
    req(input$maxY)
    
    convertVectorToSentence(round(transR(), input$dp))
  })
  
  output$correlations <- DT::renderDataTable(
    DT::datatable({tibDat()},
                  extensions = "Buttons",
                  options = list(                                                     
                    fixedColumns = TRUE,
                    pageLength = 20,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'frtipB',
                    editable = FALSE,
                    searching = FALSE,
                    buttons = c('copy', 'csv', 'excel', "pdf")
                  ),
    )
  )
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
