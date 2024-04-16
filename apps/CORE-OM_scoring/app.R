### CORE-OM_scoring
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))
suppressMessages(library(flextable))
suppressMessages(library(CECPfuns))

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "CORE-OM_scoring",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite")))


### set up CORE scoring
###############
### CORE-OM ###
###############

itemStem <- "COREOM"

### need to make scores
itemsCOREwbN <- c(4,  14, 17, 31)
itemsCOREwb <- paste(itemStem, sprintf("%02.0f", itemsCOREwbN), sep = "")
# itemsCOREwb

itemsCOREprobN <- c(2, 5, 8, 11, 13, 15, 18, 20, 23, 27, 28, 30)
itemsCOREprob <- paste(itemStem, sprintf("%02.0f", itemsCOREprobN), sep = "")
itemsCOREprob

itemsCOREfuncN <- c(1, 3, 7, 10, 12, 19, 21, 25, 26, 29, 32, 33)
itemsCOREfunc <- paste(itemStem, sprintf("%02.0f", itemsCOREfuncN), sep = "")
#itemsCOREfunc

itemsCOREriskN <- c(6, 9, 16, 22, 24, 34)
itemsCORErisk <- paste(itemStem, sprintf("%02.0f", itemsCOREriskN), sep = "")
#itemsCORErisk

itemsCOREnrN <- sort(c(itemsCOREwbN, itemsCOREprobN, itemsCOREfuncN))
itemsCOREnr <- paste(itemStem, sprintf("%02.0f", itemsCOREnrN), sep = "")
#itemsCOREnr
#length(itemsCOREnr)

# Define UI for data upload app ----
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("App to score CORE-OM from my Excel (ugh) spreadsheet #1")),
  
  p("This app uses data from the CORE-OM that has been put into the spreadsheet I created for people using Micro$oft forms",
    "to collect CORE-OM data."),
  p("You input data using the sidebar on the left to select the Excel file you want to analyse",
    "Then the data will appear in the Data tab to the right with the six scores: all items, non-risk and the four domains.",
    "If you have, say 2,500 rows of data it will definitely take a fair few seconds for the data."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose file to upload"),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",

                  tabPanel("Data", 
                           p(" "),
                           p("This shows all the unchanged data (well, Date and ID converted to character data) and computed scores.  Buttons at the bottom allow you to export the data."),
                           p(" "),
                          DT::dataTableOutput("contents")),
                  
                  tabPanel("Background", 
                           p("App created 16.iv.24 by Chris Evans specifically for Oiatillo Temirov for checking at this point.",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 16.iv.24."),
                           p("Licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           includeHTML("https://shiny.psyctc.org/boilerplate.html"))
      ),
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = FALSE) # 3. Track basics and inputs and input values

  fileSelected <- reactive({
    req(input$file1)
    input$file1$datapath
  })
  
  fullData <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    vecItems <- paste(itemStem, sprintf("%02.0f", 1:34), sep = "")
    
    suppressMessages(readxl::read_xlsx(path = fileSelected(),
                                       sheet = 2) %>%
                       select(1:36) %>%
                       rename(ID = `Email address`) %>%
                       mutate(Date = as.character(Date),
                              ID = as.character(ID))) -> dataInput
    ### rename the CORE-OM item variables to get shorter names
    colnames(dataInput)[3:36] <- vecItems
    
    ### do the scoring
    dataInput %>%
      rowwise() %>%
      mutate(COREOMtot = getScoreFromItems(c_across(all_of(vecItems)),
                                           propProrateMin = .1),
             COREOMnonRisk = getScoreFromItems(c_across(all_of(itemsCOREnr)),
                                           propProrateMin = .1),
             COREOMwellB = getScoreFromItems(c_across(all_of(itemsCOREwb)),
                                               propProrateMin = .1),
             COREOMprob = getScoreFromItems(c_across(all_of(itemsCOREprob)),
                                               propProrateMin = .1),
             COREOMfunc = getScoreFromItems(c_across(all_of(itemsCOREfunc)),
                                               propProrateMin = .1),
             COREOMrisk = getScoreFromItems(c_across(all_of(itemsCORErisk)),
                                               propProrateMin = .1)) %>%
      ungroup() -> dataInput

    return(dataInput)
  })
  
  varNames <- reactive({
    colnames(fullData())
  })
  
  selectedData <- reactive({
    req(input$var)
    fullData() %>%
      select(input$var)
  })
  
  output$summary <- DT::renderDataTable(
    DT::datatable({fullData()},
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
  
  output$top20 <- renderTable({ 
    fullData() %>%
      filter(row_number() < 21)
  })
  
  output$contents <- DT::renderDataTable(
    DT::datatable({fullData()},
                  extensions = "Buttons",
                  options = list(                                                     
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'frtipB',
                    editable = FALSE,
                    searching = FALSE,
                    buttons = c('copy', 'csv', 'excel', "pdf")
                  ),
    )
  )
  
  # output$nBins <- renderUI({
  #   numericInput("bins", "Number of bins to use in the histogram (between 3 and 60)",
  #                value = 20,
  #                min = 3,
  #                max = 60,
  #                step = 1,
  #                width = "100%")
  # })
  # output$title <- renderUI({
  #   textInput("title", "Put something here if you want a title to the plot",
  #             value = "",
  #             width ="100%")
  # })
  # output$xLab <- renderUI({
  #   textInput("xLab", "Put something here if you want to override the default x axis label",
  #             value = "",
  #             width ="100%")
  # })
  # output$yLab <- renderUI({
  #   textInput("yLab", "Put something here if you want to override the default y axis label",
  #             value = "",
  #             width ="100%")
  # })
  # output$titleText <- renderText({input$title})
  # 
  # histPlot <- reactive({
  #   req(input$bins)
  #   ggplot(data = fullData(),
  #          aes(x = !!input$var)) +
  #     geom_histogram(bins = input$bins) -> p
  #   if (input$title != "") {
  #     p +
  #       ggtitle(input$title) -> p
  #   }
  #   if (input$xLab != "") {
  #     p +
  #       xlab(input$xLab) -> p
  #   }
  #   if (input$yLab != "") {
  #     p +
  #       ylab(input$yLab) -> p
  #   }
  #   p
  # })
  # 
  # output$plot <- renderPlot({
  #   histPlot()
  # })
  # 
  # downloadGGPlotButtonServer(
  #   id = "plotDownload", # <= this should match the ID used in the UI module
  #   ggplotObject = histPlot # No parentheses here to pass *expression*
  # )

}

# Create Shiny app ----
shinyApp(ui, server)