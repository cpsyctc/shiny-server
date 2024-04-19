### CORE-OM_scoring
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))
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

### to read data efficiently
itemsCOREOM <- paste(itemStem, sprintf("%02.0f", 1:34), sep = "")

### needed to make scores
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
      
      numericInput("dp",
                   "Number of decimal places for the scores",
                   value = 2,
                   min = 0,
                   max = 5,
                   width = "100%"),
      helpText("You can change this before or after data has been selected.")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Data", 
                           p(" "),
                           p("The data will take a few seconds to appear, be patient!!"),
                           p("What appears then is the data for any rows that had any non-missing data",
                             br(),
                             "Date and ID are converted to character data, scores are prorated by CORE rules.",
                             br(),
                             "The buttons allow you to export the data to the clipboard or to save in either CSV or Excel .xlsx format.",
                             br(),
                             "You can use all three buttons in any order!"),
                           p(" "),
                           DTOutput("summary")),    

                  tabPanel("Background", 
                           p("App created 16.iv.24 by Chris Evans at this point specifically for Oiatillo Temirov for checking his data.",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 19.iv.24."),
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

    suppressMessages(readxl::read_xlsx(path = fileSelected(),
                                       ### I think this is the most efficient way to get the data
                                       range = "Data!A2:AJ2500",
                                       col_names = c("Date", "ID", itemsCOREOM), # adds names and then formats
                                       col_types = c("text", "text", rep("numeric", 34)))) -> dataInput

    
    ### do the scoring
    dataInput %>%
      rowwise() %>%
      mutate(nMissing = getNNA(c_across(COREOM01:COREOM34))) %>%
      filter(!(nMissing == 34 & is.na(Date) & is.na(ID))) %>%
      mutate(COREOMtot = round(getScoreFromItems(c_across(all_of(itemsCOREOM)),
                                           propProrateMin = .1), input$dp),
             COREOMnonRisk = round(getScoreFromItems(c_across(all_of(itemsCOREnr)),
                                               propProrateMin = .1), input$dp),
             COREOMwellB = round(getScoreFromItems(c_across(all_of(itemsCOREwb)),
                                             propProrateMin = .1), input$dp),
             COREOMprob = round(getScoreFromItems(c_across(all_of(itemsCOREprob)),
                                            propProrateMin = .1), input$dp),
             COREOMfunc = round(getScoreFromItems(c_across(all_of(itemsCOREfunc)),
                                            propProrateMin = .1), input$dp),
             COREOMrisk = round(getScoreFromItems(c_across(all_of(itemsCORErisk)),
                                            propProrateMin = .1), input$dp)) %>%
      ungroup() %>%
      select(ID, Date, COREOMtot:COREOMrisk, COREOM01:COREOM34) -> dataInput
    
    return(dataInput)
  })
  
  fileStubName <- reactive({
    req(input$file1)
    fileExt <- tools::file_ext(input$file1$name) # get the full filename
    fileStubName <- str_replace(input$file1$name, fixed(fileExt), "") # strip the extension
    fileStubName <- str_replace(fileStubName, "(\\.)+$", "") # remove any terminal "."!
    fileStubName
  })

  output$summary <- DT::renderDataTable(server = FALSE,
    DT::datatable({fullData()},
                  extensions = "Buttons",
                  selection = "none",
                  options = list(
                    buttons = list(
                      list(extend = 'csv',   filename =  paste0("scored-", fileStubName())),
                      list(extend = 'excel', filename =  paste0("scored-", fileStubName()))
                    ),
                    ### the important thing is that there is the l to allow for the lengthMenu 
                    ### https://stackoverflow.com/questions/52645959/r-datatables-do-not-display-buttons-and-length-menu-simultaneously
                    dom = 'Blrtip',
                    fixedColumns = list(leftColumns = 2, rightColumns = 6),
                    pageLength = 20,
                    autoWidth = TRUE,
                    ordering = FALSE,
                    editable = FALSE,
                    searching = FALSE),
    )
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)