### Hashing_IDs
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))
suppressMessages(library(openssl))

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Hashing_IDs",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite")))

# Define UI for data upload app ----
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("App that generates hash codes for IDs")),
  
  p("This app takes a dataset from which you choose a single variable, typically IDs values.  ",
    "It then creates a new variable containing hashed values for each value in the original ",
    "variable using a hash algorithm that you choose, either sha256 or sha512, and a hash key ",
    "you give it."),
  p("You input data using the sidebar on the left to select the file format you are using and then to",
    "choose the file to upload (you should see a file upload dialogue for that).",
    "You may need to use some of the other sidebar inputs depending on your file format."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: hash key
      p("The first thing you have to do is choose a key phrase the hash algorithm will use"),
      p("Although you will be able to download the hash values created, I strongly recommend",
        "that you write the key phrase you input here down somewhere so you can use it again."),
      textInput("hashKey", "Hashing key value you want", value = "Whatever you want!"),
      
      # Input: file type ----
      radioButtons("dataType", 
                   "What type of file do you want to use?",
                   choices = c("R data (usually stored with  'save()'" = "rda",
                               "Excel xls file" = "xls",
                               "Excel xlsx file" = "xlsx",
                               "[Libre|Open]Office ods file" = "ods",
                               "CSV (Comma Separated Variables)" = "csv",
                               "TSV (Tab Separated Variables)" = "tsv",
                               "SPSS sav file" = "sav"),
                   selected = "csv"),
      
      # Input: Select a file ----
      fileInput("file1", "Choose file if using file upload"),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", 
                    "Check this if first row of data gives variable names",
                    TRUE),
      
      numericInput("sheetNum", "If you are inputting a spreadsheet give the number of the worksheet",
                   value = 1,
                   min = 1,
                   max = 20,
                   step = 1,
                   width = "100%"),
      
      tags$p("If you are inputting a csv file you have to define the separator"),
      
      # Input: Select separator ----
      radioButtons("sep", 
                   "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";"),
                   selected = ","),
      
      tags$p("And if you are inputting a tsv or csv file you have to define the quote character"),
      
      # Input: Select quotes ----
      radioButtons("quote", 
                   "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Select variable", 
                           textOutput("variableName"),
                           
                           uiOutput('dropdownID'),
                           
                           # Output: Data file ----
                           tableOutput("top20")),
                  
                  tabPanel("Data with hashed values", 
                           p(" "),
                           p("This shows the first ten rows of the data with hashed values."),
                           p(" "),
                           tableOutput('viewHead'),
                           p("You can download the entire dataset using the following button.  ",
                             "I have chosen csv (comma separated variables) format as it's a ",
                             "nice safe format for simple data like this and can be imported into ",
                             "pretty much any software though you may have to check out how if you ",
                             "haven't done this before."),
                           downloadButton("download", "Download as csv")),
                  
                  # DT::dataTableOutput("contents")),
                  
                  tabPanel("Background", 
                           p("App created 05.iv.25 by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 05.iv.25."),
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
  
  telemetry$start_session(track_inputs = TRUE, track_values = FALSE) # 3. Track basics and inputs and not input values
  
  fileSelected <- reactive({
    req(input$dataType)
    input$file1$datapath
  })
  
  fullData <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if (input$dataType == "csv") {
      dataInput <- read.csv(fileSelected(),
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote)
    }
    
    if (input$dataType == "tsv") {
      suppressMessages(dataInput <- read_tsv(fileSelected(),
                                             col_names = input$header))
      # quote = input$quote)
    }
    
    if (input$dataType == "xls") {
      dataInput <- readxl::read_xls(path = fileSelected(),
                                    sheet = input$sheetNum,
                                    col_names = input$header)
    }
    
    if (input$dataType == "xlsx") {
      dataInput <- readxl::read_xlsx(path = fileSelected(),
                                     sheet = input$sheetNum,
                                     col_names = input$header)
    }
    
    if (input$dataType == "ods") {
      dataInput <- readODS::read_ods(path = fileSelected(),
                                     sheet = input$sheetNum,
                                     col_names = input$header)
    }
    
    if (input$dataType == "sav") {
      suppressWarnings(foreign::read.spss(file = fileSelected(),
                                          to.data.frame = TRUE) %>%
                         as_tibble() -> dataInput)
    }    
    
    if (input$dataType == "rda"){
      tmpEnv <- new.env()
      name <- load(file = fileSelected(), envir = tmpEnv)
      dataInput<- tmpEnv[[name]]
    }
    
    return(dataInput)
  })
  
  varNames <- reactive({
    colnames(fullData())
  })
  
  output$dropdownID <- renderUI({
    req(input$file1)
    varSelectInput("var",
                   "Select the variable to analyse",
                   fullData())
  })
  
  selectedData <- reactive({
    req(input$var)
    fullData() %>%
      select(input$var) 
  })
  
  hashedData <- reactive({
    req(input$var)
    # fullData() %>%
    #   select(input$var) %>%
    selectedData() %>%
      mutate(hashVal = sha256(as.character(!!input$var), input$hashKey))
  })
  
  output$top20 <- renderTable({ 
    selectedData() %>%
      filter(row_number() < 21)
  })
  
  output$viewHead <- renderTable({
    head(hashedData())
  })
  
  output$download <- downloadHandler(
    filename = "hashedValues.csv",
    contentType = "text/csv",
    content = function(file) {
      write_csv(hashedData(), file = file)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)