### file_upload_and_download

suppressMessages(library(shiny))
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "file_upload_and_download",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for data upload app ----
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: file type ----
      radioButtons("fileType", "What type of file do you want to use?",
                   choices = c("R data (usually stored with  'save()'" = "rda",
                               "Excel xls file" = "xls",
                               "Excel xlsx file" = "xlsx",
                               "[Libre|Open]Office ods file" = "ods",
                               "CSV (Comma Separated Variables)" = "csv",
                               "TSV (Tab Separated Variables)" = "tsv",
                               "SPSS sav file" = "sav"),
                   selected = "csv"),
                   
      # Input: Select a file ----
      fileInput("file1", "Choose a file in the format you chose above"),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "If format is not R or SPSS you probably have variable names in the first row", TRUE),

      numericInput("sheetNum", "If you are inputting a spreadsheet give the number of the worksheet",
                   value = 1,
                   min = 1,
                   max = 20,
                   step = 1,
                   width = "100%"),

      tags$p("If you are inputting a csv file you have to define the separator"),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";"),
                   selected = ","),
      
      tags$p("And if you are inputting a tsv or csv file you have to define the quote character"),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      # 
      # selectInput("idVar", "Choose ID variable (optional)", character(0)),
      # 
      # selectInput("genderVar", "Choose gender variable", character(0)),
      # 
      # selectInput("ageVar", "Choose age variable", character(0)),
      # 
      # selectInput("scoreVar", "Choose variable for score to classify", character(0))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # textOutput("fileName"),
      # 
      textOutput("variableName"),
      
      uiOutput('dropdownID'),
      uiOutput('dropdownGender'),
      uiOutput('dropdownAge'),
      uiOutput('dropdownScore'),
      
      # Output: Data file ----
      tableOutput("contents")
      
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
    req(input$fileType,
        input$file1)
    input$file1$datapath
  })
  
  fullData <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if (input$fileType == "csv") {
      dataInput <- read.csv(fileSelected(),
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
    }
    
    if (input$fileType == "tsv") {
      suppressMessages(dataInput <- read_tsv(fileSelected(),
                            col_names = input$header))
                            # quote = input$quote)
    }
    
    if (input$fileType == "xls") {
      dataInput <- readxl::read_xls(path = fileSelected(),
                                    sheet = input$sheetNum,
                                    col_names = input$header)
    }
    
    if (input$fileType == "xlsx") {
      dataInput <- readxl::read_xlsx(path = fileSelected(),
                                     sheet = input$sheetNum,
                                     col_names = input$header)
    }
    
    if (input$fileType == "ods") {
      dataInput <- readODS::read_ods(path = fileSelected(),
                                     sheet = input$sheetNum,
                                     col_names = input$header)
    }
    
    if (input$fileType == "sav") {
      suppressWarnings(foreign::read.spss(file = fileSelected(),
                                          to.data.frame = TRUE) %>%
                         as_tibble() -> dataInput)
    }    
    
    if (input$fileType == "rda"){
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
    selectInput('var', 'Select variable to analyse', names(fullData()))
  })
  
    selectedData <- reactive({
    req(input$var)
    
    fullData() %>%
      select(input$var)
  })

  # output$contents <- renderTable({ fullData()[c(input$idVar, input$genderVar)] })
  output$contents <- renderTable({ selectedData() })

}

# Create Shiny app ----
shinyApp(ui, server)