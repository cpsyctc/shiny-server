### paste_or_upload2
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))
suppressMessages(library(CECPfuns))

### function
parseData <- function(dat, sepChar, quoteChar, decChar, bigChar) {
  ### first do the split
  if (sepChar != "") {
    tmpDat <- str_replace_all(dat, fixed(paste0(sepChar, sepChar)), sepChar)
    tmpDat <- str_split_1(tmpDat, fixed(sepChar))
  }
  
  ### now dequote
  if (quoteChar != "") {
    tmpDat <- str_remove_all(tmpDat, quoteChar)
  }
  ### now deal with decimal separator
  if (decChar != "") {
    tmpDat <- str_replace(tmpDat, fixed(decChar), ".")
  }
  
  ### now deal with thousands separator
  if (bigChar != "") {
    tmpDat <- str_replace_all(tmpDat, fixed(bigChar), "")
  }
  suppressWarnings(as.numeric(tmpDat))
}

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 48))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Histogram_and_summary1",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite")))

ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Test of concept: upload or paste")),
  
  p("This app is really a test of concept and should be followed by other apps using both paste or uploaded data."),
  p("You input data using the sidebar on the left to select whether you are pasting or uploading."),
  p("choose the file to upload (you should see a file upload dialogue for that).",
    "You may need to use some of the other sidebar inputs depending on your file format.",
    "The first tab to the right of the sidebar will show you the top rows of the data and allow you to choose your variable.",
    "Once you have selected your variable the next tab show the histogram which you can customise a bit and can download in various formats.",
    "Similarly, the summary tab gives you some summary statistics for your dataset which you can download and the data tab shows the actual variable data."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      radioButtons("inputType", 
                   "What type of file do you want to use?",
                   choices = c("Upload your data from file" = "upload",
                               "Paste your data in" = "paste"),
                   selected = character(0)),
      helpText(paste0("I recommend uploading from file as it's less prone to mangle your data than pasting.  ",
                      "It's hard to parse pasted data properly with different decimal separators and possible use of ",
                      "a thousands separator on top of the diversity of separators and quoting.  So please check the ",
                      "summary of the data to see if it matches what you expect.")),
      
      conditionalPanel(condition = "input.inputType == 'paste'",
                       
                       p("Paste or type your data in the box below"),
                       textAreaInput("pastedData",
                                     "Paste your data here:",
                                     width = "100%",
                                     resize = "both"),
                       helpText("Pasted data can have various formats"),
                       
                       # Input: Select quotes ----
                       radioButtons("quoteChar", 
                                    "Quote",
                                    choices = c("None" = "",
                                                "Double Quote" = '"',
                                                "Single Quote" = "'"),
                                    selected = ""),
                       
                       tags$p("You may have to choose a separator character"),
                       
                       # Input: Select separator ----
                       radioButtons("sepChar", 
                                    "Separator",
                                    choices = c("Space" = " ",
                                                "Comma" = ",",
                                                "Semicolon" = ";",
                                                "tab" = "\t"),
                                    selected = " "),
                       
                       # Input: Select decimal point marker ----
                       radioButtons("decChar", 
                                    "Decimal point marker",
                                    choices = c(#"none" = "",
                                      "Full stop" = ".",
                                      "Comma" = ",",
                                      "none" = ""),
                                    selected = ""),
                       
                       radioButtons("bigChar", 
                                    "Thousands separator (often a comma",
                                    choices = c(#"none" = "",
                                      "Full stop" = ".",
                                      "Comma" = ",",
                                      "none" = ""),
                                    selected = ""),
                       
                       helpText("You can check the data in the tab on the right to see if you have the correct separator"),
                       
                       # actionButton("checkData1", "Check data format ..."),
      ),
      
      conditionalPanel(condition = "input.inputType == 'upload'",
                       
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
                       
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data", 
                           conditionalPanel(condition = "input.inputType == 'paste'",
                                            uiOutput('text2'),
                                            p(" "),
                                            p(" "),
                                            p("Here is the start of the parsed data:"),
                                            textOutput('data1'),
                                            # p(" "),
                                            # p("Here are summary statistics for the parsed data:"),
                                            # tableOutput('summary'),    
                           ),
                           conditionalPanel(condition = "input.inputType == 'upload'",
                                            p(" "),
                                            p(" "),
                                            p("Here is the start of the uploaded data:"),
                                            p(" "),
                                            tableOutput("top20"),
                                            
                                            uiOutput('dropdownID'),
                           ),
                  ),
                  
                  tabPanel("Data summary",
                           p("Please check that these summary statistics for your data are as you'd expect."),
                           tableOutput('summary'),    
                  ),
                  
                  tabPanel("Histogram", 
                           p("This shows a histogram of your non-missing data (see summary to check if there were missing values)"),
                           p("You can change the number of bins in the histogram with the next next input:"),
                           p("You can use the dialogue below the plot to download it chosing the name and the format of the plot"),
                           p(" "),
                           uiOutput("nBins"),
                           uiOutput("title"),
                           uiOutput("xLab"),
                           uiOutput("yLab"),
                           uiOutput("fileWidth"),
                           uiOutput("fileHeight"),
                           uiOutput("textSize"),
                           helpText(paste0("The onscreen plot size is set to 800px and can't be altered.",
                                           "You may want a much bigger download graphic file size but if so ",
                                           "you may have to increase the plot text size to scale it up to the ",
                                           "file size.  Experiment!")),
                           plotOutput("plot",
                                      width = "100%",
                                      height = "800px"),
                           downloadGGPlotButtonUI("plotDownload", "histogram_plot"),
                           p(" "),
                           p("Thanks to Keith Newman for the download handler: ",
                             a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file"))),
                  
                  tabPanel("Background", 
                           value = 3,
                           p("App created 16.iv.24 by Chris Evans.",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 24.iv.24."),
                           p("Licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           includeHTML("https://shiny.psyctc.org/boilerplate.html")
                  ),
                  
                  id = "tabSelected"),
    )
  )
)


server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = FALSE) # 3. Track basics and inputs and input values
  
  inputType <- eventReactive(input$compute, {
    input$inputType
  })
  
  ###
  ### handle uploaded data
  ###
  
  ### this is just the standard file input dialogue
  fileSelected <- reactive({
    req(input$dataType)
    input$file1$datapath
  })
  
  ### this now processes that file by file type
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
    
    ### select numeric variables only
    dataInput %>%
      select(where(is.numeric))
    # return(dataInput)
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
  
  output$top20 <- renderTable({ 
    selectedData() %>%
      filter(row_number() < 21)
  })
  
  ###
  ### now handle input of pasted data
  ###
  output$text2 <- renderUI(pastedDataRaw())
  
  output$data1 <- renderText(pastedDataNum()[1:5]) 
  
  tmpRawData <- reactive({
    str_sub(input$pastedData, 1, 24)
  })
  
  pastedDataRaw <- reactive({
    req(input$pastedData)
    paste0("The first 24 characters of your raw data are: ", tmpRawData())
  })
  
  pastedDataNum <- reactive({
    req(input$pastedData)
    validate(
      need((input$decChar == "" & input$bigChar  == "") | n_distinct(c(input$sepChar, input$decChar, input$bigChar)) == 3,
           'Separator, decimal marker and thousands marker must all be different')
    )
    parseData(input$pastedData,
              sepChar = input$sepChar,
              quoteChar = input$quoteChar,
              decChar = input$decChar,
              bigChar = input$bigChar)
  })
  
  ###
  ### have got data in at this point whether it came by pasting or uploading
  ### so can now construct finalData(): reactive tibble with single variable named "values"
  ###
  
  finalData <- reactive({
    if (input$inputType == "paste") {
      tibble(values = pastedDataNum()) -> tmpTib
    } else {
      selectedData() %>%
        select(!!input$var) %>%
        rename(values = !!input$var) -> tmpTib
    }
    tmpTib
  })
  
  ###
  ### sumarise finalData()
  ###
  dataSummary <- reactive({
    req(input$inputType)
    finalData() %>%
      filter(!is.na(values)) %>%
      nrow() -> tmpNOK
    
    if(tmpNOK == 0) {
      tibble("There are no non-missing values in the data")
    } else {
      finalData() %>%
        summarise(n = n(),
                  nNA = getNNA(values),
                  nOK = n - nNA,
                  min = min(values, na.rm = TRUE),
                  max = max(values, na.rm = TRUE),
                  mean = mean(values, na.rm = TRUE),
                  median = median(values, na.rm = TRUE),
                  SD = sd(values, na.rm = TRUE)) %>%
        pivot_longer(cols = everything())
    }
  })
  
  output$summary <- renderTable(dataSummary())
  
  ###
  ### can now plot
  ###
  ### inputs to modify plot
  output$nBins <- renderUI({
    numericInput("bins", "Number of bins to use in the histogram (between 3 and 60)",
                 value = 20,
                 min = 3,
                 max = 60,
                 step = 1,
                 width = "100%")
  })
  
  output$title <- renderUI({
    textInput("title", "Put something here if you want a title to the plot",
              value = "",
              width ="100%")
  })
  
  output$xLab <- renderUI({
    textInput("xLab", "Put something here if you want to override the default x axis label",
              value = "",
              width ="100%")
  })
  
  output$yLab <- renderUI({
    textInput("yLab", "Put something here if you want to override the default y axis label",
              value = "",
              width ="100%")
  })
  
  ### plot download file variables
  output$fileWidth <- renderUI({
    numericInput("fileWidth", "You can change the plot file width (pixels) here",
                 min = 400,
                 max = 10000,
                 value = 2000,
                 width ="100%")
  })
  
  output$fileHeight <- renderUI({
    numericInput("fileHeight", "You can change the plot file height (pixels) here",
                 min = 400,
                 max = 10000,
                 value = 1600,
                 width ="100%")
  })
  
  output$textSize <- renderUI({
    numericInput("textSize", "You can change the plot text size from the default (48) here",
                 min = 1,
                 value = 48,
                 width ="100%")
  })
  
  output$titleText <- renderText({input$title})
  
  ###
  ### can now construct the plot
  ###
  histPlot <- reactive({
    req(input$bins)
    ggplot(data = finalData(),
           # aes(x = !!input$var)) +
           aes(x = values)) +
      geom_histogram(bins = input$bins) -> p
    if (input$title != "") {
      p +
        ggtitle(input$title) -> p
    }
    if (input$xLab != "") {
      p +
        xlab(input$xLab) -> p
    }
    if (input$yLab != "") {
      p +
        ylab(input$yLab) -> p
    }
    if(input$textSize != 48){
      p +
        theme(text = element_text(size = input$textSize)) -> p
    }
    p
  })
  
  output$plot <- renderPlot({
    histPlot()
  })
  
  downloadGGPlotButtonServer(
    id = "plotDownload", # <= this should match the ID used in the UI module
    ggplotObject = histPlot, # No parentheses here to pass *expression*
    width = input$fileWidth,
    height = input$fileHeight
  )
}

shinyApp(ui, server)