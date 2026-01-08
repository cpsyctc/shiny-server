### Histogram_and_summary1
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Histogram_and_summary1",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite")))

# Define UI for data upload app ----
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("App giving histogram and summary statistics #1")),
  
  p("This app simply creates a histogram and summary statistics for data entered by file upload.",
    "It's largely a test of concept and should be followed by other apps using data from file uploads.",
    "I will probably embellish this app too with a bit more time."),
  p("You input data using the sidebar on the left to select the file format you are using and then to",
    "choose the file to upload. You should see a file upload dialogue for that.",
    "If you don't immediately have data of your own you want to input have a look ",
    a("here", href="https://www.psyctc.org/psyctc/root/stats/datasets/"),
    "."),
  p("You may need to use some of the other sidebar inputs depending on your file format.",
    "The first tab to the right of the sidebar will show you the top rows of the data and allow you to choose your variable.",
    "Once you have selected your variable the next tab show the histogram which you can customise a bit and can download in various formats.",
    "Similarly, the summary tab gives you some summary statistics for your dataset which you can download and the data tab shows the actual variable data."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
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
      
      p("OK, instead of uploading via a file,  you can paste the data in here"),

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
                  
                  tabPanel("Histogram", 
                           p("This shows a histogram of your non-missing data (see summary to check if there were missing values)"),
                           p("You can change the number of bins in the histogram with the next next input:"),
                           p("You can use the dialogue below the plot to download it chosing the name and the format of the plot"),
                           p(" "),
                           uiOutput("nBins"),
                           uiOutput("title"),
                           uiOutput("xLab"),
                           uiOutput("yLab"),
                           plotOutput("plot"),
                           downloadGGPlotButtonUI("plotDownload", "this-plot"),
                           p(" "),
                           p("Thanks to Keith Newman for the download handler: ",
                             a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file"))),
                  
                  tabPanel("Summary", 
                           p("The table below gives some simple descriptive statistics for your data"),
                           p(" "),
                           uiOutput("nDP"),
                           DT::dataTableOutput("summary"),
                           p(" "),
                           br(),
                           p("Most of those names should be self-explanatory but Q05 is the fifth centile, Q25 the 25th, i.e. the lower quartile, etc."),
                           p(" "),
                           br(),
                           p("Geek point: The quantiles and median are computed by R's default method, type 7.  This may lead to small differences from values",
                             "returned from SAS, which uses type 3 and from Minitab and SPSS which use type 6.",
                             "see ",
                             a("detailed wikipedia article", href="https://en.wikipedia.org/wiki/Quantile"),
                             "for much more detail on that probably entirely academic issue for our sorts of data."),
                           p(" ")),
                  
                  
                  tabPanel("Data", 
                           p(" "),
                           p("This shows all the data for the selected variable.  Buttons at the bottom allow you to export the data."),
                           p(" "),
                          DT::dataTableOutput("contents")),
                  
                  tabPanel("Background", 
                           p("App created 22.iii.24 by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Updated 5.iv.25 to add control of the decimal places in the summary output."),
                           p("Updated 8.i.26 to restrict variable selection to numeric variables and add ",
                             "link to my ",
                             a("PSYCTC.org datasets", href="https://www.psyctc.org/psyctc/root/stats/datasets/"),
                             "."),
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
    req(input$dataType)
    input$file1$datapath
  })
  
  fullData <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if (input$dataType == "csv") {
      read.csv(fileSelected(),
               header = input$header,
               sep = input$sep,
               quote = input$quote) %>%
        as_tibble() -> dataInput
    }
    
    if (input$dataType == "tsv") {
      suppressMessages(dataInput <- read_tsv(fileSelected(),
                            col_names = input$header))
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
    # colnames(fullData())
    fullData() %>%
      select(where(is.numeric)) %>%
      colnames()
  })
  
  numericData <- reactive({
    fullData() %>%
      select(where(is.numeric))
  })
  
  output$dropdownID <- renderUI({
    req(input$file1)
    varSelectInput("var",
                   "Select the variable to analyse",
                   numericData())
  })
  
  selectedData <- reactive({
    req(input$var, input$nDP)
    fullData() %>%
      select(input$var)
  })
  
  tibSummary <- reactive({
    req(input$var, input$nDP)
    selectedData() %>%
      summarise(nRows = n(),
                nMiss = sum(is.na(!!input$var)),
                nOK = nRows - nMiss,
                Min = min(!!input$var, na.rm = TRUE),
                Q05 = quantile(!!input$var,
                               probs = .05,
                               na.rm = TRUE),
                Q25 = quantile(!!input$var,
                               probs = .25,
                               na.rm = TRUE),
                Median = median(!!input$var, na.rm = TRUE),
                Q75 = quantile(!!input$var,
                               probs = .75,
                               na.rm = TRUE),
                Q95 = quantile(!!input$var,
                               probs = .95,
                               na.rm = TRUE),
                Max = max(!!input$var, na.rm = TRUE),
                Mean = mean(!!input$var, na.rm = TRUE),
                SD = sd(!!input$var, na.rm = TRUE),
                Variance = var(!!input$var, na.rm = TRUE))  %>%
      # mutate(across(Min : Variance, ~ round(.x, nDP()))) %>%
      # mutate(across(Min : Variance, round, nDP())) %>%
      rename(`n(Rows)` = nRows,
             `n(Missing)` = nMiss,
             `n(Usable)` = nOK) %>%
      pivot_longer(cols = everything()) %>%
      mutate(value = round(value, nDP()))
  })
  
  output$summary <- DT::renderDataTable(
    DT::datatable({tibSummary()},
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
    selectedData() %>%
      filter(row_number() < 21)
  })
  
  output$contents <- DT::renderDataTable(
    DT::datatable({selectedData()},
                  extensions = "Buttons",
                  options = list(                                                     
                    fixedColumns = TRUE,
                    pageLength = -1,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'frtipB',
                    editable = FALSE,
                    searching = FALSE,
                    buttons = c('copy', 'csv', 'excel', "pdf")
                  ),
    )
  )
  
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
  output$titleText <- renderText({input$title})
  
  histPlot <- reactive({
    req(input$bins)
    ggplot(data = fullData(),
           aes(x = !!input$var)) +
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
    p
  })
  
  output$plot <- renderPlot({
    histPlot()
  })
  
  downloadGGPlotButtonServer(
    id = "plotDownload", # <= this should match the ID used in the UI module
    ggplotObject = histPlot # No parentheses here to pass *expression*
  )
  
  output$nDP <- renderUI({
    numericInput("nDP", "Number of decimal places (between 1 and 7)",
                 value = 3,
                 min = 1,
                 max = 7,
                 step = 1,
                 width = "100%")
  })
  
  nDP <- reactive({
    round(as.numeric(input$nDP))
  })

}

# Create Shiny app ----
shinyApp(ui, server)