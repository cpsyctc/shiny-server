### paste_or_upload2
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))
suppressMessages(library(CECPfuns))
# suppressMessages(library(shinyFeedback))

### functions
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

cleanInputQuantiles <- function(vecChar) {
  ### function deals with challenge of validating quantile input (I hope)
  vecChar <- str_squish(vecChar)
  vecChar <- str_replace_all(vecChar, "\\;", ",")
  vecChar <- str_replace_all(vecChar, "\\,\\,", ", ,")
  vecChar <- str_replace_all(vecChar, ",([:space:])+", ",")
  vecChar <- str_replace_all(vecChar, "([:space:])+", ",")
  vecChar <- unlist(strsplit(vecChar, ","))
  retVal <- suppressWarnings(as.numeric(vecChar))
  retVal <- retVal[!(retVal <= 0)]
  retVal <- retVal[!(retVal >= 1)]
  retVal <- retVal[!is.na(retVal)]
  return(retVal)
}

plotQuantileCIsfromDat2 <- function (vecDat = NA, 
                                     vecQuantiles = NA, 
                                     method = c("Nyblom", "Exact", "Bootstrap"), 
                                     ci = 0.95, 
                                     R = 9999, 
                                     type = NULL, 
                                     xLab = "Value for probability (quantile)", 
                                     yLab = "Probability", 
                                     percent = FALSE, 
                                     colPoint = "black", 
                                     colLCL = "black", 
                                     colUCL = "black", 
                                     vertQuantiles = TRUE, 
                                     vertCLs = FALSE, 
                                     shadeCI = TRUE, 
                                     addAnnotation = TRUE, 
                                     titleText = "ECDF with quantiles and CIs around quantiles", 
                                     subtitleText = "", 
                                     titleJust = 0.5, 
                                     annotationSize) {
  getCI <- NULL
  quantile_confint_nyblom <- NULL
  quantile_confint_exact <- NULL
  quantile_confint_boot <- NULL
  prob <- NULL
  value <- NULL
  quantileCI <- NULL
  LCL <- NULL
  UCL <- NULL
  nAll <- length(vecDat)
  nMiss <- getNNA(vecDat)
  vecDat <- na.omit(vecDat)
  nOK <- length(vecDat)
  minVal <- min(vecDat)
  maxVal <- max(vecDat)
  rangeVal <- maxVal - minVal
  tibCIs <- getCIforQuantiles(vecDat, vecQuantiles, method = method, 
                              ci = ci, R = R, type = type)
  vecDat %>% as_tibble() %>% 
    filter(!is.na(value)) -> tibDat
  
  tmpPlot <- ggplot(data = tibCIs) + 
    geom_point(aes(y = prob, x = quantile), 
               colour = colPoint) + 
    geom_point(aes(y = prob, x = LCL), 
               colour = colLCL) + 
    geom_point(aes(y = prob, x = UCL), 
               colour = colUCL) + 
    geom_linerange(aes(y = prob, xmin = LCL, xmax = UCL)) + 
    xlab(xLab) + 
    ylab(yLab) + 
    ggtitle(titleText, 
            subtitle = subtitleText)
  if (percent) {
    tmpPlot <- tmpPlot + 
      scale_y_continuous(labels = paste0(seq(0, 100, 25), "%"))
  }
  if (shadeCI) {
    tmpPlot <- tmpPlot + 
      geom_rect(aes(xmin = LCL, xmax = UCL, ymin = 0, ymax = prob), 
                fill = "grey80")
  }
  if (vertQuantiles) {
    tmpPlot <- tmpPlot + 
      geom_linerange(aes(ymin = 0, ymax = prob, x = quantile), 
                     colour = "grey40")
  }
  if (vertCLs) {
    tmpPlot <- tmpPlot + geom_linerange(aes(ymin = 0, ymax = prob, 
                                            x = LCL), colour = "grey70") + geom_linerange(aes(ymin = 0, 
                                                                                              ymax = prob, x = UCL), colour = "grey70")
  }
  if (!percent) {
    tmpPlot <- tmpPlot + stat_ecdf(data = tibDat, aes(x = value), 
                                   geom = "step", pad = FALSE)
  }
  else {
    suppressMessages(tmpPlot <- tmpPlot + 
                       stat_ecdf(data = tibDat, aes(x = value), geom = "step", pad = FALSE) + 
                       scale_y_continuous(name = "Percent", labels = paste0(seq(0, 100, 25), "%")))
  }
  if (addAnnotation) {
    if (length(vecQuantiles) == 1) {
      annotateText <- paste0("n(total) = ", 
                             nAll, "\n", 
                             "n(missing) = ", 
                             nMiss, 
                             "\n", "n(usable) = ", 
                             nOK, 
                             "\n", 
                             "ci = ", ci,
                             " i.e. ", 
                             paste0(100 * ci), 
                             "%\n",
                             "quantile = ",
                             vecQuantiles)
    }
    else {
      annotateText <- paste0("n(total) = ", 
                             nAll, 
                             "\n", 
                             "n(missing) = ", 
                             nMiss, "\n", 
                             "n(usable) = ", 
                             nOK,
                             "\n", 
                             "ci = ", 
                             ci, 
                             " i.e. ", 
                             paste0(100 *  ci), 
                             "%\n", 
                             "quantiles = ", 
                             convertVectorToSentence(vecQuantiles))
    }
    tmpPlot <- tmpPlot + 
      annotate("text", 
               x = minVal + 0.015 * rangeVal, 
               y = 0.9, 
               label = annotateText, 
               hjust = 0,
               size = annotationSize / .pt) # horrible fudge, courtesy of 
    # https://stackoverflow.com/questions/65076492/ggplot-size-of-annotate-vs-size-of-element-text
    # to make sure annotate size matches other text size
  }
  return(tmpPlot)
}

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24)) 

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Histogram_and_summary1",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite")))

ui <- fluidPage(
  
  # useShinyFeedback(), # include shinyFeedback
  
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
                       
                       p("Paste or type your data in the box below."),
                       textAreaInput("pastedData",
                                     "Paste your data here (you need at least ten values):",
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
                           value = 1,
                           conditionalPanel(condition = "input.inputType == 'paste'",
                                            uiOutput('text2'),
                                            p(" "),
                                            p(" "),
                                            p("Here is the start of the parsed data:"),
                                            textOutput('data1'),
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
                           value = 2,
                           
                           p("Please check that these summary statistics for your data are as you'd expect."),
                           p("You can download the statistics with the buttons below the table."),
                           p(" "),
                           numericInput("dpSummary",
                                        "Number of decimal places",
                                        value = 3,
                                        min = 0,
                                        max = 5,
                                        width = "100%"),
                           p(" "),
                           
                           DT::dataTableOutput("summary"),
                  ),
                  
                  tabPanel("ECDF plot",
                           value = 3,
                           
                           textInput("quantiles",
                                     "The quantiles you want as numbers, probabilities, separated by spaces, commas or semicolons",
                                     value = ".05, .1, .25, .5, .75, .9, .95",
                                     width="100%"),
                           
                           uiOutput("showQuantiles"),
                           
                           numericInput("ci",
                                        "Width of CI (usually .95, i.e. 95% CI, <=.99)",
                                        value = .95,
                                        min = .7,
                                        max = .999,
                                        width = "100%"),
                           
                           conditionalPanel(
                             condition = 'output.gotCleanQuantiles',
                             
                             p("This shows the ECDF of your non-missing data (see summary to check if there were missing values)"),
                             p("You can use the dialogue below the plot to download it chosing the name and the format of the plot"),
                             p(" "),
                             uiOutput("title"),
                             uiOutput("xLab"),
                             uiOutput("yLab"),
                             uiOutput("fileWidth"),
                             uiOutput("fileHeight"),
                             helpText(paste0("These next two values will need some trial and error to get ",
                                             "the best aesthetic and will be different depending on the file size, ",
                                             "i.e. the last two values above, and for different download file formats.")),
                             uiOutput("textSize"),
                             uiOutput("annotationSize"),
                             helpText(paste0("The onscreen plot size is set to 800px and can't be altered.",
                                             "You may want a much bigger download graphic file size but if so ",
                                             "you may have to increase the plot text size to scale it up to the ",
                                             "file size.  Experiment!")),
                             
                             plotOutput("quantileCIplot", height = 500),
                             
                             downloadGGPlotButtonUI("quantileCIplotDownload", "quantileCIplot"),
                             p(" "),
                             p("Thanks to Keith Newman for the download handler: ",
                               a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file"))),
                  ),
                  
                  tabPanel("Quantiles and their CIs",
                           value = 4,
                           
                           p(" "),
                           numericInput("dpQuantiles",
                                        "Number of decimal places",
                                        value = 3,
                                        min = 0,
                                        max = 5,
                                        width = "100%"),
                           p(" "),
                           
                           conditionalPanel(
                             condition = 'output.gotCleanQuantiles',
                             
                             p("Here are the values of the quantiles and the confidence limits."),
                             p("You can download the statistics with the buttons below the table."),
                             p(" "),
                             DT::dataTableOutput("tibQuantiles"),
                           )
                  ),
                  
                  tabPanel("CI forest plot",
                           value = 5,
                           
                           conditionalPanel(
                             condition = 'output.gotCleanQuantiles',
                             
                             p("This shows the quantiles and their CIs as a forest plot."),
                             p(" "),
                             uiOutput("titleQuantiles"),
                             uiOutput("xLabQuantiles"),
                             uiOutput("yLabQuantiles"),
                             uiOutput("fileWidthQuantiles"),
                             uiOutput("fileHeightQuantiles"),
                             helpText(paste0("These next two values will need some trial and error to get ",
                                             "the best aesthetic and will be different depending on the file size, ",
                                             "i.e. the last two values above, and for different download file formats.")),
                             uiOutput("textSizeQuantiles"),
                             helpText(paste0("The onscreen plot size is set to 800px and can't be altered.",
                                             "You may want a much bigger download graphic file size but if so ",
                                             "you may have to increase the plot text size to scale it up to the ",
                                             "file size.  Experiment!")),
                             
                             plotOutput("forestPlot", height = 500),
                             
                             downloadGGPlotButtonUI("quantileForestplotDownload", "quantileForestplot"),
                             p(" "),
                             p("Thanks to Keith Newman for the download handler: ",
                               a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file"))),
                           
                  ),
                  
                  tabPanel("Details",
                           value = 6,
                           
                           p("I am increasingly convinced that percentiles, quantiles, are neglected in our field and more informative than means and SDs."),
                           p(paste0("However, it is easy to assume that percentiles or quantiles you get from a dataset are more precise than they actually are.",
                                    "  This app gives you percentiles/quantiles for any given dataset you input WITH the confidence intervals around the ",
                                    " observed values.")),
                           p("If your dataset is not large, these intervals may be such that we overlapping CIs for say the 10th and the 5th percentiles."),
                           p(" "),
                           p(paste0("There are at least three ways to get CIs for quantiles: Nyblom's method, the 'exact' method and bootstrapping.",
                                    "The paper by Nyblom\n\n",
                                    "Nyblom, J. (1992). Note on interpolated order statistics. Statistics & Probability Letters, 14(2), 129–131."),
                             a("https://doi.org/10.1016/0167-7152(92)90076-H", href="https://doi.org/10.1016/0167-7152(92)90076-H"),
                             paste0("\nseems pretty clear that his method is generally better than the 'exact' method and much quicker to compute",
                                    "than the bootstrap so I have used his method here.",
                                    "\nI have used Michael Höhle's quantileCI package"),
                             a("https://github.com/mhoehle/quantileCI", href="https://github.com/mhoehle/quantileCI"),
                             "\n to get the CIs."),
                           p(" "),
                           p("To complicate things further, though generally ignorably, there are actually a number of ways to compute quantiles.  ",
                             "I have used method 8, the default in the R quantile() function.  Search for that if it worries you!"),
                           p("If it really, really worries you and you want another quantile method, ",
                             a("contact me", href="https://www.psyctc.org/psyctc/contact-me/"),
                             " and I can tweak this app to allow you to choose any of the other R methods for quantiles.",
                             "But make sure you can persuade me the differences matter!!")
                  ),
                  
                  tabPanel("General background", 
                           value = 7,
                           
                           p("App started 16.iv.24 by Chris Evans.",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 5.v.24."),
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
    req(input$inputType == "paste" && length(pastedDataNum()) > 10 ||
          (input$inputType != "paste" & nrow(selectedData()) > 10))
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
    req(input$dpSummary)
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
        mutate(across(min : SD, ~ round(.x, input$dpSummary))) %>%
        pivot_longer(cols = everything())
    }
  })
  
  # output$summary <- renderTable(dataSummary())
  output$summary <- DT::renderDataTable(
    DT::datatable({dataSummary()},
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
                  )
    )
  )
  
  ###
  ### get the quantiles into tibQuantiles()
  ###
  tibQuantiles <- reactive({
    req(input$inputType)
    finalData() %>%
      select(values) %>%
      pull() -> vecDat
    
    suppressMessages(getCIforQuantiles(vecDat = vecDat, #finalData(), 
                                       vecQuantiles = cleanQuantiles(), 
                                       method = "N", 
                                       ci = input$ci,
                                       type = 8))
  })
  
  tibQuantilesPrint <- reactive({
    tibQuantiles() %>%
      mutate(quantile = round(quantile, input$dpQuantiles),
             LCL = round(LCL, input$dpQuantiles),
             UCL = round(UCL, input$dpQuantiles)) 
  })
  
  # output$summary <- renderTable(dataSummary())
  output$tibQuantiles <- DT::renderDataTable(
    DT::datatable({tibQuantilesPrint()},
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
                  )
    )
  )
  
  ###
  ### can now plot ECDF
  ###
  ### inputs to modify ECDF plot
  output$title <- renderUI({
    textInput("title", "Put something here if you want a different title to the plot",
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
    numericInput("textSize", "You can change the plot text size from the default (24) here",
                 min = 1,
                 value = 24,
                 width ="100%")
  })
  
  output$annotationSize <- renderUI({
    numericInput("annotationSize", "The above size changes all but the annotation size, you change that here.",
                 min = 1,
                 value = 16,
                 width ="100%")
  })
  
  
  ### same (similar) to change the forest plot
  output$titleQuantiles <- renderUI({
    textInput("titleQuantiles", "Put something here if you want a different title to the plot",
              value = "",
              width ="100%")
  })
  
  output$subTitleQuantiles <- renderUI({
    textInput("subTitleQuantiles", "Put something here if you want a different subtitle to the plot",
              value = "",
              width ="100%")
  })
  
  output$xLabQuantiles <- renderUI({
    textInput("xLabQuantiles", "Put something here if you want to override the default x axis label",
              value = "",
              width ="100%")
  })
  
  output$yLabQuantiles <- renderUI({
    textInput("yLabQuantiles", "Put something here if you want to override the default y axis label",
              value = "",
              width ="100%")
  })
  
  ### plot download file variables
  output$fileWidthQuantiles <- renderUI({
    numericInput("fileWidthQuantiles", "You can change the plot file width (pixels) here",
                 min = 400,
                 max = 10000,
                 value = 2000,
                 width ="100%")
  })
  
  output$fileHeightQuantiles <- renderUI({
    numericInput("fileHeightQuantiles", "You can change the plot file height (pixels) here",
                 min = 400,
                 max = 10000,
                 value = 1600,
                 width ="100%")
  })
  
  output$textSizeQuantiles <- renderUI({
    numericInput("textSizeQuantiles", "You can change the plot text size from the default (24) here",
                 min = 1,
                 value = 24,
                 width ="100%")
  })
  
  
  cleanQuantiles <- reactive({
    req(input$quantiles)
    req(length(finalData()) > 0)
    vecQuantiles <- cleanInputQuantiles(input$quantiles)
  })
  
  ## flag to show we have cleanQuartiles, controls conditionalPanel for ECDF plot
  output$gotCleanQuantiles <- reactive({
    req(input$inputType)
    nrow(finalData()) > 10 && length(cleanQuantiles()) > 0
  })
  outputOptions(output, "gotCleanQuantiles", suspendWhenHidden = FALSE)
  
  output$showQuantiles <- renderUI({
    if (input$quantiles == "" || length(cleanQuantiles()) == 0) {
      tagList(
        p("Something wrong with your quantiles. There must be at least one usable value greater than 0 and less than 1.0")
      )
    }
  })
  
  ### now the ECDF/quantile plot
  
  makePlot <- function(data, vecQuantiles, ci) {
    # cat(file=stderr(), "data inside makePlot() is:", unlist(data), "\n")
    # cat(file=stderr(), "str(data) inside makePlot() is:", unlist(str(data)), "\n")
    # cat(file=stderr(), "vecQuantiles inside makePlot() is:", unlist(vecQuantiles), "\n")
    
    # cat(file=stderr(), "got here #1", "\n")
    ### get data to vector form
    data %>%
      pull() -> vecData
    
    # cat(file=stderr(), "got here #2\n")
    # cat(file=stderr(), "vecData inside makePlot() 2nd time is:", vecData, "\n")
    # 
    # cat(file=stderr(), "got here #3\n")
    
    suppressWarnings(suppressMessages(plotQuantileCIsfromDat2(vecDat = vecData, 
                                                              vecQuantiles = vecQuantiles, 
                                                              method = "N", 
                                                              type = 4,
                                                              ci = ci,
                                                              annotationSize = input$annotationSize))) -> p
    
    if (length(input$title) > 0) {
      p +
        ggtitle(input$title) -> p
    }
    if (length(input$xLab) > 0) {
      p +
        xlab(input$xLab) -> p
    }
    if (length(input$yLab) > 0) {
      p +
        ylab(input$yLab) -> p
    }
    if(length(input$textSize) > 0 && input$textSize != 48){
      p +
        theme(text = element_text(size = input$textSize)) -> p
    }
    
    return(p)
  }
  
  quantileCIplot <- reactive({
    makePlot(finalData(), cleanQuantiles(), input$ci)
  })
  
  output$quantileCIplot <- renderPlot({
    quantileCIplot()
  })
  
  downloadGGPlotButtonServer(
    id = "quantileCIplotDownload", # <= this should match the ID used in the UI module
    ggplotObject = quantileCIplot, # No parentheses here to pass *expression*
    width = input$fileWidth,
    height = input$fileHeight
  )
  
  ### finally the quantile forest plot
  
  makeForestPlot <- function(tibQuantiles){
    ggplot(data = tibQuantiles, 
           aes(x = prob, y = quantile)) +
      geom_point() +
      geom_smooth(se = FALSE) +
      geom_linerange(aes(ymin = LCL, ymax = UCL)) -> p
    
    if (nchar(input$titleQuantiles) > 0) {
      p +
        ggtitle(input$titleQuantiles,
                subtitle = paste0("n = ", nrow(finalData()),
                                  ", CI = ", input$ci)) -> p
    } else {
      p +
        ggtitle("Plot of quantiles and confidence intervals",
                subtitle = paste0("Curve is non-linear smoothed fit\n",
                                  "n = ", nrow(finalData()),
                                  ", CI = ", input$ci)) -> p
    }
    
    if (nchar(input$xLabQuantiles) > 0) {
      p +
        scale_x_continuous(input$xLabQuantiles,
                           breaks = seq(0, 1, .1),
                           limits = c(0, 1)) -> p
    } else {
      p +
        scale_x_continuous("Probability (i.e. quantile requested)",
                           breaks = seq(0, 1, .1),
                           limits = c(0, 1)) -> p
    }
    if (nchar(input$yLabQuantiles) > 0) {
      p +
        ylab(input$yLabQuantiles) -> p
    }
    if(length(input$textSizeQuantiles) > 0 && input$textSizeQuantiles != 48){
      p +
        theme(text = element_text(size = input$textSizeQuantiles)) -> p
    }
    
    return(p)
  }
  
  forestPlot <- reactive({
    # cat(file=stderr(), "finalData() inside quantileCIPlot() is:", unlist(finalData()), "\n")
    # cat(file=stderr(), "cleanQuantiles() inside quantileCIPlot() is:", unlist(cleanQuantiles()), "\n")
    makeForestPlot(tibQuantiles())
  })
  
  output$forestPlot <- renderPlot({
    forestPlot()
  })
  
  downloadGGPlotButtonServer(
    id = "quantileForestplotDownload", # <= this should match the ID used in the UI module
    ggplotObject = forestPlot, # No parentheses here to pass *expression*
    width = input$fileWidth,
    height = input$fileHeight
  )
}

shinyApp(ui, server)