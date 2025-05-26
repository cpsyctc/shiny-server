### YP-CORE_2_scores
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(tidyverse))
suppressMessages(library(DT)) # for interactive tables
suppressMessages(library(janitor)) # for tabyl
suppressMessages(library(flextable)) # for non-interactive tables
suppressMessages(library(CECPfuns)) # for some utility functions

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

vecScoring <- c("Item mean (range 0-4)", 
                "Clinical (10x item mean, range 0-40)")
vecLookup <- c("Blackshaw PhD (UK & Ireland)", 
               "Twigg et al., 2016 (UK)", 
               "Di Biase et al., 2021 (Italy)")

### create the internal lookup table
tribble(~Ref, ~Age,  ~Gender,  ~CSC,
        "Blackshaw PhD (UK & Ireland)", 11, "F", 1.432,
        "Blackshaw PhD (UK & Ireland)", 12, "F", 1.337,
        "Blackshaw PhD (UK & Ireland)", 13, "F", 1.484,
        "Blackshaw PhD (UK & Ireland)", 14, "F", 1.562,
        "Blackshaw PhD (UK & Ireland)", 15, "F", 1.784,
        "Blackshaw PhD (UK & Ireland)", 16, "F", 1.909,
        "Blackshaw PhD (UK & Ireland)", 17, "F", 1.664,
        "Blackshaw PhD (UK & Ireland)", 18, "F", 1.909,
        "Blackshaw PhD (UK & Ireland)", 11, "M", 1.252,
        "Blackshaw PhD (UK & Ireland)", 12, "M", 1.104,
        "Blackshaw PhD (UK & Ireland)", 13, "M", 1.211,
        "Blackshaw PhD (UK & Ireland)", 14, "M", 1.301,
        "Blackshaw PhD (UK & Ireland)", 15, "M", 1.299,
        "Blackshaw PhD (UK & Ireland)", 16, "M", 1.487,
        "Blackshaw PhD (UK & Ireland)", 17, "M", 1.523 ,
        "Blackshaw PhD (UK & Ireland)", 18, "M", 1.523,
        "Twigg et al., 2016 (UK)", 11, "M", 1.03,
        "Twigg et al., 2016 (UK)", 12, "M", 1.03,
        "Twigg et al., 2016 (UK)", 13, "M", 1.03,
        "Twigg et al., 2016 (UK)", 14, "M", 1.41,
        "Twigg et al., 2016 (UK)", 15, "M", 1.41,
        "Twigg et al., 2016 (UK)", 16, "M", 1.41,
        "Twigg et al., 2016 (UK)", 11, "F", 1.44,
        "Twigg et al., 2016 (UK)", 12, "F", 1.44,
        "Twigg et al., 2016 (UK)", 13, "F", 1.44,
        "Twigg et al., 2016 (UK)", 14, "F", 1.59,
        "Twigg et al., 2016 (UK)", 15, "F", 1.59,
        "Twigg et al., 2016 (UK)", 16, "F", 1.59,
        "Di Biase et al., 2021 (Italy)", 11, "F", 1.34,
        "Di Biase et al., 2021 (Italy)", 12, "F", 1.34,
        "Di Biase et al., 2021 (Italy)", 13, "F", 1.34,
        "Di Biase et al., 2021 (Italy)", 14, "F", 1.34,
        "Di Biase et al., 2021 (Italy)", 15, "F", 1.47,
        "Di Biase et al., 2021 (Italy)", 16, "F", 1.47,
        "Di Biase et al., 2021 (Italy)", 17, "F", 1.47,
        "Di Biase et al., 2021 (Italy)", 11, "M", 1.18,
        "Di Biase et al., 2021 (Italy)", 12, "M", 1.18,
        "Di Biase et al., 2021 (Italy)", 13, "M", 1.18,
        "Di Biase et al., 2021 (Italy)", 14, "M", 1.18,
        "Di Biase et al., 2021 (Italy)", 15, "M", 1.23,
        "Di Biase et al., 2021 (Italy)", 16, "M", 1.18,
        "Di Biase et al., 2021 (Italy)", 17, "M", 1.18) -> tibLookup


### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "CORE-OM_scoring",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite")))


### set up scoring
###############
### CORE-OM ###
###############

itemStem <- "COREOM"

### to read data efficiently
itemsCOREOM <- paste(itemStem, sprintf("%02.0f", 1:34), sep = "")

# Define UI for data upload app ----
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("App to use dataset of two YP-CORE scores each from same person #1")),
  
  p("This app uses data from the YP-CORE that has been put into the spreadsheet I created for this purpose",
    "to collect CORE-OM data."),
  p("You input data using the sidebar on the left to select your file you want to analyse",
    "Then the data will appear in the Data tab to the right with the analyses.",
    "Do allow time for the uploading and the analyses, if you have, say 2,500 rows of data it will definitely take a fair few seconds for the data."),
  p("You can find example files with some artificial data: "),
  p(a("Libre/OpenOffice format (.ods)",
      href="https://www.coresystemtrust.org.uk/wp-content/uploads/2025/05/YP-CORE1.ods")),
  p("or:"),
  p(a("Excel format (.xlsx)",
      href="https://www.coresystemtrust.org.uk/wp-content/uploads/2025/05/YP-CORE1.xlsx")),
  p("You can download either file to your own machine to test this app and you can empty the data",
    "from the file and rename it and start putting your own data in that emptied file to use here."),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose file to upload"),
      
      radioButtons("Lookup",
                   "Which lookup data do you want to use for the CSC?",
                   vecLookup),
      radioButtons("Scoring",
                   "Which scoring do you want to use (data can be input in either or mix)?",
                   vecScoring),
      numericInput("dp",
                   "Number of decimal places for the scores",
                   value = 2,
                   min = 0,
                   max = 5,
                   width = "100%"),
      helpText("You can change this before or after data has been selected."),
      helpText("The data will take a few seconds to appear in the 'Computed scores' tab: be patient!!"),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Data", 
                           value = 1,
                           # uiOutput('text2'),
                           p(" "),
                           p(" "),
                           DTOutput("compData"),    
                  ),
                  
                  tabPanel("Summary statistics",
                           value = 2,
                           p("This is pretty indigestible but it gives all the summary statistics for all the variables across all clinicians."),
                           p(" "),
                           tableOutput('summaryStats1'),
                  ),    
                  
                  tabPanel("Summary statistics by clinician",
                           value = 3,
                           p("This breaks down the summary statistics by clinician if you have more than one."),
                           p(" "),
                           tableOutput('summaryStats1ByTher'),
                  ),   
                  
                  tabPanel("CSCtable1",
                           value = 4,
                           p("Here is the breakdown of the initial CSC categories"),
                           p(" "),
                           uiOutput('CSCtable1'),
                           p(" ") ,
                           p("And now the breakdown of the 2nd/final CSC categories"),
                           p(" "),
                           uiOutput('CSCtable2'),
                           p(" ") ,
                           p("This is the crosstabulation of the firtst and 2nd/final categories"),
                           p(" "),
                           uiOutput('CSCtable3'),
                           p(" ") ,
                           p("And this is the breakdown of the CSC classification changes"),
                           p(" "),
                           uiOutput('CSCtable4'),
                  ),    
                  
                  tabPanel("Plot1",
                           value = 5,
                           p("Probably need more here!"),
                           p(" "),
                           plotOutput('plot1'),
                  ),    
                  
                  tabPanel("Explanation of analyses",
                           value = 6,
                           p("What appears in the new dataset is the data for any rows that had any non-missing data"),
                           p("Date and ID are converted to character data, scores are prorated by CORE rules and are as follows."),
                           p("Probably need more here!"),
                           p(" "),
                           includeHTML("Variable_List.Rhtml"),
                  ),
                  
                  
                  tabPanel("Background", 
                           value = 7,
                           p("App created 22.v.25 by Chris Evans at this point specifically for Oiatillo Temirov for checking his data.",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 24.v.25."),
                           p("Licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           includeHTML("https://shiny.psyctc.org/boilerplate.html")),
                  
                  id = "tabSelected"),
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
  
  checkScore <- function(YPscore, Scoring) {
    if(YPscore < 0) {
      return(FALSE)
    }
    if(Scoring == "Clinical (10x item mean, range 0-40)") {
      if (YPscore > 40) {
        return(FALSE)
      } 
    } else {
      if (YPscore > 4) {
        return(FALSE)
      }
    }
    TRUE
  }
  
  fileSelected <- reactive({
    req(input$file1)
    input$file1$datapath
  })
  
  tibLookup2 <- reactive({
    tibLookup %>%
      filter(Ref == input$Lookup)
  })

  fullData <- reactive({
    suppressMessages(read.csv(file = fileSelected())) -> dataInput
    dataInput %>%
      as_tibble() %>%
      ### cleaning 
      ### sort out of range values
      mutate(YPmean1 = if_else(YPmean1 < 0 | YPmean1 > 4,
                               NA_real_,
                               YPmean1),
             YPmean2 = if_else(YPmean2 < 0 | YPmean2 > 4,
                               NA_real_,
                               YPmean2),
             YPclin1 = if_else(YPclin1 < 0 | YPclin1 > 40,
                               NA_real_,
                               YPclin1),
             YPclin2 = if_else(YPclin2 < 0 | YPclin2 > 40,
                               NA_real_,
                               YPclin2)) %>%
      ### get the corresponding scorings where we can
      mutate(YPmean1 = if_else(is.na(YPmean1) & !is.na(YPclin1),
                               YPclin1 / 10,
                               YPmean1),
             YPmean2 = if_else(is.na(YPmean2) & !is.na(YPclin2),
                               YPclin2 / 10,
                               YPmean2),
             YPclin1 = if_else(is.na(YPclin1) & !is.na(YPmean1),
                               YPmean1 * 10,
                               YPclin1),
             YPclin2 = if_else(is.na(YPclin2) & !is.na(YPmean2),
                               YPmean2 * 10,
                               YPclin2)) %>%
      ### simplify gender
      mutate(Gender = str_to_upper(str_sub(Gender, 1, 1)),
             Gender = if_else(is.na(Gender),
                              "",
                              Gender)) %>%
      ### deal with out of range ages
      mutate(Age = if_else(Age < 11 | Age > 25,
                           NA_integer_,
                           Age)) -> dataInput
    
    if (input$Scoring == "Item mean (range 0-4)") {
      dataInput %>%
        select(-starts_with("YPclin")) %>%
        rename(YPscore1 = YPmean1,
               YPscore2 = YPmean2) -> dataInput
    } else {
      dataInput %>%
        select(-starts_with("YPmean")) %>%
        rename(YPscore1 = YPclin1,
               YPscore2 = YPclin2) -> dataInput
    }
    
    ### get CSC values per row
    dataInput %>%
      left_join(tibLookup2(),
                by = c("Gender", "Age")) %>%
      mutate(Change = YPscore2 - YPscore1,
             YPscore1toCSC = YPscore1 - CSC,
             YPscore2toCSC = YPscore2 - CSC,
             CSCcat1 = if_else(YPscore1 > CSC,
                            "High",
                            "Low"),
             CSCcat2 = if_else(YPscore2 > CSC,
                            "High",
                            "Low"),
             CSCchange = case_when(
               CSCcat1 == "High" & CSCcat2 == "High" ~ "Stayed high",
               CSCcat1 == "High" & CSCcat2 == "Low" ~ "High to low",
               CSCcat1 == "Low" & CSCcat2 == "High" ~ "Low to high",
               CSCcat1 == "Low" & CSCcat2 == "Low" ~ "Stayed low"))
  })
  
  
  ### get the data in long format
  longDat <- reactive({
    fullData() %>%
      pivot_longer(cols = starts_with("YPscore"), names_to = "WhichScore", values_to = "Score") 
    
  })
  
  displayData <- reactive({
    fullData() %>%
      mutate(YPscore1 = round(YPscore1, input$dp),
             YPscore2 = round(YPscore2, input$dp))
  })
  
  summaryStats1 <- reactive({
    
    print(nrow(longDat()))
    
    fullData() %>%
      summarise(totalRecords = n(),
                nClients = n_distinct(RespondentID),
                nClinicians = n_distinct(TherapistID),
                # firstDate = min(Start_date, na.rm = TRUE),
                # lastDate = max(End_date, na.rm = TRUE),
                nFemale = sum(Gender == "F", na.rm = TRUE),
                nMale = sum(Gender == "M", na.rm = TRUE),
                nOther = sum(Gender == "O", na.rm = TRUE),
                nNoGender = getNNA(Gender),
                minAge = min(Age, na.rm = TRUE),
                maxAge = max(Age, na.rm = TRUE),
                meanAge = mean(Age, na.rm = TRUE),
                sdAge = sd(Age, na.rm = TRUE),
                nMissingAge = getNNA(Age),
                nAge11 = sum(Age == 11, na.rm = TRUE),
                nAge12 = sum(Age == 12, na.rm = TRUE),
                nAge13 = sum(Age == 13, na.rm = TRUE),
                nAge14 = sum(Age == 14, na.rm = TRUE),
                nAge15 = sum(Age == 15, na.rm = TRUE),
                nAge16 = sum(Age == 16, na.rm = TRUE),
                nAge17 = sum(Age == 17, na.rm = TRUE),
                nAge18 = sum(Age == 18, na.rm = TRUE),
                ### sessions
                minNsessionsAtt = min(nSessionsAttended, na.rm = TRUE),
                maxNsessionsAtt = max(nSessionsAttended, na.rm = TRUE),
                meanNsessionsAtt = mean(nSessionsAttended, na.rm = TRUE),
                sdNsessionsAtt = sd(nSessionsAttended, na.rm = TRUE),
                minNsessionsDNA = min(nSessionsDNAed, na.rm = TRUE),
                maxNsessionsDNA = max(nSessionsDNAed, na.rm = TRUE),
                meanNsessionsDNA = mean(nSessionsDNAed, na.rm = TRUE),
                sdNsessionsDNA = sd(nSessionsDNAed, na.rm = TRUE),
                minNsessionsCanc = min(nSessionsCancelled, na.rm = TRUE),
                maxNsessionsCanc = max(nSessionsCancelled, na.rm = TRUE),
                meanNsessionsCanc = mean(nSessionsCancelled, na.rm = TRUE),
                sdNsessionsCanc = sd(nSessionsCancelled, na.rm = TRUE),
                minNsessionsLate = min(nSessionsLate, na.rm = TRUE),
                maxNsessionsLate = max(nSessionsLate, na.rm = TRUE),
                meanNsessionsLate = mean(nSessionsLate, na.rm = TRUE),
                sdNsessionsLate = sd(nSessionsLate, na.rm = TRUE),
                ### scores
                nYP1valid = getNOK(YPscore1),
                nYP2valid = getNOK(YPscore2),
                minYP1 = min(YPscore1, na.rm = TRUE),
                meanYP1 = mean(YPscore1, na.rm = TRUE),
                medianYP1 = median(YPscore1, na.rm = TRUE),
                maxYP1 = max(YPscore1, na.rm = TRUE),
                sdYP1 = sd(YPscore1, na.rm = TRUE),
                minYP2 = min(YPscore2, na.rm = TRUE),
                meanYP2 = mean(YPscore2, na.rm = TRUE),
                medianYP2 = median(YPscore2, na.rm = TRUE),
                maxYP2 = max(YPscore2, na.rm = TRUE),
                sdYP2 = sd(YPscore2, na.rm = TRUE),
                minChange = min(Change, na.rm = TRUE),
                meanChange = mean(Change, na.rm = TRUE),
                medianChange = median(Change, na.rm = TRUE),
                maxChange = max(Change, na.rm = TRUE),
                sdChange = sd(Change, na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "Statistic")
  })
  
  summaryStats1ByTher <- reactive({
    fullData() %>%
      group_by(TherapistID) %>%
      summarise(totalRecords = n(),
                nClients = n_distinct(RespondentID),
                nClinicians = n_distinct(TherapistID),
                # firstDate = min(Start_date, na.rm = TRUE),
                # lastDate = max(End_date, na.rm = TRUE),
                nFemale = sum(Gender == "F", na.rm = TRUE),
                nMale = sum(Gender == "M", na.rm = TRUE),
                nOther = sum(Gender == "O", na.rm = TRUE),
                nNoGender = getNNA(Gender),
                minAge = min(Age, na.rm = TRUE),
                maxAge = max(Age, na.rm = TRUE),
                meanAge = mean(Age, na.rm = TRUE),
                sdAge = sd(Age, na.rm = TRUE),
                nMissingAge = getNNA(Age),
                nAge11 = sum(Age == 11, na.rm = TRUE),
                nAge12 = sum(Age == 12, na.rm = TRUE),
                nAge13 = sum(Age == 13, na.rm = TRUE),
                nAge14 = sum(Age == 14, na.rm = TRUE),
                nAge15 = sum(Age == 15, na.rm = TRUE),
                nAge16 = sum(Age == 16, na.rm = TRUE),
                nAge17 = sum(Age == 17, na.rm = TRUE),
                nAge18 = sum(Age == 18, na.rm = TRUE),
                ### sessions
                minNsessionsAtt = min(nSessionsAttended, na.rm = TRUE),
                maxNsessionsAtt = max(nSessionsAttended, na.rm = TRUE),
                meanNsessionsAtt = mean(nSessionsAttended, na.rm = TRUE),
                sdNsessionsAtt = sd(nSessionsAttended, na.rm = TRUE),
                minNsessionsDNA = min(nSessionsDNAed, na.rm = TRUE),
                maxNsessionsDNA = max(nSessionsDNAed, na.rm = TRUE),
                meanNsessionsDNA = mean(nSessionsDNAed, na.rm = TRUE),
                sdNsessionsDNA = sd(nSessionsDNAed, na.rm = TRUE),
                minNsessionsCanc = min(nSessionsCancelled, na.rm = TRUE),
                maxNsessionsCanc = max(nSessionsCancelled, na.rm = TRUE),
                meanNsessionsCanc = mean(nSessionsCancelled, na.rm = TRUE),
                sdNsessionsCanc = sd(nSessionsCancelled, na.rm = TRUE),
                minNsessionsLate = min(nSessionsLate, na.rm = TRUE),
                maxNsessionsLate = max(nSessionsLate, na.rm = TRUE),
                meanNsessionsLate = mean(nSessionsLate, na.rm = TRUE),
                sdNsessionsLate = sd(nSessionsLate, na.rm = TRUE),
                ### scores
                nYP1valid = getNOK(YPscore1),
                nYP2valid = getNOK(YPscore2),
                minYP1 = min(YPscore1, na.rm = TRUE),
                meanYP1 = mean(YPscore1, na.rm = TRUE),
                medianYP1 = median(YPscore1, na.rm = TRUE),
                maxYP1 = max(YPscore1, na.rm = TRUE),
                sdYP1 = sd(YPscore1, na.rm = TRUE),
                minYP2 = min(YPscore2, na.rm = TRUE),
                meanYP2 = mean(YPscore2, na.rm = TRUE),
                medianYP2 = median(YPscore2, na.rm = TRUE),
                maxYP2 = max(YPscore2, na.rm = TRUE),
                sdYP2 = sd(YPscore2, na.rm = TRUE),
                minChange = min(Change, na.rm = TRUE),
                meanChange = mean(Change, na.rm = TRUE),
                medianChange = median(Change, na.rm = TRUE),
                maxChange = max(Change, na.rm = TRUE),
                sdChange = sd(Change, na.rm = TRUE)) %>%
      pivot_longer(cols = -TherapistID, names_to = "Statistic") %>%
      pivot_wider(id_cols = "Statistic", names_from = TherapistID, values_from = "value")
  })
  
  fileStubName <- reactive({
    req(input$file1)
    fileExt <- tools::file_ext(input$file1$name) # get the full filename
    fileStubName <- str_replace(input$file1$name, fixed(fileExt), "") # strip the extension
    fileStubName <- str_replace(fileStubName, "(\\.)+$", "") # remove any terminal "."!
    fileStubName
  })
  
  # comment1 <- reactive({
  #   req(fullData())
  #   ifelse(nrow(fullData()) > 0, 
  #          paste0("The buttons here allow you to export the entire dataset to the clipboard or to save in either CSV or Excel .xlsx format.\n",
  #                 "You can use any one two or all three buttons in any order!"),
  #          "")
  # })
  
  output$compData <- DT::renderDataTable(server = FALSE,
                                         DT::datatable({displayData()},
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

  output$CSCtable1 <- renderUI({
    dataInput %>%
      tabyl(CSCcat1) %>%
      mutate(percent = str_c(round(100 * percent, 1), "%"),
             valid_percent = str_c(round(100 * valid_percent, 1), "%")) %>%
      flextable() %>%
      colformat_char(j = 1,
                     na_str = "Missing") %>%
      htmltools_value()
    })
  
  output$CSCtable2 <- renderUI({
    dataInput %>%
      tabyl(CSCcat2) %>%
      mutate(percent = str_c(round(100 * percent, 1), "%"),
             valid_percent = str_c(round(100 * valid_percent, 1), "%")) %>%
      flextable() %>%
      colformat_char(j = 1,
                     na_str = "Missing") %>%
      htmltools_value()
  })
  
  output$CSCtable3 <- renderUI({
    dataInput %>%
      tabyl(CSCcat1, CSCcat2) %>%
      # mutate(percent = str_c(round(100 * percent, 1), "%"),
      #        valid_percent = str_c(round(100 * valid_percent, 1), "%")) %>%
      rename(`Missing 2nd value` = "NA_",
             `1st value` = CSCcat1) %>%
      flextable() %>%
      colformat_char(j = 1,
                     na_str = "Missing 1st value") %>%
      autofit() %>%
      htmltools_value()
  })
  
  output$CSCtable4 <- renderUI({
    dataInput %>%
      tabyl(CSCchange) %>%
      mutate(percent = str_c(round(100 * percent, 1), "%"),
             valid_percent = str_c(round(100 * valid_percent, 1), "%")) %>%
      flextable() %>%
      colformat_char(j = 1,
                     na_str = "Missing") %>%
      htmltools_value()
  })
  
  output$summaryStats1 <- renderTable(summaryStats1())
  
  output$summaryStats1ByTher <- renderTable(summaryStats1ByTher())
  
  output$plot1 <- renderPlot({
    
    print(longDat() %>% count(is.na(Score)))
    print(longDat() %>% count(WhichScore))
    
    longDat() %>%
      filter(!is.na(Score)) %>%
      mutate(WhichScore = str_remove(WhichScore, "YPscore"),
             WhichScore = as.numeric(WhichScore)) -> tmpTib
    
    tmpNudge <- .03
    
    tmpTib %>%
      group_by(WhichScore) %>%
      summarise(mean = list(getBootCImean(Score,
                                          nLT20err = FALSE,
                                          verbose = FALSE))) %>%
      unnest_wider(mean) %>%
      ungroup() %>%
      mutate(WhichScore = if_else(WhichScore == 1,
                                  WhichScore - tmpNudge,
                                  WhichScore + tmpNudge)) -> tmpTibMeans

    ggplot(data = tmpTib,
           aes(x = WhichScore, y = Score, group = RespondentID, colour = Gender)) +
      geom_point() #+
      # geom_line() +
      # geom_point(data = tmpTibMeans,
      #            inherit.aes = FALSE,
      #            aes(x = WhichScore, y = obsmean),
      #            size = 2) +
      # geom_hline(yintercept = tmpTibMeans$obsmean[1],
      #            linetype = 2) +
      # geom_line(data = tmpTibMeans,
      #           inherit.aes = FALSE,
      #           aes(x = WhichScore, y = obsmean),
      #           linewidth = 2,
      #           linetype = 1) +
      # geom_linerange(data = tmpTibMeans,
      #                inherit.aes = FALSE,
      #                aes(x = WhichScore, 
      #                    ymin = LCLmean, ymax = UCLmean)) +
      # ylab("Score") +
      # scale_x_continuous("Occasion",
      #                    breaks = 1:2) 
  })
  
  # Print the comment on the data 
  # output$text2 <- renderPrint(comment1())
  # output$text2 <- renderUI(comment1())
  
}

# Create Shiny app ----
shinyApp(ui, server)