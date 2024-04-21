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

### CORE-10
itemsCORE10N <- c(2, 3, 7, 10, 15, 16, 18, 23, 27, 28)
itemsCORE10 <- paste(itemStem, sprintf("%02.0f", itemsCORE10N), sep = "")

### CORE-6D
itemsCORE6DN <- c(1, 15, 16, 21, 33, 8)
itemsCORE6D <- paste(itemStem, sprintf("%02.0f", itemsCORE6DN), sep = "")

### CORE-GP
itemsCOREGPN <- c(2, 3, 4, 7, 8, 12, 18, 19, 21, 25, 27, 29, 31, 32)
itemsCOREGP <- paste(itemStem, sprintf("%02.0f", itemsCOREGPN), sep = "")

### CORE-SF/A
itemsCORESFAtotN <- c(2, 4, 28, 32, 33, 14, 19, 20, 6, 23, 25, 7, 27, 29, 17, 15, 31, 34)
itemsCORESFAtot <- paste(itemStem, sprintf("%02.0f", itemsCORESFAtotN), sep = "")

### this is actually the same as SF/B wb and the CORE-OM wb
itemsCORESFAwbN <- c(4, 14, 17, 31)
itemsCORESFAwb <- paste(itemStem, sprintf("%02.0f", itemsCORESFAwbN), sep = "")

itemsCORESFAprobN <- c(2, 28, 20, 23, 27, 15)
itemsCORESFAprob <- paste(itemStem, sprintf("%02.0f", itemsCORESFAprobN), sep = "")

itemsCORESFAfuncN <- c(32, 33, 19, 25, 7, 29)
itemsCORESFAfunc <- paste(itemStem, sprintf("%02.0f", itemsCORESFAfuncN), sep = "")

itemsCORESFAriskN <- c(6, 34)
itemsCORESFArisk <- paste(itemStem, sprintf("%02.0f", itemsCORESFAriskN), sep = "")
### check SF/A
# setdiff(c(itemsCORESFAfuncN, itemsCORESFAprobN, itemsCORESFAwbN, itemsCORESFAriskN), itemsCORESFAtotN)

### CORE-SF/B
### CORE-SF/A
itemsCORESFBtotN <- c(1, 18, 31, 5, 16, 8, 12, 10, 4, 11, 13, 17, 3, 14, 22, 21, 26, 30)
itemsCORESFBtot <- paste(itemStem, sprintf("%02.0f", itemsCORESFBtotN), sep = "")

### this is actually the same as SF/B wb and the CORE-OM wb
itemsCORESFBwbN <- c(4, 14, 17, 31)
itemsCORESFBwb <- paste(itemStem, sprintf("%02.0f", itemsCORESFBwbN), sep = "")

itemsCORESFBprobN <- c(18, 5, 8, 11, 13, 30)
itemsCORESFBprob <- paste(itemStem, sprintf("%02.0f", itemsCORESFBprobN), sep = "")

itemsCORESFBfuncN <- c(1, 12, 10, 3, 21, 26)
itemsCORESFBfunc <- paste(itemStem, sprintf("%02.0f", itemsCORESFBfuncN), sep = "")

itemsCORESFBriskN <- c(16, 22)
itemsCORESFBrisk <- paste(itemStem, sprintf("%02.0f", itemsCORESFBriskN), sep = "")
### check SF/b
# setdiff(c(itemsCORESFBfuncN, itemsCORESFBprobN, itemsCORESFBwbN, itemsCORESFBriskN), itemsCORESFBtotN)

getCORE6Dutil <- function(tibDat) {
  ### assumes that tibDat is a tibble with CORE-OM items named COREOM01:COREOM34
  tibDat %>% 
    ### get the CORE-6D items
    mutate(C6I01 = COREOM01,
           C6I15 = COREOM15,
           C6I16 = COREOM16,
           C6I21 = COREOM21,
           C6I33 = COREOM33,
           C6I08 = COREOM08, 
           ### recoding
           # IF (cof01=0) co6D01=0. 
           # IF (cof01=1) co6D01=1. 
           # IF (cof01=2) co6D01=1. 
           # IF (cof01=3) co6D01=2. 
           # IF (cof01=4) co6D01=2. 
           # IF (cof01=9) co6D01=9. 
           # IF (cof01<0) OR (cof01>4) co6D01=9. 
           C6I01 = case_match(C6I01,
                              0 ~ 0,
                              c(1, 2) ~ 1,
                              c(3, 4) ~ 2,
                              .default = NA),
           # IF (cos08=0) co6D08=0. 
           # IF (cos08=1) co6D08=1. 
           # IF (cos08=2) co6D08=1. 
           # IF (cos08=3) co6D08=2. 
           # IF (cos08=4) co6D08=2. 
           # IF (cos08=9) co6D08=9. 
           # IF (cos08<0) OR (cos08>4) co6D08=9. 
           C6I08 = case_match(C6I08,
                              0 ~ 0,
                              c(1, 2) ~ 1,
                              c(3, 4) ~ 2,
                              .default = NA),
           # IF (cos15=0) co6D15=0. 
           # IF (cos15=1) co6D15=1. 
           # IF (cos15=2) co6D15=1. 
           # IF (cos15=3) co6D15=2. 
           # IF (cos15=4) co6D15=2. 
           # IF (cos15=9) co6D15=9. 
           # IF (cos15<0) OR (cos15>4) co6D15=9. 
           C6I15 = case_match(C6I15,
                              0 ~ 0,
                              c(1, 2) ~ 1,
                              c(3, 4) ~ 2,
                              .default = NA),
           # IF (cor16=0) co6D16=0. 
           # IF (cor16=1) co6D16=1. 
           # IF (cor16=2) co6D16=1. 
           # IF (cor16=3) co6D16=2. 
           # IF (cor16=4) co6D16=2. 
           # IF (cor16=9) co6D16=9. 
           # IF (cor16<0) OR (cor16>4) co6D16=9. 
           C6I16 = case_match(C6I16,
                              0 ~ 0,
                              c(1, 2) ~ 1,
                              c(3, 4) ~ 2,
                              .default = NA),
           # IF (cof21=0) co6D21=0. 
           # IF (cof21=1) co6D21=0. 
           # IF (cof21=2) co6D21=1. 
           # IF (cof21=3) co6D21=1. 
           # IF (cof21=4) co6D21=2. 
           # IF (cof21=9) co6D21=9. 
           # IF (cof21<0) OR (cof21>4) co6D21=9. 
           C6I21 = case_match(C6I21,
                              c(0, 1) ~ 0,
                              c(2, 3) ~ 1,
                              4 ~ 2,
                              .default = NA),
           # IF (cof33=0) co6D33=0. 
           # IF (cof33=1) co6D33=1. 
           # IF (cof33=2) co6D33=1. 
           # IF (cof33=3) co6D33=2. 
           # IF (cof33=4) co6D33=2. 
           # IF (cof33=9) co6D33=9. 
           # IF (cof33<0) OR (cof33>4) co6D33=9. 
           C6I33 = case_match(C6I33,
                              0 ~ 0,
                              c(1, 2) ~ 1,
                              c(3, 4) ~ 2,
                              .default = NA)) %>%
    # select(all_of(itemsCORE6D), starts_with("C6")) %>%
    mutate(
      # COMPUTE CORE6Dsc=(co6D01+co6D15+co6D16+co6D21+co6D33).
      C6Dsc = C6I01 + C6I15 + C6I16 + C6I21 + C6I33,
      # Compute CORE6Dut=(CORE6Dsc+co6D08).
      C6Dut = C6Dsc + C6I08,
      C6Dut = case_when(
        # IF (CORE6Dsc=0) AND (co6D08=0) CORE6Dut=0.95.
        C6Dsc == 0 & C6I08 == 0 ~ .95,
        # IF (CORE6Dsc=1) AND (co6D08=0) CORE6Dut=0.94.
        C6Dsc == 1 & C6I08 == 0 ~ .94,
        # IF (CORE6Dsc=2) AND (co6D08=0) CORE6Dut=0.87.
        C6Dsc == 2 & C6I08 == 0 ~ .87,
        # IF (CORE6Dsc=3) AND (co6D08=0) CORE6Dut=0.80.
        C6Dsc == 3 & C6I08 == 0 ~ .80,
        # IF (CORE6Dsc=4) AND (co6D08=0) CORE6Dut=0.72.
        C6Dsc == 4 & C6I08 == 0 ~ .72,
        # IF (CORE6Dsc=5) AND (co6D08=0) CORE6Dut=0.64.
        C6Dsc == 5 & C6I08 == 0 ~ .64,
        # IF (CORE6Dsc=6) AND (co6D08=0) CORE6Dut=0.55.
        C6Dsc == 6 & C6I08 == 0 ~ .55,
        # IF (CORE6Dsc=7) AND (co6D08=0) CORE6Dut=0.47.
        C6Dsc == 7 & C6I08 == 0 ~ .47,
        # IF (CORE6Dsc=8) AND (co6D08=0) CORE6Dut=0.38.
        C6Dsc == 8 & C6I08 == 0 ~ .38,
        # IF (CORE6Dsc=9) AND (co6D08=0) CORE6Dut=0.30.
        C6Dsc == 9 & C6I08 == 0 ~ .3,
        # IF (CORE6Dsc=10) AND (co6D08=0) CORE6Dut=0.24.
        C6Dsc == 10 & C6I08 == 0 ~ .24,
        # IF (CORE6Dsc=0) AND (co6D08=1) CORE6Dut=0.92.
        C6Dsc == 0 & C6I08 == 1 ~ .92,
        # IF (CORE6Dsc=1) AND (co6D08=1) CORE6Dut=0.90.
        C6Dsc == 1 & C6I08 == 1 ~ .90,
        # IF (CORE6Dsc=2) AND (co6D08=1) CORE6Dut=0.84.
        C6Dsc == 2 & C6I08 == 1 ~ .84,
        # IF (CORE6Dsc=3) AND (co6D08=1) CORE6Dut=0.77.
        C6Dsc == 3 & C6I08 == 1 ~ .77,
        # IF (CORE6Dsc=4) AND (co6D08=1) CORE6Dut=0.69.
        C6Dsc == 4 & C6I08 == 1 ~ .69,
        # IF (CORE6Dsc=5) AND (co6D08=1) CORE6Dut=0.61.
        C6Dsc == 5 & C6I08 == 1 ~ .61,
        # IF (CORE6Dsc=6) AND (co6D08=1) CORE6Dut=0.52.
        C6Dsc == 6 & C6I08 == 1 ~ .52,
        # IF (CORE6Dsc=7) AND (co6D08=1) CORE6Dut=0.43.
        C6Dsc == 7 & C6I08 == 1 ~ .43,
        # IF (CORE6Dsc=8) AND (co6D08=1) CORE6Dut=0.35.
        C6Dsc == 8 & C6I08 == 1 ~ .35,
        # IF (CORE6Dsc=9) AND (co6D08=1) CORE6Dut=0.26.
        C6Dsc == 9 & C6I08 == 1 ~ .26,
        # IF (CORE6Dsc=10) AND (co6D08=1) CORE6Dut=0.20.
        C6Dsc == 10 & C6I08 == 1 ~ .2,
        # IF (CORE6Dsc=0) AND (co6D08=2) CORE6Dut=0.81.
        C6Dsc == 0 & C6I08 == 2 ~ .81,
        # IF (CORE6Dsc=1) AND (co6D08=2) CORE6Dut=0.80.
        C6Dsc == 1 & C6I08 == 2 ~ .8,
        # IF (CORE6Dsc=2) AND (co6D08=2) CORE6Dut=0.73.
        C6Dsc == 2 & C6I08 == 2 ~ .73,
        # IF (CORE6Dsc=3) AND (co6D08=2) CORE6Dut=0.66.
        C6Dsc == 3 & C6I08 == 2 ~ .66,
        # IF (CORE6Dsc=4) AND (co6D08=2) CORE6Dut=0.58.
        C6Dsc == 4 & C6I08 == 2 ~ .58,
        # IF (CORE6Dsc=5) AND (co6D08=2) CORE6Dut=0.50.
        C6Dsc == 5 & C6I08 == 2 ~ .5,
        # IF (CORE6Dsc=6) AND (co6D08=2) CORE6Dut=0.41.
        C6Dsc == 6 & C6I08 == 2 ~ .41,
        # IF (CORE6Dsc=7) AND (co6D08=2) CORE6Dut=0.32.
        C6Dsc == 7 & C6I08 == 2 ~ .32,
        # IF (CORE6Dsc=8) AND (co6D08=2) CORE6Dut=0.24.
        C6Dsc == 8 & C6I08 == 2 ~ .24,
        # IF (CORE6Dsc=9) AND (co6D08=2) CORE6Dut=0.16.
        C6Dsc == 9 & C6I08 == 2 ~ .16,
        # IF (CORE6Dsc=10) AND (co6D08=2) CORE6Dut=0.10.
        # EXECUTE.
        C6Dsc == 10 & C6I08 == 2 ~ .1)) 
}

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
      helpText("You can change this before or after data has been selected."),
      helpText("The data will take a few seconds to appear in the 'Computed scores' tab: be patient!!"),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Computed scores", 
                           value = 1,
                           # p(" "),
                           # p("What appears then is the data for any rows that had any non-missing data",
                           #   br(),
                           #   "Date and ID are converted to character data, scores are prorated by CORE rules."),
                           # p(" "),
                           uiOutput('text2'),
                           p(" "),
                           p(" "),
                           DTOutput("compData"),    
                  ),
                  
                  
                  tabPanel("Names of scores",
                           value = 2,
                           p("What appears in the new dataset is the data for any rows that had any non-missing data"),
                           p("Date and ID are converted to character data, scores are prorated by CORE rules and are as follows."),
                           p(paste0("As you have full CORE-OM data, generally you will only be interested its scores, perhaps including the CORE-6D utility score.  ",
                                    "However, as you may also have CORE-10 or CORE-SF data, or even perhaps CORE-GP data, for the same people ",
                                    "I have added the scores for the embedded items of the CORE-10, CORE-SF/A, CORE-SF/B and CORE-GP as well as the CORE-OM scores.")),
                           p(" "),
                           p(paste0("Personally, I do not recommend using the CORE-OM or CORE-SF domain scores unless you have a very specific research or clinical reason to do so.  ",
                                    "The well-being, problems and function scores are generally very highly correlated (hence the non-risk score), and the risk items",
                                    "are clinically, i.e. with individuals, best treated sensibly within the clinical relationship as 'flags' of risk issues ",
                                    "rather than as a conventional scale score.")),
                           p(" "),
                           includeHTML("Variable_List.Rhtml"),
                  ),
                  
                  
                  tabPanel("Background", 
                           value = 3,
                           p("App created 16.iv.24 by Chris Evans at this point specifically for Oiatillo Temirov for checking his data.",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 21.iv.24."),
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
                                                  propProrateMin = .1), input$dp),
             COREOM10 = round(getScoreFromItems(c_across(all_of(itemsCORE10)),
                                                propProrateMin = .1), input$dp),
             CORESFAtot = round(getScoreFromItems(c_across(all_of(itemsCORESFAtot)),
                                                  propProrateMin = .1), input$dp),
             CORESFAprob = round(getScoreFromItems(c_across(all_of(itemsCORESFAprob)),
                                                   propProrateMin = .1), input$dp),
             CORESFAfunc = round(getScoreFromItems(c_across(all_of(itemsCORESFAfunc)),
                                                   propProrateMin = .1), input$dp),
             CORESFArisk = round(getScoreFromItems(c_across(all_of(itemsCORESFArisk)),
                                                   propProrateMin = .1), input$dp),
             CORESFBtot = round(getScoreFromItems(c_across(all_of(itemsCORESFBtot)),
                                                  propProrateMin = .1), input$dp),
             CORESFBprob = round(getScoreFromItems(c_across(all_of(itemsCORESFBprob)),
                                                   propProrateMin = .1), input$dp),
             CORESFBfunc = round(getScoreFromItems(c_across(all_of(itemsCORESFBfunc)),
                                                   propProrateMin = .1), input$dp),
             CORESFBrisk = round(getScoreFromItems(c_across(all_of(itemsCORESFBrisk)),
                                                   propProrateMin = .1), input$dp),
             COREOMGP = round(getScoreFromItems(c_across(all_of(itemsCOREGP)),
                                                propProrateMin = .1), input$dp),
             COREOM6Draw = round(getScoreFromItems(c_across(all_of(itemsCORE6D)),
                                                   propProrateMin = .1), input$dp)) %>%
      ungroup() -> dataInput1
    
    ### now add the CORE-6D utility score
    getCORE6Dutil(dataInput1) %>%
      select(-c(C6I01, C6I15, C6I16, C6I21, C6I33, C6I08, C6Dsc)) %>%
      select(Date, ID, 
             nMissing,	COREOMtot,	COREOMnonRisk,	COREOMwellB,	COREOMprob,	COREOMfunc,	COREOMrisk,
             CORESFAtot,	CORESFAprob,	CORESFAfunc,	CORESFArisk,
             CORESFBtot,	CORESFBprob,	CORESFBfunc,	CORESFBrisk,
             COREOMGP,
             COREOM6Draw,	C6Dut) -> dataInput2
    
    return(dataInput2)
  })
  
  fileStubName <- reactive({
    req(input$file1)
    fileExt <- tools::file_ext(input$file1$name) # get the full filename
    fileStubName <- str_replace(input$file1$name, fixed(fileExt), "") # strip the extension
    fileStubName <- str_replace(fileStubName, "(\\.)+$", "") # remove any terminal "."!
    fileStubName
  })
  
  comment1 <- reactive({
    req(fullData())
    ifelse(nrow(fullData()) > 0, 
           paste0("The buttons here allow you to export the entire dataset to the clipboard or to save in either CSV or Excel .xlsx format.\n",
                  "You can use any one two or all three buttons in any order!"),
           "")
  })
  
  output$compData <- DT::renderDataTable(server = FALSE,
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
  
  # Print the comment on the data 
  # output$text2 <- renderPrint(comment1())
  output$text2 <- renderUI(comment1())
  
}

# Create Shiny app ----
shinyApp(ui, server)