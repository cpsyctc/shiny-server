### YP-CORE_2_scores
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(tidyverse))
suppressMessages(library(DT)) # for interactive tables
suppressMessages(library(janitor)) # for tabyl
suppressMessages(library(flextable)) # for non-interactive tables
suppressMessages(library(CECPfuns)) # for some utility functions
suppressMessages(library(plotly)) # for interactivity in the graphics

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

set_flextable_defaults(table_align = "left")

vecScoring <- c("Item mean (range 0-4)", 
                "Clinical (10x item mean, range 0-40)")
vecLookup <- c("Blackshaw PhD (UK & Ireland)", 
               "Twigg et al., 2016 (UK)", 
               "Di Biase et al., 2021 (Italy)")

### create the internal lookup table
tribble(~Ref, ~Age,  ~Gender,  ~CSC, ~RCI, ~refAlpha, ~refSD,
        "Blackshaw PhD (UK & Ireland)", 11, "F", 1.432, .892, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 12, "F", 1.337, .911, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 13, "F", 1.484, .884, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 14, "F", 1.562, .883, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 15, "F", 1.784, .882, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 16, "F", 1.909, .860, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 17, "F", 1.664, .909, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 18, "F", 1.909, .947, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 11, "M", 1.252, .969, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 12, "M", 1.104, .929, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 13, "M", 1.211, .928, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 14, "M", 1.301, .909, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 15, "M", 1.299, .897, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 16, "M", 1.487, .907, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 17, "M", 1.523, .910, NA, NA,
        "Blackshaw PhD (UK & Ireland)", 18, "M", 1.523, 1.156, NA, NA,
        "Twigg et al., 2016 (UK)", 11, "M", 1.03, .83, .71, .69,
        "Twigg et al., 2016 (UK)", 12, "M", 1.03, .83, .71, .68,
        "Twigg et al., 2016 (UK)", 13, "M", 1.03, .83, .71, .64,
        "Twigg et al., 2016 (UK)", 14, "M", 1.41, .80, .74, .67,
        "Twigg et al., 2016 (UK)", 15, "M", 1.41, .80, .74, .71,
        "Twigg et al., 2016 (UK)", 16, "M", 1.41, .80, .74, .61,
        "Twigg et al., 2016 (UK)", 11, "F", 1.44, .80, .79, .74,
        "Twigg et al., 2016 (UK)", 12, "F", 1.44, .80, .79, .69,
        "Twigg et al., 2016 (UK)", 13, "F", 1.44, .80, .79, .79,
        "Twigg et al., 2016 (UK)", 14, "F", 1.59, .74, .81, .74,
        "Twigg et al., 2016 (UK)", 15, "F", 1.59, .74, .81, .72,
        "Twigg et al., 2016 (UK)", 16, "F", 1.59, .74, .81, .73,
        "Di Biase et al., 2021 (Italy)", 11, "F", 1.34, .82, .80, .837,
        "Di Biase et al., 2021 (Italy)", 12, "F", 1.34, .82, .80, .837,
        "Di Biase et al., 2021 (Italy)", 13, "F", 1.34, .82, .80, .837,
        "Di Biase et al., 2021 (Italy)", 14, "F", 1.34, .82, .80, .837,
        "Di Biase et al., 2021 (Italy)", 15, "F", 1.47, .82, .83, .727,
        "Di Biase et al., 2021 (Italy)", 16, "F", 1.47, .82, .83, .727,
        "Di Biase et al., 2021 (Italy)", 17, "F", 1.47, .82, .83, .727,
        "Di Biase et al., 2021 (Italy)", 11, "M", 1.18, .82, .69, .626,
        "Di Biase et al., 2021 (Italy)", 12, "M", 1.18, .82, .69, .626,
        "Di Biase et al., 2021 (Italy)", 13, "M", 1.18, .82, .69, .626,
        "Di Biase et al., 2021 (Italy)", 14, "M", 1.18, .82, .69, .626,
        "Di Biase et al., 2021 (Italy)", 15, "M", 1.23, .82, .81, .727,
        "Di Biase et al., 2021 (Italy)", 16, "M", 1.18, .82, .81, .727,
        "Di Biase et al., 2021 (Italy)", 17, "M", 1.18, .82, .81, .727) %>%
  mutate(ordAge = ordered(Age,
                          levels = 11:18))-> tibLookup

### vector of column names
c("RespondentID", 
  "TherapistID", 
  "Gender", 
  "Age", 
  "YPmean1", 
  "YPmean2", 
  "YPclin1", 
  "YPclin2", 
  "Comment", 
  "Start_date", 
  "End_date", 
  "nSessionsAttended", 
  "nSessionsDNAed", 
  "nSessionsCancelled", 
  "nSessionsLate", 
  "nWeeks") -> vecColNames

### col_types of the input file, read_xlsx() style
vecColTypes <- c("RespondentID" = "text", 
                 "TherapistID" = "text",
                 "Gender" = "text",
                 "Age" = "numeric",
                 "YPmean1" = "numeric",
                 "YPmean2" = "numeric",
                 "YPclin1" = "numeric",
                 "YPclin2" = "numeric",
                 "Comment" = "text",
                 "Start_date" = "text",
                 "End_date" = "text",
                 "nSessionsAttended" = "numeric",
                 "nSessionsDNAed" = "numeric",
                 "nSessionsCancelled" = "numeric",
                 "nSessionsLate" = "numeric",
                 "nWeeks" = "numeric")

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

# Define UI for app ----
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("App to use dataset of two YP-CORE scores each from same person")),
  
  p("This app uses data from a spreadsheet which you upload below. This is then analysed with findings in the various tabs to the right of this."),
  
  # Main panel
  mainPanel(
    tabsetPanel(type = "tabs",
                
                tabPanel("Introduction",
                         value = 1,
                         h2("Introduction"),
                         p(" "),
                         p("This app takes YP-CORE data in rows each with one or two scores.  These will typically be one row per usually baseline and last session.  ",
                           "(An app that will take many rows of scores per client should follow soon.  ",
                           "The data can be uploaded in CSV (Comma Separated Variables) format, R's Rda format, Excel .xlsx or Libre/OpenOffice .ods formats.  ",
                           "The column names must be RespondentID, TherapistID, Gender, Age, YPmean1, YPmean2, YPclin1, YPclin2, Comment, Start_date, End_date, ",
                           "nSessionsAttended, nSessionsDNAed, nSessionsCancelled, nSessionsLate and nWeeks but you can have your own variables to right of those ",
                           "required variables.  You don't have to fill in all the variables nor, of course, do data have to be complete.  Naturally, the more data ",
                           "you have the more useful and informative the analyses will be."),
                         p(" "),
                         p("It is safest and easiest to start with one of thes example files containing artificial data: "),
                         tags$ul(
                           tags$li(a("CSV format (.csv)",
                                     href="https://www.coresystemtrust.org.uk/wp-content/uploads/2025/06/YPwide2.csv")),
                           tags$li(a("Libre/OpenOffice format (.ods)",
                                     href="https://www.coresystemtrust.org.uk/wp-content/uploads/2025/06/YPwide2.ods")),
                           tags$li(a("Excel format (.xlsx)",
                                     href="https://www.coresystemtrust.org.uk/wp-content/uploads/2025/06/YPwide2.xlsx"),
                                   " or "),
                           tags$li(a("R data format (.Rda)",
                                     href="https://www.coresystemtrust.org.uk/wp-content/uploads/2025/06/YPwide2.rda")),
                         ),
                         p("Download one to your own machine to explore this app with those data.  Then delete the data",
                           "rename the file and start putting your own data into it to use here."),
                         p(" "),
                         p("First upload your data in one of those formats, the analyses then appear in the tabs after this one."),
                         p(" "),
                         h2("This is work in progress!!"),
                         p("This is work in progress like everything else in this app.",
                           "Each tab has a todo list for what's to come for that tab and the explanation tab has a general todo list."),
                         
                         p(" "),
                         h2("Upload data"),
                         p("This is a paragraph!"),
                         # Input: Select a file ----
                         fileInput("file1", 
                                   "Choose file to upload",
                                   accept = c(".csv", ".rda", ".xlsx", ".ods")),
                         
                         helpText("Once you have uploaded your data, choose the appropriate referential values to use with it..."),
                         
                         radioButtons("Lookup",
                                      "Which lookup data do you want to use for the CSC?",
                                      vecLookup),
                         helpText("This shows the lookup table you selected"),
                         
                         uiOutput("lookupTable1"),
                         
                         helpText("Scores can be displayed using either 'clinical' or 'mean' scoring"),
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
                         helpText("If your spreadsheet is large the data may take a few seconds for the 'Data uploaded' message to appear above: be patient!!"),
                         helpText("When the data has uploaded you can explore it in the 'Data' tab to the right and the analyses and plots will be in the ",
                                  "other tabs to the right.")
               ),
                
                tabPanel("Data", 
                         value = 2,
                         h2("Explanation"),
                         p("This tables shows the data you uploaded plus the coding of that data."),
                         tags$ul(tags$li("At the right of  the table the coding gives you the CSC categories of the YP-CORE scores (if there were any.)"),
                                 tags$li("YPscore1toCSC is the difference between the first YP-SCORE and the CSC for that age and gender."),
                                 tags$li("YPscore2toCSC is the same for the second YP-SCORE.  These will get used in some plots later, see todo list!"),
                                 tags$li("The search box is a very simple inclusive search so if you put '62' in there the table will reduce to showing you ",
                                         "any rows of data that contain '62' anywhere including say a participant ID of 62 or a score of 1.62.  "),
                                 tags$li("The filter boxes at the top of each column allow you to search in that variable.  They will often show you a ",
                                         "a drop down list of the values that exist in the data."),
                                 tags$li("For the dates the search is of the stem you input ",
                                         "so if you input '2024-04' it should show you all dates in April 2024."),
                                 tags$li("The filtering is a Boolean AND which is to say that you will get to see the rows that fulfil all the constraints you input.")
                         ),
                         p(" "),
                         h2("This is work in progress!!"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add buttons to download entire table as a file"),
                           tags$li("And buttons to download selected rows from the interactive datatable"),
                           tags$li("Probably have to add RCI at some point!")
                         ),
                         p(" "),
                         h2("Searchable table of the data"),
                         p(" "),
                         DTOutput("compData"),    
                ),
                
                tabPanel("Summary statistics",
                         value = 3,
                         h2("Explanation"),
                         p("This is pretty indigestible but it gives all the summary statistics for all the variables across all clinicians.",
                           "I think the naming of the statistics is pretty self-explanatory if not particularly easy on the eye."),
                         p(" "),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Break this up into a series of tables with headings above each"),
                           tags$li("Rather laboriously (!) add percentages where they make sense"),
                           tags$li("Use flextable() to make it more digestible."),
                           tags$li("Add button to download entire data behind the table as a file?"),
                           tags$li("Probably have to add RCI at some point!")
                         ),
                         p(" "),
                         h2("The summary stats"),
                         p(" "),
                         htmlOutput("summaryStatsText1"),
                         p(" "),
                         tableOutput('summaryStats1'),
                ),    
                
                tabPanel("Summary statistics by clinician",
                         value = 4,
                         h2("Explanation"),
                         p("As with the 'Summary statistics' tab this is pretty indigestible and gives all the same summary statistics ",
                           "for all the variables across all clinicians.",
                           "I think the naming of the statistics is pretty self-explanatory if not particularly easy on the eye."),
                         p(" "),
                         h2("Todo list"),
                         p("This is work in progress. Until such time as this is no longer true (when?!), this is work in progress like everything else in this app.",
                           
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add button to download entire table as a file."),
                           tags$li("Think how it could be made more digestible."),
                           tags$li("Perhaps add percentages but they will only make sense for some of the statistics"),
                           tags$li("Probably have to add RCI at some point!"),
                           tags$li("Will it be useful to people to have more comparative analyses comparing clinicians?")
                         ),
                         p(" "),
                         h2("Summary stats by clinician"),
                         p(" "),
                         tableOutput('summaryStats1ByTher'),
                ),   
                
                tabPanel("Histogram of session count",
                         value = 5,
                         h2("Explanation"),
                         p("This tab gives a histogram of the numbers of sessions attended"),
                         p(" "),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Put statistics into the plot, top right"),
                           tags$li("Add options to break down by clinician and perhaps by age, gender ...?"),
                           tags$li("Add options to count or add n(DNA), n(Cancelled) and n(Late)?"),
                           tags$li("Will it be useful to people to have more comparative analyses comparing clinicians?")
                         ),
                         p(" "),
                         h2("The histogram"),
                         p(" "),
                         p("Here is the histogram of the numbers of sessions attended"),
                         p(" "),
                         plotOutput('histogram1'),
                         p(" ") ,
                         
                ),    
                
                
                tabPanel("CSCtable1",
                         value = 6,
                         h2("Explanation"),
                         p("This tab gives a simple breakdown of the CSC categories: baseline, final and change counts"),
                         p(" "),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Probably have to add RCI at some point!"),
                           tags$li("Add options to break down by age, gender ...?"),
                           tags$li("Will it be useful to people to have more comparative analyses comparing clinicians?")
                         ),
                         p(" "),
                         h2("The CSC tabulations"),
                         p(" "),
                         p("Here is the breakdown of the initial CSC categories"),
                         p(" "),
                         uiOutput('CSCtable1'),
                         p(" ") ,
                         p("And now the breakdown of the 2nd/final CSC categories"),
                         p(" "),
                         uiOutput('CSCtable2'),
                         p(" ") ,
                         p("This is the crosstabulation of the first and 2nd/final categories"),
                         p(" "),
                         uiOutput('CSCtable3'),
                         p(" ") ,
                         p("And this is the breakdown of the CSC classification changes"),
                         p(" "),
                         uiOutput('CSCtable4'),
                ),    
                
                tabPanel("Plot1",
                         value = 7,
                         h2("Explanation"),
                         p("This tab gives a cat's cradle plot of the data"),
                         p(" "),
                         p("A cat's cradle plot ignores any respondent with only one usable score and shows ",
                           "the change in scores as a line.  You can identify the points by hovering over them ",
                           "when you will see a 'tooltip'",
                           "giving you the YP-CORE score, the respondent ID, therpist ID and the category ",
                           "of the CSC change."),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add options to break down by therapist, age, gender ..."),
                           tags$li("Buttons to download the plot")
                         ),
                         p(" "),
                         h2("The cat's cradle plot"),
                         p(" "),
                         p("This is a so-called 'cat's cradle' plot.",
                           "It shows all the complete pairs of scores coloured by gender with connecting lines.",
                           "The dashed horizontal reference line marks the mean baseline score and the black points,",
                           "offset somewhat from the individual points, mark the mean baseline and later scores.  ",
                           "The vertical lines through those are the bootstrap 95% confidence intervals."),
                         p(" "),
                         plotlyOutput('plot1',
                                      height = "100%"),
                ),    
                
                tabPanel("Plot2",
                         value = 8,
                         h2("Explanation"),
                         p("This tab gives a cat's cradle plot against dates (if given)"),
                         p(" "),
                         p("This is very similar to the previous plot but the x axis is by date, not occasion.",
                           "This gives a better picture of the work over time and Again, you can identify the points by hovering over them ",
                           "when you will see a 'tooltip'",
                           "giving you the YP-CORE score, the respondent ID, therpist ID and the category ",
                           "of the CSC change."),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add options to break down by therapist, age, gender ..."),
                           tags$li("Buttons to download the plot")
                         ),
                         p(" "),
                         h2("The plot"),
                         p(" "),
                         p("This shows all the complete pairs of scores coloured by therapist ID with connecting lines ",
                           "plotted against episode start and finish dates (if given).  If you hover over a point you ",
                           "should be shown the respondent ID, gender and CSC change category."),
                         p(" "),
                         plotlyOutput('plot2',
                                      height = "100%"),
                ),  
                
                tabPanel("Explanation of the app",
                         value = 9,
                         h2("Explanation"),
                         p("This app is definitely work in progress at the moment."),
                         p(" "),
                         h2("Overall todo list"),
                         p("This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Going to need to work out my position on the RCI and then include that."),
                           tags$li("Then will need new tabs including Jacobson plot which will need to be rescaled using (score - CSC)/RCI ",
                                   "so that the differing CSCs and RCIs rescale things so that the CSC lines are on y = 0 and x = 0 and there ",
                                   "can be a single pair of RCI tramlines.")
                         ),
                         p(" "),
                ),
                
                
                tabPanel("Background", 
                         value = 10,
                         p("App created 22.v.25 by Chris Evans at this point specifically for Oiatillo Temirov for checking his data.",
                           a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                         p("Last updated 8.vi.25: added histogram of sessions attended and ditched the sidebar layout."),
                         p("Licenced under a ",
                           a("Creative Commons, Attribution Licence-ShareAlike",
                             href="http://creativecommons.org/licenses/by-sa/1.0/"),
                           " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                         includeHTML("https://shiny.psyctc.org/boilerplate.html")),
                
                id = "tabSelected"),
  )
)
#)


# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ### get session parameters
  lisSessionData <- session$clientData
  
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
  
  showLookup <- reactive({
    if(input$Scoring == "Clinical (10x item mean, range 0-40)") {
      tibLookup2() %>%
        mutate(CSC = 10 * CSC,
               RCI = 10 * RCI,
               refSD = 10 * refSD) -> tmpTibLookup
    } else {
      tibLookup2() -> tmpTibLookup
    }
    
    tmpTibLookup %>%
      select(-ordAge) %>%
      select(Ref, Age, Gender, everything()) %>%
      arrange(Age, Gender) %>%
      flextable() %>%
      autofit()
  })
  
  output$lookupTable1 <- renderUI({
    showLookup() %>%
      colformat_char(j = 1,
                     na_str = "Missing") %>%
      htmltools_value()
  })
  
  
  fullData <- reactive({
    ### work out file format 
    str_replace(fileSelected(), "^(.)*?(\\.)(.*$)", "\\3") %>%
      str_to_lower() -> fileType
    ### read in the data
    if(fileType == "csv") {
      suppressMessages(read.csv(file = fileSelected())) %>%
        as_tibble() -> dataInput
    }
    if(fileType == "rda") {
      suppressMessages(load(file = fileSelected()))
      tibYPwide2 -> dataInput
      rm(tibYPwide2)
    }
    if(fileType == "xlsx") {
      suppressMessages(readxl::read_xlsx(path = fileSelected(),
                                         col_types = vecColTypes)) -> dataInput
    }
    if(fileType == "ods") {
      ### col_types of the input file
      lisColTypes <- cols("RespondentID" = "c", 
                          "TherapistID" = "c",
                          "Gender" = "c",
                          "Age" = "i",
                          "YPmean1" = "d",
                          "YPmean2" = "d",
                          "YPclin1" = "d",
                          "YPclin2" = "d",
                          "Comment" = "c",
                          "Start_date" = "c",
                          "End_date" = "c",
                          "nSessionsAttended" = "i",
                          "nSessionsDNAed" = "i",
                          "nSessionsCancelled" = "i",
                          "nSessionsLate" = "i",
                          "nWeeks" = "i")
      
      suppressMessages(suppressWarnings(readODS::read_ods(path = fileSelected(),
                                                          col_types = lisColTypes))) -> dataInput
    }
    
    validate(need(colnames(dataInput) == vecColNames,
                  "Your data don't seem to have the correct column names.  Sorry, aborting!"))
    
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
                           Age)) %>%
      ### massage dates
      mutate(Start_date = as.Date(Start_date, format = "%Y-%m-%d"),
             End_date = as.Date(End_date, format = "%Y-%m-%d")) %>%
      ### massage variables we need as factors
      mutate(RespondentID = ordered(RespondentID),
             TherapistID = ordered(TherapistID),
             Gender = ordered(Gender)) -> dataInput
    
    if (input$Scoring == "Item mean (range 0-4)") {
      dataInput %>%
        mutate(YPscore1 = YPmean1,
               YPscore2 = YPmean2) -> dataInput
    } else {
      dataInput %>%
        mutate(YPscore1 = YPclin1,
               YPscore2 = YPclin2) -> dataInput
    }
    
    ### get CSC categories per row using matching age & gender
    dataInput %>%
      left_join(tibLookup2(),
                by = c("Gender", "Age")) %>%
      mutate(Change = YPscore2 - YPscore1,
             changeMeanScoring = YPmean2 - YPmean1,
             YPscore1toCSC = YPscore1 - CSC,
             YPscore2toCSC = YPscore2 - CSC,
             CSCcat1 = if_else(YPmean1 > CSC,
                               "High",
                               "Low"),
             CSCcat2 = if_else(YPmean2 > CSC,
                               "High",
                               "Low"),
             CSCchange = case_when(
               CSCcat1 == "High" & CSCcat2 == "High" ~ "Stayed high",
               CSCcat1 == "High" & CSCcat2 == "Low" ~ "High to low",
               CSCcat1 == "Low" & CSCcat2 == "High" ~ "Low to high",
               CSCcat1 == "Low" & CSCcat2 == "Low" ~ "Stayed low")) %>%
      
      ### get RC categories
      mutate(RelChange = case_when(
        changeMeanScoring >= RCI ~ "Reliable deterioration",
        abs(changeMeanScoring) < RCI ~ "No reliable change",
        changeMeanScoring <= RCI ~ "Reliable improvement")) %>%
      
      ### get RCSC categories
      mutate(RCSCcat = case_when(
        CSCchange == "Stayed high" & RelChange == "Reliable deterioration" ~ "Stayed high AND reliable deterioration",
        CSCchange == "Stayed high" & RelChange == "No reliable change" ~ "Stayed high, no reliable change",
        CSCchange == "High to low" & RelChange == "Reliable improvement" ~ "Reliable and clinically significant improvement",
        CSCchange == "High to low" & RelChange == "No reliable change" ~ "Clinically significant improvement but no reliable change",
        CSCchange == "Low to high" & RelChange == "Reliable deterioration" ~ "Clinically significant AND reliable deterioration",
        CSCchange == "Low to high" & RelChange == "No reliable change" ~ "Clinically significant deterioration but no reliable change",
        CSCchange == "Stayed low" & RelChange == "Reliable deterioration" ~ "Stayed low BUT reliable deterioration",
        CSCchange == "Stayed low" & RelChange == "No reliable change" ~ "Stayed low and no reliable change",
        CSCchange == "Stayed low" & RelChange == "Reliable improvement" ~ "Stayed low AND reliable improvement",
      ))
    
  })
  
  
  ### get the data in long format
  longDat <- reactive({
    fullData() %>%
      pivot_longer(cols = starts_with("YPscore"), names_to = "WhichScore", values_to = "Score") 
    
  })
  
  displayData1 <- reactive({
    fullData() %>%
      select(-c(Ref, CSC, RCI, refAlpha, refSD,
                YPmean1, YPmean2, YPclin1, YPclin2,
                changeMeanScoring)) %>%
      rename(nSessAtt = nSessionsAttended,
             nSessDNA = nSessionsDNAed,
             nSessCanc = nSessionsCancelled,
             nSessLate = nSessionsLate) %>%
      mutate(YPscore1 = round(YPscore1, input$dp),
             YPscore2 = round(YPscore2, input$dp),
             Change = round(Change, input$dp),
             YPscore1toCSC = round(YPscore1toCSC, input$dp),
             YPscore2toCSC = round(YPscore2toCSC, input$dp))
  })
  
  tibSummaryStatsWide <- reactive({
    fullData() %>%
      summarise(totalRecords = n(),
                nClients = n_distinct(RespondentID),
                nClinicians = n_distinct(TherapistID),
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
                sdChange = sd(Change, na.rm = TRUE),
                ### CSC
                nCSCcat1high = sum(CSCcat1 == "High", na.rm = TRUE),
                nCSCcat1low = sum(CSCcat1 == "Low", na.rm = TRUE),
                nCSCcat2high = sum(CSCcat2 == "High", na.rm = TRUE),
                nCSCcat2low = sum(CSCcat2 == "Low", na.rm = TRUE),
                nCSCstayedHigh = sum(CSCchange == "Stayed high", na.rm = TRUE),
                nCSCHighToLow = sum(CSCchange == "High to low", na.rm = TRUE),
                nCSCstayedLow = sum(CSCchange == "Stayed low", na.rm = TRUE),
                nCSCLowToHigh = sum(CSCchange == "Low to high", na.rm = TRUE))
  })
  
  summaryStats1 <- reactive({
    tibSummaryStatsWide() %>%
      pivot_longer(cols = everything(), names_to = "Statistic")
  })
  
  summaryStatsText1  <- reactive({
    str_c("You have uploaded ",
          tibSummaryStatsWide()$totalRecords,
          " rows of data, with information from ",
          tibSummaryStatsWide()$nClients,
          " different client IDs and ",
          tibSummaryStatsWide()$nClinicians,
          " different clinicians")
  })
  output$summaryStatsText1 <- renderText(summaryStatsText1())
  
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
  
  output$compData <- DT::renderDataTable(server = FALSE,
                                         DT::datatable({displayData1()},
                                                       filter = "top",
                                                       extensions = "Buttons",
                                                       selection = "none",
                                                       options = list(
                                                         buttons = list(
                                                           list(extend = 'csv',   filename =  paste0("scored-", fileStubName())),
                                                           list(extend = 'excel', filename =  paste0("scored-", fileStubName()))
                                                         ),
                                                         ### the important thing is that there is the l to allow for the lengthMenu 
                                                         ### https://stackoverflow.com/questions/52645959/r-datatables-do-not-display-buttons-and-length-menu-simultaneously
                                                         # dom = 'Blrtip',
                                                         dom = "Qlfrtip",
                                                         searchBuilder = TRUE,
                                                         autoWidth = TRUE),
                                                       # fixedColumns = list(leftColumns = 2, rightColumns = 6),
                                                       # pageLength = 20,
                                                       # autoWidth = TRUE,
                                                       # ordering = FALSE,
                                                       # editable = FALSE,
                                                       # searching = FALSE),
                                         )
  )
  
  output$CSCtable1 <- renderUI({
    fullData() %>%
      tabyl(CSCcat1) %>%
      mutate(percent = str_c(round(100 * percent, 1), "%"),
             valid_percent = str_c(round(100 * valid_percent, 1), "%")) %>%
      flextable() %>%
      colformat_char(j = 1,
                     na_str = "Missing") %>%
      htmltools_value()
  })
  
  output$CSCtable2 <- renderUI({
    fullData() %>%
      tabyl(CSCcat2) %>%
      mutate(percent = str_c(round(100 * percent, 1), "%"),
             valid_percent = str_c(round(100 * valid_percent, 1), "%")) %>%
      flextable() %>%
      colformat_char(j = 1,
                     na_str = "Missing") %>%
      htmltools_value()
  })
  
  output$CSCtable3 <- renderUI({
    fullData() %>%
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
    fullData() %>%
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
  
  catsCradle1 <- reactive({
    ### massage the data
    longDat() %>%
      ### drop any missing scores
      filter(!is.na(Score)) %>%
      ### remove the transition values as we don't want those
      filter(!str_detect(WhichScore, fixed("toCSC"))) %>%
      mutate(WhichScore = str_remove(WhichScore, "YPscore"),
             WhichScore = as.numeric(WhichScore)) -> tmpTib
    
    if(input$Scoring == "Item mean (range 0-4)") {
      yLims <- c(0, 4)
    } else {
      yLims <- c(0, 40)
    }
    ySteps <- yLims[2] / 20
    ### nudge value to offset the means either side of the raw values
    tmpNudge <- .03
    
    ### get the means and their CIs
    tmpTib %>%
      group_by(WhichScore) %>%
      summarise(mean = list(getBootCImean(Score,
                                          nLT20err = FALSE,
                                          verbose = FALSE))) %>%
      unnest_wider(mean) %>%
      ungroup() %>%
      mutate(x = if_else(WhichScore == 1,
                         WhichScore - tmpNudge,
                         WhichScore + tmpNudge)) -> tmpTibMeans
    
    ggplot(data = tmpTib,
           aes(x = WhichScore, y = Score, group = RespondentID, colour = Gender,
               label2 = TherapistID, label3 = CSCchange)) +
      geom_point() +
      geom_line() +
      geom_point(data = tmpTibMeans,
                 inherit.aes = FALSE,
                 aes(x = x, y = obsmean),
                 size = 2) +
      geom_hline(yintercept = tmpTibMeans$obsmean[1],
                 linetype = 2) +
      geom_line(data = tmpTibMeans,
                inherit.aes = FALSE,
                aes(x = WhichScore, y = obsmean),
                linewidth = 2,
                linetype = 1) +
      geom_linerange(data = tmpTibMeans,
                     inherit.aes = FALSE,
                     aes(x = x,
                         ymin = LCLmean, ymax = UCLmean)) +
      scale_y_continuous("YP-CORE score",
                         breaks = seq(0, 
                                      yLims[2], 
                                      ySteps),
                         limits = yLims) +
      scale_x_continuous("Occasion",
                         breaks = 1:2) 
    
    ggplotly(tooltip = c("RespondentID", 
                         "TherapistID", 
                         "Score",
                         "label2",
                         "label3"),
             width = lisSessionData$output_pid_width, height = 800)
  })
  
  output$plot1 <- renderPlotly(
    catsCradle1()
  )
  
  catsCradle2 <- reactive({
    ### massage the data
    fullData() %>%
      select(-Age) %>%
      ### drop any missing scores
      filter(!is.na(YPscore1)) %>%
      filter(!is.na(YPscore2)) %>%
      filter(!is.na(Start_date)) %>%
      filter(!is.na(End_date)) -> tmpTib
    
    if(input$Scoring == "Item mean (range 0-4)") {
      yLims <- c(0, 4)
    } else {
      yLims <- c(0, 40)
    }
    ySteps <- yLims[2] / 20
    
    ggplot(data = tmpTib,
           aes(x = Start_date, y = YPscore1, group = RespondentID, colour = TherapistID,
               label1 = RespondentID,
               label2 = Gender,
               label3 = YPscore1,
               label4 = YPscore2, 
               label5 = CSCchange)) +
      geom_point(shape = 22) +
      geom_point(aes(x = End_date, y = YPscore2),
                 shape = 23) +
      geom_segment(aes(x = Start_date, xend = End_date,
                       y = YPscore1, yend = YPscore2)) +
      xlab("Date") +
      scale_y_continuous("YP-CORE score",
                         breaks = seq(0,
                                      yLims[2],
                                      ySteps),
                         limits = yLims) 
    
    
    ggplotly(tooltip = c("label1",
                         "label2",
                         "label3",
                         "label4",
                         "label5"),
             width = lisSessionData$output_pid_width, height = 800)
  })
  
  output$plot2 <- renderPlotly(
    catsCradle2()
  )
  
  ### create histogram
  histo1 <- reactive({
    fullData() %>%
      filter(!is.na(nSessionsAttended)) %>%
      summarise(nOK = n(),
                min = min(nSessionsAttended),
                mean = mean(nSessionsAttended),
                median = median(nSessionsAttended),
                max = max(nSessionsAttended)) -> tmpTibStats
    
    ggplot(data = fullData(),
           aes(x = nSessionsAttended)) +
      geom_histogram(fill = "grey") +
      geom_vline(xintercept = tmpTibStats$mean,
                 colour = "blue") +
      geom_vline(xintercept = tmpTibStats$median,
                 colour = "green") +
      xlab("n(Sessions attended)") +
      ggtitle("Histogram of numbers of sessions attended",
              subtitle = "Vertical reference lines: blue = mean, green = median")
  })
  
  output$histogram1 <- renderPlot(histo1(),
                                  height = 800)
  
  ### end server
}

# Create Shiny app ----
shinyApp(ui, server)