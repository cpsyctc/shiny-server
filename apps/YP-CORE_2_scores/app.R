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
suppressMessages(library(shinyDownload))

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
### RCI and CSC are based on mean scoring
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

vecCSCcategoriesOrdered <- c("Clinically significant AND reliable deterioration",
                             "Stayed high AND reliable deterioration",
                             "Clinically significant deterioration but no reliable change",
                             "Stayed high, no reliable change",
                             "Stayed low BUT reliable deterioration",
                             "Stayed low and no reliable change",
                             "Clinically significant improvement but no reliable change",
                             "Stayed low AND reliable improvement",
                             "Reliable and clinically significant improvement")

### function(s)
flexTabulateWithCI <- function(tibDat, varName){
  tibDat %>%
    count({{varName}}) %>%
    mutate(nTot = sum(n),
           nUsable = nTot - last(n)) %>%
    rowwise() %>%
    mutate(PercAll = round(100 * n / nTot, 1),
           CI = list(Hmisc::binconf(n, nUsable)[1, ])) %>%
    ungroup() %>%
    unnest_wider(CI) %>%
    # select(-totN) %>%
    rename(PercUsable = PointEst,
           LCL = Lower,
           UCL = Upper) %>%
    mutate(across(PercUsable : UCL, ~ round(.x * 100, 1))) %>%
    mutate(PercAll = str_c(PercAll, "%"),
           PercUsable = str_c(PercUsable, "%"),
           CI = str_c(LCL, 
                      " to ",
                      UCL,
                      "%"),
           CI = if_else(is.na({{varName}}),
                        NA_character_,
                        CI)) %>%
    select(-c(LCL, UCL)) %>%
    flextable() %>%
    autofit() %>%
    align(j = 3 : 4, align = "right")
}


### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "YP-CORE_2_scores",
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
                
                tabPanel("Upload",
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
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                           ),
                         p(" "),
                         h2("Download entire data"),
                         p("These buttons allow you to download your entire dataset now with the correct CSC, RCI and RCSC categories for each person's age and gender.  ",
                           "It also contains the change and rescaled scores used to make the Jacobson plot (see that tab).  Then you can see the data in a ",
                           "table you can filter or search."),
                         p(" "),
                         downloadButton("downloadCSV", "Download as csv"),
                         downloadButton("downloadXLSX", "Download as Excel xlsx"),
                         downloadButton("downloadODS", "Download as Libre/OpenOffice ods"),
                         p(" "),
                         p(" "),
                         h2("Searchable table of the data"),
                         p("Now the searchable and filterable data table.  You can download the data you have filtered/searched ",
                           "using the buttons at the bottom of the table.  If your selection is bigger than the default 50 rows shown ",
                           "use the length menu '(Show)' to select 'All' and then you can download your entire selection."),
                         p(" "),
                         DTOutput("searchableData"),    
                ),
                
                tabPanel("Sociodemographics",
                         value = 3,
                         h2("Explanation"),
                         p("This is pretty indigestible but it gives all the summary statistics for all the variables across all clinicians.",
                           "I think the naming of the statistics is pretty self-explanatory if not particularly easy on the eye."),
                         p(" "),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("[Technical] Maybe change the plot to a plotly plot?"),
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                         ),
                         p(" "),
                         h2("The summary stats"),
                         uiOutput("uploadStatusSocioDem"),
                         p(" "),
                         htmlOutput("summaryStatsText1"),
                         p(" "),
                         p("The gender breakdown is as follows."),
                         uiOutput("Gender1"),
                         p(" "),
                         htmlOutput("AgeText1"),
                         p(" "),
                         uiOutput("ageTable"),
                         p(" "),
                         p("Here is a histogram of the ages.  I don't think it's particularly likely ",
                           "that the ages would be equally represented but that is the only plausible ",
                           "reference line for now."),
                         div(
                           class = "panel panel-primary",
                           div(
                             class = "panel-heading",
                             h2("Ages", class = "panel-title")
                           ),
                           div(style="height: 620px",
                             class = "h-75; panel-body",
                             plotOutput("ageHist")
                           ),
                           div(
                             class = "panel-footer",
                             downloadGGPlotButtonUI("ageHistDownload", "ageHist"),
                             p("Avoid PDF: it doesn't format properly but I can't remove the option!")
                           )
                         ),
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
                           tags$li("Will it be useful to people to have more comparative analyses comparing clinicians?"),
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                         ),
                         p(" "),
                         h2("Summary stats by clinician"),
                         uiOutput("uploadStatusStatsByClin"),
                         p(" "),
                         tableOutput('summaryStats1longByTher'),
                ),
                
                tabPanel("Attendance",
                         value = 5,
                         h2("Explanation"),
                         p("This tab gives the statistics about attendance.  These are often undervalued and people focus ",
                         "largely on the scores and score changes.  This is a pity as it's hard to learn from scores if we ",
                         "completely ignore attendance.  However, it is undoubtedly the case that understanding score changes ",
                         "taking patterns of attendance and possible markers of ambivalence/conflict about change in the ",
                         "attendance data is complicated.  I hope to add more analyses later that may help bring score change ",
                         "and attendance information together but this is a start!"),
                         p(" "),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add options to break down by clinician and perhaps by age, gender ...?"),
                           tags$li("Think more about graphs and/or analyses to extract more from these variables."),
                           tags$li("Will it be useful to people to have more comparative analyses comparing clinicians?"),
                           tags$li("[Technical] Maybe change the plot to a plotly plot?"),
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                         ),
                         p(" "),
                         h2("Summarising attendance data"),
                         uiOutput("uploadStatusAttendance"),
                         p(" "),
                         p("The variables relating to attendance are: nSessionsAttended, nSessionsDNAed, nSessionsCancelled, nSessionsLate, nWeeks. ",
                           "Here is the breakdown of any missingness across those variables."),
                         uiOutput("TableAttendanceMissingness"),
                         p(" "),
                         p("Here are the summary statistics for each of those variables across all clients."),
                         p(" "),
                         uiOutput("TableAttendanceStats"),
                         p(" "),
                         p("I have assumed that you have counted the late attendances within the total number of attendances ",
                           "for each client so the total of sessions offered = nSessionsAttended + nSessionsDNAed + nSessionsCancelled ",
                           "so we can calculate per client the proportion of sessions offered that were attended, attended late, DNAed ",
                           "and cancelled.  Here is the summary statistics for those per client values."),
                         uiOutput("TableAttendanceStats2"),
                         
                         h2("Histogram of session count"),
                         p("Here is the histogram of the numbers of sessions attended"),
                         p(" "),
                         plotOutput('attendanceHistogram1'),
                         p(" ") ,
                         
                ),    
                
                tabPanel("Scores",
                         value = 6,
                         h2("Explanation"),
                         p("This tab gives descriptive statistics about the scores"),
                         p(" "),
                         p("Very much work in progress."),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add analyses of all t1, t2 and change by gender, age ..."),
                           tags$li("... and therapist?"),
                           tags$li("[Technical] Maybe change the plots to plotly plots?"),
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                         ),
                         p(" "),
                         h2("The YP-CORE scores"),
                         uiOutput("uploadStatusScores"),
                         p(" "),
                         htmlOutput("summaryScoresTxt1"),
                         p(" "),
                         uiOutput("scoreStatsTable1"),
                         p(" "),
                         plotOutput("histScores1",
                                    height = 600),
                         p(" "),
                         plotOutput("histScores2",
                                    height = 600),
                         p(" "),
                         plotOutput("histChange",
                                    height = 600),
                ),   
                
                tabPanel("Change (a)",
                         value = 7,
                         h2("Explanation"),
                         p("This tab gives a cat's cradle plot of the data"),
                         p(" "),
                         p("A cat's cradle plot ignores any respondent with only one usable score and shows ",
                           "the change in scores as a line.  You can identify the points by hovering over them ",
                           "when you will see a 'tooltip'",
                           "giving you the YP-CORE score, the respondent ID, therapist ID and the category ",
                           "of the CSC change."),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add options to colour down by therapist or age instead of gender, or not to colour"),
                           tags$li("Add option to map n(sessions) on x axis"),
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                         ),
                         p(" "),
                         h2("The cat's cradle plot"),
                         uiOutput("uploadStatusChangeA"),
                         p(" "),
                         p("This is a so-called 'cat's cradle' plot.",
                           "It shows all the complete pairs of scores coloured by gender with connecting lines.",
                           "The dashed horizontal reference line marks the mean baseline score and the black points,",
                           "offset somewhat from the individual points, mark the mean baseline and later scores.  ",
                           "The vertical lines through those are the bootstrap 95% confidence intervals (CI).",
                           "If the vertical CI line to the right, for the second scores does not extend ",
                           "through the horizontal reference line then the mean change is unlikely to be ",
                           "down to chance."),
                         p(" "),
                         p("If you hover over the top of the plot you see the plotly 'modebar' which allows you to save the plot as a jpeg ",
                           "and to interact with it in various way some of which may be useful to you.  See ",
                           a("here",
                           href="https://plotly.com/chart-studio-help/getting-to-know-the-plotly-modebar/"),
                           " for more on the modebar."),
                         plotlyOutput('catsCradle1',
                                      height = "100%"),
                ),    
                
                tabPanel("Change (b)",
                         value = 8,
                         h2("Explanation"),
                         p("This tab gives a cat's cradle plot against dates (if given)"),
                         p(" "),
                         p("This is very similar to the previous plot but the x axis is by date, not occasion.",
                           "This gives a better picture of the work over time and Again, you can identify the points by hovering over them ",
                           "when you will see a 'tooltip'",
                           "giving you the YP-CORE score, the respondent ID, therapist ID and the category ",
                           "of the CSC change."),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add options to break down by therapist, age, gender ..."),
                           tags$li("[Technical] add buttons to download the plot or change to ggplotly plot?"),
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                         ),
                         p(" "),
                         h2("The plot"),
                         uiOutput("uploadStatusChangeB"),
                         p(" "),
                         p("This shows all the complete data with scores and dates for both occasions coloured by therapist ID with connecting lines ",
                           "plotted against episode start and finish dates (if given).  If you hover over a point you ",
                           "should be shown the respondent ID, gender and CSC change category."),
                         p(" "),
                         p("If you hover over the top of the plot you see the plotly 'modebar' which allows you to save the plot as a jpeg ",
                           "and to interact with it in various way some of which may be useful to you.  See ",
                           a("here",
                             href="https://plotly.com/chart-studio-help/getting-to-know-the-plotly-modebar/"),
                           " for more on the modebar."),
                         plotlyOutput('catsCradle2',
                                      height = "100%"),
                ),  
                
                tabPanel("Change (c)",
                         value = 9,
                         h2("Explanation"),
                         p("This tab plots change against sessions attended (if given)"),
                         p(" "),
                         p("This is very similar to the previous plots but the x axis is by sessions attended.",
                           "This gives a picture of whether change relates to sessions attended.  Again you ",
                           "can identify the points by hovering over them when you will see a 'tooltip'",
                           "giving you the YP-CORE score, the respondent ID, therapist ID and the category ",
                           "of the CSC change."),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add options to break down by therapist, age, gender ..."),
                           tags$li("[Technical] add buttons to download the plot or change to ggplotly plot?"),
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                         ),
                         p(" "),
                         h2("The plot"),
                         uiOutput("uploadStatusChangeB"),
                         p(" "),
                         p("This shows all the data with scores and dates for both occasions.  The wavy curve in blue is the ",
                           "LOESS smoothed fit mapping change to numbers of sessions attendeed.  The grey envelope around the ",
                           "curve gives the 95% confidence interval around that."),
                         p("The individual change scores are in grey and the blue points mark the mean change for each number ",
                           "of sessions attended.  If you hover over an individual point you ",
                           "should be shown the respondent ID, gender and CSC change category. "),
                         p("Points below the -1 mark reliable improvement and points above +1 reliable deterioration.",
                           "If the dotted mean change line lies inside the grey confidence envelope there is no evidence, ",
                           "of a systematic relationship between change and numbers of sessions attended."),
                         p(" "),
                         p("If you hover at the top of the plot you see the plotly 'modebar' which allows you to save the plot as a jpeg ",
                           "and to interact with it in various way some of which may be useful to you.  See ",
                           a("here",
                             href="https://plotly.com/chart-studio-help/getting-to-know-the-plotly-modebar/"),
                           " for more on the modebar."),
                         plotlyOutput('loessPlot1',
                                      height = "100%"),
                ),  
                
                tabPanel("RCSC analyses",
                         value = 10,
                         h2("Explanation"),
                         p("This tab gives a simple breakdown of the CSC categories: baseline, final and change counts"),
                         p(" "),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add options to colour by age or gender instead of therapist, or to use single colour?"),
                           tags$li("Will it be useful to people to have more comparative analyses comparing clinicians?"),
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                         ),
                         p(" "),
                         h2("The CSC tabulations"),
                         uiOutput("uploadStatusRCSC"),
                         p(" "),
                         p("I can see that this might be a bit overwhelming at first sight. Play around with it and it will get ",
                           "familiar and you will learn which of these values are really of interest to you.  The confidence intervals ",
                           "(CI) may be unfamiliar so you can ignore them at least first but they are useful as they indicate how we ",
                           "should see these observed rates as predictors of the likely long term values for your services.",
                           "The larger the number of clients with data the better your observed values become as predictores of future ",
                           "values.  For the background on CIs see ",
                           a("this entry in my OMbook glossary",
                             href="https://www.psyctc.org/psyctc/glossary2/confidence-intervals-cis/"),
                           " and ",
                           a("this page on my PSYCTC.org site",
                             href="https://www.psyctc.org/psyctc/root/stats/rcsc/"),),
                         p(" "),
                         h3("CSC analyses"),
                         p("Here is the breakdown of the initial CSC categories"),
                         p(" "),
                         uiOutput('CSCtable1'),
                         p(" ") ,
                         p("And now the breakdown of the 2nd/final CSC categories"),
                         p(" "),
                         uiOutput('CSCtable2'),
                         p(" ") ,
                         p("This is the crosstabulation of the first and 2nd/final categories (where you have both scores)"),
                         p(" "),
                         uiOutput('CSCtable3'),
                         p(" ") ,
                         p("And this is the breakdown of the CSC classification changes (basically the crosstabulation above unwrapped)."),
                         p(" "),
                         uiOutput('CSCtable4'),
                         h3("RCI analyses"),
                         p("Now the breakdown of the RCI change categories"),
                         p(" "),
                         uiOutput('RCItable1'),
                         h3("And finally: the full RCSC analyses"),
                         p(" "),
                         p("Crosstabulation of CSC and RCI change categories"),
                         uiOutput('RCSCcrosstable1'),
                         p(" "),
                         p("Essentially the same unfolded with CIs"),
                         uiOutput('RCSCtable1'),
                         p(" "),
                         p("It is, of course, impossible to achieve clinically significant improvement if you start below the CSC ",
                           "for this reason people sometimes report the CSC restricting to the participants who start above the CSC.",
                           "Here is RCS crosstabulation for that, censored, dataset."),
                         uiOutput('RCSCcrosstable2'),
                         p(" "),
                         p("The same unfolded with CIs"),
                         uiOutput('RCSCtable2'),
                         p(" "),
                ),    
                
                tabPanel("Jacobson plot",
                         value = 11,
                         h2("Explanation"),
                         p("This tab gives a scaled Jacobon plot"),
                         p(" "),
                         p("The Jacobson plot plots the 2nd/final score against the first. Points lying on the leading diagonal mark clients ",
                           "whose last YP-CORE score is exactly the same as their fist score.  Points below the leading diagonal have lower ",
                           "last scores than their first scores, points above the leading diagonal, <i>vice versa</i>."),
                         p("The 'tramlines' either side of the leading diagonal mark the RCI for the YP-CORE for that person's age and gender.",
                           "As the scores have been rescaled here everyone, regardless of age or gender, has tramlines on 1 above or 1 below ",
                           "the leading diagonal.  Points inside the tramlines mark people whose scores changed but less than the RCI ",
                           "(or RCC: Reliable Change Criterion), i.e. 'no reliable change'.  Points below the lower tramline come from people ",
                           "whose YP-CORE scores improved more than the RCI and those above the upper tramline deteriorated more than the RCI."),
                         p("The black vertical and horizontal lines mark the CSC.  Points to the left of the vertical line started below the ",
                           "CSC, those to the right started above it.  Similarly, points above the horizontal line finished above the CSC and ",
                           "those below it finished below the CSC"),
                         p("As in previous plots you can identify the points by hovering over them ",
                           "when you will see a 'tooltip'",
                           "giving you the person's gender, age, first and last YP-CORE scores (not rescaled), the respondent ID and therapist ID."),
                         h2("Todo list"),
                         p("This is work in progress like everything else in this app.",
                           "This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Add options to colour the points by age and therapist instead of gender ..."),
                           tags$li("And perhaps facetting by those variables or to change the plot by those variables."),
                           tags$li("What else would you as a user want here?",
                                   a("Contact me",
                                     href="https://www.coresystemtrust.org.uk/home/contact-form/")),
                         ),
                         p(" "),
                         h2("The plot"),
                         uiOutput("uploadStatusJacobson"),
                         p(" "),
                         p("If you hover over the top of the plot you see the plotly 'modebar' which allows you to save the plot as a jpeg ",
                           "and to interact with it in various way some of which may be useful to you.  See ",
                           a("here",
                             href="https://plotly.com/chart-studio-help/getting-to-know-the-plotly-modebar/"),
                           " for more on the modebar."),
                         div(style="width:100%;height:0;padding-top:100%;position:relative;",
                             div(style="position: absolute;
                                      top: 0;
                                      left: 0;
                                      width: 100%;
                                      height: 100%;",
                                 plotlyOutput("Jacobson1", height="100%")))
                ),
                
                tabPanel("Explanation of the app",
                         value = 12,
                         h2("Explanation"),
                         p("This app is definitely work in progress at the moment."),
                         p(" "),
                         h2("Overall todo list"),
                         p("This is the todo list for this tab as I see it at this point"),
                         tags$ul(
                           tags$li("Better cross-referencing to supporting documents"),
                           tags$li("Ideally some recorded 'talk-you-through' pocasts?")
                         ),
                         p(" "),
                ),
                
                tabPanel("Background", 
                         value = 12,
                         p("App created 22.v.25 by Chris Evans.",
                           a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                         p("Last updated 18.vi.25: added simple stats to attendance histogram ",),
                         p("Much work still to do but getting there!"),
                         p("Licenced under a ",
                           a("Creative Commons, Attribution Licence-ShareAlike",
                             href="http://creativecommons.org/licenses/by-sa/1.0/"),
                           " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                         includeHTML("https://shiny.psyctc.org/boilerplate.html")),
                
                id = "tabSelected"),
  )
)


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
    req(input$file1)
    tibLookup %>%
      filter(Ref == input$Lookup)
  })
  
  showLookup <- reactive({
    req(input$file1)
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
  
  ### tabs: all
  uploadPlease <- reactive({
    if(is.null(input$file1)) {
      txt <- HTML("<b>No data uploaded yet, go to upload tab and upload some data please!</b>")
    } else {
      txt <- HTML("  ")
    }
  })
  ### you have to have separate calls for each tab
  output$uploadStatusData <- renderUI(uploadPlease())
  output$uploadStatusSocioDem <- renderUI(uploadPlease())
  output$uploadStatusStatsByClin <- renderUI(uploadPlease())
  output$uploadStatusAttendance <- renderUI(uploadPlease())
  output$uploadStatusScores <- renderUI(uploadPlease())
  output$uploadStatusChangeA <- renderUI(uploadPlease())
  output$uploadStatusChangeB <- renderUI(uploadPlease())
  output$uploadStatusRCSC <- renderUI(uploadPlease())
  output$uploadStatusJacobson <- renderUI(uploadPlease())
  
  ### tab: sociodemographics
  output$lookupTable1 <- renderUI({
    req(input$file1)
    showLookup() %>%
      colformat_char(j = 1,
                     na_str = "Missing") %>%
      htmltools_value()
  })
  
  
  fullData <- reactive({
    # req(input$file1)
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
             YPscaled1toCSC = YPscore1toCSC / RCI,
             YPscaled2toCSC = YPscore2toCSC / RCI,
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
    req(input$file1)
    fullData() %>%
      pivot_longer(cols = starts_with("YPscore"), names_to = "WhichScore", values_to = "Score") 
    
  })
  
  displayData1 <- reactive({
    req(input$file1)
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
             YPscore2toCSC = round(YPscore2toCSC, input$dp),
             YPscaled1toCSC = round(YPscaled1toCSC, input$dp),
             YPscaled2toCSC = round(YPscaled2toCSC, input$dp),)
  })
  
  tibSummaryStatsWide <- reactive({
    req(input$file1)
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
                nYPbothValid = sum(!is.na(YPscore1) & !is.na(YPscore2)),
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
  
  summaryStats1long <- reactive({
    req(input$file1)
    tibSummaryStatsWide() %>%
      pivot_longer(cols = everything(), names_to = "Statistic")
  })
  
  summaryStatsText1  <- reactive({
    req(input$file1)
    str_c("You have uploaded ",
          tibSummaryStatsWide()$totalRecords,
          " rows of data, with information from ",
          tibSummaryStatsWide()$nClients,
          " different client IDs and ",
          tibSummaryStatsWide()$nClinicians,
          " different clinicians")
  })
  output$summaryStatsText1 <- renderText(summaryStatsText1())
  
  
  ### tab: sociodemographics
  genderStatsText1 <- reactive({
    req(input$file1)
    fullData() %>%
      mutate(Gender = ordered(Gender,
                              levels = c("F", "M", "O"))) %>%
      flexTabulateWithCI(Gender) %>%
      autofit() %>%
      htmltools_value()
  })
  output$Gender1 <- renderUI(genderStatsText1())
  
  summaryStatsTextAge1  <- reactive({
    req(input$file1)
    str_c("There were ",
          getNNA(fullData()$Age),
          " rows with missing ages.  Ages for the remaining records ranged from ",
          min(fullData()$Age, na.rm = TRUE),
          " to ",
          max(fullData()$Age, na.rm = TRUE),
          " with mean ",
          round(mean(fullData()$Age, na.rm = TRUE), input$dp),
          ", median ",
          round(median(fullData()$Age, na.rm = TRUE), input$dp),
          " and, for what's worth here, SD ",
          round(sd(fullData()$Age, na.rm = TRUE), input$dp),
          ".  Here is the table of ages.")
  })
  output$AgeText1 <- renderText(summaryStatsTextAge1())
  
  tableAges <- reactive({
    req(input$file1)
    summaryStats1long() %>%
      filter(str_sub(Statistic, 1, 4) == "nAge") %>%
      rename(n = value) %>%
      mutate(nUsable = sum(n)) %>%
      rowwise() %>%
      mutate(CI = list(Hmisc::binconf(n, nUsable)[1, ])) %>%
      ungroup() %>%
      unnest_wider(CI) %>%
      mutate(across(PointEst : Upper, ~ .x * 100),
             across(PointEst : Upper, ~ round(.x, input$dp))) %>%
      rename(Perc = PointEst,
             LCL = Lower,
             UCL = Upper) %>%
      mutate(Perc = str_c(Perc, "%"),
             CI = str_c(LCL,
                        " to ",
                        UCL,
                        "%")) %>%
      select(-c(LCL, UCL)) %>%
      flextable() %>%
      autofit() %>%
      align(j = 4 :5,
            align = "right") %>%
      htmltools_value()
  })
  output$ageTable <- renderUI(tableAges())
  
  ageHisto <- reactive({
    req(input$file1)
    summaryStats1long() %>%
      filter(str_sub(Statistic, 1, 4) == "nAge") %>%
      rename(Age = Statistic,
             n = value) %>%
      mutate(Age = as.numeric(str_remove(Age, fixed("nAge"))),
             nUsable = sum(n)) %>%
      rowwise() %>%
      mutate(CI = list(Hmisc::binconf(n, nUsable)[1, ])) %>%
      ungroup() %>%
      unnest_wider(CI) %>%
      mutate(across(PointEst : Upper, ~ .x * nUsable)) %>%
      rename(Perc = PointEst,
             LCL = Lower,
             UCL = Upper) -> tmpTib
    
    tmpTib %>%
      reframe(meanN = nUsable / n()) %>%
      select(meanN) %>%
      pull() -> tmpValMeanN
    
    ggplot(data = tmpTib,
           aes(x = Age,
               y = n)) +
      geom_point() +
      geom_bar(stat = "identity",
               fill = "grey") +
      geom_linerange(aes(ymin = LCL, ymax = UCL)) +
      geom_hline(yintercept = tmpValMeanN,
                 linetype = 3) +
      ggtitle("Barchart of ages",
              subtitle = str_c("Horizontal reference line is mean n had ages been equally frequent",
                               "\nVertical reference lines are binomial 95% CIs for observed n values."))
  })
  output$ageHist <- renderPlot(ageHisto(),
                               height = 600)
  
  downloadGGPlotButtonServer(
    id = "ageHistDownload", # <= this should match the ID used in the UI module
    ggplotObject = ageHisto, # No parentheses here to pass *expression*
    width = 1500,
    height = 800
    # width = input$fileWidth,
    # height = input$fileHeight
  )
  
  ### tab: stats by therapist
  summaryStats1longByTher <- reactive({
    req(input$file1)
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
  
  
  ### tab: data
  output$downloadCSV <- downloadHandler(
    filename = "YP-CORE_data.csv",
    contentType = "text/csv",
    content = function(file) {
      write_csv(fullData(), file = file)
    })
  
  output$downloadXLSX <- downloadHandler(
    filename = "YP-CORE_data.xlsx",
    contentType = "text/csv",
    content = function(file) {
      writexl::write_xlsx(fullData(), path = file)
    })
  
  output$downloadODS <- downloadHandler(
    filename = "YP-CORE_data.ods",
    contentType = "text/csv",
    content = function(file) {
      readODS::write_ods(fullData(), path = file)
    })
  
  
  output$searchableData <- DT::renderDataTable(server = FALSE,
                                               DT::datatable({displayData1()},
                                                             extensions = "Buttons",
                                                             filter = "top",
                                                             selection = "none",
                                                             options = list(
                                                               lengthMenu = list(c(50, 100, -1), 
                                                                                 c('50','100', 'All')),
                                                               ### I was trying to put some space before the download buttons but this
                                                               ### doesn't do it!
                                                               # searchPanes = list(
                                                               #   viewTotal = TRUE,
                                                               #   i18n = list(
                                                               #     count = '{total} found',
                                                               #     countFiltered = '{shown} ({total}   )'
                                                               #   )
                                                               # ),
                                                               buttons = c('copy', 'csv', 'excel'),
                                                               autoWidth = TRUE,
                                                               ### the important thing is that there is the l to allow for the lengthMenu 
                                                               ### https://stackoverflow.com/questions/52645959/r-datatables-do-not-display-buttons-and-length-menu-simultaneously
                                                               # dom = 'Blrtip',
                                                               dom = "QlfrtipB")
                                               )
  )

  output$CSCtable1 <- renderUI({
    flexTabulateWithCI(fullData(), CSCcat1) %>%
      htmltools_value()
  })
  
  output$CSCtable2 <- renderUI({
    flexTabulateWithCI(fullData(), CSCcat2) %>%
      htmltools_value()
  })
  
  output$CSCtable3 <- renderUI({
    fullData() %>%
      filter(!is.na(CSCcat1) & !is.na(CSCcat2)) %>%
      tabyl(CSCcat1, CSCcat2) %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  
  output$CSCtable4 <- renderUI({
    flexTabulateWithCI(fullData(), CSCchange) %>%
      htmltools_value()
  })
  
  output$RCItable1 <- renderUI({
    flexTabulateWithCI(fullData(), RelChange) %>%
      htmltools_value()
  })
  
  output$RCSCcrosstable1 <- renderUI({
    fullData() %>%
      filter(!is.na(CSCchange) & !is.na(RelChange)) %>%
      mutate(CSCchange = ordered(CSCchange,
                                 levels = c("Stayed high",
                                            "High to low",
                                            "Low to high",
                                            "Stayed low")),
             Relchange = ordered(RelChange,
                                 levels = c("Reliable deterioration",
                                            "No reliable change",
                                            "Reliable improvement"))) %>%
      tabyl(CSCchange, RelChange) %>%
      flextable() %>%
      htmltools_value()
  })
  
  output$RCSCtable1 <- renderUI({
    fullData() %>%
      mutate(RCSCcat = ordered(RCSCcat,
                               levels = vecCSCcategoriesOrdered)) %>%
      select(RCSCcat) -> tmpTib
    
    flexTabulateWithCI(tmpTib, RCSCcat) %>%
      htmltools_value()
  })
  
  output$RCSCcrosstable2 <- renderUI({
    fullData() %>%
      filter(!is.na(CSCchange) & !is.na(RelChange)) %>%
      filter(CSCcat1 == "High") %>%
      mutate(CSCchange = ordered(CSCchange,
                                 levels = c("Stayed high",
                                            "High to low")),
             Relchange = ordered(RelChange,
                                 levels = c("Reliable deterioration",
                                            "No reliable change",
                                            "Reliable improvement"))) %>%
      tabyl(CSCchange, RelChange) %>%
      flextable() %>%
      htmltools_value()
  })
  
  output$RCSCtable2 <- renderUI({
    fullData() %>%
      filter(CSCcat1 == "High") %>%
      mutate(RCSCcat = ordered(RCSCcat,
                               levels = vecCSCcategoriesOrdered)) %>%
      select(RCSCcat) -> tmpTib
    
    flexTabulateWithCI(tmpTib, RCSCcat) %>%
      htmltools_value()
  })
  
  output$summaryStats1long <- renderTable(summaryStats1long())
  
  output$summaryStats1longByTher <- renderTable(summaryStats1longByTher())
  
  catsCradle1 <- reactive({
    req(input$file1)
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
      summarise(mean = list(suppressMessages(getBootCImean(Score,
                                          nLT20err = FALSE,
                                          verbose = FALSE)))) %>%
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
  
  output$catsCradle1 <- renderPlotly(
    catsCradle1()
  )
  
  catsCradle2 <- reactive({
    req(input$file1)
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
  
  output$catsCradle2 <- renderPlotly(
    catsCradle2()
  )
  
  ### tab: change(c)
  loessPlot1 <- reactive({
    req(input$file1)
    
    fullData() %>%
      print()
    
    ### massage the data
    fullData() %>%
      ### drop any missing scores
      filter(!is.na(YPscore1)) %>%
      filter(!is.na(YPscore2)) %>%
      filter(!is.na(nSessionsAttended)) %>%
      filter(!is.na(RCI)) %>%
      mutate(tmpChange = (YPscore2 - YPscore1),
             scaledChange = tmpChange / RCI) -> tmpTib
    
    tmpTib %>%
      group_by(nSessionsAttended) %>%
      summarise(n = n(),
                mean = mean(scaledChange),
                min = min(scaledChange),
                max = max(scaledChange)) %>%
      ungroup() -> tmpTibStats

    suppressWarnings(ggplot(data = tmpTib,
                            aes(x = nSessionsAttended, y = scaledChange)) +
                       geom_smooth(aes(x = nSessionsAttended, y = scaledChange)) +
                       geom_point(aes(label1 = RespondentID,
                                      label2 = Gender,
                                      label3 = YPscore1,
                                      label4 = YPscore2, 
                                      label5 = CSC),
                                  colour = "grey") +
                       geom_point(data = tmpTibStats,
                                  inherit.aes = FALSE,
                                  aes(x = nSessionsAttended,
                                      y = mean),
                                  size = 2.5,
                                  shape = 16,
                                  colour = "blue") +
                       geom_linerange(data = tmpTibStats,
                                      inherit.aes = FALSE,
                                      aes(x = nSessionsAttended,
                                          ymin = min, ymax = max),
                                      linetype = 3,
                                      colour = "grey") +
                       geom_hline(yintercept = 0) +
                       geom_hline(yintercept = mean(tmpTib$scaledChange),
                                  linetype = 3) +
                       xlab("Date") +
                       scale_y_continuous("Change, scaled to the RCI")) +
      ggtitle("Change scores, scaled to the appropriate RCI",
              subtitle = str_c("A change equal to the RCI has score 1.",
                               "Solid horizontal reference line marks no change, ",
                               "dashed line marks overall mean change."))
    
    
    ggplotly(tooltip = c("label1",
                         "label2",
                         "label3",
                         "label4",
                         "label5"),
             width = lisSessionData$output_pid_width, height = 800)
  })
  
  output$loessPlot1 <- renderPlotly(
    loessPlot1()
  )
  
  
  ### tab: attendance
  attendanceDataCounts <- reactive({
    req(input$file1)
    fullData() %>%
      select(nSessionsAttended, nSessionsDNAed, nSessionsCancelled, nSessionsLate, nWeeks) %>%
      pivot_longer(everything(), names_to = "Variable") %>%
      group_by(Variable) %>%
      summarise(n = n(),
                nMissing = getNNA(value),
                nOK = getNOK(value)) %>%
      mutate(percMissing = str_c(round(100 * nMissing / n, input$dp), "%"),
             percOK = str_c(round(100 * nOK / n, input$dp), "%")) %>%
      select(Variable, n, nMissing, percMissing, nOK, percOK) %>%
      flextable() %>%
      align(j = c(4, 6),
            align = "right") %>%
      autofit() %>%
      htmltools_value()
  })
  output$TableAttendanceMissingness <- renderUI(attendanceDataCounts())
  
  attendanceDataStats <- reactive({
    req(input$file1)
    fullData() %>%
      select(nSessionsAttended, nSessionsDNAed, nSessionsCancelled, nSessionsLate, nWeeks) %>%
      pivot_longer(everything(), names_to = "Variable") %>%
      group_by(Variable) %>%
      summarise(nOK = getNOK(value),
                min = min(value, na.rm = TRUE),
                mean = round(mean(value, na.rm = TRUE), input$dp),
                median = round(median(value, na.rm = TRUE), input$dp),
                SD = sd(mean(value, na.rm = TRUE), input$dp),
                max = max(value, na.rm = TRUE)) %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  output$TableAttendanceStats <- renderUI(attendanceDataStats())
  
  attendanceDataStats2 <- reactive({
    req(input$file1)
    fullData() %>%
      select(nSessionsAttended, nSessionsDNAed, nSessionsCancelled, nSessionsLate, nWeeks) %>%
      rowwise() %>%
      mutate(nSessionsOffered = sum(nSessionsAttended, nSessionsDNAed, nSessionsLate, na.rm = FALSE)) %>%
      ungroup() %>%
      mutate(percSessAtt = round(100 * nSessionsAttended / nSessionsOffered, input$dp),
             percSessDNA = round(100 * nSessionsDNAed / nSessionsOffered, input$dp),
             percSessCanc = round(100 * nSessionsCancelled / nSessionsOffered, input$dp),
             percSessAttLate = round(100 * nSessionsLate / nSessionsAttended, input$dp)) %>%
      select(starts_with("perc")) %>%
      pivot_longer(cols = everything(),
                   names_to = "Variable") %>%
      group_by(Variable) %>%
      summarise(min = round(min(value, na.rm = TRUE), input$dp),
                mean = round(mean(value, na.rm = TRUE), input$dp),
                median = round(median(value, na.rm = TRUE), input$dp),
                SD = round(sd(value, na.rm = TRUE), input$dp),
                max = round(max(value, na.rm = TRUE), input$dp)) %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  output$TableAttendanceStats2 <- renderUI(attendanceDataStats2())
  
  ### create attendance histogram
  attendanceHisto1 <- reactive({
    req(input$file1)
    fullData() %>%
      # filter(!is.na(nSessionsAttended)) %>%
      summarise(nTot = n(),
                nOK = getNOK(nSessionsAttended),
                min = min(nSessionsAttended, na.rm = TRUE),
                mean = mean(nSessionsAttended, na.rm = TRUE),
                median = median(nSessionsAttended, na.rm = TRUE),
                max = max(nSessionsAttended, na.rm = TRUE)) -> tmpTibStats
    
    str_c("Total n = ",
          tmpTibStats$nTot,
          ",  n with session count = ",
          tmpTibStats$nOK,
          "\nMean = ",
          round(tmpTibStats$mean, input$dp),
          ", Median = ",
          round(tmpTibStats$median, input$dp),
          "\nMin = ",
          tmpTibStats$min,
          ", Max = ",
          tmpTibStats$max) -> tmpLabel
    
    fullData() %>%
      filter(!is.na(nSessionsAttended)) %>%
      count(nSessionsAttended) -> tmpTib

    ggplot(data = tmpTib,
           aes(x = nSessionsAttended,
               y = n)) +
      geom_bar(stat = "identity",
               fill = "grey") +
      geom_vline(xintercept = tmpTibStats$mean,
                 colour = "blue") +
      geom_vline(xintercept = tmpTibStats$median,
                 colour = "green") +
      scale_x_continuous("n(Sessions attended)",
                         breaks = seq(0, tmpTibStats$max)) +
      annotate("text",
               x = tmpTibStats$max, 
               y = max(tmpTib$n),
               label = tmpLabel,
               hjust = 1,
               vjust = 1,
               size = 10) +
      ggtitle("Histogram of numbers of sessions attended",
              subtitle = "Vertical reference lines: blue = mean, green = median")
  })
  
  output$attendanceHistogram1 <- renderPlot(attendanceHisto1(),
                                  height = 800)
  
  ### tab: scores
  summaryScoresTxt1  <- reactive({
    req(input$file1)
    str_c("Across your ",
          tibSummaryStatsWide()$totalRecords,
          " and ",
          tibSummaryStatsWide()$nClinicians,
          " different clinicians you have ",
          tibSummaryStatsWide()$nYP1valid,
          " usable initial scores, ",
          tibSummaryStatsWide()$nYP2valid,
          " usable t2 scores and ",
          tibSummaryStatsWide()$nYPbothValid,
          " usable on both occasions.  ",
          "Here is a table of some summary statistics for the scores.  ",
          "'LCLmean' is the lower 95% confidence limit of the mean ",
          "and 'UCLmean' the upper CL calculated using the percentile bootstrap.")
  })
  output$summaryScoresTxt1 <- renderText(summaryScoresTxt1())
  
  scoreStatsTable1 <- reactive({
    fullData() %>%
      select(YPscore1, YPscore2, Change) %>%
      pivot_longer(cols = everything(),
                   names_to = "Score") %>%
      mutate(Score = case_when(
        Score == "YPscore1" ~ "Occasion 1",
        Score == "YPscore2" ~ "Occasion 2",
        Score == "Change" ~ "Change"),
        Score = ordered(Score,
                        levels = c("Occasion 1",
                                   "Occasion 2",
                                   "Change"))) %>%
      group_by(Score) %>%
      reframe(nOK = getNOK(value),
                min = min(value, na.rm = TRUE),
                CI = list(suppressMessages(getBootCImean(value))),
                median = median(value, na.rm = TRUE),
                max = max(value, na.rm = TRUE),
                sd = sd(value, na.rm = TRUE)) %>%
      unnest_wider(CI) %>%
      flextable() %>%
      colformat_double(digits = input$dp) %>%
      autofit() %>%
      htmltools_value()
  })
  output$scoreStatsTable1 <- renderUI(scoreStatsTable1())
  
  histScores1 <- reactive({
    req(input$file1)
    fullData() %>%
      summarise(mean = mean(YPscore1, na.rm = TRUE),
                median = median(YPscore1, na.rm = TRUE)) -> tmpTibStats
    
    ggplot(data = fullData(),
           aes(x = YPscore1)) +
      geom_histogram(fill = "grey",
                     center = 0) +
      geom_vline(xintercept = tmpTibStats$mean,
                 colour = "blue") +
      geom_vline(xintercept = tmpTibStats$median,
                 colour = "green") +
      scale_x_continuous("First YP-CORE scores",
                         breaks = seq(0, 4, .2),
                         limits = c(0, 4)) +
      ggtitle("Histogram of first YP-CORE scores",
              subtitle = "Blue vertical reference line is mean, green is median")
  })
  output$histScores1 <- renderPlot(histScores1())
  
  histScores2 <- reactive({
    req(input$file1)
    fullData() %>%
      summarise(mean = mean(YPscore2, na.rm = TRUE),
                median = median(YPscore2, na.rm = TRUE)) -> tmpTibStats
    
    ggplot(data = fullData(),
           aes(x = YPscore2)) +
      geom_histogram(fill = "grey",
                     center = 0) +
      geom_vline(xintercept = tmpTibStats$mean,
                 colour = "blue") +
      geom_vline(xintercept = tmpTibStats$median,
                 colour = "green") +
      scale_x_continuous("Second YP-CORE scores",
                         breaks = seq(0, 4, .2),
                         limits = c(0, 4)) +
      ggtitle("Histogram of second YP-CORE scores",
              subtitle = "Blue vertical reference line is mean, green is median")
  })
  output$histScores2 <- renderPlot(histScores2())
  
  histChange <- reactive({
    req(input$file1)
    fullData() %>%
      summarise(mean = mean(Change, na.rm = TRUE),
                median = median(Change, na.rm = TRUE)) -> tmpTibStats
    
    ggplot(data = fullData(),
           aes(x = Change)) +
      geom_histogram(fill = "grey",
                     center = 0) +
      geom_vline(xintercept = tmpTibStats$mean,
                 colour = "blue") +
      geom_vline(xintercept = tmpTibStats$median,
                 colour = "green") +
      scale_x_continuous("Change scores") +
      ggtitle("Histogram of change scores",
              subtitle = "Blue vertical reference line is mean, green is median")
  })
  output$histChange <- renderPlot(histChange())
  
  ### tab: Jacobson
  jacobson1 <- reactive({
    req(input$file1)
    fullData() %>%
      select(RespondentID, TherapistID, Gender, Age, YPscore1, YPscore2, YPscaled1toCSC, YPscaled2toCSC) -> tmpTib
    
    tmpTib %>%
      summarise(min = min(c(min(YPscaled1toCSC),
                            min(YPscaled2toCSC))),
                max = max(c(max(YPscaled1toCSC),
                            max(YPscaled2toCSC)))) -> tmpTibLimits
    
    ggplot(tmpTib,
           aes(x = YPscaled1toCSC,
               y = YPscaled2toCSC,
               colour = Gender,
               label1 = Age,
               label2 = RespondentID,
               label3 = TherapistID,
               label4 = YPscore1,
               label5 = YPscore2)) +
      geom_point() +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_abline(slope = 1, intercept = 0) +
      geom_abline(slope = 1, intercept = 1,
                  linetype = 3) +
      geom_abline(slope = 1, intercept = -1,
                  linetype = 3) +
      ggtitle("Jacobson plot of YP-CORE score change") +
      scale_x_continuous("First YP-CORE score, rescaled",
                         limits = c(tmpTibLimits$min, 
                                    tmpTibLimits$max)) +
      scale_y_continuous("Second YP-CORE score, rescaled",
                         limits = c(tmpTibLimits$min, 
                                    tmpTibLimits$max)) 
    
    ggplotly(tooltip = c("Gender",
                         "label1", 
                         "label2",
                         "label3", 
                         "label4",
                         "label5"),
             width = lisSessionData$output_pid_width, height = 800)
  })
  
  output$Jacobson1 <- renderPlotly(jacobson1())
  
  ### end server
}

# Create Shiny app ----
shinyApp(ui, server)
