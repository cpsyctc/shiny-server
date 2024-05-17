### COREpapers1

suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))
suppressMessages(library(shinyjs)) # for reset button
# suppressMessages(library(CECPfuns))

appPath <- getwd()
# cat(file=stderr(), "serverPath is:", serverPath, "\n")
# appPath <- paste0(serverPath, "/apps/COREpapers1")
suppressMessages(read_csv(paste0(appPath, "/", "tibDat.csv"),
                          progress = FALSE)) -> tibDat

### values
vecCOREmeasures <-  c("CORE-OM" = "CORE-OM",
                      "CORE-6D scoring of CORE-OM" = "CORE-6D",
                      "CORE-10" = "CORE-10",
                      "CORE-SF/A" = "CORE-SF/A",
                      "CORE-SF/B" = "CORE-SF/B",
                      "GP-CORE" = "GP-CORE",
                      "YP-CORE" = "YP-CORE",
                      "LD-CORE" = "LD-CORE (14 item version)",
                      "LD-CORE-30" = "LD-CORE-30",
                      "CORE-5" = "CORE-5",
                      "CORE-A TAF" = "CORE-A (TAF)",
                      "CORE-A EoT" = "CORE-A (EoT)"
)

vecCOREmeasures2 <-  c("CORE-OM" = "CORE-OM",
                       "CORE-6D scoring of CORE-OM" = "CORE-6D",
                       "CORE-10" = "CORE-10",
                       "CORE-SF/A" = "SF/A",
                       "CORE-SF/B" = "SF/B",
                       "GP-CORE" = "CORE-GP",
                       "YP-CORE" = "YP-CORE",
                       "LD-CORE (any length)" = "LD-CORE",
                       "LD-CORE-30" = "LD-CORE-30",
                       "CORE-5" = "CORE-5",
                       "CORE-A TAF" = "TAF",
                       "CORE-A EoT" = "EoT"
)

vecCORElanguages <- c("Brazilian", 
                      "British Sign Language (BSL)", 
                      "Burmese", 
                      "Chinese", 
                      "Croatian", 
                      "Czech", 
                      "Danish", 
                      "English", 
                      "Finnish", 
                      "French", 
                      "Icelandic", 
                      "Italian", 
                      "Japanese", 
                      "Korean", 
                      "Lithuanian", 
                      "Norwegian", 
                      "Polish", 
                      "Portuguese", 
                      "Romanian", 
                      "Russian", 
                      "Serbian", 
                      "Slovak", 
                      "Spanish", 
                      "Swahili", 
                      "Swedish", 
                      "Vietnamese", 
                      "Xhosa")

vecCountries <- c("Australia",
                  "Belorussia",
                  "Brazil",
                  "Canada",
                  "China",
                  "Colombia",
                  "Croatia",
                  "Czech Republic",
                  "Denmark",
                  "Ecuador",
                  "Finland",
                  "France",
                  "Germany",
                  "Iceland",
                  "India",
                  "Ireland",
                  "Israel",
                  "Italy",
                  "Japan",
                  "Kenya",
                  "Korea",
                  "Lithuania",
                  "Malta",
                  "Mexico",
                  "Myanmar",
                  "New Zealand",
                  "Norway",
                  "Peru",
                  "Philippines",
                  "Poland",
                  "Portugal",
                  "Romania",
                  "Russia",
                  "Serbia",
                  "Slovakia",
                  "South Africa",
                  "Spain",
                  "Sweden",
                  "UK",
                  "USA",
                  "Uganda",
                  "Vietnam")

vecGenderCats <- c("Women",
                   "Men",
                   "Unspecified",
                   "Transgender", 
                   "Other",
                   "Non-binary",
                   "Not applicable",
                   "Unknown")

vecFormats <- c("Unknown",
                "Not applicable",
                "Paper and pencil",
                "Electronic/automated instruments",
                "As an interview/Interview-assisted")

tibDat %>%
  select(AssessmentStructure) %>%
  distinct() %>%
  pull() -> vecAssStructure

findMatches <- function(tibDat, searchVarName, idVarName, vecMatches, or = TRUE) {
  ### function to find matches to members of vecMatches in tibDat$varname
  ### can do it as Boolean OR or AND
  
  vecSelected <- NA # initialise vector
  
  for (i in 1:length(vecMatches)) {
    tibDat %>%
      filter(str_detect({{searchVarName}}, fixed(vecMatches[i]))) %>%
      select({{idVarName}}) %>%
      pull() -> tmpVec
    
    if (or) {
      vecSelected <- c(vecSelected, tmpVec)
    } else {
      if (i == 1) {
        ### first test so 
        vecSelected <- tmpVec
      } else {
        vecSelected <- intersect(vecSelected, tmpVec)
      }
    }
  }
  
  vecSelected <- vecSelected[!is.na(vecSelected)]
  tibDat %>%
    filter({{idVarName}} %in% vecSelected)
}

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24)) 

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "COREpapers1",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite")))

ui <- fluidPage(

  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Search interface to CORE related papers (to end of 2021)")),
  p(paste0("This app uses the database of 721 papers, in English or Spanish, ",
           "and indexed before the end of 2021 that referenced or used elements ",
           "of the CORE system.")),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      shinyjs::useShinyjs(), # initialises shinyjs package of functions so can use reset button
      id = "side-panel",
      p(paste0("Search using entries in this sidebar, the impact on the numbers ",
               "can be seen in the graph on the right and the table below it.  To get ",
               "to more detail on the papers go to the tab titled 'DOIs and URLs' ",
               "where you have access to the DOI and/or URL to find the papers selected.  ",
               "There is a reset button at the bottom of this sidebar if you want to ",
               "start again with no selections set.")),
      sliderInput("date1",
                  "Date range: earliest date (inclusive)",
                  min = 1998,
                  max = 2021,
                  value = 1998,
                  step = 1,
                  round = TRUE,
                  sep = ""),
      sliderInput("date2",
                  "Date range: last date (inclusive)",
                  min = 1998,
                  max = 2021,
                  value = 2021,
                  step = 1,
                  round = TRUE,
                  sep = ""),
      
      radioButtons("therOrGen",
                   "Restrict papers by focus (or not)",
                   choices = c("Treatment", "General population", "Either"),
                   selected = "Either"),
      
      radioButtons("reqEmpCOREdata",
                   "Restrict papers with CORE instrument data",
                   choices = c("No", "Yes"),
                   selected = "No"),
      
      radioButtons("reqOA",
                   "Restrict to Open Access papers",
                   choices = c("No", "Yes"),
                   selected = "No"),
      
      radioButtons("reqOpenData",
                   "Restrict to papers with data available",
                   choices = c("No", "Yes"),
                   selected = "No"),
      
      radioButtons("paperLang",
                   "Restrict by language of paper",
                   choices = c("English",
                               "Spanish",
                               "Either"),
                   selected = "Either"),
      
      radioButtons("embedded",
                   "Was at least one CORE measure used embedded in the intervention",
                   choices = c("Not important to me", "Yes please!"),
                   selected = "Not important to me"),
      helpText(paste0("This refers to use of a CORE measure in 'ECM',",
                      "Embedded Change Management, or 'FIT': Feedback Informed Therapy")),
      
      radioButtons("filterCOREmeasures", 
                   "Do you want to filter by CORE instruments used? ('Yes' brings up dialogue for this.)",
                   choices = c("Yes" = "Yes",
                               "No" = "No"),
                   selected = "No"),
      
      conditionalPanel(condition = "input.filterCOREmeasures == 'Yes'",
                       radioButtons("or",
                                    "Use Boolean OR or AND across instruments",
                                    choices = c("OR", "AND"),
                                    selected = "OR"),
                       selectInput("vecWhichCOREused",
                                   "CORE instruments (select one or more)",
                                   vecCOREmeasures2,
                                   width = "100%",
                                   selectize = FALSE,
                                   size = 5,
                                   multiple = TRUE),
                       helpText("Shift click for more than one adjacent instrument, command click for non-adjacent ones on Apples, control click on other machines"),
      ),
      
      radioButtons("filterCORElanguages", 
                   "Do you want to filter by languages of CORE instruments used? ('Yes' brings up dialogue for this.)",
                   choices = c("Yes" = "Yes",
                               "No" = "No"),
                   selected = "No"),
      
      conditionalPanel(condition = "input.filterCORElanguages == 'Yes'",
                       radioButtons("or2",
                                    "Use Boolean OR or AND across languages",
                                    choices = c("OR", "AND"),
                                    selected = "OR"),
                       selectInput("vecCORElanguages",
                                   "Languages of CORE instruments (select one or more)",
                                   vecCORElanguages,
                                   width = "100%",
                                   selectize = FALSE,
                                   size = 5,
                                   multiple = TRUE),
                       helpText("Shift click for more than one adjacent language, command click for non-adjacent ones on Apples, control click on other machines"),
      ),
      
      radioButtons("filterAssStructure", 
                   "Do you want to filter by how CORE instrument used? ('Yes' brings up dialogue for this.)",
                   choices = c("Yes" = "Yes",
                               "No" = "No"),
                   selected = "No"),
      
      conditionalPanel(condition = "input.filterAssStructure == 'Yes'",
                       radioButtons("or3",
                                    "Use Boolean OR or AND across options",
                                    choices = c("OR", "AND"),
                                    selected = "OR"),
                       selectInput("vecAssStructure",
                                   "How CORE instrument(s) used (select one or more options)",
                                   vecAssStructure,
                                   width = "100%",
                                   selectize = FALSE,
                                   size = 5,
                                   multiple = TRUE),
                       helpText("Shift click for more than one adjacent option, command click for non-adjacent ones on Apples, control click on other machines"),
      ),
      
      radioButtons("filterGenderCats", 
                   "Do you want to filter by gender categories used? ('Yes' brings up dialogue for this.)",
                   choices = c("Yes" = "Yes",
                               "No" = "No"),
                   selected = "No"),
      
      conditionalPanel(condition = "input.filterGenderCats == 'Yes'",
                       radioButtons("or4",
                                    "Use Boolean OR or AND across categories",
                                    choices = c("OR", "AND"),
                                    selected = "OR"),
                       selectInput("vecGenderCats",
                                   "Gender categories used (select one or more)",
                                   vecGenderCats,
                                   width = "100%",
                                   selectize = FALSE,
                                   size = 5,
                                   multiple = TRUE),
                       helpText("Shift click for more than one adjacent category, command click for non-adjacent ones on Apples, control click on other machines"),
      ),
      
      radioButtons("filterFormats", 
                   "Do you want to filter by measure formats? ('Yes' brings up dialogue for this.)",
                   choices = c("Yes" = "Yes",
                               "No" = "No"),
                   selected = "No"),
      
      conditionalPanel(condition = "input.filterFormats == 'Yes'",
                       radioButtons("or5",
                                    "Use Boolean OR or AND across formats",
                                    choices = c("OR", "AND"),
                                    selected = "OR"),
                       selectInput("vecFormats",
                                   "Formats used (select one or more)",
                                   vecFormats,
                                   width = "100%",
                                   selectize = FALSE,
                                   size = 5,
                                   multiple = TRUE),
                       helpText("Shift click for more than one adjacent format, command click for non-adjacent ones on Apples, control click on other machines"),
      ),
      
      textInput("authName",
                "Text to search for in names of authors",
                value = ""),
      helpText("Matching is case insensitive, no wildcards"),
      
      
      textInput("otherMeasure",
                "Text to search for in names of any non-CORE instruments used",
                value = ""),
      helpText("Matching is case insensitive, no wildcards"),
      
      tags$hr(),
      helpText(paste0("This button will, as it says, reset all your selections above ",
                      "so you can start over again easily.")),
      actionButton("reset_input", "Reset inputs")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Papers",
                           value = 1,
                           plotOutput("mainPlot", height = 500),
                           
                           downloadGGPlotButtonUI("mainPlotDownload", "mainPlot"),
                           p(" "),
                           p("Thanks to Keith Newman for the download handler: ",
                             a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file")),
                           DT::dataTableOutput("papers"),
                           
                  ),
                  
                  
                  
                  tabPanel("DOIs and URLs",
                           value = 2,
                           
                           DT::dataTableOutput("papers2"),
                  ),
                  
                  tabPanel("Non-CORE measures",
                           value = 3,
                           
                           p("This shows the non-CORE measures used for papers selected where a non-CORE paper was used"),
                           p("You can search within the table with the search box"),
                           p(" "),
                           DT::dataTableOutput("otherMeasures"),
                  ),
                  
                  tabPanel("General background", 
                           value = 4,
                           
                           p("App started 10.v.24 by Chris Evans.",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 17.v.24."),
                           p("Licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           # includeHTML("https://shiny.psyctc.org/boilerplate.html")
                  ),
                  
                  id = "tabSelected"),
    ),
    
    
  ),
  
  textOutput("whichCOREused")
)

server <- function(input, output, session) {
  
  # cat(file=stderr(), "colnames:", colnames(tibDat), "\n")
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = FALSE) # 3. Track basics and inputs and input values
  
  ### reset button (requires shinyjs package)
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  })
  
  output$whichCOREused <- renderText({
    input$whichCOREused
  })
  
  reqOA <- reactive({
    if (input$reqOA == "Yes") {
      return("Yes")
    } else {
      return("No")
    }
  })
  
  reqOpenData <- reactive({
    if (input$reqOpenData == "Yes") {
      return("Yes")
    } else {
      return("No")
    }
  })
  
  reqEmbedded <- reactive({
    if (input$embedded == "Yes please!") {
      return("Yes")
    } else {
      return("No")
    }
  })
  
  orVal <- reactive({
    req(input$or)
    if(input$or == "OR") {
      TRUE
    } else {
      FALSE
    }
  })
  
  orVal2 <- reactive({
    req(input$or2)
    if(input$or2 == "OR") {
      TRUE
    } else {
      FALSE
    }
  })
  
  orVal3 <- reactive({
    req(input$or3)
    if(input$or3 == "OR") {
      TRUE
    } else {
      FALSE
    }
  })
  
  orVal4 <- reactive({
    req(input$or4)
    if(input$or4 == "OR") {
      TRUE
    } else {
      FALSE
    }
  })
  
  orVal5 <- reactive({
    req(input$or5)
    if(input$or5 == "OR") {
      TRUE
    } else {
      FALSE
    }
  })
  
  data <- reactive({
    validate(
      need(input$date2 >= input$date1, "Second date must be greater or same as first")
    )
    
    ### now filter by what CORE instruments used
    if (input$filterCOREmeasures == "Yes" & length(input$vecWhichCOREused) > 0) {
      findMatches(tibDat, 
                  WhichCOREInstruments, 
                  shinyID, 
                  input$vecWhichCOREused, 
                  or = orVal()) -> tibDat
    } 
    
    if (input$filterCORElanguages == "Yes" & length(input$vecCORElanguages) > 0) {
      findMatches(tibDat, 
                  LanguageOfCORE, 
                  shinyID, 
                  input$vecCORElanguages, 
                  or = orVal2()) -> tibDat
    } 
    
    if (input$filterAssStructure == "Yes" & length(input$vecAssStructure) > 0) {
      findMatches(tibDat, 
                  AssessmentStructure, 
                  shinyID, 
                  input$vecAssStructure, 
                  or = orVal3()) -> tibDat
    } 
    
    if (input$filterGenderCats == "Yes" & length(input$vecGenderCats) > 0) {
      findMatches(tibDat, 
                  GenderCategories, 
                  shinyID, 
                  input$vecGenderCats, 
                  or = orVal4()) -> tibDat
    } 
    
    if (input$filterFormats == "Yes" & length(input$vecFormats) > 0) {
      findMatches(tibDat, 
                  FormatOfMeasure, 
                  shinyID, 
                  input$vecFormats, 
                  or = orVal5()) -> tibDat
    } 
    
    
    ### now filter by year
    tibDat %>%
      filter(Year2021Num >= input$date1 & Year2021Num <= input$date2) -> tibDat
    
    ### therapy or general population or either
    if(input$therOrGen != "Either") {
      tibDat %>%
        filter(TherOrGeneral == input$therOrGen) -> tibDat
    }
    
    ### now filter by OA or not
    if(reqOA() == "Yes") {
      tibDat %>%
        filter(OpenAccess == "Y") -> tibDat
    }
    
    ### now filter by OD or not
    if(reqOpenData() == "Yes") {
      tibDat %>%
        filter(DataAvailable == "Y") -> tibDat
    }
    
    if (input$reqEmpCOREdata == "Y") {
      tibDat %>%
        filter(Empirical == "Empirical") -> tibDat
    }
    
    if(reqEmbedded() == "Yes") {
      tibDat %>%
        filter(EmbeddedInTherapy == "Y") -> tibDat
    }
    
    ### filter by paper language
    if(input$paperLang != "Either") {
      tibDat %>%
        filter(PaperLanguage %in% input$paperLang) -> tibDat
    }
    
    ### filter for author name
    if (input$authName != "") {
      tmp <- str_to_lower(input$authName)
      tibDat %>%
        filter(str_detect(Author, fixed(tmp))) -> tibDat
    }
    
    ### filter for author name
    if (input$otherMeasure != "") {
      tmp <- str_to_lower(input$otherMeasure)
      tibDat %>%
        filter(str_detect(OtherMeasureNamesLwr, fixed(tmp))) -> tibDat
    }
    
    return(tibDat)
  })
  
  tibPaperDat <- reactive({
    if(nrow(data()) > 0) {
      data() %>%
        select(cite, DOI, URL)
    } else {
      tibble(ref = "No papers fit these requirements")
    }
  })
  
  tibPaperRefs <- reactive({
    if(nrow(data()) > 0) {
      data() %>%
        select(ref)
    } else {
      tibble(ref = "No papers fit these requirements")
    }
  })
  
  tibOtherMeasures <- reactive({
    if(nrow(data()) > 0) {
      data() %>%
        select(ref, OtherMeasureNames) %>%
        filter(OtherMeasureNames != "")
    } else {
      tibble(ref = "No papers fit these requirements")
    }
  })
  
  output$papers <- DT::renderDataTable(
    DT::datatable({tibPaperRefs()},
                  extensions = "Buttons",
                  options = list(                                                     
                    fixedColumns = FALSE,
                    pageLength = 10,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'frtip',
                    editable = FALSE,
                    searching = TRUE
                  )
    )
  )
  
  output$papers2 <- DT::renderDataTable(
    DT::datatable({tibPaperDat()},
                  extensions = "Buttons",
                  escape = FALSE,
                  options = list(                                                     
                    fixedColumns = FALSE,
                    pageLength = 50,
                    ### apparently you have to set autoWidth TRUE to control widths
                    autoWidth = TRUE,
                    ### this is two nested lists, the targets include the row numbers
                    ### and are indexed from zero
                    columnDefs = list(list(width = '300px', targets = c(1, 3))),
                    ### this next seemed to be necessary to get any control over widths
                    scrollX = TRUE,
                    ordering = TRUE,
                    dom = 'frtipB',
                    editable = FALSE,
                    searching = TRUE,
                    buttons = c('copy', 'csv', 'excel', "pdf")
                  )
    )
  )  
  
  output$otherMeasures <- DT::renderDataTable(
    DT::datatable({tibOtherMeasures()},
                  extensions = "Buttons",
                  escape = FALSE,
                  options = list(                                                     
                    fixedColumns = FALSE,
                    pageLength = 50,
                    ### apparently you have to set autoWidth TRUE to control widths
                    autoWidth = TRUE,
                    ### this is two nested lists, the targets include the row numbers
                    ### and are indexed from zero
                    columnDefs = list(list(width = '350px', targets = c(1, 2))),
                    ### this next seemed to be necessary to get any control over widths
                    scrollX = TRUE,
                    ordering = TRUE,
                    dom = 'frtipB',
                    editable = FALSE,
                    searching = TRUE,
                    buttons = c('copy', 'csv', 'excel', "pdf")
                  )
    )
  )  
  
  ### the basic plot against years
  makePlot <- function(data, date1, date2) {
    ggplot(data = data,
           aes(x = Year2021Num)) +
      geom_bar() +
      geom_vline(xintercept = date1,
                 colour = "red") +
      geom_vline(xintercept = date2,
                 colour = "red") +
      scale_x_continuous("Years",
                         breaks = 1998:2021,
                         limits = c(1998, 2021)) +
      scale_y_continuous("No",
                         breaks = seq(0, 100, 10),
                         limits = c(0, 100)) +
      ggtitle("Your selection so far",
              subtitle = paste0(nrow(data),
                                " papers of the 721 selected so far")) +
      theme(axis.text.x = element_text(angle = 80,
                                       hjust = 1)) -> p
    suppressWarnings(print(p))
  }
  
  mainPlot <- reactive({
    makePlot(data(), input$date1, input$date2)
  })
  
  output$mainPlot <- renderPlot({
    mainPlot()
  })
  
  downloadGGPlotButtonServer(
    id = "mainPlotDownload", # <= this should match the ID used in the UI module
    ggplotObject = mainPlot, # No parentheses here to pass *expression*
    width = input$fileWidth,
    height = input$fileHeight
  )
}

shinyApp(ui, server)

### todo

### perhaps
# ? ServiceType
# ? Ages
# ? foci

### cosmetics/UE
# ?? popup abstract?
# ?? clickable tiles in plot?
