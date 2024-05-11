### COREpapers1

suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))
# suppressMessages(library(CECPfuns))

if(interactive()) {
  # setwd("/media/chris/Clevo_SSD2/Data/MyR/shiny.psyctc.org/apps/COREpapers1") 
  suppressMessages(read_csv("tibDat.csv",
                            progress = FALSE)) -> tibDat
} else {
  suppressMessages(read_csv("./apps/COREpapers1/tibDat.csv",
                            progress = FALSE)) -> tibDat
}

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

findMatches <- function(tibDat, searchVarName, idVarName, vecMatches, or = TRUE) {
  ### function to find matches to members of vecMatches in tibDat$varname
  ### can do it as Boolean OR or AND
  # tibDat %>%
  #   select({{idVarName}}, {{searchVarName}} ) -> tmpDat
  
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
  
  # useShinyFeedback(), # include shinyFeedback
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Search interface to CORE related papers (to end of 2021)")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      sliderInput("date1",
                  "Earliest date (inclusive)",
                  min = 1998,
                  max = 2021,
                  value = 1998,
                  step = 1,
                  round = TRUE,
                  sep = ""),
      sliderInput("date2",
                  "Latest date (inclusive)",
                  min = 1998,
                  max = 2021,
                  value = 2021,
                  step = 1,
                  round = TRUE,
                  sep = ""),
      
      radioButtons("reqEmpCOREdata",
                   "Restrict papers with CORE instrument data",
                   choices = c("N", "Y"),
                   selected = "N"),
      
      radioButtons("reqOA",
                   "Restrict to Open Access papers",
                   choices = c("N", "Y"),
                   selected = "N"),
      
      radioButtons("reqOpenData",
                   "Restrict to papers with data available",
                   choices = c("N", "Y"),
                   selected = "N"),
      
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
                   "Do you want to filter by CORE instruments used?",
                   choices = c("Y" = "Y",
                               "N" = "N"),
                   selected = "N"),
      
      conditionalPanel(condition = "input.filterCOREmeasures == 'Y'",
                       radioButtons("or",
                                    "Use Boolean OR or AND across instruments",
                                    choices = c("OR", "AND"),
                                    selected = "OR"),
                       
                       checkboxGroupInput("vecWhichCOREused",
                                          "Paper must have used this/these CORE instruments (unselect to choose)",
                                          vecCOREmeasures2,
                                          vecCOREmeasures2),
      ),
      
      radioButtons("filterCORElanguages", 
                   "Do you want to filter by languages of CORE instruments used?",
                   choices = c("Y" = "Y",
                               "N" = "N"),
                   selected = "N"),
      
      conditionalPanel(condition = "input.filterCORElanguages == 'Y'",
                       radioButtons("or2",
                                    "Use Boolean OR or AND across instruments",
                                    choices = c("OR", "AND"),
                                    selected = "OR"),
                       
                       checkboxGroupInput("vecCORElanguages",
                                          "Paper must have used this/these CORE instruments (unselect to choose)",
                                          vecCORElanguages,
                                          vecCORElanguages),
      ),
      
      textInput("authName",
                "Text to search for in names of authors",
                value = ""),
      helpText("Matching is case insensitive, no wildcards")
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
                  
                  
                  
                  tabPanel("Details",
                           value = 6,
                           
                           p("I am increasingly convinced that percentiles, quantiles, are neglected in our field and more informative than means and SDs."),
                           
                  ),
                  
                  tabPanel("General background", 
                           value = 7,
                           
                           p("App started 10.v.24 by Chris Evans.",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 10.v.24."),
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
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = FALSE) # 3. Track basics and inputs and input values
  
  output$whichCOREused <- renderText({
    input$whichCOREused
  })
  
  reqOA <- reactive({
    if (input$reqOA == "Y") {
      return("Y")
    } else {
      return("N")
    }
  })
  
  reqOpenData <- reactive({
    if (input$reqOpenData == "Y") {
      return("Y")
    } else {
      return("N")
    }
  })
  
  reqEmbedded <- reactive({
    if (input$embedded == "Yes please!") {
      return("Y")
    } else {
      return("N")
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
  
  data <- reactive({
    validate(
      need(input$date2 >= input$date1, "Second date must be greater or same as first")
    )
    
    ### now filter by what CORE instruments used
    if (input$filterCOREmeasures == "Y" & length(input$vecWhichCOREused) > 0) {
      # cat(file=stderr(), "input$or is:", input$or, "\n")
      # # cat(file=stderr(), "orVal() is:", orVal, "\n")
      # cat(file=stderr(), "input$vecWhichCOREused is:", input$vecWhichCOREused, "\n")
      findMatches(tibDat, 
                  WhichCOREInstruments, 
                  shinyID, 
                  input$vecWhichCOREused, 
                  or = orVal()) -> tibDat
    } 
    
    if (input$filterCORElanguages == "Y" & length(input$vecCORElanguages) > 0) {
      # cat(file=stderr(), "input$or is:", input$or, "\n")
      # # cat(file=stderr(), "orVal() is:", orVal, "\n")
      # cat(file=stderr(), "input$vecWhichCOREused is:", input$vecWhichCOREused, "\n")
      findMatches(tibDat, 
                  LanguageOfCORE, 
                  shinyID, 
                  input$vecCORElanguages, 
                  or = orVal2()) -> tibDat
    } 
    
    ### now filter by year
    tibDat %>%
      filter(Year2021Num >= input$date1 & Year2021Num <= input$date2) -> tibDat
    
    ### now filter by OA or not
    if(reqOA() == "Y") {
      tibDat %>%
        filter(OpenAccess == "Y") -> tibDat
    }
    
    ### now filter by OD or not
    if(reqOpenData() == "Y") {
      tibDat %>%
        filter(DataAvailable == "Y") -> tibDat
    }
    
    if (input$reqEmpCOREdata == "Y") {
      tibDat %>%
        filter(Empirical == "Empirical") -> tibDat
    }
    
    if(reqEmbedded() == "Y") {
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
    
    return(tibDat)
  })
  
  tibPaperDat <- reactive({
    if(nrow(data()) > 0) {
      data() %>%
        select(ref, DOI, URL)
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
  
  output$papers <- DT::renderDataTable(
    DT::datatable({tibPaperRefs()},
                  extensions = "Buttons",
                  options = list(                                                     
                    fixedColumns = FALSE,
                    pageLength = 10,
                    autoWidth = TRUE,
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
      scale_y_continuous("n",
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
# sort out shinyID
# different listings in different tabs
# additional filtering
