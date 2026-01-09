### Forest_plot_rates
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
telemetry <- Telemetry$new(app_name = "Forest_plot_rates",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite")))

# Define UI for data upload app ----
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("App that creates a forest plot of rates")),
  
  p("This app simply creates an annotated forest plot of rates for count data entered by file upload."),
  p("You input data using the sidebar on the left to select the file format you are using and then to",
    "choose the file to upload. You should see a file upload dialogue for that.",
    "If you don't immediately have data of your own you want to input you can use ",
    a("TMN_counts.csv", href="https://www.psyctc.org/psyctc/wp-content/uploads/2026/01/TMN_counts.csv"),
    ".",
    a("TMN_counts_x20.csv", href="https://www.psyctc.org/psyctc/wp-content/uploads/2026/01/TMN_counts_x20.csv"),
    ".",
    "and",
    a("TMN_counts_sim.csv", href="https://www.psyctc.org/psyctc/wp-content/uploads/2026/01/TMN_counts_sim.csv"),
    ".",
    "to give you a sense of what the app is doing."),
    p("See ",
    a("https://www.psyctc.org/psyctc/root/stats/datasets/",
      href = "https://www.psyctc.org/psyctc/root/stats/datasets/"),
    "for more information about these and other datasets."),
  
  p("You may need to use some of the other sidebar inputs depending on your particular file format."),
  
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
                  tabPanel("Data", 
                           p(" "),
                           p("This shows all the data input."),
                           p(" "),
                           dataTableOutput(outputId = "rawData")),
                  
                  tabPanel("Forest plot", 
                           p("This shows a forest plot of your rates.",
                           "You can use this next dialogue to select where the annotation should be placed ",
                           "and you can use the dialogue below the plot to download it chosing the name and the format of the plot."),
                           p(" "),
                           uiOutput("annotationPosn"),
                           plotOutput("plot"),
                           downloadGGPlotButtonUI("plotDownload", "this-plot"),
                           p(" "),
                           p("Thanks to Keith Newman for the download handler: ",
                             a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file"))),
                  
                  
                  
                  tabPanel("Background", 
                           p("App created 8.i.26 by Chris Evans",
                             a("PSYCTC.org", href="https://www.psyctc.org/psyctc/about-me/")),
                           p("It came from thinking about the ",
                             a("Does brief therapy yield better outcomes?",
                               href="https://therapymeetsnumbers.com/does-brief-therapy-yield-better-outcomes/"),
                             " post in the excellent ",
                             a("Therapy Meets Numbers",
                               href="https://therapymeetsnumbers.com/"),
                             " website."),
                           p("The Fisher test p value is computed by Monte Carlo simulation ",
                             "allowing large tables and large total n to be handled"),
                           
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
  
  dataInput <- reactive({
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
  
  
  output$annotationPosn <- renderUI({
    radioButtons("textPosn", 
                 "Position for annotation",
                 choices = c("Top right" = "topRight",
                             "Bottom right" = "btmRight",
                             "Top left" = "topLeft",
                             "Bottom left" = "btmLeft"),
                 selected = 'topRight')
  })
  
  ### process the data
  tibDat <- reactive({
    req(input$dataType)
    dataInput() %>%
      rowwise() %>%
      mutate(improvedCI = list(Hmisc::binconf(nImproved, n)[1, ])) %>%
      ungroup() %>%
      unnest_wider(improvedCI) %>%
      rename(improvedEst = PointEst,
             improvedLCL = Lower,
             improvedUCL = Upper) #-> tibDat
  })

  output$rawData <- renderDataTable({
    req(input$file1)
    validate(
      need(colnames(dataInput()) == c("Year", "n", "nImproved"), 
           "Your data can only have variable names 'Year', 'n' and 'nImproved'.  You will  have to fix that."))
    dataInput()
  })
  
  forestPlot <- reactive({
    req(input$textPosn)
    ### start to build the annotation
    tibDat() %>% 
      select(n, nImproved) %>% 
      as.matrix() -> matImproved
    
    chisq.test(matImproved) -> lisChisqImproved 
    
    fisher.test(matImproved, 
                simulate.p.value = TRUE) -> lisFisherImproved
    
    tibDat() %>%
      summarise(totN = sum(n),
                totNimproved = sum(nImproved)) %>%
      mutate(improvedProp = totNimproved / totN) -> tibSummary
    
    str_c("Chisq = ",
          round(lisChisqImproved$statistic, 2),
          ", d.f. = ",
          lisChisqImproved$parameter,
          ", p = ",
          round(lisChisqImproved$p.value, 3)) -> txtAnnotate1
    str_c("Fisher test (2-sided), p = ",
          round(lisFisherImproved$p.value, 2)) -> txtAnnotate2
    
    str_c(txtAnnotate1,
          "\n",
          txtAnnotate2) -> txtAnnotate
    
    ### allow for relative positioning
    tibDat() %>% 
      select(Year) %>%
      summarise(min = min(Year, na.rm = TRUE)) %>%
      pull() -> xMin
    
    tibDat() %>% 
      select(Year) %>%
      summarise(max = max(Year, na.rm = TRUE)) %>%
      pull() -> xMax
    
    posnProp <- .95

    if (input$textPosn == "topRight") {
      xPos <- xMin + ((xMax - xMin) * posnProp)
      yPos <- posnProp
      vJust <- 1
      hJust <- 1
    }
    if (input$textPosn == "btmRight") {
      xPos <- xPos <- xMin + ((xMax - xMin) * posnProp)
      yPos <- 1 - posnProp
      vJust <- 0
      hJust <- 1
    }
    if (input$textPosn == "topLeft") {
      xPos <- xMin + ((xMax - xMin) * (1 - posnProp))
      yPos <- posnProp
      vJust <- 1
      hJust <- 0
    }
    if (input$textPosn == "btmLeft") {
      xPos <- xMin + ((xMax - xMin) * (1 - posnProp))
      yPos <- 1 - posnProp
      vJust <- 0
      hJust <- 0
    }
    
    ### this deals with trivial out of range CLs
    tibDat() %>% 
      mutate(improvedLCL = if_else(improvedUCL < 0,
                                   0,
                                   improvedLCL),
             improvedUCL = if_else(improvedUCL > 1,
                                   1,
                                   improvedUCL)) -> tibDat
    
    ggplot(data = tibDat, #() using the internal tibDat not the reactive
           aes(x = Year,
               y = improvedEst)) +
      geom_point() +
      geom_linerange(aes(ymin = improvedLCL,
                         ymax = improvedUCL)) +
      geom_hline(data = tibSummary,
                 aes(yintercept = improvedProp),
                 linetype = 3) +
      annotate("text",
               x = xPos,
               y = yPos,
               label = txtAnnotate,
               vjust = vJust,
               hjust = hJust) +
      xlab("Year") +
      # coord_cartesian(ylim = c(0, 1),
      #                 expand = FALSE) +
      ylim(0, 1) +
      ylab("Proportion improved") +
      ggtitle("Forest plot of proportion improved by year",
              subtitle = "Error bars are 95% CIs, reference line is overall rate")
  })
  
  output$plot <- renderPlot({
    forestPlot()
  })
  
  downloadGGPlotButtonServer(
    id = "plotDownload", # <= this should match the ID used in the UI module
    ggplotObject = forestPlot # No parentheses here to pass *expression*
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)