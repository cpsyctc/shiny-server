### CSClookup2a
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "CSClookup2a",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

vecGender <- c("Female", 
               "Male")
vecScoring <- c("Item mean (range 0-4)", 
                "Clinical (10x item mean, range 0-40)")
vecLookup <- c("Blackshaw PhD (UK)", 
               "Twigg et al., 2016 (UK & Ireland)", 
               "Di Biase et al., 2021 (Italy)")

### create the internal lookup table
tribble(~Ref, ~Age,  ~Gender,  ~CSC,
        "Blackshaw PhD (UK)", 11, "M", 1.0,
        "Blackshaw PhD (UK)", 12, "M", 1.0,
        "Blackshaw PhD (UK)", 13, "M", 1.0,
        "Blackshaw PhD (UK)", 14, "M", 1.3,
        "Blackshaw PhD (UK)", 15, "M", 1.4,
        "Blackshaw PhD (UK)", 16, "M", 1.5,
        "Blackshaw PhD (UK)", 11, "F", 1.1,
        "Blackshaw PhD (UK)", 12, "F", 1.5,
        "Blackshaw PhD (UK)", 13, "F", 1.4,
        "Blackshaw PhD (UK)", 14, "F", 1.6,
        "Blackshaw PhD (UK)", 15, "F", 1.6,
        "Blackshaw PhD (UK)", 16, "F", 1.5,
        "Twigg et al., 2016 (UK & Ireland)", 11, "M", 1.03,
        "Twigg et al., 2016 (UK & Ireland)", 12, "M", 1.03,
        "Twigg et al., 2016 (UK & Ireland)", 13, "M", 1.03,
        "Twigg et al., 2016 (UK & Ireland)", 14, "M", 1.41,
        "Twigg et al., 2016 (UK & Ireland)", 15, "M", 1.41,
        "Twigg et al., 2016 (UK & Ireland)", 16, "M", 1.41,
        "Twigg et al., 2016 (UK & Ireland)", 11, "F", 1.44,
        "Twigg et al., 2016 (UK & Ireland)", 12, "F", 1.44,
        "Twigg et al., 2016 (UK & Ireland)", 13, "F", 1.44,
        "Twigg et al., 2016 (UK & Ireland)", 14, "F", 1.59,
        "Twigg et al., 2016 (UK & Ireland)", 15, "F", 1.59,
        "Twigg et al., 2016 (UK & Ireland)", 16, "F", 1.59,
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

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css for head blocks
  tags$head(
    tags$style(
      ".title {margin: auto; align: center}"
    )
  ),
  tags$div(class="title", titlePanel("Getting the correct CSC for a YP-CORE score by age and gender\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones", align="center"),
      radioButtons("Gender",
                   "Gender, 'M' or 'F' only so far as we do not have referential data for non-binary genders",
                   vecGender),
      numericInput("Age",
                   "Age of the person completing the YP-CORE (11 to 16 only currently)",
                   value = 14,
                   min = 11,
                   max = 16,
                   width="100%"),
      numericInput("YPscore",
                   "The YP-CORE score",
                   value = .7,
                   min = 0,
                   max = 40,
                   width="100%"),
      radioButtons("Scoring",
                   "Which scoring system was used for that YP-CORE score?",
                   vecScoring),
      radioButtons("Lookup",
                   "Which lookup data do you want to use for the CSC?",
                   vecLookup)
    ),
    
    mainPanel(
      h3("Your input and results",align="center"),
      verbatimTextOutput("res"),
      p("This incredibly basic shiny app uses logic from the function lookupCSCgenderAndAge() from my package",
        a("CECPfuns", href="https://github.com/cpsyctc/CECPfuns"),
        "to take a YP-CORE score, scoring system (mean or 'clinical'), gender and age to get the correct CSC for that age and gender.",
        "There is an explanation in my ",
        a("Rblog",  href = "https://www.psyctc.org/Rblog/"),
        "post about ",
        a("CSC lookups", href = "https://www.psyctc.org/Rblog/posts/2024-01-21-lookupcsc2/"),
        ".  This trivial app should be complemented by one using the full power of the lookupCSCgenderAndAge() to ",
        "handle datasets of age, gender and score data ... however, I need to sort out data uploading to achieve that!"),
      p("App created by Chris Evans",
        a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/"),
        "licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href="http://creativecommons.org/licenses/by-sa/1.0/"),
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
      hr(),
      includeHTML("https://shiny.psyctc.org/boilerplate.html")
    )
  )
)


# Define server logic required
### this is the standard shiny server constructor
server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # 3. Track basics and inputs and input values
  
  ### 
  ### start with validation functions
  ### I don't think I actually use any these as I've now used numericInput() to set the ranges
  
  ### 
  ### now the functions adapted from CECPfuns plotCIcorrelation
  ###
  getCSC <- function(YPscoreInp, GenderInp, AgeInp, Scoring, Lookup) {
    GenderInp1 <- str_sub(GenderInp, 1, 1)
    tibLookup %>%
    #   mutate(CSC = if_else(Scoring == "Clinical (10x item mean, range 0-40)",
    #                        CSC = 10 * CSC, CSC)) %>%
      filter(Ref == Lookup) %>%
      filter(Gender == GenderInp1) %>%
      filter(Age == AgeInp) %>%
      select(CSC) %>%
      pull() -> CSC
    

    if (YPscoreInp >= CSC) {
      CSCcategory <- "High"
    } else {
      CSCcategory <- "Low"
    }
    
    retText <- paste0("Given:\n",
                      "   YP-CORE score: ", YPscoreInp,"\n",
                      "   Scoring: ", Scoring, "\n",
                      "   Gender:", GenderInp, "\n",
                      "   Age: ", AgeInp, "\n",
                      "   and lookup to use: ", Lookup, ".\n",
                      " Then the appropriate CSC is: ", CSC, "\n",
                      " and so that score is: ", CSCcategory)
    return(retText)
  }
  
  output$res <- renderText({
    validate(
      need(10 < input$Age,
           "Currently lookup data only exist for ages from 11 to 16 inclusive"),
      need(input$Age < 17,
           "Currently lookup data only exist for ages from 11 to 16 inclusive"),
      need(checkScore(input$YPscore, input$Scoring),
           "YP-CORE score is impossible for that scoring system")
    )
    getCSC(input$YPscore,
           input$Gender,
           input$Age,
           input$Scoring,
           input$Lookup)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
