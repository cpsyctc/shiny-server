### random1
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(tibble))
suppressMessages(library(CECPfuns))

### telemetry 1. Initialize telemetry with default options (store to a local sqlite database)
telemetry <- Telemetry$new(app_name = "random1",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # telemetry 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Random permutation of numbers"), 
     align = "center"),
  HTML(paste0("This app is intended to help people permute a set of n objects,",
              " i.e. to get the same numbers but rearranged, randomly, into another order.",
              " You set the number of objects and the seed to the random number generator. ",
              " The seed determines the particular permuatation you will get.",
              " (If that is unfamiliar to you, just leave the seed as '12345'.)",
              " When you have put in the number of objects and perhaps the seed",
              " that you want, hit the 'Do the permutation' button at the bottom",
              " and the results will appear in the 'Results' tab on the right.")),
  p(" "),
  p(" "),
  HTML(paste0("I have written this to support people who want to use the method of",
              " derangements which starts with a randomly permuted set of n objects",
              " that the judge is asked to map back to their origins.  See the",
              " Explanation/information tab for more on the method of derangements.")),
  p(" "),
  p(" "),
  hr(),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Replace the values here with your own", align="center"),
      helpText(paste0("Put the number of objects to permute here.")),
      numericInput("valN",
                   "The number of objects you want to permute",
                   value = 7,
                   min = 4,
                   max = 10^5,
                   width="100%"),
      numericInput("valSeed",
                   "The seed for the random number generator",
                   value = 12345,
                   min = 0,
                   max = 10^5,
                   width="100%"),
      helpText("When you have filled in the above, hit this next button to get the RCI!"),
      actionButton("compute", "Do the permutation!")
    ),
    
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Result", 
                           h3("Result"),
                           p(HTML("Here is a little table with your input vector of numbers and the permuted numbers.")),
                           p("As I hope you can see, the buttons at the bottom of the table allow you to export the data."),
                           DT::dataTableOutput("dataTable")
                  ),
                  
                  tabPanel("Explanation/information", 
                           h3("The method of derangements"),
                           p("This is a little utility app to support people using the method of derangements.",
                                " The method is a rigorous method of testing whether purely idiographic data, i.e.",
                                " data specific perhaps to just one one person such as projective tests, repertory",
                                " grids, can be shown to convey valid information to a judge. The method was first",
                                " presented in",
                                " Evans, C., Hughes, J., & Houston, J. (2002). Significance testing the validity",
                                " of ideographic methods: A little derangement goes a long way.", 
                                " British  Journal of  Mathematical and Statistical Psychology, 55(2), 385–390. ",
                                a(" https://doi.org/10.1348/000711002760554525.",
                                  href = "https://doi.org/10.1348/000711002760554525"),
                                "That's not open access ",
                                a(" so contact me", href = "https://www.psyctc.org/psyctc/contact-me/"),
                                " if you want a copy.)"),
                           p(" "),
                           p("A more comprehensive and less specialist account of the method is given in:",
                                " Evans, C., Carlyle, J., & Paz, C. (2023). Rigorous idiography: Exploring subjective ",
                                " and idiographic data with rigorous methods—The method of derangements. ",
                                " Frontiers in Psychology, 13, 1007685.",
                                a(" https://doi.org/10.3389/fpsyg.2022.1007685", href = "https://doi.org/10.3389/fpsyg.2022.1007685"),
                                " (OA)."),
                           p(" "),
                           p("A graphical summary of the method is at",
                                a("https://www.psyctc.org/psyctc/2022/07/23/sometimes-n4-is-enough/", 
                                  href = "https://www.psyctc.org/psyctc/2022/07/23/sometimes-n4-is-enough/")),
                           p(" "),
                           p("The method involves asking a judge to map from four or more idiographic items",
                                " back to their origins, for example, the data might be repertory grids from", 
                                " members of a therapy grid and the judge would be someone who knew the clients,", 
                                " for example the group therapist/conductor as in the original paper.  The grids",
                                " presented in random order ('permuted') and this app gives you random permutations",
                                " of n numbers 1 to n and gives a citable source for your permutation (giving the",
                                " seed you used) if submitting use of the method of derangements to a journal."),
                           p(" "),
                           p(" "),
                           h3("Computation background"),
                           p(HTML("This uses the base R function <em>sample()</em>")),
                           p("Unless you are very familiar with them, do please now read the 'Attribution/citation' and 'Background' tabs.")
                  ),
                  
                  tabPanel("Attribution/citation", 
                           h3("Attribution",
                              align = "center"),
                           p("App created by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/"),
                             "licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           h3("Citing and referencing",
                              align = "center"),
                           p("I really am not keen on the obsessionalities of style guides but I do think referencing well is important",
                             "so here is my understanding of the correct citing and referencing of the app."),
                           h4("APA"),
                           p("I think the correct citation in APA 7 format is just (Evans, 2024) and the reference format is:\n\n"),
                           p("Evans, C. (2024, November 16). ",
                             em("Random permutation of numbers."),
                             "PSYCTC.org. Retrieved November 16, 2024, from ",
                             a("https://shiny.psyctc.org/apps/random1/",
                               href="https://shiny.psyctc.org/apps/random1/")),
                           p("Where the name of the app, i.e. 'Random permutation of numbers' should be in italics.",
                             "(Hard to force that here in HTML!)"),
                           h4("BPS"),
                           p("I am sure the correct citation in BPS format is again just (Evans, 2024) and I",
                             em("think"),
                             "the BPS reference format is:\n\n"),
                           p("Evans, C. (2024). Random permutation of numbers. PSYCTC.org.  Retrieved November 16, 2024, from ",
                             a("https://shiny.psyctc.org/apps/random1/",
                               href="https://shiny.psyctc.org/apps/random1/")),
                           p("Where the name of the app is ",
                             em("not"), 
                             "in italics."),
                           p("But, as I say, I am no expert on these style guide obsessionalities:",
                             "as with everything in shiny.psyctc.org, use at your own risk and do sensible checking.")),
                  
                  tabPanel("Background",
                           
                           includeHTML("https://shiny.psyctc.org/boilerplate.html"))
      ),
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
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # telemetry 3. Track basics and inputs and input values
  
  ### 
  ### start with validation functions
  ###
  
  checkIsInteger <- function(x) {
    isTRUE(all.equal(x, round(x), tolerance = sqrt(.Machine$double.eps)))
  }
  checkIsGtrThan <- function(x, y) {
    x > y
  }
  checkIsGtrOrEq <- function(x, y) {
    x >= y
  }
  
  ### 
  ### now the functions
  ###
  getPermutation <- function(valN, seed) {
    set.seed(seed)
    vecStart <- 1 : valN
    valLength <- length(vecStart)
    vecPerm <- sample(vecStart, valLength, replace = FALSE)
    tibble(Original = vecStart,
           Permuted = vecPerm)
  }
  
  output$dataTable <- DT::renderDataTable(
    DT::datatable({as_tibble(tibPermed())},
                  extensions = "Buttons",
                  options = list(                                                     
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'frtipB',
                    editable = FALSE,
                    searching = FALSE,
                    buttons = c('copy', 'csv', 'excel', "pdf"))
    )
  )
  
  tibPermed <- eventReactive(input$compute, {
    validate(
      ### values must exist
      need(input$valN, "You must give a usable value for valHighest"),
      need(input$valSeed, "You must give a usable value for seed"),
      ### check values
      need(checkIsInteger(input$valN),
           "The number of objects must be an integer greater than 3"),
      need(checkIsGtrThan(input$valN, 3),
           "The number of objects must be an integer greater than 3"),
      need(checkIsInteger(input$valSeed),
           "The number to set the random number generator must be an integer"),
    )
    getPermutation(input$valN,
                   input$valSeed)
  })
  
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
