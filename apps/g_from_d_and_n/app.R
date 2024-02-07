### g_from_d_and_n
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(esc))
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "g_from_d_and_n",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 


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
  tags$div(class="title", titlePanel("Getting Hedges's g from Cohen's d and n\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones", align="center"),
      numericInput("d",
                   "Reported Cohen's d value",
                   value = .7,
                   min = -50,
                   max = 50,
                   width="100%"),
      numericInput("n",
                   "Total n, (zero or positive integer)",
                   value = 100,
                   min = 0,
                   max = 10^9,
                   width="100%"),
    numericInput("dp",
                 "Number of decimal places",
                 value = 2,
                 min = 0,
                 max = 5,
                 width="100%")
  ),
  
  mainPanel(
    h3("Your input and results",align="center"),
    verbatimTextOutput("res"),
    p("This incredibly basic shiny app uses the function hedges_g() from the package",
      a("esc", href="https://strengejacke.github.io/esc"),
      "with thanks to its author ",
      a("Daniel Lüdecke", href="https://orcid.org/0000-0002-8895-3206"),
      ".  I created it partly to complement my ",
      a("Rblog",  href = "https://www.psyctc.org/Rblog/"),
      "post about ",
      a("Hedges's g", href = "https://www.psyctc.org/Rblog/posts/2024-01-19-hedgess-g/"),
      "."),
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
  getG <- function(d, n, dp = 2) {
    g <- esc::hedges_g(d, n)
    retText <- paste0("Given:\n",
                      "   d = ", d,"\n",
                      "   n = ", n,"\n",
                      "   g is ", round(g, dp),"\n\n")
    return(retText)
  }
  
  output$res <- renderText({
    # validate(
    #   need(checkForPosInt(input$n, minInt = 0), 
    #        "n must be a positive integer > 10 and < 10^9"),
    #   need(checkNumRange(input$R, minx = -1, maxx = 1, incEq = TRUE),
    #        "R must be a value >= -1.0 and <= 1.0"),
    #   need(checkNumRange(input$ci, minx = .69999, maxx = 1, incEq = FALSE),
    #        "ci must be a value > .7 and < .99"),
    #   need(checkForPosInt(input$dp, minInt = 1, maxInt = 5),
    #        "dp must be a value between 1 and 5")
    # )
    getG(input$d,
         input$n,
         input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
