### RCI
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))

# Define UI for application that does the work
ui <- fluidPage(
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css for head blocks
  tags$head(
    tags$style(
      ".title {margin: auto; align: center}"
    )
  ),
  tags$div(class="title", titlePanel("Compute RCI from SD and reliability (and inclusion interval if not 95%)\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      p("This shiny app is one of a growing number in ",
        a("my shiny server", href = "https://shiny.psyctc.org/"),
        "They complement (1) ",
        a("my Rblog", href = "https://www.psyctc.org/Rblog/index.html"),
        "of posts about using R, (2) the ",
        a("glossary", href = "https://www.psyctc.org/psyctc/book/glossary/"),
        "linked with ",
        a("the OMbook, ", href = "https://www.psyctc.org/psyctc/book/"),
        " and it's all part of the resources of (3)",
        a("PSYCTC.org", href = "https://www.psyctc.org/psyctc/"),
        " and linked with (4)",
        a("the CORE system web site", href = "https://www.coresystemtrust.org.uk"),
        ""),
      p("There is a form if you want to ",
        a("contact me", href = "https://www.psyctc.org/psyctc/contact-me/"),
        " so do please use that if you think there is anything wrong here,",
        " or anything that could be improved."),
      br(),
      p("There is now an Email announcement list, never updating more than monthly, where I will put up developments of new apps here,",
        " a summary of updates to the",
        a("online glossary", href = "https://www.psyctc.org/psyctc/book/glossary/"),
        "and new posts in the ",
        a("Rblog.", href = "https://www.psyctc.org/Rblog/index.html"),
        "You can sign up for that ",
        a("here", href = "https://ombook.psyctc.org/signup")),
      h3("Put your values in here, replacing the existing ones", align="center"),
    numericInput("SD",
                 "This is your baseline score standard deviation (must be positive))",
                 value = 11.1,
                 min = 0,
                 max = 10^9,
                 width="100%"),
    numericInput("rel",
                 "Appropriate reliability, generally this should be the Cronback alpha from your data but may be a referential value.",
                 value = .9,
                 min = 0,
                 max = 1,
                 width="100%"),
    numericInput("ci",
                 "Width of inclusion interval of the RCI (usually .95, i.e. 95%)",
                 value = .95,
                 min = .699999,
                 max = .999,
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
    p("This uses the function getRCIfromSDandAlpha() in my CECPfuns R package but the maths is trivial and\n
       straight out of the original work on the RCI.  If you have the raw item data and n > 20 I recommend you\n
       compute the Cronbach alpha internal reliability for your own data. I will put up an app to compute that\n
       for raw item data when I can.\n\n"),
    p("App created by Chris Evans",
      a("PSYCTC.org",href="https://shiny.psyctc.org/RCI1/"),
      "licenced under a ",
      a("Creative Commons, Attribution Licence-ShareAlike",
        href="http://creativecommons.org/licenses/by-sa/1.0/"),
      " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
    verbatimTextOutput("boilerplate"),
  )
)
)


# Define server logic required
### this is the standard shiny server constructor
server <- function(input, output, session) {
  ### 
  ### start with validation functions
  ### I don't think I actually use any these as I've now used numericInput() to set the ranges

  ### read standard text
  getBoilerPlate <- function(){
    readr::read_file("../../boilerplate.txt") 
  }
  ### 
  ### now the functions adapted from CECPfuns plotCIcorrelation
  ###
  getRCI <- function(SD, rel, ci = 0.95, dp = 2) {
    RCI <- CECPfuns::getRCIfromSDandAlpha(SD, rel, ci)
    ci.perc <- round(100 * ci)
    retText <- paste0("Given:\n",
                      "   SD = ", round(SD, dp), "\n",
                      "   Reliability = ", round(rel, dp), " and \n",
                      "   ", ci.perc, "% inclusion interval gives\n",
                      "   RCI = ", round(RCI, dp), "\n\n")
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
    getRCI(input$SD,
          input$rel,
          input$ci,
          input$dp)
  })
  output$boilerplate <- renderPrint({
    HTML(getBoilerPlate())
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
