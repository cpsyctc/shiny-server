### CIdiff2proportions
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))


# Define UI for application that does the work
ui <- fluidPage(
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css
  tags$head(
    tags$style(
      ".title {margin: auto; align: center}"
    )
  ),
  tags$div(class="title", titlePanel("Confidence interval for a simple proportion\n\n")),
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("n1",
                   "Total n in first set of data, (n1, positive integer > 3)",
                   value = 100,
                   min = 3,
                   max = 10^9,
                   width = "100%"),
      numericInput("x1",
                   "Number of of those n1 that were counted as positive/important/interesting (x1, zero or positive integer < n1)",
                   value = 34,
                   min = 0,
                   max = 10^9,
                   width = "100%"),
      numericInput("n2",
                   "Total n in second set of data, (n2, positive integer > 3)",
                   value = 50,
                   min = 3,
                   max = 10^9,
                   width = "100%"),
      numericInput("x2",
                   "Number of of those n2 that were counted as positive/important/interesting (x2, zero or a positive integer)",
                   value = 17,
                   min = 0,
                   max = 10^9,
                   width = "100%"),
      numericInput("ci",
                   "Width of CI (usually .95, i.e. 95% CI, <=.99)",
                   value = .95,
                   min = .7,
                   max = .999,
                   width = "100%"),
      numericInput("dp",
                   "Number of decimal places",
                   value=2,
                   width="100%")
    ),
    mainPanel(
      h3("Your input and results",align="center"),
      verbatimTextOutput("res"),
      plotOutput("CIplot", height = 270),
      p(" "),
      p("I think most modern browsers will let you save/download that image."),
      p(" "),
      p(" "),
      p("App created by Chris Evans",
        a("PSYCTC.org",href="https://shiny.psyctc.org/CIproportion/"),
        "licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href="http://creativecommons.org/licenses/by-sa/1.0/"),
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
      htmlTemplate("boilerplate.html")
    )
  )
)

# Define server logic required
server <- function(input, output, session) {
  ### 
  ### start with validation functions: none needed
  ###

  ### 
  ### now the functions from https://www.statology.org/confidence-interval-in-r/
  ###
  getCI <- function(n1, x1, n2, x2, ci = 0.95, dp = 2) {
    CI1 <- Hmisc::binconf(x1, n1, 1 - ci)[1, ]
    CI2 <- Hmisc::binconf(x2, n2, 1 - ci)[1, ]
    p1 <- CI1[1]
    p2 <- CI2[1]
    diffp <- p1 - p2
    ci.perc <- round(100 * ci)
    normVal <- 1 - (1 - ci)/2
    margin <- qnorm(normVal) * sqrt(p1 * (1 - p1) / n1 + p2 * (1 -p2 ) / n2)
    LCL <- diffp - margin
    UCL <- diffp + margin

    retText <- paste0("Given x1 = ", x1, " and n1 = ", n1," proportion in first dataset is ",
                      round(p1, dp),
                      "\n with ", ci.perc, "% confidence interval from ", round(CI1[2], dp),
                      " to ", round(CI1[3], dp),"\n\n",
                      "and given x2 = ", x2, " and n2 = ", n2," proportion in second dataset is ",
                      round(p2, dp),
                      "\n with ", ci.perc, "% confidence interval from ", round(CI2[2], dp),
                      " to ", round(CI2[3], dp),"\n\n",
                      "so the difference p1 - p2 is ",
                      round(diffp, dp),
                      "\n with ", ci.perc, "% confidence interval from  ",
                      round(LCL, dp), " to ", round(UCL, dp))
    return(retText)
  }
  
  makePlot <- function(n1, x1, n2, x2, ci, dp) {
    CI1 <- Hmisc::binconf(x1, n1, 1 - ci)[1, ]
    CI2 <- Hmisc::binconf(x2, n2, 1 - ci)[1, ]
    p1 <- CI1[1]
    p2 <- CI2[1]
    diffp <- p1 - p2
    ci.perc <- round(100 * ci)
    normVal <- 1 - (1 - ci)/2
    margin <- qnorm(normVal) * sqrt(p1 * (1 - p1) / n1 + p2 * (1 -p2 ) / n2)
    LCL <- diffp - margin
    UCL <- diffp + margin
    n <- n1 + n2
    x <- x1 + x2
    pAll <- x / n
    ci.perc <- round(100 * ci)
    titleText <- paste0("Biplane plot of proportions")
    subTitleText <- paste0("Vertical lines mark ",
                           ci.perc, " confidence intervals for proportions, ",
                           "\nhorizontal reference line is overall proportion across both datasets.")
    tribble(~dataSet, ~p, ~LCL, ~UCL,
            "Dataset 1", CI1[1], CI1[2], CI1[3],
            "Dataset 2", CI2[1], CI2[2], CI2[3]) -> tmpTib
    ggplot(data = tmpTib,
           aes(x = dataSet, y = p)) +
      geom_point() +
      geom_linerange(aes(ymin = LCL, ymax = UCL )) +
      geom_hline(yintercept = pAll) +
      ylim(c(0, 1)) +
      xlab("Groups") + 
      ylab("Proportion") +
      ggtitle(titleText,
              subTitleText) +
      theme_bw() +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5)) -> p
    return(p)
  }
  
  output$res <- renderText({
    getCI(input$n1,
          input$x1,
          input$n2,
          input$x2,
          input$ci,
          input$dp)
  })
  output$CIplot <- renderPlot({
    makePlot(input$n1, input$x1, input$n2, input$x2, input$ci, input$dp)
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
