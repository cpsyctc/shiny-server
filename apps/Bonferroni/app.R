### Bonferroni1

### shiny app to model the basic screening situation

suppressMessages(library(tidyverse))
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(pwr)) # for power calculation
# suppressMessages(library(shinyvalidate)) # input validation

# Define UI for application that does the work
ui <- fluidPage(
  setBackgroundColor("#ffff99"),
  ### this is from
  ### https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  ### and centers the first title across the whole page by tweaking the css
  # tags$head(
  #   tags$style(
  #     ".title {margin: auto; align: center}"
  #   )
  # ),
  # tags$div(class="title", titlePanel("Power loss with Bonferroni correction\n\n")),
  HTML(r"(
    <h1><center>Power loss with Bonferroni correction</center></h1>
    <p>This (cosmetically horrible!) app shows the effect of using the Bonferroni correction for a given number of tests, <i>k</i>.</p>
    <p>It is using the model of t-tests so the effect size you put into the input panel on the left is the same as Cohen's d for the t-test but the principle generalises to any test.</p>
    <p>The calculations are for independent t-tests of two groups of the same size so this is a very limited model but the principle of the power costs of the Bonferroni correction will hold across all sorts of null hypothesis tests.</p>
    <p>The app gives the power for one test, the power for your chossen number of tests then a plot which maps power against sample size for a range of <i>k</i>, the number of tests highlighting the number of tests you chose in red and mapping for k from 1 to maxK.  Finally, it tabulates the mapping of power against <i>n</i> for your <i>k</i>.</p>
    <p>The overall, experimentwise or reportwise alpha you want defaults to .05 but you can change that.</p>
    <p><b>Shiny currently (24.xii.23) throws a warning if you empty any of the inputs so try to change them using the adjuster arrows without ever emptying them while I try to understand and fix that!</b></p>
  )"),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, adjust with the adjuster arrows to replace the existing ones",
         align = "center"),
      numericInput("overallAlpha",
                   "The overall, experimentwise, reportwise, alpha you want",
                   value = .05,
                   min = .01,
                   max = .2,
                   width="100%"),
      numericInput("effectSize",
                   "Effect size for each test",
                   value = .5,
                   min = .01,
                   max = 5,
                   width="100%"),
      numericInput("yourK",
                   "Your number of tests (your k))",
                   value = 3,
                   min = 10,
                   max = 10^5,
                   width="100%"),
      numericInput("maxK",
                   "Maximum number of tests (maxK: for plot))",
                   value = 10,
                   min = 10,
                   max = 99,
                   width="100%"),
      numericInput("yourN",
                   "Your n (at least 10))",
                   value = 10,
                   min = 10,
                   max = 10^5,
                   width="100%"),
      numericInput("minN",
                   "Minimum n (at least 10: for plot))",
                   value = 10,
                   min = 10,
                   max = 10^5,
                   width="100%"),
      numericInput("maxN",
                   "Maximum n (up to 10,000: for plot)",
                   value = 100,
                   min = 50,
                   max = 10^5,
                   width="100%"),
      numericInput("dp",
                   "Decimal places you want in the PPV and NPV results (0 to 5)",
                   value = 2,
                   min = 0,
                   max = 5)
    ),
    
    mainPanel(
      # h1("Trade off of power against number of tests using Bonferroni correction", align = "center"),
      
      # h2("Input"),
      p("Your input values are"),
      verbatimTextOutput("retInputs"),
      
      htmlOutput("retComputed"),
      
      h2("Plot of power"),
      plotOutput("powerPlot", height = 500),
      
      h2("Power table"),
      p("In case you need to plan a power analysis, this probably long, table gives you power for all n range for your k ..."),
      tableOutput('table'),
      
      br(),
      br(),
      p("App created by Chris Evans",
        a("PSYCTC.org", href="https://shiny.psyctc.org/CIproportion/"),
        "licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href="http://creativecommons.org/licenses/by-sa/1.0/"),
        " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
      hr(),
      includeHTML("https://shiny.psyctc.org/boilerplate.html")
    )
  )
)

retInputs <- function(overallAlpha, effectSize, yourK, maxK, minN, maxN) {
  retText1 <- paste0("You input:\n",
                    "   Overall Alpha = ", overallAlpha, "\n",
                    "   Effect size   = ", effectSize, "\n",
                    "   Your k        = ", yourK, "\n",
                    "   Maximum k     = ", maxK, "\n",
                    "   Minimum n     = ", minN, "\n",
                    "   Maximum n     = ", maxN, "\n\n")
  return(retText1)
}

# Define server logic required
server <- function(input, output, session) {
  ### input validation #1. Create an InputValidator object (from shinyvalidate)
  ### see https://rstudio.github.io/shinyvalidate/articles/shinyvalidate.html
  # iv <- InputValidator$new()
  
  ### input validation #2. Add validation rules
  # iv$add_rule("overallAlpha", sv_required())
  # iv$add_rule("effectSize", sv_required())
  # iv$add_rule("yourK", sv_required())
  # iv$add_rule("maxK", sv_required())
  # iv$add_rule("minN", sv_required())
  # iv$add_rule("maxN", sv_required())
  ### 
  # iv$add_rule("overallAlpha", sv_numeric())
  # iv$add_rule("effectSize", sv_numeric())
  # iv$add_rule("yourK", sv_integer())
  # iv$add_rule("maxK", sv_integer())
  # iv$add_rule("minN", sv_integer())
  # iv$add_rule("maxN", sv_integer())

  
  ### input validation #3. Start displaying errors in the UI
  # iv$enable()
  
  checkGT <- function(maxN, minN){
    if (maxN <= minN) {
      return(FALSE)
    }
    return(TRUE)
  }
  checkGE <- function(maxN, minN){
    if (maxN < minN) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  getPower <- function(n, d, alpha){
    pwr::pwr.t.test(n = n, d = d, sig.level = alpha)$power
  }
  ### function to make the power table for the chosen k
  makeTibYourK <- function(overallAlpha,
                           effectSize,
                           yourK,
                           minN,
                           maxN){
    tibble(n = minN : maxN) %>%
      mutate(overallAlpha = overallAlpha,
             effectSize = effectSize,
             k = yourK) %>%
      mutate(power = getPower(n, effectSize, overallAlpha / k)) %>%
      select(n, power)
  }
  
  ### 
  ### now the reactive values
  ###
  reacOneTestPower <- reactive({getPower(input$yourN, input$effectSize, input$overallAlpha)})
  reacYourPower <- reactive({getPower(input$yourN, input$effectSize, input$overallAlpha / input$yourK)})

  reacTibYourK <- reactive({
    makeTibYourK(input$overallAlpha,
               input$effectSize,
               input$yourK,
               input$minN,
               input$maxN)
  })
  
  makePlot <- function(overallAlpha,
                       effectSize,
                       yourK,
                       maxK,
                       minN,
                       maxN){
    tibble(n = minN : maxN) %>%
      mutate(overallAlpha = overallAlpha,
             effectSize = effectSize,
             k = list(1 : maxK)) %>%
      unnest_longer(k) %>%
      mutate(k = as.integer(k)) %>%
      mutate(power = getPower(n, effectSize, overallAlpha / k)) -> tibAll
    
    tibAll %>%
      filter(k == input$yourK) -> tibYourK
    
    
    tibAll %>%
      filter(n == maxN) %>%
      mutate(labelK = paste0("k = ", sprintf("%2s", k))) -> tibLabels
    
    nudgeX <- 3 #maxN / 16
    
    theme_set(theme_bw())
    theme_update(plot.title = element_text(hjust = .5),
                 plot.subtitle = element_text(hjust = .5))
                 # text = element_text(size = 24))
    
    ggplot(data = tibAll,
           aes(x = n, y = power, group = k)) +
      geom_point(alpha = .5) +
      geom_line(alpha = .5) +
      geom_point(data = tibYourK,
                 colour = "red") +
      geom_line(data = tibYourK,
                colour = "red") +
      # geom_text(data = tibLabels,
      #           aes(label = labelK),
      #           nudge_x = nudgeX,
      #           hjust = 0) +
      # xlim(c(10, maxN + 15)) +
      ggtitle(paste0("Power for two-group t-test and effect size", effectSize, " against n",
              "\nSeparate lines for different numbers of tests (k) with Bonferroni correction",
              "\nThese show from k = 1 at the top to k = ", maxK,
              " at the bottom, with your chosen k marked in red")) -> p
    return(p)
  }
  
  ###
  ### now the outputs
  ###
  
  output$powerPlot <- renderPlot({
    ### input validation 4. Don't proceed if any input is invalid
    # req(iv$is_valid())
    
    # validate(
    #   need(checkGT(input$maxN, input$minN),
    #        "maxN must be greater than minN"),
    #   need(checkGT(input$yourK, 1),
    #        "yourK must be greater than 1"),
    #   need(checkGE(input$maxK, input$yourK),
    #        "maxK must be greater than or equal to yourK")
    # )
    makePlot(input$overallAlpha,
             input$effectSize,
             input$yourK,
             input$maxK,
             input$minN,
             input$maxN)
  })
  
  ### now the reactive text output of the PPV and NPV
  reacComputed <- reactive({
    paste("The power for only one test is:                          ", round(reacOneTestPower(), input$dp),
          "<br>The power after applying the Bonferroni correction is: ", round(reacYourPower(), input$dp))
  })
  
  ### outputting the inputs
  output$retInputs <- renderText({
    retInputs(input$overallAlpha,
              input$effectSize,
              input$yourK,
              input$maxK,
              input$minN,
              input$maxN)
  })
  
  ### the computed power
  output$retComputed <- renderText({
    reacComputed()
  })
  
  ### the power table
  output$table <- renderTable(reacTibYourK())

}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
