### Bonferroni1

### shiny app to model the Bonferroni correction and its costs in power terms

suppressMessages(library(tidyverse))
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinyDownload))
suppressMessages(library(pwr)) # for power calculation
suppressMessages(library(shiny.telemetry))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Bonferroni1",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite")))


# Define UI for application that does the work
ui <- fluidPage(
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  setBackgroundColor("#ffff99"),
  HTML(r"(
    <h1><center>Power loss with Bonferroni correction</center></h1>
    <p>This app shows the effect of using the Bonferroni correction for a given number of tests, <i>k</i>.</p>
    <p>It is using the model of t-tests so the effect size you put into the input panel on the left is the same as Cohen's d for the t-test.
    The calculations are for independent t-tests of two groups of the same size so this is a very limited model, however the principle 
    generalises to all sorts of null hypothesis tests.</p>
    <p>The app gives on two text lines after showing you your input the power for one test and then the power for your chosen number of tests.<br>
    It then gives a plot which maps power against sample size for a range of <i>k</i>, the number of tests highlighting the number of tests you chose in red
    and mapping for k from 1 to maxK: the maximum number of tests you requested to be modelled.<br>
    Finally, it tabulates the mapping of power against <i>n</i> for your <i>k</i>.</p>
    <p>The overall, experimentwise or reportwise alpha you want defaults to the conventional .05 but you can change that.</p>
  )"),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here",
         align = "center"),
      numericInput("overallAlpha",
                   "The overall alpha, experimentwise, reportwise, that you want",
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
                   "The number of tests in your report/experiment",
                   value = 3,
                   min = 10,
                   max = 10^5,
                   width="100%"),
      numericInput("maxK",
                   "Maximum number of tests to model and to plot",
                   value = 10,
                   min = 10,
                   max = 99,
                   width="100%"),
      numericInput("yourN",
                   "Your dataset size, n (at least 10))",
                   value = 25,
                   min = 10,
                   max = 10^5,
                   width="100%"),
      numericInput("minN",
                   "Minimum n to model (at least 10)",
                   value = 10,
                   min = 10,
                   max = 10^5,
                   width="100%"),
      numericInput("maxN",
                   "Maximum n to model (up to 10,000)",
                   value = 100,
                   min = 50,
                   max = 10^5,
                   width="100%"),
      numericInput("dp",
                   "Decimal places you want in the results (0 to 5)",
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
      
      downloadGGPlotButtonUI("mainPlotDownload", "powerPlot"),
      
      h2("Power table"),
      p("In case you need to plan a power analysis this, probably long, table gives you power for all
        n across the range you requested and for your k, i.e. the number of tests you plan to do.  
        At the end of the table there is a button that allows you to download the table in CSV format 
        if that is helpful for you."),
      tableOutput('table'),
      
      p("You can download the entire dataset using the following button.  ",
        "I have chosen csv (comma separated variables) format as it's a ",
        "nice safe format for simple data like this and can be imported into ",
        "pretty much any software though you may have to check out how if you ",
        "haven't done this before."),
      downloadButton("download", "Download as csv"),
      br(),
      br(),
      p("App created by Chris Evans",
        a("PSYCTC.org", href="https://www.psyctc.org/psyctc/about-me/"),
        "some time before 24.xii.24.  Licenced under a ",
        a("Creative Commons, Attribution Licence-ShareAlike",
          href="http://creativecommons.org/licenses/by-sa/1.0/"),
        " Please respect that and put an acknowledgement and link back to here if re-using 
        anything from here."),
      p("Updated 4.v.25 and 5.v.25 sorting out various infelicities in the original."),
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
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = FALSE, track_values = FALSE) # 3. Track basics and inputs and input values
  
  ### input validation 
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
  
  ### function to get power using pwr.t.test() from pwr package
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
  reacOneTestPower <- reactive({
    validate(
      need(!is.na(input$overallAlpha), 
           "You must give a value for the overall alpha"),
      need(!is.na(input$effectSize), 
           "You must give a value for the effect size"),
      need(!is.na(input$yourK), 
           "You must give a value for k, the number of tests you are doing"),
      need(!is.na(input$minN), 
           "You must give a value for the minimum n to model"),
      need(!is.na(input$maxN), 
           "You must give a value for the minimum n to model"),
    )
    getPower(input$yourN, input$effectSize, input$overallAlpha)
    })
  
  reacYourPower <- reactive({
    validate(
      need(!is.na(input$overallAlpha), 
           "You must give a value for the overall alpha"),
      need(!is.na(input$effectSize), 
           "You must give a value for the effect size"),
      need(!is.na(input$yourK), 
           "You must give a value for k, the number of tests you are doing"),
      need(!is.na(input$minN), 
           "You must give a value for the minimum n to model"),
      need(!is.na(input$maxN), 
           "You must give a value for the minimum n to model"),
      need(!is.na(input$dp), 
           "You must give a value for number of decimal places in the output!")
    )
    getPower(input$yourN, input$effectSize, input$overallAlpha / input$yourK)
    })

  reacTibYourK <- reactive({
    validate(
      need(!is.na(input$overallAlpha), 
           "You must give a value for the overall alpha"),
      need(!is.na(input$effectSize), 
           "You must give a value for the effect size"),
      need(!is.na(input$yourK), 
           "You must give a value for k, the number of tests you are doing"),
      need(!is.na(input$minN), 
           "You must give a value for the minimum n to model"),
      need(!is.na(input$maxN), 
           "You must give a value for the minimum n to model"),
      need(!is.na(input$dp), 
           "You must give a value for number of decimal places in the output!")
    )
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
                       yourN,
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
      ### intersect lines for current situation
      geom_vline(xintercept = yourN,
                 linetype = 3,
                 colour = "red") +
      geom_hline(yintercept = reacYourPower(),
                 linetype = 3,
                 colour = "red") +
      ### annotations
      annotate("text",
               x = maxN,
               y = reacYourPower() - 0.02,
               hjust = 1,
               vjust = 1,
               label = paste0("Your power with ",
                              yourK,
                              " tests and n = ",
                              yourN,
                              " is ",
                              round(reacYourPower(), input$dp))) +
      annotate("text",
               x = yourN,
               y = .95,
               hjust = 0,
               vjust = 1,
               label = paste0("  Your n is ",
                              yourN)) +
      ggtitle(paste0("Power for two-group t-test and effect size", effectSize, " against n",
              "\nSeparate lines for different numbers of tests (k) with Bonferroni correction",
              "\nThese show from k = 1 at the top to k = ", maxK,
              " at the bottom, with your chosen k marked in red",
              "\nYour current scenario is shown by the intersecting dotted (red) lines and annotation.")) -> p
    return(p)
  }
  
  ###
  ### now the outputs
  ###
  
  output$powerPlot <- renderPlot({
    validate(
      need(!is.na(input$overallAlpha), 
           "You must give a value for the overall alpha"),
      need(!is.na(input$effectSize), 
           "You must give a value for the effect size"),
      need(!is.na(input$yourK), 
           "You must give a value for k, the number of tests you are doing"),
      need(!is.na(input$maxK), 
           "You must give a value for the maximum number of tests you want to model"),
      need(!is.na(input$minN), 
           "You must give a value for the minimum n to model"),
      need(!is.na(input$maxN), 
           "You must give a value for the minimum n to model"),
      need(!is.na(input$dp), 
           "You must give a value for number of decimal places in the output!")
    )
    # validate(
    #   need(checkGT(input$maxN, input$minN),
    #        "maxN must be greater than minN"),
    #   need(checkGT(input$yourK, 1),
    #        "yourK must be greater than 1"),
    #   need(checkGE(input$maxK, input$yourK),
    #        "maxK must be greater than or equal to yourK")
    # )
    mainPlot()
  })
  
  mainPlot <- reactive({
    validate(
      need(!is.na(input$overallAlpha), 
           "You must give a value for the overall alpha"),
      need(!is.na(input$effectSize), 
           "You must give a value for the effect size"),
      need(!is.na(input$yourK), 
           "You must give a value for k, the number of tests you are doing"),
      need(!is.na(input$maxK), 
           "You must give a value for the maximum number of tests you want to model"),
      need(!is.na(input$minN), 
           "You must give a value for the minimum n to model"),
      need(!is.na(input$maxN), 
           "You must give a value for the minimum n to model"),
      need(!is.na(input$dp), 
           "You must give a value for number of decimal places in the output!")
    )
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
             input$yourN,
             input$maxN)
  })
  
  ### now the reactive text output of the power values for this situation
  reacComputed <- reactive({
    paste("The power for only one test is:                          ", 
          round(reacOneTestPower(), input$dp),
          "<br>The power after applying the Bonferroni correction is: ", 
          round(reacYourPower(), input$dp))
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
  
  downloadGGPlotButtonServer(
    id = "mainPlotDownload", # <= this should match the ID used in the UI module
    ggplotObject = mainPlot, # No parentheses here to pass *expression*
    width = 1500,
    height = 800
  )
  
  ### the computed power
  output$retComputed <- renderText({
    reacComputed()
  })
  
  ### the power table
  output$table <- renderTable(reacTibYourK())
  
  output$download <- downloadHandler(
    filename = "powerValues.csv",
    contentType = "text/csv",
    content = function(file) {
      write_csv(reacTibYourK(), file = file)
    }
  )

}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
