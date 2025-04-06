### useConvFiveNum
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(metafor))
suppressMessages(library(shiny.telemetry))
# suppressMessages(library(shinyDownload)) # from remotes::install_github("keithnewman/shinyDownload")

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "useConvFiveNum",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Estimate SD and perhaps mean from some of the five numbers\n\n")),
  
  p("The five number summary of a sample of values on a continuous variable is the set of:\n"),
  tags$ul(
    tags$li("the minimum"), 
    tags$li("the lower quartile (q1 or q25)"), 
    tags$li("the median"),
    tags$li("the upper quartile (q3 or q75) and ..."),
    tags$li("the maximum.")
  ),
  
  p("I am using the conv.fivenum() function from Wolfgang Viechtbauer's brilliant R package: metafor to power this. ",
    "His function uses some statistical theory to estimate standard deviations (SDs) and, generally, means ",
    "if given some or all of the five number summary values, and the size of a dataset."),
  p("The function can work with the following:"),
  tags$ul(
    tags$li("Case 1: minimum, median, maximum and n"),
    tags$li("Case 2: q1, median, q3 and n"),
    tags$li("Case 3: min, q1, median, q3, maximum and n")
  ),
  p("It will also work without the median for each of those cases but in that situation it only returns the estimated SD. ",
    "That situation tends to arise when a report has given other parts of the five number summary but the mean instead of ",
    "the median so often you will aready have the mean."),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("min",
                   "Minimum value in the dataset",
                   value = -10,
                   min = -10^6,
                   max = 10^6,
                   width = "100%"),
      numericInput("q1",
                   "First quartile, i.e. 25th (per)centile",
                   value = -2,
                   min = -10^6,
                   max = 10^6,
                   width = "100%"),
      numericInput("median",
                   "Median, i.e. 50th (per)centile",
                   value = 0,
                   min = -10^6,
                   max = 10^6,
                   width = "100%"),
      numericInput("q3",
                   "Third quartile, i.e. 75th (per)centile",
                   value = 2,
                   min = -10^6,
                   max = 10^6,
                   width = "100%"),
      numericInput("max",
                   "Maximum value in the dataset",
                   value = 10,
                   min = -10^6,
                   max = 10^6,
                   width = "100%"),
      numericInput("n",
                   "n: dataset size, must be at least 5 but should be larger",
                   value = 50,
                   min = 5,
                   max = 10^6,
                   width = "100%"),
      p("If you are in the situation that you don't have the median but do have the mean ",
        "put it in here."),
      numericInput("obsMean",
                   "Mean (mostly you won't have this so leave it as NA).",
                   value = NA,
                   min = -10^6,
                   max = 10^6,
                   width = "100%"),
      p("By default the conv.fivenum() function tests for skewness on the basis of the numbers ",
        "it is given and won't estimate the mean and SD if the skewness is statistically significant.",
        "You can override the testing here."),
      checkboxInput("test", "test for skewness",
                    value = TRUE),
      numericInput("dp",
                   "Number of decimal places for the estimates (0 to 7)",
                   value = 3,
                   min = 0,
                   max = 7,
                   width="100%"),
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Estimates and plot", 
                           verbatimTextOutput("estimates"),
                           plotOutput("relPlot"),
                           p(" "),
                           downloadGGPlotButtonUI("plotDownload", "EstimatedDistribution"),
                           p(" "),
                           p("Thanks to Keith Newman for the download handler: ",
                             a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file"))),
                  
                  tabPanel("Explanation",
                           p("All the clever computing and the statistical theory that drives that are from ",
                             "Wolfgang Viechtbauer's brilliant R package: metafor and specifically from his ",
                             "conv.fivenum() function."),
                           p("I have left the function to use its default estimation method: 'luo/wan/shi'. ",
                             "The references for that, which I am sure are too sophisticated for me, can be ",
                             "found in the help page for the function.  The background is in the comprehensive ",
                             a("help page for the function", href="https://wviechtb.github.io/metafor/reference/conv.fivenum.html"),
                             "."),
                           p("I have added the plot which I hope is of some pedagogical usefulness ",
                             "(it helped me understand things!).  The quartiles are drawn from R's dnorm() ",
                             "function but the minima and maxima I created by simulating 100 samples from ",
                             "a Gaussian distribution with the estimated mean, SD and n and taking the mean ",
                             "minimum and maximum across those 100 simulated samples.  Clearly, the larger ",
                             "the n, the wider out the observed minimum and maximum will be, though the ",
                             "extent to which the observed limits widen with increasing n falls rapidly ",
                             "as n gets to sensible dataset sizes.")),
                  
                  tabPanel("Background", 
                           p("App created 6.iv.25 by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 6.iv.25: so no updates yet!"),
                           p("Licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           includeHTML("https://shiny.psyctc.org/boilerplate.html"))
      ),
    ),
  )
)

# Define server logic required
server <- function(input, output, session) {
  
  ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
  session$onSessionEnded(function() {
    stopApp()
  })
  
  telemetry$start_session(track_inputs = TRUE, track_values = TRUE) # 3. Track basics and inputs and input values
  
  ### 
  ### start with validation functions
  ###
  checkOrder2vals <- function(x, y){
    ### checks that y is greater than or equal to x
    if(!is.numeric(x) | !is.numeric(y)) {
      return(FALSE)
    }
    if (y < x) {
      return(FALSE)
    }
    return(TRUE)
  }
  checkOrderAllVals <- function(min, q1, median, q3, max) {
    vals <- c(min, q1, median, q3, max)
    vals <- na.omit(vals)
    ordOK <- TRUE
    for (i in 2:length(vals)){
      if(vals[i] < vals[i - 1]) {
        ordOK <- FALSE
      }
    }
    ordOK
  }
  checkEnoughN <- function(n) {
    if(!is.numeric(n)) {
      return(FALSE)
    }
    if(is.na(n)) {
      return(FALSE)
    }
    if (n < 5) {
      return(FALSE)
    }
    return(TRUE)
  }
  checkEnoughStats <- function(min, q1, median, q3, max, n){
    ### case 1
    if (!is.na(min) & !is.na(max)) {
      return(TRUE)
    }
    ### case 2
    if (!is.na(q1) & !is.na(q3)) {
      return(TRUE)
    }
    ### case 3
    # if (!is.na(min) & !is.na(q1) & !is.na(q3) & !is.na(max)) {
    #   return(TRUE)
    # }
    return(FALSE)
  }
  reactiveDP <- reactive({
    req(input$dp)
    validate(
      need(input$dp >= 0,
           "Number of decimal places must be between 0 and 7 inclusive"),
      need(input$dp < 8,
           "Number of decimal places must be between 0 and 7 inclusive")
    )
    round(input$dp)
  })
  ### 
  ###
  
  estimatesVec <- reactive({
    validate(
      need(checkEnoughN(input$n), 
           "The dataset size must be at least 5, preferably considerably larger!"),
      need(checkEnoughStats(input$min, input$q1, input$median, input$q3, input$max, input$n),
           "You don't seem to have given enough sample statistics, see the intro. to the app."),
      need(checkOrderAllVals(input$min, input$q1, input$median, input$q3, input$max),
           "At least two of min, q1, median, q3, max are out of size order.")
    )
    
    conv.fivenum(min = input$min, 
                 q1 = input$q1, 
                 median = input$median, 
                 q3 = input$q3,
                 max = input$max, 
                 n = input$n,
                 test = input$test)
  })
  
  
  estimatesText <- reactive({
    estimatesVec <- round(estimatesVec(), reactiveDP())
    if(is.na(estimatesVec[1]) & is.na(estimatesVec[2])) {
      return(paste0("Neither the mean nor the SD could be estimated ",
                    "If testing for skewness was asked for, the default, ",
                    "then that is almost certainly the reason there are ",
                    "no estimated values.  You can try setting testing ",
                    "off but estimates could be misleading or very misleading."))
    }
    if(is.na(input$obsMean) & is.na(estimatesVec[1])) {
      return(paste0("The mean could not be estimated\n (as you had no median) ",
                    "the estimated SD is: ",
                    estimatesVec[2]))
    } 
    if(!is.na(input$obsMean) & is.na(estimatesVec[1])) {
      return(paste0("The mean could not be estimated (as you had no median) \n",
                    "but you input a mean of ",
                    round(input$obsMean, reactiveDP()),
                    "  The estimated SD is: ",
                    estimatesVec[2]))
    }
    return(paste0("The estimated mean is: ",
                  estimatesVec[1],
                  " and the estimated SD is: ",
                  estimatesVec[2],
                  "\n",
                  "The plot below shows the best fitting Gaussian according to the estimated mean and SD.\n",
                  "The observed statistics you have from the data are shown as green lines and the \n",
                  "Estimated values are shown in red (and shading for the quartiles).\n",
                  "This gives you an idea of how good the fit of that Gaussian based on the estimates parameters\n",
                  "is against the observed statistics.\n",
                  "If the fit is good:\n",
                  "* the observed median will be close to or on the estimated mean\n",
                  "* the observed quartiles will be near the predicted ones (indicated by shading)\n",
                  "* my simulated (see explanation tab) minimum and maximum will be close the observed values.\n\n",
                  "Of course, you may not have had all five observed values but the principles apply to the smaller\n",
                  " sets of observed values that still produce estimates."))
  })
  
  plotFit <- function(min, q1, median, q3, max, n, 
                      obsMean, 
                      estMean, estSD) {
    ### this is necessary as otherwise they are named vectors of length one
    ### that throws dplyr when it uses them later, see "HERE!"
    estMean <- as.numeric(estMean)
    estSD <- as.numeric(estSD)
    
    ### bit of simulation to get likely minimum and maximum given Gaussian and dataset size
    set.seed(12345)
    ### I am using 100 simulation runs
    tibble(iterN = 1 : 100,
           n = n,
           ### HERE!  This was creating variable names mean$mean and mean$sd
           mean = estMean,
           SD = estSD) %>%
      ### now do the simulations
      rowwise() %>%
      mutate(values = list(rnorm(n, estMean, estSD)),
             min = min(unlist(values)),
             max = max(unlist(values))) %>%
      ### don't need the simulated values any more
      select(-values) %>%
      ungroup() %>%
      ### get the means of the extremes seen
      summarise(simMin = mean(min),
                simMax = mean(max)) -> tibExtremes
    
    ### get to long form for labelling
    tibExtremes %>%
      pivot_longer(cols = everything()) %>%
      ### create a y value for the labels
      mutate(y = .04) -> tibExtremesLong
    
    tribble(~stat, ~value,
            "obsMin", min,
            "obsQ1", q1,
            "obsMedian", median,
            "obsQ3", q3,
            "obsMax", max) %>%
      ### create a y value for the labels
      mutate(y = .13) -> tibObsStats
    
    ### work out sensible x axis
    if(!is.na(min) & !is.na(max)) {
      range <- max - min
      minX <- min - range / 10
      maxX <- max + range / 10 
    } else {
      range <- 6 * estSD # generous estimate
      minX <- estMean - range / 2
      maxX <- estMean + range / 2
    }
    
    ### create Gaussian density line using estimated values
    tibble(x = seq(minX, maxX, length.out = 100),
           y = dnorm(x, mean = estMean, sd = estSD)) -> tibDnorm
    
    qnorm(.25, estMean, estSD) -> estQ1
    qnorm(.75, estMean, estSD) -> estQ3
    
    ### create tibble for mean and those estimated quartiles
    tribble(~label, ~x,  
            "estMean", estMean,
            "estQ1", estQ1,
            "estQ3", estQ3) %>%
      mutate(ymin = 0,
             ymax = .04) -> tibEstValues
    
    ### OK create the plot
    ggplot(data = tibDnorm,
           aes(x = x, y = y)) +
      geom_line() +
      geom_vline(data = tibObsStats,
                 aes(xintercept = value),
                 colour = "green") +
      geom_text(data = tibObsStats,
                aes(x = value, y = y, label = stat),
                angle = 70,
                hjust = 1) +
      stat_function(fun = dnorm, 
                    args = list(mean = estMean, sd = estSD), 
                    xlim = c(estQ3, maxX),
                    geom = "area", 
                    fill = "red", alpha = .2) +
      stat_function(fun = dnorm, 
                    args = list(mean = estMean, sd = estSD), 
                    xlim = c(minX, estQ1),
                    geom = "area", 
                    fill = "red", alpha = .2) +
      geom_segment(data = tibExtremes,
                   aes(x = simMin, xend = simMin,
                       y = 0, yend = .04),
                   colour = "red") +
      geom_segment(data = tibExtremes,
                   aes(x = simMax, xend = simMax,
                       y = 0, yend = .04),
                   colour = "red") +
      geom_text(data = tibExtremesLong,
                aes(x = value, y = y, label = name),
                angle = 70,
                hjust = 0,
                colour = "red") +
      geom_linerange(inherit.aes = FALSE,
                     data = tibEstValues,
                     aes(x = x, 
                         ymin = ymin, 
                         ymax = ymax),
                     colour = "red") +
      geom_text(data = tibEstValues,
                aes(x = x, 
                    y = ymax, 
                    label = label),
                angle = 70,
                hjust = 0,
                colour = "red") +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) -> p
    
    if (!is.na(input$obsMean)) {
      p +
        geom_linerange(aes(x = input$obsMean, ymin = 0, ymax = .13),
                       colour = "black") +
        annotate("text",
                 x = obsMean,
                 y = .06,
                 label = "obsMean",
                 colour = "black",
                 angle = 70,
                 hjust = 0,
                 vjust = 0) -> p
    }
    
    suppressWarnings(print(p))
  }
  
  estMean <- reactive({
    estimatesVec()[1]
  })
  
  estSD <- reactive({
    estimatesVec()[2]
  })
  
  relPlot <- reactive({
    validate(
      need(!is.na(estMean()), "")
    )
    plotFit(input$min, input$q1, input$median, input$q3, input$max, input$n, 
            obsMean = input$obsMean,
            estMean = estMean(), estSD = estSD())
  })
  
  # output$titleText <- renderText({input$title})
  
  output$relPlot <- renderPlot({
    relPlot()
  })
  
  downloadGGPlotButtonServer(
    id = "plotDownload", # <= this should match the ID used in the UI module
    ggplotObject = relPlot # No parentheses here to pass *expression*
  )
  
  relDat2 <- reactive({
    req(input$dp)
    relDat() %>%
      mutate(rel = round(rel, input$dp)) 
  })
  
  
  output$estimates <- renderText({
    estimatesText()
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
