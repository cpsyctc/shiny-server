### Gaussian1 (needs rewriting!)
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(flextable))
suppressMessages(library(ggpubr)) ## for ggqqplot()

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Gaussian1",
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
  tags$div(class="title", titlePanel("Show sample statistics, histogram, ecdf and qqplot for samples from Gaussian population\n\n")),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      p("This app is really for messing around, particularly to see how you only get close approximations to Gaussian shape with large n, even though your samples are all (simulated) true samples from a Gaussian population distribution."),
      h3("Put your values in here, replacing the existing ones", align="center"),
      p("Changing the number of bins in the histogram from the default of 30 can give you a finer grained sense of your data if your n is large"),
      numericInput("n",
                   "The size of the sample you want (must be positive!)",
                   value = 100,
                   min = 10,
                   max = 10^5,
                   width="100%"),
      numericInput("mean",
                   "This is the mean for the population from which you take your sample.",
                   value = 0,
                   min = -10^5,
                   max = 10^5,
                   width="100%"),
      numericInput("SD",
                   "Population SD (must be positive).",
                   value = 1,
                   min = 0,
                   max = 10^5,
                   width="100%"),
      numericInput("nBins",
                   "Number of bins in the histogram (must be positive).",
                   value = 30,
                   min = 0,
                   max = 10^5,
                   width="100%"),
      numericInput("seed",
                   "This is the 'seed' for your sample, same seed gives same sample (for given mean, SD and n.",
                   value = 12345,
                   min = 1,
                   max = 10^5,
                   width="100%"),
      numericInput("dp",
                   "Number of decimal places",
                   value = 2,
                   min = 0,
                   max = 5,
                   width="100%"),
      p("If you put different values in the above, the plots and sample statistics will adjust to reflect those."),
    ),
    
    mainPanel(
      h3("Your input and results",align="center"),
      p("Here are the sample statistics for your sample"),
      fluidRow(
        uiOutput("flextable")
      ),
      p("Here is the histogram of those data"),
      plotOutput("histogram", height = 500),
      p(" "),
      p("The vertical reference line is the sample mean.  The red curve is a smoothed fit to the distribution of the data (don't ask!) and the blue line is a perfect Gaussian distribution with mean of the observed mean of the sample and SD that of the sample, i.e. the nearest fitting Gaussian shape to the observed sample statistics."),
      p(" "),
      p(" "),
      p("Next is the plot of the ecdf (empirical cumulative density function) of those data"),
      plotOutput("ecdf", height = 500),
      p(" "),
      p("Here again the vertical reference line is the sample mean and the ecdf curve shows how the proportions of the data with values up to the x-axis value rise across the sample."),
      p(" "),
      p(" "),
      p("And finally the qq (quantile-quantile) lot of the same data"),
      plotOutput("qqplot", height = 500),
      p(" "),
      p("The qq plot plots how the observed proportions up to an observed score (as in the ecdf plot) map onto those which would have been expected had the distribution been Gaussian but with mean and SD those seen in the sample (the solid straight diagonal line).  The grey shaded area is the 95% interval around the perfect fit (based on the sample size)."),
      p(" "),
      hr(),
      hr(),
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
  
  makeTibDat <- function(n, mean, SD, seed) {
    set.seed(seed)
    tibble(x = rnorm(n, mean, SD)) -> tibDat
  }
  tmpTib <- reactive(makeTibDat(input$n,
                                input$mean,
                                input$SD,
                                input$seed))
  
  getStats <- function(tibDat) {
    tibDat %>%
      summarise(n = as.integer(n()),
                min = min(x),
                max = max(x),
                mean = mean(x),
                SD = sd(x),
                var = var(x),
                SE = SD / sqrt(n),
                parLCL95 = mean - SE,
                parUCL95 = mean + SE) 
  }
  
  tmpTibStats <- reactive(getStats(tmpTib()))
  
  output$flextable <- renderUI({
    htmltools_value(tmpTibStats() %>%
                      pivot_longer(cols = everything()) %>%
                      flextable() %>%
                      colformat_double(digits = 3))
  })
  
  plotGauss <- function(tmpTib, tmpTibStats, nBins) {
    ggplot(data = tmpTib,
           aes(x = x)) +
      geom_histogram(aes(x = x, 
                         after_stat(density)),
                     bins = nBins,
                     fill = "grey") +
      geom_density(colour = "red",
                   linewidth = 2) +
      geom_vline(xintercept = tmpTibStats$mean,
                 linetype = 2,
                 linewidth = 1.5) +
      stat_function(fun = dnorm,
                    args = list(mean = tmpTibStats$mean,
                                sd = tmpTibStats$SD),
                    colour = "blue",
                    linewidth = 2) +
      ylab("Density (probability of scores in bin)") +
      xlab("Values in your sample") +
      ggtitle("Histogram of your sample data",
              subtitle= "Red curve is smoothed density fit to the data, blue is best fitting Gaussian distribution") -> p
    return(p)
  }
  
  ### and now a histogram of the data
  output$histogram <- renderPlot({
    plotGauss(tmpTib(), 
              tmpTibStats(),
              input$nBins)
  })

  
  plotECDF <- function(tmpTib, tmpTibStats) {
    ggplot(data = tmpTib,
           aes(x = x)) +
      stat_ecdf() +
      geom_vline(xintercept = tmpTibStats$mean,
                 linetype = 2,
                 linewidth = 1.5) +
      ylab("Cumulative proportion scoring below or at the value") +
      xlab("Values in your sample") +
      ggtitle("ECDF of your sample data") -> p
    return(p)
  }
  
  output$ecdf <- renderPlot({
    plotECDF(tmpTib(), tmpTibStats())
  })
  
  plotGGQQplot <- function(tmpTib) {
    ggqqplot(data = tmpTib, x = "x",
             ggtheme = theme_bw()) +
      ggtitle("ECDF of your sample data") +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5)) -> p
    return(p)
  }
  
  output$qqplot <- renderPlot({
    plotGGQQplot(tmpTib())
  })
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
