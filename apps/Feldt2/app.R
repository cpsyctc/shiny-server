### Testing for a difference between two Cronbach alpha values from independent samples

suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))
suppressMessages(library(CECPfuns))
suppressMessages(library(shiny.telemetry))
suppressMessages(library(shinyDownload)) # from remotes::install_github("keithnewman/shinyDownload")
# suppressMessages(library(DT))

### set ggplot defaults
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = .5),
             plot.subtitle = element_text(hjust = .5),
             text = element_text(size = 24))

### 1. Initialize telemetry with default options (store to a local logfile)
telemetry <- Telemetry$new(app_name = "Feldt2",
                           data_storage = DataStorageSQLite$new(db_path = file.path("../../telemetry.sqlite"))) 

### function that gets the attenuated R
getAttenuatedR <- function(unattR, rel1, rel2) {
  ### function that takes an unattenuated correlation 
  ### and the reliabilities of the two variables
  ### and returns the attenuated correlation
  unattR * sqrt(rel1 * rel2)
}

# Define UI for application that does the work
ui <- fluidPage(
  
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  
  setBackgroundColor("#ffff99"),
  h1(HTML("Parametric test of the difference between two Cronbach alpha values\n\n")),
  
  p("This app uses Feldt's method from his 1969 paper."),
  p("Input the values for the number of items (assumed the same in each sample) and the alpha values and n for each sample."),
  
  # Get input values
  sidebarLayout(
    sidebarPanel(
      h3("Put your values in here, replacing the existing ones",
         align = "center"),
      numericInput("k",
                   "k: the number of items (5 <= k <= 500)",
                   value = 34,
                   min = 5,
                   max = 500,
                   width = "100%"),
      numericInput("alpha1",
                   "Alpha in one sample: .2 < R < 1",
                   value = .65,
                   min = .2,
                   max = 1,
                   width = "100%"),
      helpText("The order of the samples doesn't matter."),
      numericInput("n1",
                   "n: sample size for that sample: 20 <= n <= 10,000",
                   value = 50,
                   min = 20,
                   max = 10^5,
                   width = "100%"),
      numericInput("alpha2",
                   "Alpha in the other sample: .2 < R < 1",
                   value = .65,
                   min = .2,
                   max = 1,
                   width = "100%"),
      
      numericInput("n2",
                   "n2: sample size for that other sample: 20 <= n <= 10,000",
                   value = 50,
                   min = 20,
                   max = 10^5,
                   width = "100%"),
      
      numericInput("CI",
                   "CI: the confidence interval you want for the plot, usually .95, i.e. 95% interval",
                   value = .95,
                   min = .8,
                   max = .99,
                   width = "100%"),
      
      numericInput("minY",
                   "Smallest alpha on the y axis you want plotted",
                   value = -1,
                   min = -1,
                   max = .5,
                   width = "100%"),
      numericInput("maxY",
                   "Largest alpha on the y axis you want plotted",
                   value = 1,
                   min = .5,
                   max = 1,
                   width = "100%"),
      numericInput("dp",
                   "Number of decimal places for the p value",
                   value = 3,
                   min = 1,
                   max = 7,
                   width="100%"),
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Analysis",
                           p("The p value for the difference between those alpha values is:"),
                           verbatimTextOutput("res"),
                           p("This shows the plot and you can see a table of all the attenuated correlations for all the inputs you gave in the tab 'All_data'"),
                           p("You can use the dialogue below the plot to download it chosing the name and the format of the plot"),
                           p(" "),
                           # uiOutput("title"),
                           plotOutput("corrPlot",
                                      height = "800px"),
                           downloadGGPlotButtonUI("plotDownload", "Alpha values"),
                           p(" "),
                           p("Thanks to Keith Newman for the download handler: ",
                             a("shinyDownload", href="https://github.com/keithnewman/shinyDownload/?tab=readme-ov-file"))),
                  
                  tabPanel("Explanation",
                           p(paste0("This is ancient psychometrics but still of some use as we may have published alpha values with the dataset sizes ",
                                    "from the literature or one reported value and our one value from the same measure.  The p value given here is the usual ",
                                    "null hypothesis test giving the probability that a difference (in either direction) would have happened by chance ",
                                    "sampling vagaries given two datasets of the size you are inputting given that there were no difference in the population ",
                                    "between the alpha values.")),
                           p(" "),
                           p(paste0("The p value is based on the assumption that the samples are independent (probably true), that the variables involved ",
                                    "are all Gaussian (dodgy, value may be moderately robust to non-Gaussian distributions) and that there is some ",
                                    "plausibility in the idea that the two datasets are random samples from enormous populations (can be dodgy!).")),
                           p(" "),
                           p(paste0("If you have raw item data from two samples I'm sure you are better off getting bootstrap CIs around the observed ",
                                    "alpha values and comparing those but we often have data for one or neither alpha values so this can be useful.")),
                           p(" "),
                           p("There is an explanation of the simulation I did to check Feldt's method and my implementation of it at my ",
                             a("Rblog",
                           href = "https://www.psyctc.org/Rblog/index.html"),
                           "post:",
                             a("Using simulation to check a function",
                               href = "https://www.psyctc.org/Rblog/posts/2024-11-12-getpdiff2alphas/")),
                           ),
                  
                  tabPanel("Background", 
                           p("App created 26.x.24 by Chris Evans",
                             a("PSYCTC.org",href="https://www.psyctc.org/psyctc/about-me/")),
                           p("Last updated 26.x.24."),
                           p("Licenced under a ",
                             a("Creative Commons, Attribution Licence-ShareAlike",
                               href="http://creativecommons.org/licenses/by-sa/1.0/"),
                             " Please respect that and put an acknowledgement and link back to here if re-using anything from here."),
                           includeHTML("https://shiny.psyctc.org/boilerplate.html"))
      ),
    )
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
  checkXbetween <- function(X, minX, maxX){
    if (X < minX | X > maxX) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  ###
  ### substantive functions
  ###
  
  ### 
  ### get p value
  ###
  getPdiff2alphas <- function (alpha1, alpha2, k, n1, n2) {
    ### function from one of same name in CECPfuns but stripped of input checking
    if (alpha1 > alpha2) {
      tmp <- alpha1
      alpha1 <- alpha2
      alpha2 <- tmp
      tmp <- n1
      n1 <- n2
      n2 <- tmp
    }
    w <- (1 - alpha2)/(1 - alpha1)
    df1 <- n1 - 1
    df4 <- n2 - 1
    df3 <- (n2 - 1) * (k - 1)
    df2 <- (n1 - 1) * (k - 1)
    A <- df4 * df2/((df4 - 2) * (df2 - 2))
    B <- (df1 + 2) * (df3 + 2) * df4^2 * df2^2/((df4 - 2) * (df4 - 
                                                               4) * df1 * (df2 - 2) * (df2 - 4) * df3)
    v2 <- 2 * A/(A - 1)
    v1 <- 2 * A^2/(2 * B - A * B - A^2)
    pf(w, v2, v1)
  }
  
  feldt1 <- function(obs.a, n, k, ci = 0.95, null.a = 0) {
    #***********************************************************#
    #* program using methods described in Feldt, Woodruff &    *#
    #* Salih (1987) Applied Psychological Measurement 11(1),   *#
    #* pp. 93-103 to carry out omnibus inferential test of     *#
    #* similarity of alpha values from a single sample         *#
    #***********************************************************#
    # obs.a is the observed alpha
    # n is the sample size
    # k is the number of items in the measure or scale
    # ci is the width of the confidence interval about obs.a desired
    ci.perc <- 100 * ci	# purely for printing as a percentage
    # null.a is the null model alpha, usually zero
    # the testing of the observed against the null is a simple F test
    if(obs.a > null.a)
      f <- (1 - obs.a)/(1 - null.a)
    else f <- (1 - null.a)/(1 - obs.a)	# allows for testing against a higher null
    n.den <- (n - 1) * (k - 1)
    n.num <- n - 1
    # set the upper and lower p values for the desired C.I.
    p1 <- (1 - ci)/2
    p2 <- ci + p1	
    f1 <- qf(p1, n.num, n.den) # corresponding F values
    f2 <- qf(p2, n.num, n.den)	
    lwr <- 1 - (1 - obs.a) * f2 # confidence interval
    upr <- 1 - (1 - obs.a) * f1
    return(c(lwr, obs.a, upr))
  }
  
  
  ###
  ### reactive components
  ###
  
  pVal <- reactive({
    ### get value
    pVal <- getPdiff2alphas(input$alpha1, input$alpha2, input$k, input$n1, input$n2)
    round(pVal, input$dp)
  })
  
  tibDat <- reactive({
    tmpVec1 <- feldt1(input$alpha1, input$n1, input$k, input$CI)
    tmpVec2 <- feldt1(input$alpha2, input$n2, input$k, input$CI)
    ### make the tiny tibble for the plot
    matrix(c(tmpVec1,
             tmpVec2),
           ncol = 3,
           byrow = TRUE) %>%
      as.data.frame() %>%
      as_tibble() %>%
      mutate(Sample = c("Sample1", "Sample2")) %>%
      rename(LCL = V1,
             Alpha = V2,
             UCL = V3) 
  })
  
  
  plotAlphaValues <- function(tibDat, minY, maxY) {
    
    suppressWarnings(suppressMessages(ggplot(data = tibDat,
                                             aes(x = Sample, y = Alpha)) +
                                        geom_point() +
                                        geom_linerange(aes(ymin = LCL, ymax = UCL)) +
                                        ylim(c(minY, maxY)) +
                                        ylab("Alpha") +
                                        xlab("Samples") +
                                        ggtitle("Observed alpha values",
                                                subtitle = paste0("Vertical error bars are ",
                                                                  100 * input$CI,
                                                                  "% confidence intervals")) -> p ))
    
    return(p)
  }
  
  corrPlot <- reactive({
    plotAlphaValues (tibDat(), input$minY, input$maxY)
  })
  
  ### rendering
  
  output$corrPlot <- renderPlot({
    corrPlot()
  })
  
  downloadGGPlotButtonServer(
    id = "plotDownload", # <= this should match the ID used in the UI module
    ggplotObject = corrPlot # No parentheses here to pass *expression*
  )
  
  output$res <- renderText({
    ### input validation
    req(input$k)
    req(input$alpha1)
    req(input$n1)
    req(input$alpha2)
    req(input$n2)
    req(input$CI)
    req(input$minY)
    req(input$maxY)
    req(input$dp)
    validate(
      need(checkXbetween(input$k, 5, 500),
           "k must be between 5 and 500 (inclusive)"),
      need(checkXbetween(input$alpha1, .2, 1),
           "alpha1 must be between .2 and 1 (inclusive)"),
      need(checkXbetween(input$n1, 20, 10^5),
           "n1 must be between 5 and 500 (inclusive)"),
      need(checkXbetween(input$alpha2, .2, 1),
           "alpha2 must be between .2 and 1 (inclusive)"),
      need(checkXbetween(input$n2, 20, 10^5),
           "n2 must be between 5 and 500 (inclusive)"),
      need(checkXbetween(input$CI, .8, .99),
           "CI must be between .8 and .99 (inclusive)"),
      need(checkXbetween(input$minY, -1, .5),
           "minY must be between -1 and .5 (inclusive)"),
      need(checkXbetween(input$maxY, .5, 1),
           "maxY must be between .5 and 1 (inclusive)"),
      need(checkXbetween(input$dp, 1, 7),
           "dp must be between -1 and .5 (inclusive)")
    )
    pVal()
  })
  
  output$correlations <- DT::renderDataTable(
    DT::datatable({tibDat()},
                  extensions = "Buttons",
                  options = list(                                                     
                    fixedColumns = TRUE,
                    pageLength = 20,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'frtipB',
                    editable = FALSE,
                    searching = FALSE,
                    buttons = c('copy', 'csv', 'excel', "pdf")
                  ),
    )
  )
}

# Run the application (ends all shiny apps in the one file, app.R format)
shinyApp(ui = ui, server = server)
