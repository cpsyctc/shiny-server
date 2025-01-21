# Load Shiny ----
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(kableExtra)
library(knitr)
library(BayesFactor)
library(markdown)
library(Rcpp)
library(fontawesome)
# install devtools in server
# install the following specific kableExtra from Github (other version of this package will break
#   tables with using the collapse_rows() function):
#   devtools::install_github(repo="haozhu233/kableExtra", ref="a6af5c0")

# Function computing the t-test from summaries ----
t.test.summ <- function(m1, m2, sd1, sd2, n1, n2, alternative = "H1.diff0")
{
  group1 <- scale(1:n1) * sd1 + m1
  group2 <- scale(1:n2) * sd2 + m2
  # t.test(x = group1, y = group2, var.equal = TRUE)
  sp     <- sqrt((((n1-1)*(sd1^2)+(n2-1)*(sd2^2)))/(n1+n2-2))
  t      <- (m1 - m2) / (sp * sqrt(1/n1 + 1/n2))
  df     <- n1 + n2 - 2
  p      <- switch(alternative,
                   "H1.diff0"    = 2 * pt(abs(t), df, lower.tail = FALSE),
                   "H1.smaller0"  = pt(t, df, lower.tail = TRUE),
                   "H1.larger0" = pt(t, df, lower.tail = FALSE)
  )
  c(t = t, df = df, p = p)
}

# Load the functions allowing to compute the Bayes factors:
source("R_scripts/BayesFactors.R")
# Load setSliderColor(), which is soon to be deprecated from shinyWidgets:
source("R_scripts/setSliderColor.R")

# Load BOOST's non-central t (better precision and less warnings than R's):
# https://stackoverflow.com/questions/39183938/rs-t-distribution-says-full-precision-may-not-have-been-achieved
# sourceCpp("www/boost_noncentralt.cpp", cacheDir = "cache_rcpp/")
sourceCpp("www/boost_noncentralt.cpp")

# To edit the LaTeX tables:
js <- "
window.MathJax = {
  loader: {
    load: ['[tex]/colortbl']
  },
  tex: {
    inlineMath: [['$', '$']], 
    displayMath: [['$$', '$$']],
    packages: {'[+]': ['colortbl']}
  }
};
"

# User interface ----
ui <- fluidPage(
  title = "The Bayes factor", # Browser tab title
  includeCSS("www/mystyle.css"),
  #!#!#!#!
  # tags$head(
  #   tags$script(async = "",
  #               src   = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
  #   tags$script( HTML(js) )
  # ), 
  #!#!#!#!
  
  # Type LaTeX code, including inline $expressions$:
  # withMathJax(),
  # tags$script(
  #   type = "text/x-mathjax-config", 
  #   "MathJax.Hub.Config({
  #             tex2jax: {inlineMath: [['$','$'], ['$$','$$']]}
  #           });"
  # ),
  
  # Style tabs:
  tags$style(HTML("
                  .tabbable > .nav > li > a                  {background-color: #C0C0C0; color:white; width: 10VW;}
                  .tabbable > .nav > li[class=active]    > a {background-color: #005E3C; color:white; width: 10VW; font-weight: bold}
                  "), 
             HTML(".not_bold label {font-weight:normal;}")),
  
  titlePanel(""),
  #tagList(tags$head(tags$style(type = 'text/css','')),
  tags$style(type = 'text/css', HTML(
    ".container-fluid > .nav > li >
                        a[data-value='The Bayes Factor'] {font-size: 20px}"
  )), 
  navbarPage(
    title = "", #h4(HTML("The Bayes factor!")),
    id = "maintabs", 
    # type = "pills",
    selected = "The Bayes Factor", ##### "Keep in mind", ##### "Bayesian t-test", ##### "Introduction", 
    position = "fixed-top", 
    tabPanel("The Bayes Factor", 
             icon = icon("house-chimney"), 
             uiOutput("instructions")
    ),
    tabPanel("Introduction", 
             tags$style(
               ".btn-mycolor        {background-color: #005E3C1A}",
               ".btn-mycolor.active {background-color: #DCA55966}", 
               ".btn                {text-align: left}"
             ), 
             radioGroupButtons(
               inputId = "intro",
               label = "",
               choices = c("1. Background"                                  = "intro.topic1", 
                           "2. Null hypothesis significance testing (NHST)" = "intro.topic2", 
                           "3. The Bayes factor \u2013 Definition 1"        = "intro.topic3", 
                           "4. The Bayes factor \u2013 Definition 2"        = "intro.topic4", 
                           "5. Priors"                                      = "intro.topic5", 
                           "6. Supporting $\\mathcal{H}_0$"                 = "intro.topic6"
               ),
               direction = "vertical", 
               status = "mycolor", 
               checkIcon = list(
                 yes = icon("ok", lib = "glyphicon")
                 
               )
             ), 
             br(), 
             conditionalPanel("input.intro == 'intro.topic1'", 
                              uiOutput("introduction1a"), 
                              plotOutput("intro.topic1.plot1"), 
                              uiOutput("introduction1b")), 
             conditionalPanel("input.intro == 'intro.topic2'",
                              h3(strong("Null hypothesis significance testing (NHST)")), 
                              sidebarLayout(
                                sidebarPanel(
                                  # style = "position:fixed;width:30%;",
                                  fluidRow(
                                    column(4), 
                                    column(4, align = "center", 
                                           tagList(useShinyjs(),
                                                   actionButton("go_tab2", "Ready", icon = character(0), style = "color: black; background-color: #DCA55980; border-color: #DCA559; border-width: 2px; animation: none;"))
                                    ),
                                    column(4)
                                  ), 
                                  hr(style = "border-top: 2px solid #005E3C;"),
                                  h3(strong("Descriptives")),
                                  h4("Group A"),
                                  fluidRow(
                                    column(4, align = "left",
                                           div(numericInput(inputId = "mean1.tab2",
                                                            label   = em("Mean:"),
                                                            min     = -16,      # -10
                                                            max     = 16,       # 10
                                                            value   = 4.2,   # 0
                                                            step    = 0.1,
                                                            width = "100%"), class = "not_bold")
                                    ),
                                    column(4, align = "left",
                                           div(numericInput(inputId = "sd1.tab2",
                                                            label   = em("SD:"),
                                                            min     = 0,
                                                            max     = NA,
                                                            value   = 2.6,    # 1
                                                            step    = .1,
                                                            width = "100%"), class = "not_bold")
                                    ),
                                    column(4, align = "left",
                                           div(numericInput(inputId = "n1.tab2",
                                                            label   = em("$N$:"),
                                                            min     = 5,
                                                            max     = 50,
                                                            value   = 10,    # 30
                                                            width = "100%"), class = "not_bold")
                                    )
                                  ),
                                  h4("Group B"),
                                  fluidRow(
                                    column(4, align = "left",
                                           div(numericInput(inputId = "mean2.tab2",
                                                            label   = em("Mean:"),
                                                            min     = -16,       # -10
                                                            max     = 16,        # 10
                                                            value   = 2.1,     # 0.2
                                                            step    = 0.1,
                                                            width = "100%"), class = "not_bold")
                                    ),
                                    column(4, align = "left",
                                           div(numericInput(inputId = "sd2.tab2",
                                                            label   = em("SD:"),
                                                            min     = 0,
                                                            max     = NA,
                                                            value   = 1.7,    # 1
                                                            step    = .1,
                                                            width = "100%"), class = "not_bold")
                                    ),
                                    column(4, align = "left",
                                           div(numericInput(inputId = "n2.tab2",
                                                            label   = em("$N$:"),
                                                            min     = 5,
                                                            max     = 50,
                                                            value   = 15,    # 30
                                                            width = "100%"), class = "not_bold")
                                    )
                                  ), 
                                  hr(style = "border-top: 2px solid #005E3C;"),
                                  h3(strong("Classical test")),
                                  em("Null and alternative hypotheses:"),
                                  fluidRow(
                                    column(width = 4, uiOutput("H1hyp.cls")),
                                    column(width = 8,
                                           div(prettyRadioButtons(inputId  = "H1hyp.cls",
                                                                  label    = em("$\\mathcal{H}_1:\\hspace{1mm}$"),
                                                                  choices  = list("$\\delta\\not=0$" = "H1.diff0",
                                                                                  "$\\delta>0$" = "H1.larger0",
                                                                                  "$\\delta<0$" = "H1.smaller0"),
                                                                  selected = "H1.diff0",
                                                                  inline   = TRUE,
                                                                  width    = "100%",
                                                                  status   = "success",
                                                                  shape    = "round",
                                                                  fill     = TRUE
                                           ), class = "not_bold")
                                    )
                                  )
                                ), 
                                
                                mainPanel(
                                  uiOutput("introduction2a"), 
                                  br(), 
                                  fluidRow(
                                    align = "left", 
                                    column(4, align = "left", 
                                           tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), 
                                           div(sliderInput("alpha", em("Sig. level:"), 
                                                           min   = .001, 
                                                           max   = 0.2, 
                                                           value = .05, 
                                                           step  = .01, 
                                                           ticks = FALSE, 
                                                           width = '100%'), class = "not_bold")), 
                                    column(8)
                                  ), 
                                  plotOutput("ttest.crit"), 
                                  br(), br(), 
                                  # tableOutput("ttest"), 
                                  # br(), 
                                  tableOutput("ttestB"),  
                                  uiOutput("introduction2b")
                                )
                              ), 
             ), 
             conditionalPanel("input.intro == 'intro.topic3'", uiOutput("introduction3")), 
             conditionalPanel("input.intro == 'intro.topic4'", uiOutput("introduction4a")), 
             conditionalPanel("input.intro == 'intro.topic4'", h4(em("Box: How to derive the Bayes factor.")), div(style = "border-style: solid; border-color: #DCA559; padding: 10px; color: gray;",
                                                                                                                   uiOutput("introduction4b"))), 
             conditionalPanel("input.intro == 'intro.topic4'", br(), br()), 
             conditionalPanel("input.intro == 'intro.topic5'", 
                              uiOutput("introduction5a"), 
                              sidebarLayout(
                                sidebarPanel(
                                  h3(strong("Bayesian test")),
                                  em("Null and alternative hypotheses:"),
                                  fluidRow(
                                    column(width = 4, uiOutput("H1hyp.bys")),
                                    column(width = 8,
                                           div(prettyRadioButtons(inputId  = "H1hypbys",
                                                                  label    = em("$\\mathcal{H}_1$:$\\hspace{1mm}$"),
                                                                  choices  = list("$\\delta\\not=0$" = "H1.diff0",
                                                                                  "$\\delta>0$" = "H1.larger0",
                                                                                  "$\\delta<0$" = "H1.smaller0",
                                                                                  "$\\delta=\\delta_1$" = "H1.point"),
                                                                  selected = "H1.diff0",
                                                                  inline   = TRUE,
                                                                  width    = "100%",
                                                                  status   = "success",
                                                                  shape    = "round",
                                                                  fill     = TRUE
                                           ), class = "not_bold"),
                                           conditionalPanel("input.H1hypbys == 'H1.point'",
                                                            div(sliderInput(inputId = "H1pointslide.bys", 
                                                                            label   = NULL, 
                                                                            min     = -2, 
                                                                            max     = 2, 
                                                                            value   = .2, 
                                                                            ticks   = FALSE, 
                                                                            step    = .1, 
                                                                            width   = "100%"), class = "not_bold"))
                                    )
                                  ),
                                  br(),
                                  conditionalPanel("input.H1hypbys != 'H1.point'", 
                                                   div(prettyRadioButtons(inputId  = "priorbys",
                                                                          label    = em("Prior for $\\delta$ under $\\mathcal{H}_1$:"),
                                                                          choices  = list("Cauchy" = "cauchy", "Normal" = "normal", "$t$-Student" = "t.student"),
                                                                          selected = "cauchy",
                                                                          inline   = TRUE,
                                                                          width    = "100%",
                                                                          status   = "success",
                                                                          shape    = "round",
                                                                          fill     = TRUE
                                                   ), class = "not_bold"), 
                                                   fluidRow(
                                                     column(width = 4,
                                                            conditionalPanel("input.priorbys == 'cauchy'",
                                                                             div(sliderInput(inputId = "location.c.bys",
                                                                                             label   = em("Location:"),
                                                                                             min     = -3,
                                                                                             max     = 3,
                                                                                             value   = 0,
                                                                                             ticks   = FALSE,
                                                                                             step    = 0.1,
                                                                                             width   = "100%"), class = "not_bold")),
                                                            conditionalPanel("input.priorbys == 'normal'",
                                                                             div(sliderInput(inputId = "location.n.bys",
                                                                                             label   = em("Location:"),
                                                                                             min     = -3,
                                                                                             max     = 3,
                                                                                             value   = 0,
                                                                                             ticks   = FALSE,
                                                                                             step    = 0.1,
                                                                                             width   = "100%"), class = "not_bold")),
                                                            conditionalPanel("input.priorbys == 't.student'",
                                                                             div(sliderInput(inputId = "location.t.bys",
                                                                                             label   = em("Location:"),
                                                                                             min     = -3,
                                                                                             max     = 3,
                                                                                             value   = 0,
                                                                                             ticks   = FALSE,
                                                                                             step    = 0.1,
                                                                                             width   = "100%"), class = "not_bold"))
                                                     ),
                                                     column(width = 4,
                                                            # uiOutput("prior.param2")
                                                            conditionalPanel("input.priorbys == 'cauchy'",
                                                                             div(sliderTextInput(inputId  = "scale.c.bys",
                                                                                                 label    = em("Scale:"),
                                                                                                 choices  = sort(c(seq(.1, 2, by = .1), .707)), 
                                                                                                 selected = .707, 
                                                                                                 width    = "100%"), class = "not_bold")),
                                                            conditionalPanel("input.priorbys == 'normal'",
                                                                             div(sliderTextInput(inputId  = "scale.n.bys",
                                                                                                 label    = em("Scale:"),
                                                                                                 choices  = sort(c(seq(.1, 2, by = .1), .707)), 
                                                                                                 selected = 1, 
                                                                                                 width    = "100%"), class = "not_bold")),
                                                            conditionalPanel("input.priorbys == 't.student'",
                                                                             div(sliderTextInput(inputId  = "scale.t.bys",
                                                                                                 label    = em("Scale:"),
                                                                                                 choices  = sort(c(seq(.1, 2, by = .1), .707)), 
                                                                                                 selected = 1, 
                                                                                                 width    = "100%"), class = "not_bold"))
                                                     ),
                                                     column(width = 4,
                                                            # uiOutput("prior.param3")
                                                            conditionalPanel("input.priorbys == 't.student'",
                                                                             div(sliderInput(inputId = "df.t.bys",
                                                                                             label   = em("df:"),
                                                                                             min     = 1,
                                                                                             max     = 100,
                                                                                             value   = 1,
                                                                                             ticks   = FALSE,
                                                                                             step    = 1,
                                                                                             width   = "100%"), class = "not_bold"))
                                                     )
                                                   ), 
                                                   br(),
                                  ), 
                                  width = 4
                                ),
                                mainPanel(
                                  plotOutput("intro.topic5.plot1"), 
                                  width=8
                                )
                              ), 
                              
                              br(), br(),
                              uiOutput("introduction5b"), 
                              br(), 
                              fluidRow(
                                column(2),
                                column(8, align = 'center', plotOutput("intro.topic5.plot2")), 
                                column(2)), 
                              br(), 
                              uiOutput("introduction5c"), 
                              br(), br(), 
                              h4(em("Box: The marginal likelihood.")), 
                              div(style = "border-style: solid; border-color: #DCA559; padding: 10px; color: gray;",
                                  uiOutput("introduction5d")), 
                              br(), br()
             ),
             conditionalPanel("input.intro == 'intro.topic6'", 
                              uiOutput("introduction6"), 
                              fluidRow(
                                align = "center", 
                                column(3),
                                column(6, align = "center", sliderInput("Ncommon.BF.p", "Sample size:", 
                                                                        min     = 50, 
                                                                        max     = 5000, 
                                                                        value   = 50, 
                                                                        step    = 50, 
                                                                        animate = animationOptions( interval = 100 ), 
                                                                        width   = '100%', 
                                                                        ticks   = FALSE)), 
                                column(3)
                              ),
                              fluidRow(
                                column(1),
                                column(5, align = 'center', plotOutput("intro.topic6.plot1")), 
                                column(5, align = 'center', plotOutput("intro.topic6.plot2")), 
                                column(1))
             ), 
             br(), br()
    ), 
    tabPanel("Bayesian t-test", 
             br(), 
             sidebarLayout(
               sidebarPanel(
                 # style = "position:fixed;width:30%;", 
                 fluidRow(
                   column(4), 
                   column(4, align = "center", 
                          tagList(useShinyjs(),
                                  actionButton("go", "Ready", icon = character(0), style = "color: black; background-color: #DCA55980; border-color: #DCA559; border-width: 2px; animation: none;"))
                   ),
                   column(4)
                 ), 
                 hr(style = "border-top: 2px solid #005E3C;"),
                 h3(strong("Descriptives")),
                 h4("Group A"),
                 fluidRow(
                   column(4, align = "left",
                          div(numericInput(inputId = "mean1",
                                           label   = em("Mean:"),
                                           min     = -16,      # -10
                                           max     = 16,       # 10
                                           value   = 4.2,   # 0
                                           step    = 0.1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "sd1",
                                           label   = em("SD:"),
                                           min     = 0,
                                           max     = NA,
                                           value   = 2.6,    # 1
                                           step    = .1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "n1",
                                           label   = em("$N$:"),
                                           min     = 5,
                                           max     = 50,
                                           value   = 10,    # 30
                                           width = "100%"), class = "not_bold")
                   )
                 ),
                 h4("Group B"),
                 fluidRow(
                   column(4, align = "left",
                          div(numericInput(inputId = "mean2",
                                           label   = em("Mean:"),
                                           min     = -16,       # -10
                                           max     = 16,        # 10
                                           value   = 2.1,     # 0.2
                                           step    = 0.1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "sd2",
                                           label   = em("SD:"),
                                           min     = 0,
                                           max     = NA,
                                           value   = 1.7,    # 1
                                           step    = .1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "n2",
                                           label   = em("$N$:"),
                                           min     = 5,
                                           max     = 50,
                                           value   = 15,    # 30
                                           width = "100%"), class = "not_bold")
                   )
                 ),
                 hr(style = "border-top: 2px solid #005E3C;"),
                 h3(strong("Bayesian test")),
                 em("Null and alternative hypotheses:"),
                 fluidRow(
                   column(width = 4, uiOutput("H0hyp")),
                   column(width = 8,
                          div(prettyRadioButtons(inputId  = "H1hyp",
                                                 label    = em("$\\mathcal{H}_1$:$\\hspace{1mm}$"),
                                                 choices  = list("$\\delta\\not=0$" = "H1.diff0",
                                                                 "$\\delta>0$" = "H1.larger0",
                                                                 "$\\delta<0$" = "H1.smaller0",
                                                                 "$\\delta=\\delta_1$" = "H1.point"),
                                                 selected = "H1.diff0",
                                                 inline   = TRUE,
                                                 width    = "100%",
                                                 status   = "success",
                                                 shape    = "round",
                                                 fill     = TRUE
                          ), class = "not_bold"),
                          # uiOutput("H1point")
                          conditionalPanel("input.H1hyp == 'H1.point'",
                                           div(sliderInput(inputId = "H1pointslide", 
                                                           label   = NULL, 
                                                           min     = -2, 
                                                           max     = 2, 
                                                           value   = .2, 
                                                           ticks   = FALSE, 
                                                           step    = .1, 
                                                           width   = "100%"), class = "not_bold"))
                   )
                 ),
                 br(),
                 conditionalPanel("input.H1hyp != 'H1.point'", 
                                  div(prettyRadioButtons(inputId  = "prior",
                                                         label    = em("Prior for $\\delta$ under $\\mathcal{H}_1$:"),
                                                         choices  = list("Cauchy" = "cauchy", "Normal" = "normal", "$t$-Student" = "t.student"),
                                                         selected = "cauchy",
                                                         inline   = TRUE,
                                                         width    = "100%",
                                                         status   = "success",
                                                         shape    = "round",
                                                         fill     = TRUE
                                  ), class = "not_bold"), 
                                  fluidRow(
                                    column(width = 4,
                                           # uiOutput("prior.param1")
                                           conditionalPanel("input.prior == 'cauchy'",
                                                            div(sliderInput(inputId = "location.c",
                                                                            label   = em("Location:"),
                                                                            min     = -3,
                                                                            max     = 3,
                                                                            value   = 0,
                                                                            ticks   = FALSE,
                                                                            step    = 0.1,
                                                                            width   = "100%"), class = "not_bold")),
                                           conditionalPanel("input.prior == 'normal'",
                                                            div(sliderInput(inputId = "location.n",
                                                                            label   = em("Location:"),
                                                                            min     = -3,
                                                                            max     = 3,
                                                                            value   = 0,
                                                                            ticks   = FALSE,
                                                                            step    = 0.1,
                                                                            width   = "100%"), class = "not_bold")),
                                           conditionalPanel("input.prior == 't.student'",
                                                            div(sliderInput(inputId = "location.t",
                                                                            label   = em("Location:"),
                                                                            min     = -3,
                                                                            max     = 3,
                                                                            value   = 0,
                                                                            ticks   = FALSE,
                                                                            step    = 0.1,
                                                                            width   = "100%"), class = "not_bold"))
                                    ),
                                    column(width = 4,
                                           # uiOutput("prior.param2")
                                           conditionalPanel("input.prior == 'cauchy'",
                                                            div(sliderTextInput(inputId  = "scale.c",
                                                                                label    = em("Scale:"),
                                                                                choices  = sort(c(seq(.1, 2, by = .1), .707)), 
                                                                                selected = .707, 
                                                                                width    = "100%"), class = "not_bold")),
                                           conditionalPanel("input.prior == 'normal'",
                                                            div(sliderTextInput(inputId  = "scale.n",
                                                                                label    = em("Scale:"),
                                                                                choices  = sort(c(seq(.1, 2, by = .1), .707)), 
                                                                                selected = 1, 
                                                                                width    = "100%"), class = "not_bold")),
                                           conditionalPanel("input.prior == 't.student'",
                                                            div(sliderTextInput(inputId  = "scale.t",
                                                                                label    = em("Scale:"),
                                                                                choices  = sort(c(seq(.1, 2, by = .1), .707)), 
                                                                                selected = 1, 
                                                                                width    = "100%"), class = "not_bold"))
                                    ),
                                    column(width = 4,
                                           # uiOutput("prior.param3")
                                           conditionalPanel("input.prior == 't.student'",
                                                            div(sliderInput(inputId = "df.t",
                                                                            label   = em("df:"),
                                                                            min     = 1,
                                                                            max     = 100,
                                                                            value   = 1,
                                                                            ticks   = FALSE,
                                                                            step    = 1,
                                                                            width   = "100%"), class = "not_bold"))
                                    )
                                  ), 
                                  br(),
                 ),
                 div(prettyRadioButtons(inputId  = "BF10.01",
                                        label    = em("Bayes Factor:"),
                                        choices  = list("$BF_{10}$" = "BF10",
                                                        "$BF_{01}$" = "BF01"),
                                        selected = "BF10",
                                        inline   = TRUE,
                                        width    = "100%",
                                        status   = "success",
                                        shape    = "round",
                                        fill     = TRUE), class = "not_bold"),
                 br(),
                 em("Prior probability:"),
                 fluidRow(
                   column(width = 6,
                          setSliderColor(c("#DCA559", "#DCA559"), c(1, 2)),
                          div(sliderInput(inputId = "priorprob0",
                                          label   = em("$\\mathcal{H}_0$:"),
                                          min     = 0,
                                          max     = 100,
                                          value   = 50,
                                          post    = "%",
                                          ticks   = FALSE,
                                          step    = 5), class = "not_bold")),
                   column(width = 6,
                          div(sliderInput(inputId = "priorprob1",
                                          label   = em("$\\mathcal{H}_1$:"),
                                          min     = 0,
                                          max     = 100,
                                          value   = 50,
                                          post    = "%",
                                          ticks   = FALSE,
                                          step    = 5), class = "not_bold"))
                 ), 
                 br(),
                 width = 4
               ),
               
               mainPanel(
                 h4("Summary"),
                 br(), 
                 tableOutput("BF.df1B"),
                 # br(),
                 # tableOutput("BF.df1"),
                 br(), br(),
                 # h4("Bayes factor and the prior distribution for $\\delta$ under $\\mathcal{H}_1$"),
                 # uiOutput("BF.df1"),
                 # br(), br(),
                 h4("Prior and posterior model probabilities"),
                 paste0("Conditional on the two specific models being tested ($\\mathcal{H}_0$ and $\\mathcal{H}_1$), below is a breakdown of how the prior probabilities, the Bayes factor, and the posterior probabilities relate to each other:"), 
                 tableOutput("BF.df2"),
                 br(), br(),
                 h4("Posterior distribution of $\\delta$ under $\\mathcal{H}_1$"),
                 plotOutput("BFplot3"),
                 br(), br(),
                 h4("Bayes factor ", HTML("&#8212;"), " Interpretation 1"),
                 withMathJax(uiOutput("BFint1")),
                 # fluidRow(align = 'center',
                 #          column(3),
                 #          column(3, align = 'center', plotOutput("BFplot1")),
                 #          column(3, align = 'center', tags$style(HTML('
                 #                            .verticalcenter {
                 #                            display: table-cell;
                 #                            height: 400px;
                 #                            vertical-align: middle;
                 #                            }')),
                 #                 tags$div(class = "verticalcenter", uiOutput("BF.formula1"))),
                 #          column(3)),
                 br(), br(),
                 h4("Bayes factor ", HTML("&#8212;"), " Interpretation 2"),
                 withMathJax(uiOutput("BFint2")),
                 br(), 
                 fluidRow(uiOutput("BF.formula3"), align = "center"),
                 br(), br(), 
                 withMathJax(uiOutput("BFint3")),
                 br(),
                 fluidRow(uiOutput("BF.formula2"), align = "center"),
                 br(), br(),
                 plotOutput("BFplot2"),
                 br(), br(), width=8
               )
             )
             
    ),
    tabPanel("Keep in mind",
             br(), 
             sidebarLayout(
               sidebarPanel(
                 # style = "position:fixed;width:30%;", 
                 fluidRow(
                   column(4), 
                   column(4, align = "center", 
                          tagList(useShinyjs(),
                                  actionButton("go_tab4", "Ready", icon = character(0), style = "color: black; background-color: #DCA55980; border-color: #DCA559; border-width: 2px; animation: none;"))
                   ),
                   column(4)
                 ), 
                 hr(style = "border-top: 2px solid #005E3C;"),
                 h3(strong("Descriptives")),
                 h4("Group A"),
                 fluidRow(
                   column(4, align = "left",
                          div(numericInput(inputId = "mean1.tab4",
                                           label   = em("Mean:"),
                                           min     = -16,      # -10
                                           max     = 16,       # 10
                                           value   = 4.2,   # 0
                                           step    = 0.1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "sd1.tab4",
                                           label   = em("SD:"),
                                           min     = 0,
                                           max     = NA,
                                           value   = 2.6,    # 1
                                           step    = .1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "n1.tab4",
                                           label   = em("$N$:"),
                                           min     = 5,
                                           max     = 50,
                                           value   = 10,    # 30
                                           width = "100%"), class = "not_bold")
                   )
                 ),
                 h4("Group B"),
                 fluidRow(
                   column(4, align = "left",
                          div(numericInput(inputId = "mean2.tab4",
                                           label   = em("Mean:"),
                                           min     = -16,       # -10
                                           max     = 16,        # 10
                                           value   = 2.1,     # 0.2
                                           step    = 0.1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "sd2.tab4",
                                           label   = em("SD:"),
                                           min     = 0,
                                           max     = NA,
                                           value   = 1.7,    # 1
                                           step    = .1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "n2.tab4",
                                           label   = em("$N$:"),
                                           min     = 5,
                                           max     = 50,
                                           value   = 15,    # 30
                                           width = "100%"), class = "not_bold")
                   )
                 ),
                 hr(style = "border-top: 2px solid #005E3C;"),
                 h3(strong("Bayesian test")),
                 em("Null and alternative hypotheses:"),
                 fluidRow(
                   column(width = 4, uiOutput("H0hyp.tab4")),
                   column(width = 8,
                          div(prettyRadioButtons(inputId  = "H1hyptab4",
                                                 label    = em("$\\mathcal{H}_1$:$\\hspace{1mm}$"),
                                                 choices  = list("$\\delta\\not=0$" = "H1.diff0",
                                                                 "$\\delta>0$" = "H1.larger0",
                                                                 "$\\delta<0$" = "H1.smaller0",
                                                                 "$\\delta=\\delta_1$" = "H1.point"),
                                                 selected = "H1.diff0",
                                                 inline   = TRUE,
                                                 width    = "100%",
                                                 status   = "success",
                                                 shape    = "round",
                                                 fill     = TRUE
                          ), class = "not_bold"),
                          # uiOutput("H1point.tab4")
                          conditionalPanel("input.H1hyptab4 == 'H1.point'",
                                           div(sliderInput(inputId = "H1pointslide.tab4", 
                                                           label   = NULL, 
                                                           min     = -2, 
                                                           max     = 2, 
                                                           value   = .2, 
                                                           ticks   = FALSE, 
                                                           step    = .1, 
                                                           width   = "100%"), class = "not_bold"))
                   )
                 ),
                 br(),
                 conditionalPanel("input.H1hyptab4 != 'H1.point'", 
                                  div(prettyRadioButtons(inputId  = "priortab4",
                                                         label    = em("Prior for $\\delta$ under $\\mathcal{H}_1$:"),
                                                         choices  = list("Cauchy" = "cauchy", "Normal" = "normal", "$t$-Student" = "t.student"),
                                                         selected = "cauchy",
                                                         inline   = TRUE,
                                                         width    = "100%",
                                                         status   = "success",
                                                         shape    = "round",
                                                         fill     = TRUE
                                  ), class = "not_bold"),
                                  fluidRow(
                                    column(width = 4, 
                                           # uiOutput("prior.param1.tab4")
                                           conditionalPanel("input.priortab4 == 'cauchy'", 
                                                            div(sliderInput(inputId = "location.c.tab4", 
                                                                            label   = em("Location:"), 
                                                                            min     = -3, 
                                                                            max     = 3, 
                                                                            value   = 0, 
                                                                            ticks   = FALSE, 
                                                                            step    = 0.1, 
                                                                            width   = "100%"), class = "not_bold")), 
                                           conditionalPanel("input.priortab4 == 'normal'", 
                                                            div(sliderInput(inputId = "location.n.tab4", 
                                                                            label   = em("Location:"), 
                                                                            min     = -3, 
                                                                            max     = 3, 
                                                                            value   = 0, 
                                                                            ticks   = FALSE, 
                                                                            step    = 0.1, 
                                                                            width   = "100%"), class = "not_bold")), 
                                           conditionalPanel("input.priortab4 == 't.student'", 
                                                            div(sliderInput(inputId = "location.t.tab4", 
                                                                            label   = em("Location:"), 
                                                                            min     = -3, 
                                                                            max     = 3, 
                                                                            value   = 0, 
                                                                            ticks   = FALSE, 
                                                                            step    = 0.1, 
                                                                            width   = "100%"), class = "not_bold"))
                                    ),
                                    column(width = 4, 
                                           # uiOutput("prior.param2.tab4")
                                           conditionalPanel("input.priortab4 == 'cauchy'", 
                                                            div(sliderTextInput(inputId  = "scale.c.tab4",
                                                                                label    = em("Scale:"),
                                                                                choices  = sort(c(seq(.1, 2, by = .1), .707)), 
                                                                                selected = .707, 
                                                                                width    = "100%"), class = "not_bold")), 
                                           conditionalPanel("input.priortab4 == 'normal'", 
                                                            div(sliderTextInput(inputId  = "scale.n.tab4",
                                                                                label    = em("Scale:"),
                                                                                choices  = sort(c(seq(.1, 2, by = .1), .707)), 
                                                                                selected = 1, 
                                                                                width    = "100%"), class = "not_bold")), 
                                           conditionalPanel("input.priortab4 == 't.student'", 
                                                            div(sliderTextInput(inputId  = "scale.t.tab4",
                                                                                label    = em("Scale:"),
                                                                                choices  = sort(c(seq(.1, 2, by = .1), .707)), 
                                                                                selected = 1, 
                                                                                width    = "100%"), class = "not_bold"))
                                    ),
                                    column(width = 4, 
                                           conditionalPanel("input.priortab4 == 't.student'", 
                                                            div(sliderInput(inputId = "df.t.tab4", 
                                                                            label   = em("df:"), 
                                                                            min     = 1, 
                                                                            max     = 100, 
                                                                            value   = 1, 
                                                                            ticks   = FALSE, 
                                                                            step    = 1, 
                                                                            width   = "100%"), class = "not_bold"))
                                    )
                                  ),
                                  br()
                 ),
                 div(prettyRadioButtons(inputId  = "BF10.01.tab4",
                                        label    = em("Bayes Factor:"),
                                        choices  = list("$BF_{10}$" = "BF10",
                                                        "$BF_{01}$" = "BF01"),
                                        selected = "BF10",
                                        inline   = TRUE,
                                        width    = "100%",
                                        status   = "success",
                                        shape    = "round",
                                        fill     = TRUE), class = "not_bold"),
                 br(),
                 em("Prior probability:"),
                 fluidRow(
                   column(width = 6,
                          setSliderColor(c("#DCA559", "#DCA559"), c(1, 2)),
                          div(sliderInput(inputId = "priorprob0.tab4",
                                          label   = em("$\\mathcal{H}_0$:"),
                                          min     = 0,
                                          max     = 100,
                                          value   = 50,
                                          post    = "%",
                                          ticks   = FALSE,
                                          step    = 5), class = "not_bold")),
                   column(width = 6,
                          div(sliderInput(inputId = "priorprob1.tab4",
                                          label   = em("$\\mathcal{H}_1$:"),
                                          min     = 0,
                                          max     = 100,
                                          value   = 50,
                                          post    = "%",
                                          ticks   = FALSE,
                                          step    = 5), class = "not_bold"))
                 ),
                 br(),
                 width = 4
               ),
               
               mainPanel(
                 br(), 
                 h5("Below is a list of important things to keep in mind when interpreting the Bayes factor."),
                 br(), 
                 tags$style(
                   ".btn-mycolor        {background-color: #005E3C1A}",
                   ".btn-mycolor.active {background-color: #DCA55966}", 
                   ".btn                {text-align: left}"
                 ), 
                 radioGroupButtons(
                   inputId = "keepinmind",
                   label = "Keep in mind that...",
                   choices = c("1. Bayes factors are not posterior odds!"                                = "topic1", 
                               "2. Priors matter!"                                                       = "topic2", 
                               "3. Bayes factors provide relative assessments!"                          = "topic3", 
                               "4. Bayes factors cannot establish the presence or absence of an effect!" = "topic4", 
                               "5. Bayes factors are not effect sizes!"                                  = "topic5", 
                               "6. Inconclusive evidence is not evidence of absence!"                    = "topic6", 
                               "7. Using description labels is not problem-free!"                        = "topic7"
                   ),
                   direction = "vertical", 
                   status = "mycolor", 
                   checkIcon = list(
                     yes = icon("ok", lib = "glyphicon")
                     
                   )
                 ), 
                 br(), 
                 h3(strong("Explanation")), 
                 uiOutput("kim.out"), 
                 # br(), 
                 conditionalPanel("input.keepinmind == 'topic1'", 
                                  br(), 
                                  htmlOutput("kim.out.topic1.df1"), 
                                  h4("Interpretation"), 
                                  uiOutput("kim.out.topic1.part2"), 
                                  br(), br(), 
                                  fluidRow(
                                    align = "center", 
                                    column(3),
                                    column(6, align = "center", sliderInput("priorH0.kim.t1", "Prior probability $\\mathcal{H}_0$:", 
                                                                            min     = .01, 
                                                                            max     = .99, 
                                                                            value   = .01, 
                                                                            step    = .01, 
                                                                            animate = animationOptions( interval = 100 ), 
                                                                            width   = '100%', 
                                                                            ticks   = FALSE)), 
                                    column(3)), 
                                  br(), 
                                  fluidRow(
                                    column(12, align = 'center', plotOutput("kim.out.topic1.plot1")),  
                                    ), 
                                  br(), br(), 
                                  uiOutput("kim.out.topic1.part3"), 
                                  br(), br()
                 ), 
                 conditionalPanel("input.keepinmind == 'topic2'", 
                                  br(), 
                                  plotOutput("kim.out.topic2.plot1"), 
                                  br(), br(), 
                                  htmlOutput("kim.out.topic2.df1"), 
                                  uiOutput("kim.out.topic2.part2"), 
                                  br(), br()
                 ),
                 conditionalPanel("input.keepinmind == 'topic3'", 
                                  br(), 
                                  htmlOutput("kim.out.topic3.df1"), 
                                  br(), 
                                  uiOutput("kim.out.topic3.part2"), 
                                  br(), 
                                  plotOutput("kim.out.topic3.plot1"),
                                  br(), 
                                  uiOutput("kim.out.topic3.part3"), 
                                  br(), br()
                 ),
                 conditionalPanel("input.keepinmind == 'topic5'", 
                                  htmlOutput("kim.out.topic5.df1"), 
                                  uiOutput("kim.out.topic5.part2"), 
                                  br(), 
                                  fluidRow(
                                    align = "center", 
                                    column(3),
                                    column(6, align = "center", sliderInput("Ncommon", "Sample size:", 
                                                                            min     = 50, 
                                                                            max     = 5000, 
                                                                            value   = 50, 
                                                                            step    = 50, 
                                                                            animate = animationOptions( interval = 100 ), 
                                                                            width   = '100%', 
                                                                            ticks   = FALSE, )), 
                                    column(3)), 
                                  fluidRow(
                                    column(1),
                                    column(5, align = 'center', plotOutput("kim.out.topic5.plot1")), 
                                    column(5, align = 'center', plotOutput("kim.out.topic5.plot2")), 
                                    column(1)), 
                                  uiOutput("kim.out.topic5.part3")
                 ), 
                 conditionalPanel("input.keepinmind == 'topic7'",
                                  fluidRow(
                                    column(4, selectInput("BFClassTbl", "Choose:",
                                                          choices = c("Jeffreys (1961)",
                                                                      "Kass and Raftery (1995)",
                                                                      "Lee and Wagenmakers (2014)"),
                                                          selected = "Kass and Raftery (1995)", width = '100%')),
                                    column(8, htmlOutput("kim.out.topic7.dfchosen"))), 
                                  uiOutput("kim.out.topic7.part2"), 
                                  br(), 
                                  htmlOutput("kim.out.topic7.df4"), 
                                  uiOutput("kim.out.topic7.part3"), 
                                  br(), br()
                 ), 
                 width = 8
               ))), 
    tabPanel("Let's practice!", 
             br(), 
             fluidRow( htmlOutput("practice") ))
  )#)
  
)

# Server function ----
server <- function(input, output, session) {
  
  # Tab 1 - Instructions:
  source("Tab1-Instructions.R", local = TRUE)
  
  # Tab 2 - Introduction:
  source("Tab2-Introduction.R", local = TRUE)
  
  # Tab 3 - Bayesian t-test:
  source("Tab3-Bayesian-ttest.R", local = TRUE)
  
  # Tab 4 - Keep in mind:
  source("Tab4-KeepInMind.R", local = TRUE)
  
  # Tab 5 - Let's practice:
  output$practice <- renderUI({
    tags$iframe(
      src=paste0("../learnBF_tutorial/Tutorial", sample(1:20, 1), "/"),
      width="100%", height="100%", frameBorder=0, style="height: 100vh;", scrolling = 'yes'
    )
  })
  
  # Change prior prob of H1 when that of H0 changes:
  observeEvent(input$priorprob0,  {
    delay(500, 
          updateSliderInput(session = session, 
                            inputId = "priorprob1", 
                            value   = 100 - input$priorprob0)
    )
  })
  # Change prior prob of H0 when that of H1 changes:
  observeEvent(input$priorprob1,  {
    delay(500, 
          updateSliderInput(session = session, 
                            inputId = "priorprob0", 
                            value   = 100 - input$priorprob1))
  })
  # Change prior prob of H1 when that of H0 changes (tab4):
  observeEvent(input$priorprob0.tab4,  {
    delay(500, 
          updateSliderInput(session = session, 
                            inputId = "priorprob1.tab4", 
                            value   = 100 - input$priorprob0.tab4))
  })
  # Change prior prob of H1 when that of H0 changes (tab4):
  observeEvent(input$priorprob1.tab4,  {
    delay(500, 
          updateSliderInput(session = session, 
                            inputId = "priorprob0.tab4", 
                            value   = 100 - input$priorprob1.tab4))
  })
  
  # Hyperlinks to tabs:
  observeEvent(input$intro.tab2a, {
    updateTabsetPanel(session, "maintabs", "Introduction")
  })
  observeEvent(input$intro.tab2b, {
    updateTabsetPanel(session, "maintabs", "Introduction")
  })
  observeEvent(input$intro.tab2c, {
    updateTabsetPanel(session, "maintabs", "Introduction")
  })
  observeEvent(input$intro.tab2d, {
    updateTabsetPanel(session, "maintabs", "Introduction")
  })
  observeEvent(input$intro.tab3a, {
    updateTabsetPanel(session, "maintabs", "Bayesian t-test")
  })
  observeEvent(input$intro.tab4a, {
    updateTabsetPanel(session, "maintabs", "Keep in mind")
  })
  observeEvent(input$intro.tab4b, {
    updateTabsetPanel(session, "maintabs", "Keep in mind")
  })
  observeEvent(input$intro.tab4c, {
    updateTabsetPanel(session, "maintabs", "Keep in mind")
  })
  observeEvent(input$intro.tab4d, {
    updateTabsetPanel(session, "maintabs", "Keep in mind")
  })
  observeEvent(input$intro.tab5a, {
    updateTabsetPanel(session, "maintabs", "Let's practice!")
  })
  
  # Cohen's d:
  cohen.d <- reactive({ (rv$mean1 - rv$mean2) / sqrt(((rv$n1-1) * (rv$sd1^2) + (rv$n2-1) * (rv$sd2^2)) / (rv$n1 + rv$n2 - 2)) })
  
  # observeEvent(input$go1, {
  #   updateNumericInput(session, 'num', value = input$num1)
  #   updateNumericInput(session, 'num2', value = input$num1)
  # })
  # observeEvent(input$go2, {
  #   updateNumericInput(session, 'num', value = input$num2)
  #   updateNumericInput(session, 'num1', value = input$num2)
  # })
  
  rv <- reactiveValues(mean1 = 4.2,     mean1.tab2 = 4.2,     mean1.tab4 = 4.2, 
                       sd1   = 2.6,      sd1.tab2   = 2.6,      sd1.tab4   = 2.6, 
                       n1    = 10,         n1.tab2    = 10,         n1.tab4    = 10, 
                       mean2 = 2.10,     mean2.tab2 = 2.10,     mean2.tab4 = 2.10, 
                       sd2   = 1.7,      sd2.tab2   = 1.7,      sd2.tab4   = 1.7, 
                       n2    = 15,         n2.tab2    = 15,         n2.tab4    = 15, 
                       H1hyp = "H1.diff0", H1hyptab4 = "H1.diff0", 
                       H1pointslide = .2,  H1pointslide.tab4 = .2, 
                       prior = "cauchy",   priortab4 = "cauchy", 
                       location.c = 0,     location.c.tab4 = 0, 
                       location.n = 0,     location.n.tab4 = 0, 
                       location.t = 0,     location.t.tab4 = 0, 
                       scale.c = .707,     scale.c.tab4 = .707, 
                       scale.n = 1,        scale.n.tab4 = 1, 
                       scale.t = 1,        scale.t.tab4 = 1, 
                       df.t = 1,           df.t.tab4 = 1, 
                       BF10.01 = "BF10",   BF10.01.tab4 = "BF10", 
                       priorprob0 = 50,    priorprob1.tab4 = 50, 
                       priorprob1 = 50,    priorprob1.tab4 = 51
  )
  
  observeEvent(input$go, {
    rv$mean1        <- input$mean1;         rv$mean1.tab2 <- input$mean1;               rv$mean1.tab4 <- input$mean1;
    rv$sd1          <- input$sd1;           rv$sd1.tab2   <- input$sd1;                 rv$sd1.tab4   <- input$sd1; 
    rv$n1           <- input$n1;            rv$n1.tab2    <- input$n1;                  rv$n1.tab4    <- input$n1; 
    rv$mean2        <- input$mean2;         rv$mean2.tab2 <- input$mean2;               rv$mean2.tab4 <- input$mean2; 
    rv$sd2          <- input$sd2;           rv$sd2.tab2   <- input$sd2;                 rv$sd2.tab4   <- input$sd2; 
    rv$n2           <- input$n2;            rv$n2.tab2    <- input$n2;                  rv$n2.tab4    <- input$n2; 
    rv$H1hyp        <- input$H1hyp;         rv$H1hyptab4 <- input$H1hyp; 
    rv$H1pointslide <- input$H1pointslide;  rv$H1pointslide.tab4 <- input$H1pointslide; 
    rv$prior        <- input$prior;         rv$priortab4 <- input$prior; 
    rv$location.c   <- input$location.c;    rv$location.c.tab4 <- input$location.c; 
    rv$location.n   <- input$location.n;    rv$location.n.tab4 <- input$location.n; 
    rv$location.t   <- input$location.t;    rv$location.t.tab4 <- input$location.t; 
    rv$scale.c      <- input$scale.c;       rv$scale.c.tab4 <- input$scale.c; 
    rv$scale.n      <- input$scale.n;       rv$scale.n.tab4 <- input$scale.n; 
    rv$scale.t      <- input$scale.t;       rv$scale.t.tab4 <- input$scale.t; 
    rv$df.t         <- input$df.t;          rv$df.t.tab4 <- input$df.t; 
    rv$BF10.01      <- input$BF10.01;       rv$BF10.01.tab4 <- input$BF10.01; 
    rv$priorprob0   <- input$priorprob0;    rv$priorprob1.tab4 <- input$priorprob0; 
    rv$priorprob1   <- input$priorprob1;    rv$priorprob1.tab4 <- input$priorprob1;
    # 
    updateNumericInput(session, 'mean1.tab2', value = rv$mean1); updateNumericInput(session, 'mean1.tab4', value = rv$mean1)
    updateNumericInput(session, 'sd1.tab2',   value = rv$sd1);   updateNumericInput(session, 'sd1.tab4',   value = rv$sd1)
    updateNumericInput(session, 'n1.tab2',    value = rv$n1);    updateNumericInput(session, 'n1.tab4',    value = rv$n1)
    updateNumericInput(session, 'mean2.tab2', value = rv$mean2); updateNumericInput(session, 'mean2.tab4', value = rv$mean2)
    updateNumericInput(session, 'sd2.tab2',   value = rv$sd2);   updateNumericInput(session, 'sd2.tab4',   value = rv$sd2)
    updateNumericInput(session, 'n2.tab2',    value = rv$n2);    updateNumericInput(session, 'n2.tab4',    value = rv$n2)
    updateNumericInput(session, 'H1hyptab4',        value = rv$H1hyp)
    updateNumericInput(session, 'H1pointslide.tab4', value = rv$H1pointslide)
    updateNumericInput(session, 'priortab4',        value = rv$prior)
    updateNumericInput(session, 'location.c.tab4', value = rv$location.c)
    updateNumericInput(session, 'location.n.tab4', value = rv$location.n)
    updateNumericInput(session, 'location.t.tab4', value = rv$location.t)
    updateSliderTextInput(session, 'scale.c.tab4', selected = rv$scale.c)
    updateSliderTextInput(session, 'scale.n.tab4', selected = rv$scale.n)
    updateSliderTextInput(session, 'scale.t.tab4', selected = rv$scale.t)
    updateNumericInput(session, 'df.t.tab4',    value = rv$df.t)
    updateNumericInput(session, 'BF10.01.tab4',    value = rv$BF10.01)
    updateNumericInput(session, 'priorprob0.tab4', value = rv$priorprob0)
    updateNumericInput(session, 'priorprob1.tab4', value = rv$priorprob1)
  })
  
  observeEvent(input$go_tab2, {
    rv$mean1        <- input$mean1.tab2;         rv$mean1.tab2 <- input$mean1.tab2;               rv$mean1.tab4 <- input$mean1.tab2;
    rv$sd1          <- input$sd1.tab2;           rv$sd1.tab2   <- input$sd1.tab2;                 rv$sd1.tab4   <- input$sd1.tab2; 
    rv$n1           <- input$n1.tab2;            rv$n1.tab2    <- input$n1.tab2;                  rv$n1.tab4    <- input$n1.tab2; 
    rv$mean2        <- input$mean2.tab2;         rv$mean2.tab2 <- input$mean2.tab2;               rv$mean2.tab4 <- input$mean2.tab2; 
    rv$sd2          <- input$sd2.tab2;           rv$sd2.tab2   <- input$sd2.tab2;                 rv$sd2.tab4   <- input$sd2.tab2; 
    rv$n2           <- input$n2.tab2;            rv$n2.tab2    <- input$n2.tab2;                  rv$n2.tab4    <- input$n2.tab2; 
    # 
    updateNumericInput(session, 'mean1', value = rv$mean1); updateNumericInput(session, 'mean1.tab4', value = rv$mean1)
    updateNumericInput(session, 'sd1',   value = rv$sd1);   updateNumericInput(session, 'sd1.tab4',   value = rv$sd1)
    updateNumericInput(session, 'n1',    value = rv$n1);    updateNumericInput(session, 'n1.tab4',    value = rv$n1)
    updateNumericInput(session, 'mean2', value = rv$mean2); updateNumericInput(session, 'mean2.tab4', value = rv$mean2)
    updateNumericInput(session, 'sd2',   value = rv$sd2);   updateNumericInput(session, 'sd2.tab4',   value = rv$sd2)
    updateNumericInput(session, 'n2',    value = rv$n2);    updateNumericInput(session, 'n2.tab4',    value = rv$n2)
  })
  
  observeEvent(input$go_tab4, {
    rv$mean1        <- input$mean1.tab4;         rv$mean1.tab2 <- input$mean1.tab4;               rv$mean1.tab4 <- input$mean1.tab4;
    rv$sd1          <- input$sd1.tab4;           rv$sd1.tab2   <- input$sd1.tab4;                 rv$sd1.tab4   <- input$sd1.tab4; 
    rv$n1           <- input$n1.tab4;            rv$n1.tab2    <- input$n1.tab4;                  rv$n1.tab4    <- input$n1.tab4; 
    rv$mean2        <- input$mean2.tab4;         rv$mean2.tab2 <- input$mean2.tab4;               rv$mean2.tab4 <- input$mean2.tab4; 
    rv$sd2          <- input$sd2.tab4;           rv$sd2.tab2   <- input$sd2.tab4;                 rv$sd2.tab4   <- input$sd2.tab4; 
    rv$n2           <- input$n2.tab4;            rv$n2.tab2    <- input$n2.tab4;                  rv$n2.tab4    <- input$n2.tab4; 
    rv$H1hyp        <- input$H1hyptab4;         rv$H1hyptab4 <- input$H1hyptab4;
    rv$H1pointslide <- input$H1pointslide.tab4;  rv$H1pointslide.tab4 <- input$H1pointslide.tab4;
    rv$prior        <- input$priortab4;         rv$priortab4 <- input$priortab4;
    rv$location.c   <- input$location.c.tab4;    rv$location.c.tab4 <- input$location.c.tab4;
    rv$location.n   <- input$location.n.tab4;    rv$location.n.tab4 <- input$location.n.tab4;
    rv$location.t   <- input$location.t.tab4;    rv$location.t.tab4 <- input$location.t.tab4;
    rv$scale.c      <- input$scale.c.tab4;       rv$scale.c.tab4 <- input$scale.c.tab4;
    rv$scale.n      <- input$scale.n.tab4;       rv$scale.n.tab4 <- input$scale.n.tab4;
    rv$scale.t      <- input$scale.t.tab4;       rv$scale.t.tab4 <- input$scale.t.tab4;
    rv$df.t         <- input$df.t.tab4;          rv$df.t.tab4 <- input$df.t.tab4;
    rv$BF10.01      <- input$BF10.01.tab4;       rv$BF10.01.tab4 <- input$BF10.01.tab4;
    rv$priorprob0   <- input$priorprob0.tab4;    rv$priorprob1.tab4 <- input$priorprob0.tab4;
    rv$priorprob1   <- input$priorprob1.tab4;    rv$priorprob1.tab4 <- input$priorprob1.tab4;
    # 
    updateNumericInput(session, 'mean1', value = rv$mean1); updateNumericInput(session, 'mean1.tab2', value = rv$mean1)
    updateNumericInput(session, 'sd1',   value = rv$sd1);   updateNumericInput(session, 'sd1.tab2',   value = rv$sd1)
    updateNumericInput(session, 'n1',    value = rv$n1);    updateNumericInput(session, 'n1.tab2',    value = rv$n1)
    updateNumericInput(session, 'mean2', value = rv$mean2); updateNumericInput(session, 'mean2.tab2', value = rv$mean2)
    updateNumericInput(session, 'sd2',   value = rv$sd2);   updateNumericInput(session, 'sd2.tab2',   value = rv$sd2)
    updateNumericInput(session, 'n1',    value = rv$n2);    updateNumericInput(session, 'n2.tab2',    value = rv$n2)
    updateNumericInput(session, 'H1hyp',        value = rv$H1hyp)
    updateNumericInput(session, 'H1pointslide', value = rv$H1pointslide)
    updateNumericInput(session, 'prior',        value = rv$prior)
    updateNumericInput(session, 'location.c', value = rv$location.c)
    updateNumericInput(session, 'location.n', value = rv$location.n)
    updateNumericInput(session, 'location.t', value = rv$location.t)
    updateSliderTextInput(session, 'scale.c', selected = rv$scale.c)
    updateSliderTextInput(session, 'scale.n', selected = rv$scale.n)
    updateSliderTextInput(session, 'scale.t', selected = rv$scale.t)
    updateNumericInput(session, 'df.t',    value = rv$df.t)
    updateNumericInput(session, 'BF10.01',    value = rv$BF10.01)
    updateNumericInput(session, 'priorprob0', value = rv$priorprob0)
    updateNumericInput(session, 'priorprob1', value = rv$priorprob1)
  })
  
  # To force these inputs to be available:
  location   <- reactive({ switch(rv$prior, 
                                  'cauchy'    = req(rv$location.c), 
                                  'normal'    = req(rv$location.n), 
                                  't.student' = req(rv$location.t)) })
  scale      <- reactive({ switch(rv$prior, 
                                  'cauchy'    = req(rv$scale.c), 
                                  'normal'    = req(rv$scale.n), 
                                  't.student' = req(rv$scale.t)) })
  df         <- reactive({ switch(rv$prior, 
                                  'cauchy'    = NULL, 
                                  'normal'    = NULL, 
                                  't.student' = req(rv$df.t)) })
  
  # Glow action buttons:
  # Tab 2:
  observeEvent(c(input$mean1.tab2, input$sd1.tab2, input$n1.tab2, 
                 input$mean2.tab2, input$sd2.tab2, input$n2.tab2), 
               {
                 updateActionButton(session, "go_tab2", "Update!", icon("paper-plane"))
                 runjs(paste0('$("#go_tab2").css("animation","glowing 0.9s infinite")'))
               }, 
               ignoreInit = TRUE)
  observeEvent(input$go_tab2, 
               {
                 delay(200, 
                       {
                         updateActionButton(session, "go_tab2", "Ready", character(0))
                         updateActionButton(session, "go",      "Ready", character(0))
                         updateActionButton(session, "go_tab4", "Ready", character(0))
                         runjs(paste0('$("#go_tab2").css("animation","none")'))
                         runjs(paste0('$("#go").css("animation","none")'))
                         runjs(paste0('$("#go_tab4").css("animation","none")'))
                       }
                 )
               }, 
               ignoreInit = TRUE)
  # Tab 3:
  observeEvent(c(input$mean1, input$sd1, input$n1, 
                 input$mean2, input$sd2, input$n2, 
                 input$H1hyp, input$H1pointslide, input$prior, 
                 input$location.c, input$location.n, input$location.t, 
                 input$scale.c, input$scale.n, input$scale.t, input$df.t, 
                 input$BF10.01, input$priorprob0, input$priorprob1), 
               {
                 updateActionButton(session, "go", "Update!", icon("paper-plane"))
                 runjs(paste0('$("#go").css("animation","glowing 0.9s infinite")'))
               }, 
               ignoreInit = TRUE)
  observeEvent(input$go, 
               {
                 delay(200, 
                       {
                         updateActionButton(session, "go_tab2", "Ready", character(0))
                         updateActionButton(session, "go",      "Ready", character(0))
                         updateActionButton(session, "go_tab4", "Ready", character(0))
                         runjs(paste0('$("#go_tab2").css("animation","none")'))
                         runjs(paste0('$("#go").css("animation","none")'))
                         runjs(paste0('$("#go_tab4").css("animation","none")'))
                       }
                 )
               }, 
               ignoreInit = TRUE)
  # Tab 4:
  observeEvent(c(input$mean1.tab4, input$sd1.tab4, input$n1.tab4, 
                 input$mean2.tab4, input$sd2.tab4, input$n2.tab4, 
                 input$H1hyptab4, input$H1pointslide.tab4, input$priortab4, 
                 input$location.c.tab4, input$location.n.tab4, input$location.t.tab4, 
                 input$scale.c.tab4, input$scale.n.tab4, input$scale.t.tab4, input$df.t.tab4, 
                 input$BF10.01.tab4, input$priorprob0.tab4, input$priorprob1.tab4), 
               {
                 updateActionButton(session, "go_tab4", "Update!", icon("paper-plane"))
                 runjs(paste0('$("#go_tab4").css("animation","glowing 0.9s infinite")'))
               }, 
               ignoreInit = TRUE)
  observeEvent(input$go_tab4, 
               {
                 delay(200, 
                       {
                         updateActionButton(session, "go_tab2", "Ready", character(0))
                         updateActionButton(session, "go",      "Ready", character(0))
                         updateActionButton(session, "go_tab4", "Ready", character(0))
                         runjs(paste0('$("#go_tab2").css("animation","none")'))
                         runjs(paste0('$("#go").css("animation","none")'))
                         runjs(paste0('$("#go_tab4").css("animation","none")'))
                       }
                 )
               }, 
               ignoreInit = TRUE)
  
}

# Call the app ----
shinyApp(ui = ui, server = server)

