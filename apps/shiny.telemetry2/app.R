library(shiny)
library(shinyjs)
library(shiny.telemetry)

jsCode <- "
 shinyjs.bindcookie = function(params) {
   var cookie = Cookies.get(params[0]);
   Shiny.setInputValue('jscookie', [params[0], cookie]);
 }


 shinyjs.setcookie = function(params) {
   Cookies.set(params[1], escape(params[0]), { expires: 0.5 });
 }
"

data_storage <- DataStorageLogFile$new("logs.txt")
telemetry <- Telemetry$new("myApp", data_storage)

shinyApp(
  ui = fluidPage(
    tags$head(tags$script(src = "js.cookie.js")),
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("bindcookie", "setcookie")),
    use_telemetry(),
    numericInput("n", "n", 1),
    plotOutput('plot'),
    verbatimTextOutput('output')
  ),
  server = function(input, output, session) {
    telemetry$start_session(login = FALSE)
    js$bindcookie("uid")
    
    ### from https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951
    session$onSessionEnded(function() {
      stopApp()
    })
    
    proxy_user_id <- reactive({
      req(input$n)
      uid <- input$jscookie[2]
      if(is.null(uid) || is.na(uid)) {
        uid <- uuid::UUIDgenerate() 
        js$setcookie(uid, "uid") 
        } 
      uid }) |>
      bindEvent(input$jscookie, ignoreNULL = FALSE)
    
    observe({
      telemetry$log_login(username = proxy_user_id())
    })
    
    output$plot <- renderPlot({ hist(runif(input$n)) })
  }
)

