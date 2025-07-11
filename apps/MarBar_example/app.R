library(shiny)
library(shinymanager)

# Credentials data
credentials <- data.frame(
  user = c("chris", "shinymanager"),
  password = c("azerty", "12345"),
  # password will automatically be hashed
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# you can use keyring package to set database key
library(keyring)
key_set("R-shinymanager-key", "obiwankenobi")

# Init the database
create_db(
  credentials_data = credentials,
  sqlite_path = "./database.sqlite", # will be created
  passphrase = key_get("R-shinymanager-key", "obiwankenobi")
  # passphrase = "passphrase_wihtout_keyring"
)

# Wrap your UI with secure_app, enabled admin mode or not
ui <- shinymanager::secure_app(ui, enable_admin = TRUE)


server <- function(input, output, session) {
  
  # check_credentials directly on sqlite db
  res_auth <- shinymanager::secure_server(
    check_credentials = check_credentials(
      "./database.sqlite",
      passphrase = key_get("R-shinymanager-key", "obiwankenobi")
      # passphrase = "passphrase_wihtout_keyring"
    )
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # your classic server logic

  
  shinyApp(ui, server)
}