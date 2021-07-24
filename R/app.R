source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shiny::shinyOptions(url.townforged = "http://127.0.0.1:28881/json_rpc")
# So the argument url.townforged given to launch the Shiny app is passed down to the server as url.townforged
shiny::shinyOptions(usecairo = TRUE)

shiny::shinyApp(
  ui = uiFaucet,
  server = serverFaucet
)
