library(shiny)
library(shinydashboard)


ui <- fluidPage(
  "Hello, world !!!"
)

server <- function(input,output,session)
shinyApp (ui = ui, server = server)

shiny::runApp()
