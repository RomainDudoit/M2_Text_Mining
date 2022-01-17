
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)


ui = shinyUI(fluidPage(
  dashboardPage(skin = "purple",
                title = "Application - Text Mining", # Titre dans le navigateur
                dashboardHeader(title = "Application - Text mining", titleWidth = 300), 
                dashboardSidebar(width = 300,
                                 sidebarMenu(
                                   menuItem("Connexion à l'API", tabName = "page1"),
                                   menuItem("Exploration des données", tabName = "page2"),
                                   menuItem("Cartographie des offres", tabName = "page3")
                                 )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "page1", "Test page 1"),
                    tabItem(tabName = "page2", "Test page 2"),
                    tabItem(tabName = "page3", "Test page 3")
                  )
                )
  )
))


server = shinyServer(function(input, output) {
  
})

# Create Shiny object
shinyApp(ui, server)








