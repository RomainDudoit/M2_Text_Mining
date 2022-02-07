
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(FactoMineR)
library(factoextra)
library(gplots)
library(graphics)
library(corrplot)
library(questionr)
library(RMySQL)
library(plotly)
library(httr)
library(jsonlite)
library(stringr)
library(RMySQL)
library(utf8)
#library(shinyWidgets)


eval(parse("ui.R", encoding="UTF-8"))
eval(parse("server.R", encoding="UTF-8"))
#eval(parse("mise_a_jour.R",encoding = "UTF-8"))

shinyApp(ui = ui, server = server)






