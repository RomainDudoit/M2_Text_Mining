
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


eval(parse("ui.R", encoding="UTF-8"))
eval(parse("serv.R", encoding="UTF-8"))

shinyApp(ui = ui, server = server)






