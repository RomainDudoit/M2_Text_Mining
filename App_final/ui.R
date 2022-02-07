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


ui = shinyUI(fluidPage(
  dashboardPage(skin = "purple",
                title = "Application - Text Mining", # Titre dans le navigateur
                dashboardHeader(title = "Application - Text mining", titleWidth = 300), 
                dashboardSidebar(width = 300,
                                 actionButton("maj", "Mise à jour de la base de données"),
                                 sidebarMenu(
                                   menuItem("Statistiques descriptives", tabName = "page4"),
                                   menuItem("Analyse des offres", tabName = "page2"),
                                   menuItem("Analyse des compétences", tabName = "page3")
                                 )
                ),
                dashboardBody(
                  tabItems(
                    # tabItem(tabName = "page1",
                    #         fluidRow(
                    #           column(12, h2("Importation des données", align = "center")),
                    #           column(12, h4("I. Alimentation de la base de données à l'aide de l'API pôle emploi")),
                    #           column(12, h4("II. Récupération des données"))
                    #           
                    #         )),
                    tabItem(tabName = "page2", 
                            #titlePanel("Analyse par métier"),
                            fluidRow(
                              column(12, h1("Analyse des offres", align = "center")),
                              column(12, sliderInput("nb", "Nombre de mots:", min = 5, max = 100, value = 20)),
                              column(4, h3("DATA ANALYST",align="center"), plotOutput("wordcloud_DATA_ANALYST")),
                              column(4, h3("DATA SCIENTIST",align="center"), plotOutput("wordcloud_DATA_SCIENTIST")),
                              column(4, h3("DATA ENGINEER",align="center"), plotOutput("wordcloud_DATA_ENGINEER")),
                              column(12, h2("Répartition des métiers par département", align = "center"), plotOutput("afc_dep_cat"))
                            )),
                    tabItem(tabName = "page3",
                            fluidRow(
                              column(12, h2("Analyse des compétences", align = "center")),
                              column(12, checkboxGroupInput("competences", "Séléction des compétences à analyser :",
                                                            choices = c("r", "python", "sql", "spark", "powerbi", "cloud","bigdata", "algorithme",
                                                                        "basededonnees", "businessintelligence", "modele", "machinelearning", 
                                                                        "decision", "sas", "azure", "aws", "java","scala", "reporting", "anglais", "statistique"), 
                                                            selected = c("r", "python", "sql", "spark", "powerbi", "cloud","bigdata", "algorithme",
                                                                         "basededonnees", "businessintelligence", "modele", "machinelearning", 
                                                                         "decision", "sas", "azure", "aws", "java","scala", "reporting", "anglais", "statistique"),
                                                            inline = TRUE)),
                              column(12, sliderInput("top", "Top des compétences par métier:", min = 1, max = 15, value = 5)),
                              column(4, h4("DATA ANALYST"), dataTableOutput("top_competences_DATA_ANALYST")),
                              column(4, h4("DATA SCIENTIST"), dataTableOutput("top_competences_DATA_SCIENTIST")),
                              column(4, h4("DATA ENGINEER"), dataTableOutput("top_competences_DATA_ENGINEER")),
                              column(12, h2("Comparaison par metier", align = "center")),
                              column(12, plotOutput("afc_plot"))
                              
                            )),
                    tabItem(tabName = "page4",
                            fluidRow(
                              column(12, h1("Statistiques descriptives par métier", align = "center")),
                              br(),br(),
                              column(12, valueBoxOutput("value1"),valueBoxOutput("value2"),valueBoxOutput("value3")),
                              column(12, checkboxGroupInput("metier_stat", "Métiers à analyser :",
                                                            choices = c("DATA ANALYST","DATA SCIENTIST","DATA ENGINEER"), 
                                                            selected = c("DATA ANALYST","DATA SCIENTIST","DATA ENGINEER"),
                                                            inline = TRUE)),
                              #column(12, sliderInput("Top_secteur", "Top des secteurs d'activité :", min = 1, max = 20, value = 5)),
                              column(7, h3("Nombre d'offres par secteur d'activité",align="center"), sliderInput("Top_secteur", "Top des secteurs d'activité :", min = 1, max = 20, value = 5), dataTableOutput('plot_Stat_desc_1')),
                              column(5, h3("Cartographie des offres par département",align="center"),plotlyOutput('plot_carto')),
                              column(6, h3("Répartition des types de contrat",align="center"), plotlyOutput('plot_Stat_desc_2')),
                              column(6, h3("Répartition des expériences exigées",align="center"), plotlyOutput('plot_Stat_desc_3')),
                              column(12, h3("Top 5 des métiers ROME liés à la Data"), dataTableOutput("plot_Stat_desc_4"))
                            )
                    )
                  )
                )
  )
))