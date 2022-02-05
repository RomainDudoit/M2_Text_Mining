

ui = shinyUI(fluidPage(
  dashboardPage(skin = "purple",
                title = "Application - Text Mining", # Titre dans le navigateur
                dashboardHeader(title = "Application - Text mining", titleWidth = 300), 
                dashboardSidebar(width = 300,
                                 sidebarMenu(
                                   menuItem("Connexion à l'API", tabName = "page1"),
                                   menuItem("Analyse par métier", tabName = "page2"),
                                   menuItem("Analyse des compétences", tabName = "page3"),
                                   menuItem("Statistiques descriptives", tabName = "page4"),
                                   menuItem("Cartographie des offres", tabName = "page5")
                                 )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "page1",
                            fluidRow(
                              column(12, h2("Importation des données", align = "center")),
                              column(12, h4("I. Alimentation de la base de données à l'aide de l'API pôle emploi")),
                              column(12, h4("II. Récupération des données"))
                              
                            )),
                    tabItem(tabName = "page2", 
                            #titlePanel("Analyse par métier"),
                            fluidRow(
                              column(12, h2("Analyse par métier", align = "center")),
                              column(12, sliderInput("nb", "Nombre de mots:", min = 5, max = 100, value = 20)),
                              column(4, h4("DATA ANALYST"), plotOutput("wordcloud_DATA_ANALYST")),
                              column(4, h4("DATA SCIENTIST"), plotOutput("wordcloud_DATA_SCIENTIST")),
                              column(4, h4("DATA ENGINEER"), plotOutput("wordcloud_DATA_ENGINEER"))
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
                              column(12, actionButton("OK1", "OK")),
                              column(12, sliderInput("top", "Top des compétences par métier:", min = 1, max = 15, value = 5)),
                              column(4, h4("DATA ANALYST"), dataTableOutput("top_competences_DATA_ANALYST")),
                              column(4, h4("DATA SCIENTIST"), dataTableOutput("top_competences_DATA_SCIENTIST")),
                              column(4, h4("DATA ENGINEER"), dataTableOutput("top_competences_DATA_ENGINEER")),
                              column(12, h2("Analyse Factorielle des Correspondances", align = "center")),
                              column(12, plotOutput("afc_plot"))
                              
                            )),
                    tabItem(
                      tabName = "page4",
                      fluidRow(
                        column(12, h2("Analyse par secteur d'activité", align = "center")),
                        column(12, plotlyOutput('plot_Stat_desc_1'))
                      )
                    ),
                    tabItem(
                      tabName = "page5",
                      "test page 5"
                      
                    )
                  )
                )
  )
))





