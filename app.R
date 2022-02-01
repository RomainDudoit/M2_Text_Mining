
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
                              column(4, h4("DATA ENGINEER"), plotOutput("wordcloud_DATA_ENGINEER")),
                              column(12, h2("Comparaison par métier", align = "center")),
                              column(12, checkboxGroupInput("mots_afc", "Mots à comparer :",
                                                            choices = c("r", "python", "sql", "spark", "powerbi", "cloud","bigdata", "algorithme",
                                                                        "basededonnees", "businessintelligence", "modele", "machinelearning", 
                                                                        "decision", "sas"), 
                                                            selected = c("r", "python", "sql", "spark", "powerbi", "cloud","bigdata", "algorithme",
                                                                         "basededonnees", "businessintelligence", "modele", "machinelearning", 
                                                                         "decision", "sas"),
                                                            inline = TRUE)),
                              column(12, plotOutput("afc_plot"))#,
                              # column(12, h4("Aperçu du corpus")),
                              # column(12, sidebarPanel(
                              #   column(12, h4("Aperçu du corpus"))
                              # ))
                            )),
                    tabItem(tabName = "page3",
                            fluidRow(
                              column(12, h2("Analyse des compétences", align = "center")),
                              column(12, checkboxGroupInput("compétences", "Séléction des compétences à analyser :",
                                                            choices = c("r", "python", "sql", "spark", "powerbi", "cloud","bigdata", "algorithme",
                                                                        "basededonnees", "businessintelligence", "modele", "machinelearning", 
                                                                        "decision", "sas", "azure", "aws", "java","scala", "reporting", "anglais", "statistique"), 
                                                            selected = c("r", "python", "sql", "spark", "powerbi", "cloud","bigdata", "algorithme",
                                                                         "basededonnees", "businessintelligence", "modele", "machinelearning", 
                                                                         "decision", "sas", "azure", "aws", "java","scala", "reporting", "anglais", "statistique"),
                                                            inline = TRUE)),
                              column(12, sliderInput("top", "Top des compétences par métier:", min = 1, max = 15, value = 5)),
                            
                            )),
                    tabItem(tabName = "page4", "Test page 4"),
                    tabItem(tabName = "page5", "Test page 5")
                  )
                )
  )
))


server = shinyServer(function(input, output) {
  # A executer une seule fois :
  # reset_baseb_donnes()
  
  
  
  
  ##############################################################################
  ## Utile pour tout 
  ##############################################################################
  
  Unaccent <- function(text) {
    text <- gsub("['`^~\"]", " ", text)
    text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
    text <- gsub("['`^~\"]", "", text)
    return(text)
  }
  
  Nettoyage_dfdescription_offre = function(dfdescription_offre){
    
    # Nettoyage
    dfdescription_offre = Unaccent(dfdescription_offre)
    dfdescription_offre = str_to_lower(dfdescription_offre)
    dfdescription_offre  <- gsub("\n"," ",dfdescription_offre)
    dfdescription_offre  <- gsub("[0-9]"," ",dfdescription_offre)
    dfdescription_offre  <- gsub("[[:punct:]]"," ",dfdescription_offre)
    
    # Correction orthographique & Concatenation mots
    
    dfdescription_offre  <- gsub("model[[:alnum:]]*( )","modele ",dfdescription_offre)
    
    dfdescription_offre  <- gsub("developp[[:alnum:]]*( )","developpement ",dfdescription_offre)
    dfdescription_offre  <- gsub("accompagn[[:alnum:]]*( )","accompagner ",dfdescription_offre)
    dfdescription_offre  <- gsub("particip[[:alnum:]]*( )","participer ",dfdescription_offre)
    dfdescription_offre  <- gsub("amelior[[:alnum:]]*( )","ameliorer ",dfdescription_offre)
    dfdescription_offre  <- gsub("transform[[:alnum:]]*( )","transformer ",dfdescription_offre)
    
    dfdescription_offre  <- gsub(" reportings "," reporting ",dfdescription_offre)
    dfdescription_offre  <- gsub(" big data "," bigdata ",dfdescription_offre)
    
    dfdescription_offre  <- gsub(" apprentissage automatique "," machinelearning ",dfdescription_offre)
    dfdescription_offre  <- gsub(" machine learning "," machinelearning ",dfdescription_offre)
    
    dfdescription_offre  <- gsub(" mise en place "," miseenplace ",dfdescription_offre)
    dfdescription_offre  <- gsub(" mise en oeuvre "," miseenplace ",dfdescription_offre)
    dfdescription_offre  <- gsub(" mettre en place "," miseenplace ",dfdescription_offre)
    
    dfdescription_offre  <- gsub(" power bi "," powerbi ",dfdescription_offre)
    dfdescription_offre  <- gsub(" business intelligence ","businessintelligence",dfdescription_offre)
    dfdescription_offre  <- gsub(" bi "," businessintelligence ",dfdescription_offre)
    #dfdescription_offre  <- gsub("businessintelligencedata","businessintelligence",dfdescription_offre)
    
    dfdescription_offre  <- gsub(" base de donnees "," basededonnees ",dfdescription_offre)
    dfdescription_offre  <- gsub(" bases de donnees "," basededonnees ",dfdescription_offre)
    
    dfdescription_offre  <- gsub(" dataviz "," datavisualisation ",dfdescription_offre)
    dfdescription_offre  <- gsub(" data visualisation "," datavisualisation ",dfdescription_offre)
    dfdescription_offre  <- gsub(" visualisation de donnees "," datavisualisation ",dfdescription_offre)
    
    dfdescription_offre  <- gsub(" web scraping "," webscraping ",dfdescription_offre)
    dfdescription_offre  <- gsub(" data management "," datamanagement ",dfdescription_offre)
    dfdescription_offre  <- gsub(" non supervise "," nonsupervise ",dfdescription_offre)
    dfdescription_offre  <- gsub(" non supervises "," nonsupervise ",dfdescription_offre)
    dfdescription_offre  <- gsub(" supervises "," supervise ",dfdescription_offre)
    dfdescription_offre  <- gsub(" etudes "," etude ",dfdescription_offre)
    dfdescription_offre  <- gsub(" besoins "," besoin ",dfdescription_offre)
    dfdescription_offre  <- gsub(" solutions "," solution ",dfdescription_offre)
    
    dfdescription_offre  <- gsub(" missions "," mission ",dfdescription_offre)
    dfdescription_offre  <- gsub(" indicateurs "," indicateur ",dfdescription_offre)
    dfdescription_offre  <- gsub(" kpis "," kpi ",dfdescription_offre)
    dfdescription_offre  <- gsub(" catalogues "," catalogue ",dfdescription_offre)
    dfdescription_offre  <- gsub(" dashbords "," dashbord ",dfdescription_offre)
    dfdescription_offre  <- gsub(" decisions "," decision ",dfdescription_offre)
    dfdescription_offre  <- gsub(" statistiques "," statistique ",dfdescription_offre)
    dfdescription_offre  <- gsub(" projets "," projet ",dfdescription_offre)
    dfdescription_offre  <- gsub(" algorithmes "," algorithme ",dfdescription_offre)
    dfdescription_offre  <- gsub(" analyses "," analyse ",dfdescription_offre)
    dfdescription_offre  <- gsub(" analyser "," analyse ",dfdescription_offre)
    dfdescription_offre  <- gsub(" clients "," client ",dfdescription_offre)
    dfdescription_offre  <- gsub(" techniques "," technique ",dfdescription_offre)
    dfdescription_offre  <- gsub(" informations "," information ",dfdescription_offre)
    dfdescription_offre  <- gsub(" resultats "," resultat ",dfdescription_offre)
    
    #dfdescription_offre  <- gsub(" missions "," resultat ",dfdescription_offre)
    return(dfdescription_offre)
  }
  
  # Liste des stopwords spécifiques 
  
  stopwords_spe = c("data", "donnees", "donnee", "cadre", "profil", "formation", "science", "suivante", "suivantes",
                    "france", "chez", "minimum", "depuis", "jour", "departement", "asie", "pays",
                    "scientists", "scientist", "scientiste", "analyst", "analysts", "engineer", "engineers", "poste",
                    "descriptif", "description", "remuneration", "eur", "ans","recrutement", "salaire",
                    "recherchons", "recherche", "rejoignez", "secteur", "reference", "recrute", "venez",
                    "bnp", "paribas", "fffd", "carrefour", # Ajouter les noms d'entreprises
                    "selon", "alors", "autour", "avant","numero", "toute",
                    "etre", "bac", "cdi", "cdd", "travaille", "notamment", "type", "egalement",
                    "quoi","vue", "fr", "bien", "different", "differents",
                    "afin", "plus", "etc", "www", "sein","deja", "mieux","ca", 
                    "doit", "donne", "faire", "fait", "jusqu", "bon", "bonnes","metier", "metiers", "travail",
                    "bonne", "tous", "toutes", "re", "ainsi", "aussi", "tant", "travailler", "travaillez", "autres",
                    "sous", "chaque", "personnes", "points", "rh", "carriere","emploi", "plein", "pole", "ci",
                    "compte", "langages", "rejoindre", "tout", "titre", "avoir", "tres", "lors","aujourd", "hui",
                    "skill","skills", "partie", "demande", "group", "massy", "demain", "enfin", "region", "ville", "prime",
                    "niveau", "nouveaux", "nouvelles", "mise", "place", "offre","offres",
                    "produits","produit", "collaborateurs","collaborateur", "entreprise", "environnement","charge", 
                    "mission", "projet",
                    "outils","groupe","equipe","equipes","real","connaissance","connaissances","competence","competences","qualite","qualites",
                    "estate", "experience","capacite","capacites","necessaire","necessaires","forces","force","quotidien","services","service") 
  
  
  df_desc_cat <- reactive({
    df <- read.csv("df_textmining.csv", encoding = "latin1")
    df = df %>% select(-X)
  })
  
  
  output$afc_plot <- renderPlot({
    
    df = df_desc_cat()
    
    # Nettoyage
    df$description_offre = Nettoyage_dfdescription_offre(df$description_offre)
    
    # Création du corpus
    corpus = tibble(line = 1:nrow(df), desc = df$description_offre)

    res = corpus %>% 
      unnest_tokens(output = word, input = desc) %>% 
      filter(!word %in% Unaccent(stopwords("french"))) %>%  # Enlever les stopwords francais
      filter(!word %in% stopwords_spe) %>%                  # Enlever les stopwords_spe
      filter(!word %in% setdiff(letters, "r"))              # Enlever les lettres seules saufs "r"
    
    dico = res %>% count(word, sort = TRUE) %>% arrange(-n)
    
    # Comptage des termes par document
    compte = res %>% 
      group_by(line, word) %>% 
      summarize(freq=n())
    
    # Matrice termes documents 
    mtd = as.matrix(compte %>%  cast_dtm(document = line, term = word, value = freq))
    
    app_termes = apply(mtd, 2, function(x){sum(x>1)})
    
    mtd_filtre = as.data.frame(mtd[,app_termes > 10])
    
    # Construction de la table de contingence 
    contingence = aggregate.data.frame(x = mtd_filtre, by = list(df$categorie), sum)
    rownames(contingence) = contingence$Group.1
    contingence = contingence %>% select(-Group.1)
    contingence = contingence[, colnames(contingence) %in% input$mots_afc]
    
    # calcul de l'AFC
    res.ca <- CA(contingence, graph = FALSE) 
    
    # Graphique AFC
    fviz_ca_biplot (res.ca, repel = TRUE, title	= "Analyse Factorielle des Correspondances")
    
  }) 
  
  #-----------------------------------------------------------------------------
  # Wordcloud par metier
  #-----------------------------------------------------------------------------
  output$wordcloud_DATA_ANALYST <- renderPlot({
    df = df_desc_cat() %>% filter(categorie == "DATA ANALYST")
    
    # Nettoyage
    df$description_offre = Nettoyage_dfdescription_offre(df$description_offre)
    
    # Création du corpus
    corpus = tibble(line = 1:nrow(df), desc = df$description_offre)
    
    res = corpus %>% 
      unnest_tokens(output = word, input = desc) %>% 
      filter(!word %in% Unaccent(stopwords("french"))) %>%
      filter(!word %in% stopwords_spe) %>%
      filter(!word %in% letters) 
    
    dico = res %>% count(word, sort = TRUE) %>% arrange(-n)
    
    dico = as.data.frame(head(dico, input$nb))
    set.seed(0)
    wordcloud(words = dico$word, freq = dico$n, color = brewer.pal(8, "Dark2"))
  })
  
  output$wordcloud_DATA_SCIENTIST <- renderPlot({
    df = df_desc_cat() %>% filter(categorie == "DATA SCIENTIST")
    
    # Nettoyage
    df$description_offre = Nettoyage_dfdescription_offre(df$description_offre)
    
    # Création du corpus
    corpus = tibble(line = 1:nrow(df), desc = df$description_offre)
    
    res = corpus %>% 
      unnest_tokens(output = word, input = desc) %>% 
      filter(!word %in% Unaccent(stopwords("french"))) %>%
      filter(!word %in% stopwords_spe) %>%
      filter(!word %in% letters) 
    
    dico = res %>% count(word, sort = TRUE) %>% arrange(-n)
    
    dico = as.data.frame(head(dico, input$nb))
    set.seed(0)
    wordcloud(words = dico$word, freq = dico$n, color = brewer.pal(8, "Dark2"))
  })
  
  output$wordcloud_DATA_ENGINEER <- renderPlot({
    df = df_desc_cat() %>% filter(categorie == "DATA ENGINEER")
    
    # Nettoyage
    df$description_offre = Nettoyage_dfdescription_offre(df$description_offre)
    
    # Création du corpus
    corpus = tibble(line = 1:nrow(df), desc = df$description_offre)
    
    res = corpus %>% 
      unnest_tokens(output = word, input = desc) %>% 
      filter(!word %in% Unaccent(stopwords("french"))) %>%
      filter(!word %in% stopwords_spe) %>%
      filter(!word %in% letters) 
    
    dico = res %>% count(word, sort = TRUE) %>% arrange(-n)
    
    dico = as.data.frame(head(dico, input$nb))
    set.seed(0)
    wordcloud(words = dico$word, freq = dico$n, color = brewer.pal(8, "Dark2"))
  })
    

  
  
})

shinyApp(ui, server)






