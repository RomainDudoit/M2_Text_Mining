
rm(list = ls(all = TRUE))
setwd("~/Documents/M2_SISE/Text_Mining/Projet")


library(dplyr)
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

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

df <- read.csv("df_textmining.csv", encoding = "latin1")
df = df %>% select(-X)
table(df$categorie)

######################## NUAGE DE MOTS ######################## 

wordcloud_cat = function(df, cat, nb){
  df = df %>% filter(categorie == cat)
  
  # Nettoyage
  df$description_offre = Unaccent(df$description_offre)
  df$description_offre = str_to_lower(df$description_offre)
  df$description_offre  <- gsub("\n"," ",df$description_offre)
  df$description_offre  <- gsub("[0-9]"," ",df$description_offre)
  df$description_offre  <- gsub("[[:punct:]]"," ",df$description_offre)
  
  # Correction orthographique & Concatenation mots
  
  df$description_offre  <- gsub("model[[:alnum:]]*( )","modele ",df$description_offre)
  
  df$description_offre  <- gsub("developp[[:alnum:]]*( )","developpement ",df$description_offre)
  df$description_offre  <- gsub("accompagn[[:alnum:]]*( )","accompagner ",df$description_offre)
  df$description_offre  <- gsub("particip[[:alnum:]]*( )","participer ",df$description_offre)
  df$description_offre  <- gsub("amelior[[:alnum:]]*( )","ameliorer ",df$description_offre)
  df$description_offre  <- gsub("transform[[:alnum:]]*( )","transformer ",df$description_offre)
  
  df$description_offre  <- gsub(" reportings "," reporting ",df$description_offre)
  df$description_offre  <- gsub(" big data "," bigdata ",df$description_offre)
  
  df$description_offre  <- gsub(" apprentissage automatique "," machinelearning ",df$description_offre)
  df$description_offre  <- gsub(" machine learning "," machinelearning ",df$description_offre)
  
  df$description_offre  <- gsub(" mise en place "," miseenplace ",df$description_offre)
  df$description_offre  <- gsub(" mise en oeuvre "," miseenplace ",df$description_offre)
  df$description_offre  <- gsub(" mettre en place "," miseenplace ",df$description_offre)
  
  df$description_offre  <- gsub(" power bi "," powerbi ",df$description_offre)
  df$description_offre  <- gsub(" business intelligence ","businessintelligence",df$description_offre)
  df$description_offre  <- gsub(" bi "," businessintelligence ",df$description_offre)
  #df$description_offre  <- gsub("businessintelligencedata","businessintelligence",df$description_offre)
  
  df$description_offre  <- gsub(" base de donnees "," basededonnees ",df$description_offre)
  df$description_offre  <- gsub(" bases de donnees "," basededonnees ",df$description_offre)
  
  df$description_offre  <- gsub(" dataviz "," datavisualisation ",df$description_offre)
  df$description_offre  <- gsub(" data visualisation "," datavisualisation ",df$description_offre)
  df$description_offre  <- gsub(" visualisation de donnees "," datavisualisation ",df$description_offre)
  
  df$description_offre  <- gsub(" web scraping "," webscraping ",df$description_offre)
  df$description_offre  <- gsub(" data management "," datamanagement ",df$description_offre)
  df$description_offre  <- gsub(" non supervise "," nonsupervise ",df$description_offre)
  df$description_offre  <- gsub(" non supervises "," nonsupervise ",df$description_offre)
  df$description_offre  <- gsub(" etudes "," etude ",df$description_offre)
  df$description_offre  <- gsub(" besoins "," besoin ",df$description_offre)
  df$description_offre  <- gsub(" solutions "," solution ",df$description_offre)
  
  df$description_offre  <- gsub(" missions "," mission ",df$description_offre)
  df$description_offre  <- gsub(" indicateurs "," indicateur ",df$description_offre)
  df$description_offre  <- gsub(" kpis "," kpi ",df$description_offre)
  df$description_offre  <- gsub(" catalogues "," catalogue ",df$description_offre)
  df$description_offre  <- gsub(" dashbords "," dashbord ",df$description_offre)
  df$description_offre  <- gsub(" decisions "," decision ",df$description_offre)
  df$description_offre  <- gsub(" statistiques "," statistique ",df$description_offre)
  df$description_offre  <- gsub(" projets "," projet ",df$description_offre)
  df$description_offre  <- gsub(" algorithmes "," algorithme ",df$description_offre)
  df$description_offre  <- gsub(" analyses "," analyse ",df$description_offre)
  df$description_offre  <- gsub(" analyser "," analyse ",df$description_offre)
  df$description_offre  <- gsub(" clients "," client ",df$description_offre)
  df$description_offre  <- gsub(" techniques "," technique ",df$description_offre)
  df$description_offre  <- gsub(" informations "," information ",df$description_offre)
  df$description_offre  <- gsub(" resultats "," resultat ",df$description_offre)
  #df$description_offre  <- gsub("( )*[[:alnum:]]businessintelligence[[:alnum:]]*( )","businessintelligence",df$description_offre)
  
  # Création du corpus
  corpus = tibble(line = 1:nrow(df), desc = df$description_offre)
  #print(head(corpus))
  
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
                    "outils","groupe","equipe","equipes","real","connaissance","connaissances","competence","competences","qualite","qualites",
                    "estate", "experience","capacite","capacites","necessaire","necessaires","forces","force","quotidien","services","service") 
  
  
  res = corpus %>% 
    unnest_tokens(output = word, input = desc) %>% 
    filter(!word %in% Unaccent(stopwords("french"))) %>%
    filter(!word %in% stopwords_spe) %>%
    filter(!word %in% letters) 
  
  dico = res %>% count(word, sort = TRUE) %>% arrange(-n)
  
  dico = as.data.frame(head(dico, nb))
  #head(dico)
  set.seed(0)
  wordcloud(words = dico$word, freq = dico$n, color = brewer.pal(8, "Dark2"))
}

wordcloud_cat(df = df, cat = "DATA ANALYST", nb = 30)

wordcloud_cat(df = df, cat = "DATA ENGINEER", nb = 30)

wordcloud_cat(df = df, cat = "DATA SCIENTIST", nb = 30)


######################## AFC ######################## 

mots_afc = c("r", "python", "sql", "spark", "powerbi", "cloud", "bigdata", "algorithme", "basededonnees", "businessintelligence", "modele", 
         "machinelearning", "decision", "sas")

AFC_par_cat = function(df, mots_afc){
  
  # Nettoyage
  df$description_offre = Unaccent(df$description_offre)
  df$description_offre = str_to_lower(df$description_offre)
  df$description_offre  <- gsub("\n"," ",df$description_offre)
  df$description_offre  <- gsub("[0-9]"," ",df$description_offre)
  df$description_offre  <- gsub("[[:punct:]]"," ",df$description_offre)
  
  # Correction orthographique & Concatenation mots
  
  df$description_offre  <- gsub("model[[:alnum:]]*( )","modele ",df$description_offre)
  
  df$description_offre  <- gsub("developp[[:alnum:]]*( )","developpement ",df$description_offre)
  df$description_offre  <- gsub("accompagn[[:alnum:]]*( )","accompagner ",df$description_offre)
  df$description_offre  <- gsub("particip[[:alnum:]]*( )","participer ",df$description_offre)
  df$description_offre  <- gsub("amelior[[:alnum:]]*( )","ameliorer ",df$description_offre)
  df$description_offre  <- gsub("transform[[:alnum:]]*( )","transformer ",df$description_offre)
  
  df$description_offre  <- gsub(" reportings "," reporting ",df$description_offre)
  df$description_offre  <- gsub(" big data "," bigdata ",df$description_offre)
  
  df$description_offre  <- gsub(" apprentissage automatique "," machinelearning ",df$description_offre)
  df$description_offre  <- gsub(" machine learning "," machinelearning ",df$description_offre)
  
  df$description_offre  <- gsub(" mise en place "," miseenplace ",df$description_offre)
  df$description_offre  <- gsub(" mise en oeuvre "," miseenplace ",df$description_offre)
  df$description_offre  <- gsub(" mettre en place "," miseenplace ",df$description_offre)
  
  df$description_offre  <- gsub(" power bi "," powerbi ",df$description_offre)
  df$description_offre  <- gsub(" business intelligence ","businessintelligence",df$description_offre)
  df$description_offre  <- gsub(" bi "," businessintelligence ",df$description_offre)
  #df$description_offre  <- gsub("businessintelligencedata","businessintelligence",df$description_offre)
  
  df$description_offre  <- gsub(" base de donnees "," basededonnees ",df$description_offre)
  df$description_offre  <- gsub(" bases de donnees "," basededonnees ",df$description_offre)
  
  df$description_offre  <- gsub(" dataviz "," datavisualisation ",df$description_offre)
  df$description_offre  <- gsub(" data visualisation "," datavisualisation ",df$description_offre)
  df$description_offre  <- gsub(" visualisation de donnees "," datavisualisation ",df$description_offre)
  
  df$description_offre  <- gsub(" web scraping "," webscraping ",df$description_offre)
  df$description_offre  <- gsub(" data management "," datamanagement ",df$description_offre)
  df$description_offre  <- gsub(" non supervise "," nonsupervise ",df$description_offre)
  df$description_offre  <- gsub(" non supervises "," nonsupervise ",df$description_offre)
  df$description_offre  <- gsub(" etudes "," etude ",df$description_offre)
  df$description_offre  <- gsub(" besoins "," besoin ",df$description_offre)
  df$description_offre  <- gsub(" solutions "," solution ",df$description_offre)
  
  df$description_offre  <- gsub(" missions "," mission ",df$description_offre)
  df$description_offre  <- gsub(" indicateurs "," indicateur ",df$description_offre)
  df$description_offre  <- gsub(" kpis "," kpi ",df$description_offre)
  df$description_offre  <- gsub(" catalogues "," catalogue ",df$description_offre)
  df$description_offre  <- gsub(" dashbords "," dashbord ",df$description_offre)
  df$description_offre  <- gsub(" decisions "," decision ",df$description_offre)
  df$description_offre  <- gsub(" statistiques "," statistique ",df$description_offre)
  df$description_offre  <- gsub(" projets "," projet ",df$description_offre)
  df$description_offre  <- gsub(" algorithmes "," algorithme ",df$description_offre)
  df$description_offre  <- gsub(" analyses "," analyse ",df$description_offre)
  df$description_offre  <- gsub(" analyser "," analyse ",df$description_offre)
  df$description_offre  <- gsub(" clients "," client ",df$description_offre)
  df$description_offre  <- gsub(" techniques "," technique ",df$description_offre)
  df$description_offre  <- gsub(" informations "," information ",df$description_offre)
  df$description_offre  <- gsub(" resultats "," resultat ",df$description_offre)
  #df$description_offre  <- gsub("( )*[[:alnum:]]businessintelligence[[:alnum:]]*( )","businessintelligence",df$description_offre)
  
  # Création du corpus
  corpus = tibble(line = 1:nrow(df), desc = df$description_offre)
  #print(head(corpus))
  
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
                    "outils","groupe","equipe","equipes","real","connaissance","connaissances","competence","competences","qualite","qualites",
                    "estate", "experience","capacite","capacites","necessaire","necessaires","forces","force","quotidien","services","service") 
  
  res = corpus %>% 
    unnest_tokens(output = word, input = desc) %>% 
    filter(!word %in% Unaccent(stopwords("french"))) %>%
    filter(!word %in% stopwords_spe) %>%
    filter(!word %in% setdiff(letters, "r")) # Enlever les lettres seules saufs "r"
  
  dico = res %>% count(word, sort = TRUE) %>% arrange(-n)
  
  # Comptage des termes par document
  compte = res %>% 
    group_by(line, word) %>% 
    summarize(freq=n())
  
  mtd = as.matrix(compte %>%  cast_dtm(document = line, term = word, value = freq))
  
  app_termes = apply(mtd, 2, function(x){sum(x>1)})
  
  mtd_filtre = as.data.frame(mtd[,app_termes > 10])
  
  # Construction de la table de contingence 
  contingence = aggregate.data.frame(x = mtd_filtre, by = list(df$categorie), sum)
  rownames(contingence) = contingence$Group.1
  contingence = contingence %>% select(-Group.1)
  contingence = contingence[, colnames(contingence) %in% mots_afc]
  
  # calcul de l'AFC
  res.ca <- CA(contingence, graph = FALSE) 
  
  # Graphique AFC
  fviz_ca_biplot (res.ca, repel = TRUE)
}


AFC_par_cat(df, mots_afc)












