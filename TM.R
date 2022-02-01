
rm(list = ls(all = TRUE))
setwd("~/Documents/M2_SISE/Text_Mining/Projet")

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

library(dplyr)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)


df <- read.csv("df_textmining.csv", encoding = "latin1")
df = df %>% select(-X)
# Nettoyage
df$description_offre = Unaccent(df$description_offre)
df$description_offre = str_to_lower(df$description_offre)
df$description_offre  <- gsub("\n"," ",df$description_offre)
df$description_offre  <- gsub("[0-9]"," ",df$description_offre)
df$description_offre  <- gsub("[[:punct:]]"," ",df$description_offre)

# Correction orthographique 

df$description_offre  <- gsub("model[[:alnum:]]*( )","modele ",df$description_offre)

df$description_offre  <- gsub("developp[[:alnum:]]*( )","developpement ",df$description_offre)
df$description_offre  <- gsub("accompagn[[:alnum:]]*( )","accompagner ",df$description_offre)
df$description_offre  <- gsub("particip[[:alnum:]]*( )","participer ",df$description_offre)
df$description_offre  <- gsub("amelior[[:alnum:]]*( )","ameliorer ",df$description_offre)
df$description_offre  <- gsub("transform[[:alnum:]]*( )","transformer ",df$description_offre)

# Concatenation mots
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
df$description_offre  <- gsub(" analyses ","analyse ",df$description_offre)
df$description_offre  <- gsub(" analyser ","analyse ",df$description_offre)
df$description_offre  <- gsub(" clients ","client ",df$description_offre)
df$description_offre  <- gsub(" techniques ","technique ",df$description_offre)

#df$description_offre  <- gsub("( )*[[:alnum:]]businessintelligence[[:alnum:]]*( )","businessintelligence",df$description_offre)

#df$description_offre  <- gsub(stopwords_spe,"",df$description_offre)



df$description_offre[11]


# m = regexpr("client[[:alnum:]_]*( |:)", df$description_offre) ; print(m)
# regmatches(df$description_offre,m)


# Création du corpus
corpus = tibble(line = 1:nrow(df), desc = df$description_offre)
print(head(corpus))

tweets_corpus <- tm_map(corpus, removeWords,stopwords_spe)

stopwords_spe = c("data", "donnees", "donnee", "cadre", "profil", "formation", "science",
                  "france", "chez", "minimum", "depuis", "jour", "departement","departements", "asie",
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
                  "produits","produit", "collaborateurs","collaborateur", "entreprise", "environnement",
                  "outils","groupe","equipe","equipes","real","connaissance","connaissances","competence","competences", "estate", "experience") 

res = corpus %>% 
  unnest_tokens(output = word, input = desc) %>% 
  filter(!word %in% Unaccent(stopwords("french"))) %>%
  filter(!word %in% stopwords_spe) %>%
  filter(!word %in% setdiff(letters, "r")) # Enlever les lettres seules saufs "r"

dico = res %>% count(word, sort = TRUE) %>% arrange(-n)

dico = as.data.frame(head(dico, 50))
head(dico)
set.seed(0)
wordcloud(words = dico$word, freq = dico$n, color = brewer.pal(8, "Dark2"))

# Comptage des termes par document
compte = res %>% 
  group_by(line, word) %>% 
  summarize(freq=n())

mtd = as.matrix(compte %>%  cast_dtm(document = line, term = word, value = freq))

app_termes = apply(mtd, 2, function(x){sum(x>1)})
summary(app_termes)

mtd_filtre = mtd[,app_termes > 10]

matC = ifelse(mtd_filtre>0,1,0)
dim(matC)
dfC = as.data.frame(matC)
# dfC = cbind.data.frame(
#   matC,
#   categorie = df$categorie)
# dim(dfC)

table(df$categorie)
sum_per_class = aggregate.data.frame(x = dfC, by = list(df$categorie), sum)
print(sum_per_class[,1:15])

aggregate(x = dfC, by = list(df$categorie), sum)

8/(table(df$categorie)[1])

max(sum_per_class[1,])


mtd_pasfiltre = mtd[,app_termes <= 2]
dim(mtd_pasfiltre)
colnames(mtd_pasfiltre)



mdtB = compteB %>%
  cast_dtm(document = line, term = word, value = freq)

matB = as.matrix(mdtB)

app_termes = apply(matB, 2, function(x){sum(x>1)})

matB_filtre = matB[,app_termes <120]
print(dim(matB_filtre))


# Reconstitution du dataframe propre : df_ok
desc1B = res %>% filter(line == 1) %>% select(word)
desc1B$word

###################################### AFC ####################################
library(FactoMineR)
library(factoextra)
library(gplots)
library(graphics)
library(corrplot)
library(questionr)

#mtd_filtre = mtd[,app_termes > 2 & app_termes <95]

mtd_filtre = as.data.frame(mtd[,app_termes > 10])

table(df$categorie)
contingence = aggregate.data.frame(x = mtd_filtre, by = list(df$categorie), sum)
print(contingence[,1:15])

rownames(contingence) = contingence$Group.1
contingence = contingence %>% select(-Group.1)

colnames(contingence)
comp = c("r", "python", "sql", "spark", "powerbi", "cloud", "bigdata", "algorithme", "basededonnees", "businessintelligence", "modele", 
  "machinelearning", "decision", "sas")

contingence = contingence[, colnames(contingence) %in% comp]

# calcul de l'AFC
res.ca <- CA(contingence, graph = FALSE) 

# Graphique AFC
fviz_ca_biplot (res.ca, repel = TRUE)





# **Choix du nombre d'axes**

eig.val <- get_eigenvalue(res.ca) ; eig.val
fviz_eig(res.ca, addlabels = TRUE, ylim = c(0, 100))

# 88% de l'inertie du nuage est représentée par l'axe 1, ce qui est beaucoup. On va surtout interpréter un seul axe, le premier.

# **Interprétation sémantique de l'axe 1 avec les profils lignes**

row<-get_ca_row(res.ca)

coord<-row$coord[,1]
contrib<-row$contrib[,1]
cos2<-row$cos2[,1]
display<-cbind(coord,contrib,cos2) ; display


# Le profil des "très satisfaits" caractérise le côté positif de l'axe 1. Le profil des "insatisfaits" et celui des "très insatisfaits" 
# caractérisent le côté négatif de l'axe 1 et ils s'opposent au profil des "très satisfaits". Cela veut dire que la distribution de 
# l'âge des "très satisfaits" n'est pas la même que celle des "insatisfaits" ou "très insatisfaits".

# En conclusion, ceux qui sont très satisfaits n'ont pas le même âge que ceux qui sont insatisfaits ou très insatisfaits.


library("corrplot")
corrplot(row$cos2, is.corr = FALSE)# graphique du Cos2 des profils lignes
corrplot(row$contrib, is.corr=FALSE)# graphique de la contributions desprofils lignes sur tous les axes

#La représentation graphique des profils lignes sur le 1e plan factoriel

fviz_ca_row (res.ca, col.row = "cos2",
             title ="Profils lignes selon leur cosinus²",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#ou bien

fviz_ca_row(res.ca, pointsize = "contrib", 
            title="Profils lignes selon leur contribution",
            repel = TRUE)



# **Interprétation sémantique de l'axe 1 avec les profils colonnes**

col<-get_ca_col(res.ca)

coord<-col$coord[,1]
contrib<-col$contrib[,1]
cos2<-col$cos2[,1]
display<-cbind(coord,contrib,cos2) ; display

# Le profil des 50-64 ans et celui des +65 ans caractérisent le côté positif de l'axe 1 par opposition au profil des 25-34 ans qui 
# caractérise le côté négatif de l'axe 1. Cela veut dire que les 50-64 ans et les +65 ans n'ont pas la même opinion sur leur vie que les 25-34 ans.


library("corrplot")
corrplot(col$cos2, is.corr = FALSE)# graphique du Cos2 des profils 
corrplot(col$contrib, is.corr=FALSE)# graphique de la contributions desprofils

# La représentation graphique des profils colonnes sur le 1e plan factoriel

fviz_ca_col(res.ca, col.col = "cos2",
            title ="Profils colonnes selon leur cosinus²",
            gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)

#ou bien

fviz_ca_col(res.ca, pointsize = "contrib", 
            title="Profils colonnes selon leur contribution",
            repel = TRUE)


# **Représentation graphique simultanée des profils lignes et des profils colonnes**
fviz_ca_biplot (res.ca, repel = TRUE)

###############################################################################

df <- read.csv("df_textmining.csv", encoding = "latin1")
df = df %>% select(-X)
table(df$categorie)

wordcloud_cat = function(df, cat, nb){
  df = df %>% filter(categorie == cat)

  # Nettoyage
  df$description_offre = Unaccent(df$description_offre)
  df$description_offre = str_to_lower(df$description_offre)
  df$description_offre  <- gsub("\n"," ",df$description_offre)
  df$description_offre  <- gsub("[0-9]"," ",df$description_offre)
  df$description_offre  <- gsub("[[:punct:]]"," ",df$description_offre)
  
  # Correction orthographique 

  df$description_offre  <- gsub("model[[:alnum:]]*( )","modele ",df$description_offre)
  
  df$description_offre  <- gsub("developp[[:alnum:]]*( )","developpement ",df$description_offre)
  df$description_offre  <- gsub("accompagn[[:alnum:]]*( )","accompagner ",df$description_offre)
  df$description_offre  <- gsub("particip[[:alnum:]]*( )","participer ",df$description_offre)
  df$description_offre  <- gsub("amelior[[:alnum:]]*( )","ameliorer ",df$description_offre)
  df$description_offre  <- gsub("transform[[:alnum:]]*( )","transformer ",df$description_offre)
  
  # Concatenation mots
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



###############################################################################




###############################################################################


# Reconstitution du dataframe propre : df_ok

desc1B = res %>% filter(line == 1) %>% select(word)
desc1B$word

str_to_lower
Aletters

?gsub

corpus = tibble(line = 1:nrow(df), desc = df$description_offre)
print(head(corpus))

Unaccent(stopwords("french"))
stopwords("french")
stopwords_spe = c("data", "donnees", "france",
                  "scientist", "analyst", "engineer", "poste",
                  "descriptif", "description",
                  "recherchons", "recherche",
                  "afin", "plus", "etc")

resB = corpus %>% 
  mutate(desc = gsub(x = desc, pattern = "[0-9]", replacement=" ")) %>% 
  mutate(desc = gsub(x = desc, pattern = "'", replacement=" ")) %>% 
  mutate(desc = gsub(x = desc, pattern = "_", replacement=" ")) %>% 
  unnest_tokens(output = word, input = desc) %>% 
  filter(!word %in% Unaccent(stopwords("french"))) %>%
  filter(!word %in% stopwords_spe) %>%
  filter(!word %in% letters) %>%
  select(line, word)
print(resB)


desc1B = resB %>% filter(line == 1) %>% select(word)
desc1B$word

dicoB = resB %>% count(word, sort = TRUE) %>% arrange(-n)
dico = as.data.frame(head(dico, 50))
head(dico)
set.seed(0)
wordcloud(words = dico$word, freq = dico$n, color = brewer.pal(8, "Dark2"))
wordcloud(words = dico$word, freq = dico$n,
          color = brewer.pal(8, "Dark2"))

compteB = resB %>% 
  group_by(line, word) %>% 
  summarize(freq=n())

print(compteB)

mdtB = compteB %>%
  cast_dtm(document = line, term = word, value = freq)

matB = as.matrix(mdtB)

app_termes = apply(matB, 2, function(x){sum(x>1)})

matB_filtre = matB[,app_termes > 5 & app_termes < 400]
print(dim(matB_filtre))

######################################################################################
## n-gramme

library(tidytext)
library(gutenbergr)

resB = corpus %>% 
  mutate(desc = gsub(x = desc, pattern = "[0-9]", replacement=" ")) %>% 
  mutate(desc = gsub(x = desc, pattern = "'", replacement=" ")) %>% 
  mutate(desc = gsub(x = desc, pattern = "_", replacement=" ")) %>% 
  unnest_tokens(output = word, input = desc) %>% 
  filter(!word %in% Unaccent(stopwords("french"))) %>%
  filter(!word %in% stopwords_spe) %>%
  filter(!word %in% letters)
print(resB)


resB = corpus %>% 
  mutate(desc = gsub(x = desc, pattern = "[0-9]", replacement="")) %>% 
  mutate(desc = gsub(x = desc, pattern = "'", replacement=" ")) %>% 
  mutate(desc = gsub(x = desc, pattern = "_", replacement=" ")) %>% 
  unnest_tokens(output = word, input = desc) %>% 
  filter(!word %in% stopwords("french")) %>%
  filter(!word %in% stopwords_spe) %>%
  filter(!word %in% letters) %>%
  select(line, word)
print(resB)

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

Unaccent("à é êè î ø ü ç ß ñ ƒ")

df_bigram = data_frame(line = 1:nrow(df), desc = df$description_offre) %>%
  unnest_tokens(bigram, desc, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)


book <- gutenberg_works(title == "Flatland: A Romance of Many Dimensions")$gutenberg_id %>%
  gutenberg_download() %>% 
  gutenberg_strip()


#Bigram
book_bigram <- data_frame(line = 1:nrow(book), text = book$text)  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
#Trigram
book_trigram <- data_frame(line = 1:nrow(book), text = book$text)  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3)%>%
  count(bigram, sort = TRUE)
#Four-gram 
book_fourgram <- data_frame(line = 1:nrow(book), text = book$text)  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 4)%>%
  count(bigram, sort = TRUE)
#et ainsi de suite ! 


library(tidytext)
#Bigram
book_bigram_clean <- book_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ")
#Trigram
book_trigram_clean <- book_trigram  %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  unite(bigram, word1, word2, word3, sep = " ")




######################################################################################

################################################
# https://www.youtube.com/watch?v=FWYZrf2p7MQ&list=PLi0b6yHwHZcEW0q0dSf6xXhE852fbfwQG&index=1
library(tidyverse)
library(tidytext)

corpus = tibble(line = 1:nrow(df), desc = df$description)
print(head(corpus))

resA = corpus %>% unnest_tokens(output = word, input = desc)
print(resA)

desc1 = resA %>% filter(line == 1) %>% select(word)
desc1$word

# Fréquence des termes dans le 1er doc
desc1 %>% group_by(word) %>% count(sort = TRUE)

# Comptage des termes par document
compteA = resA %>% group_by(line, word) %>% 
  summarize(freq=n())
head(compteA)

# Dictionnaire global des termes
dicoA = resA %>% count(word, sort = TRUE)
head(dicoA)

#------------#
library(tm)
stopwords("french")

resB = corpus %>% 
  mutate(desc = gsub(x = desc, pattern = "[0-9]", replacement="")) %>% 
  unnest_tokens(output = word, input = desc) %>% 
  filter(!word %in% stopwords("french")) %>%
  select(line, word)
print(resB)

desc1B = resB %>% filter(line == 1) %>% select(word)
desc1B$word

dicoB = resB %>% count(word, sort = TRUE)
head(dicoB)

library(wordcloud)

wordcloud(words = dicoB$word, freq = dicoB$n,
          max.words = 50, color = brewer.pal(8, "Dark2"))

#------------#
library(tm)
stopwords("french")
stopwords_spe = c("data", "données", "scientist", "analyst", "scientist", "afin", "plus", "france", "etc")

resB = corpus %>% 
  mutate(desc = gsub(x = desc, pattern = "[0-9]", replacement="")) %>% 
  mutate(desc = gsub(x = desc, pattern = "'", replacement=" ")) %>% 
  mutate(desc = gsub(x = desc, pattern = "_", replacement=" ")) %>% 
  unnest_tokens(output = word, input = desc) %>% 
  filter(!word %in% stopwords("french")) %>%
  filter(!word %in% stopwords_spe) %>%
  filter(!word %in% letters) %>%
  select(line, word)
print(resB)

desc1B = resB %>% filter(line == 1) %>% select(word)
desc1B$word

dicoB = resB %>% count(word, sort = TRUE)
head(dicoB)

library(wordcloud)

set.seed(0)
wordcloud(words = dicoB$word, freq = dicoB$n,
          max.words = 70, color = brewer.pal(8, "Dark2"))

compteB = resB %>% 
  group_by(line, word) %>% 
  summarize(freq=n())

print(compteB)

mdtB = compteB %>%
  cast_dtm(document = line, term = word, value = freq)

matB = as.matrix(mdtB)

app_termes = apply(matB, 2, function(x){sum(x>1)})

matB_filtre = matB[,app_termes <120]
print(dim(matB_filtre))

# Pondération bianire
matC = ifelse(matB_filtre > 0, 1, 0)
matC = as.data.frame(matC)

tmp = as.data.frame(apply(matC, 2, sum))
dicoC = cbind.data.frame(
  word = rownames(tmp),
  n = tmp$`apply(matC, 2, sum)`
)
dicoC <- dicoC[order(-dicoC$n),]
head(dicoC)

dicoC = dicoC %>%  arrange(desc(n))
set.seed(0)
wordcloud(words = dicoC$word, freq = dicoC$n,
          max.words = 70, color = brewer.pal(8, "Dark2"))



df$intitule


###############################################################################################


rm(list = ls(all = TRUE))
setwd("~/Documents/M2_SISE/Text_Mining/Projet")

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

library(dplyr)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)


df <- read.csv("df_textmining.csv", encoding = "latin1")
df = df %>% select(-X)

corpus = Corpus(VectorSource(df$description_offre))

print(corpus$content[5])

corpus <- tm_map(corpus, stemDocument, language = "french")


corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords,c("\n"))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

tweets_corpus <- tm_map(tweets_corpus, removeNumbers) #enlever les nombre
tweets_corpus <- tm_map(tweets_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractères de ponctuation
tweets_corpus <- tm_map(tweets_corpus, content_transformer(tolower)) #mettre en minuscule
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots français "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
tweets_corpus <- tm_map(tweets_corpus, removeWords,c("IA","AI", "artificiel","intelligence","artificalintelligent","artificiel","artificial","ml", "deeplearning")) #enelver le terme commun
tweets_corpus <- tm_map(tweets_corpus, stemDocument, language = "french") #on cherche les radicaux des termes







dico$word[dico$n<10]















