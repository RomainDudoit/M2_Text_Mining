connect<-function(user='root', password='root', dbname='textmining', host='127.0.0.1', port=3306){
  mydb = RMySQL::dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host, port=port, encoding = "utf8mb4")
  return (mydb)
}

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}


Nettoyage_dfdescription_offre = function(dfdescription_offre){
  # Nettoyage
  #dfdescription_offre = Unaccent(dfdescription_offre)
  dfdescription_offre = str_to_lower(dfdescription_offre)
  dfdescription_offre = chartr("àâäéèêëïîôöùûüÿç", "aaaeeeeiioouuuyc", dfdescription_offre)
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
  dfdescription_offre  <- gsub(" tableaux "," tableau ",dfdescription_offre)
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

wordcloud_metier = function(metier,df1){
  df = df1 %>% filter(categorie == metier)

  # Nettoyage
  df$description_offre = Nettoyage_dfdescription_offre(df$description_offre)

  # Création du corpus
  corpus = tibble(line = 1:nrow(df), desc = df$description_offre)

  res = corpus %>%
    unnest_tokens(output = word, input = desc) %>%
    filter(!word %in% Unaccent(stopwords("french"))) %>%
    filter(!word %in% c(stopwords_spe,listEntreprise)) %>%
    filter(!word %in% letters)

  dico = res %>% count(word, sort = TRUE) %>% arrange(-n)

  dico = as.data.frame(head(dico, input$nb))
  set.seed(0)
  
  plotly::ggplotly(wordcloud(words = dico$word, freq = dico$n, color = brewer.pal(8, "Dark2")))

}

top_competences_metier = function(metier,df1){
  df = df1 %>% filter(categorie == metier)


  # Nettoyage
  df$description_offre = Nettoyage_dfdescription_offre(df$description_offre)

  # Création du corpus
  corpus = tibble(line = 1:nrow(df), desc = df$description_offre)

  res = corpus %>%
    unnest_tokens(output = word, input = desc) %>%
    filter(!word %in% Unaccent(stopwords("french"))) %>%  # Enlever les stopwords francais
    filter(!word %in% c(stopwords_spe,listEntreprise)) %>%
    filter(!word %in% setdiff(letters, "r"))              # Enlever les lettres seules saufs "r"

  dico = res %>% count(word, sort = TRUE) %>% arrange(-n)

  dico_bis = dico[dico$word %in% input$competences,]
  dico_bis$Freq = round((dico_bis$n/sum(dico_bis$n))*100, 2)

  dico_bis = dico_bis %>% select(-n)
  colnames(dico_bis) = c("Compétences", "Fréquence d'apparition (en %)")

  head(dico_bis, input$top)
}