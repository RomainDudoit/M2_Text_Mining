
#eval(parse("functions.R", encoding="UTF-8"))


server = shinyServer(function(input, output) {
  # A executer une seule fois :
  # reset_baseb_donnes()
  
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
    
    wordcloud(words = dico$word, freq = dico$n, color = brewer.pal(8, "Dark2"))
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
  
  mydb=connect() # Ouverture de la connexion
  df1 = dbFetch(dbSendQuery(mydb,"SELECT categorie, intitule_offre, description_offre from offre_emploi;"))
  Encoding(df1[["intitule_offre"]]) = "UTF-8"
  Encoding(df1[["description_offre"]]) = "UTF-8"
  df1 <- df1[df1$categorie!="AUTRE",]
  dbDisconnect(mydb)
  
  mydb=connect()
  dfEntreprise <- dbFetch(dbSendQuery(mydb,"SELECT DISTINCT nom_entreprise from offre_emploi;"))
  Encoding(dfEntreprise[["nom_entreprise"]]) = "UTF-8"
  dbDisconnect(mydb) # Fermeture de la connexion
  
  listEntreprise = chartr("àâäéèêëïîôöùûüÿç", "aaaeeeeiioouuuyc", str_to_lower(unlist(dfEntreprise$nom_entreprise)))
  print(head(dfEntreprise))
  
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
                    "estate", "experience","capacite","capacites","necessaire","necessaires","forces","force","quotidien","services","service","etes") 
  

  ############################################################################## Page 2
  
  #-----------------------------------------------------------------------------
  # Wordcloud par metier
  #-----------------------------------------------------------------------------
  output$wordcloud_DATA_ANALYST <- renderPlot({
    wordcloud_metier(metier = "DATA ANALYST",df1)
  })
  
  
  
  output$wordcloud_DATA_SCIENTIST <- renderPlot({
    wordcloud_metier(metier = "DATA SCIENTIST",df1)
  })
  
  output$wordcloud_DATA_ENGINEER <- renderPlot({
    wordcloud_metier(metier = "DATA ENGINEER",df1)
  })
  ############################################################################## Page 3
  
  output$top_competences_DATA_ANALYST <- renderDataTable({
    top_competences_metier(metier = "DATA ANALYST",df1)
  }, options = list(searching = FALSE, paging = FALSE))
  
  output$top_competences_DATA_SCIENTIST <- renderDataTable({
    top_competences_metier(metier = "DATA SCIENTIST",df1)
  }, options = list(searching = FALSE, paging = FALSE))
  
  output$top_competences_DATA_ENGINEER <- renderDataTable({
    top_competences_metier(metier = "DATA ENGINEER",df1)
  }, options = list(searching = FALSE, paging = FALSE))
  
  output$afc_plot <- renderPlot({
    
    df = df1
    
    # Nettoyage
    df$description_offre = Nettoyage_dfdescription_offre(df$description_offre)
    
    # Création du corpus
    corpus = tibble(line = 1:nrow(df), desc = df$description_offre)
    
    res = corpus %>% 
      unnest_tokens(output = word, input = desc) %>% 
      filter(!word %in% Unaccent(stopwords("french"))) %>%    # Enlever les stopwords francais
      filter(!word %in% c(stopwords_spe,listEntreprise)) %>%  # Enlever les stopwords_spe + listEntreprise
      filter(!word %in% setdiff(letters, "r"))                # Enlever les lettres seules saufs "r"
    
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
    contingence = contingence[, colnames(contingence) %in% input$competences]
    
    # calcul de l'AFC
    res.ca <- CA(contingence, graph = FALSE) 
    
    # Graphique AFC
    fviz_ca_biplot (res.ca, repel = TRUE, title	= "Analyse Factorielle des Correspondances")
    
  })
  ############################################################################## Page 4

  mydb=connect() # Ouverture de la connexion
  df_11 = dbGetQuery(mydb,
    "SELECT offre.categorie, COUNT(*) as nb, secteur.libelle_secteur 
    FROM offre_emploi offre LEFT JOIN secteur_activite secteur
    ON offre.id_secteur = secteur.id_secteur
    GROUP BY offre.id_secteur;")
  Encoding(df_11[["libelle_secteur"]]) = "UTF-8"
  dbDisconnect(mydb)

  
  output$value1 <- renderValueBox({
    valueBox(nrow(df1), "Nombre d'offres :", icon = icon("stats",lib='glyphicon'), color = "aqua")
  })
  
  
  output$plot_Stat_desc_1 <- renderDataTable({
    df_11 = df_11 %>% filter(libelle_secteur!="NR") %>% arrange(-nb) 
    df_11 = head(df_11,7)
  }, options = list(searching = FALSE, paging = FALSE))
  

  mydb=connect() # Ouverture de la connexion
  df_12=dbGetQuery(
    mydb,
    "SELECT offre.categorie, COUNT(*) as nb, contrat.type_contrat
    FROM offre_emploi offre LEFT JOIN contrat
    ON offre.id_contrat = contrat.id_contrat
    GROUP BY contrat.type_contrat, offre.categorie;")
  dbDisconnect(mydb)
  
  output$plot_Stat_desc_2 <- renderPlotly({
    #df_12 = df_12 %>% group_by(type_contrat) 
    
    fig <- plot_ly(df_12, labels = ~type_contrat, values = ~nb, type = 'pie') %>%
      layout(title = "Répartition des types de contrat (par catégorie)",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  mydb=connect() # Ouverture de la connexion
  df_13=dbGetQuery(
    mydb,
    "SELECT offre.categorie, COUNT(*) as nb, experience.experience_exigee
    FROM offre_emploi offre LEFT JOIN experience
    ON offre.id_experience = experience.id_experience
    GROUP BY experience.experience_exigee, offre.categorie;")
  dbDisconnect(mydb)
  
  df_13$experience_exigee <- as.factor(df_13$experience_exigee)
  levels(df_13$experience_exigee) <- c("Débutant", "Exgigée", "Souhaitée")
  
  
  output$plot_Stat_desc_3 <- renderPlotly({
    fig <- plot_ly(df_13, labels = ~experience_exigee, values = ~nb, type = 'pie') %>%
      layout(title = "Répartition des expériences exigées par catégorie",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  ############################################################################## Page 5
  
  # output$plot <- renderPlot({
  #   df %>% group_by(region) %>%
  #     summarise(count = n()) %>%
  #     ggplot() +
  #     geom_col(aes(x = count, y= region), fill = "#23798E", width=.6) +
  #     ggtitle("Repartition des offres selon les régions")
  # })
  
  
})





