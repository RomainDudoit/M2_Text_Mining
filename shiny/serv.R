
eval(parse("functions.R", encoding="UTF-8"))


server = shinyServer(function(input, output) {
  # A executer une seule fois :
  # reset_baseb_donnes()
  
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
    wordcloud_metier(metier = "DATA ANALYST")
  })
  
  output$wordcloud_DATA_SCIENTIST <- renderPlot({
    wordcloud_metier(metier = "DATA SCIENTIST")
  })
  
  output$wordcloud_DATA_ENGINEER <- renderPlot({
    wordcloud_metier(metier = "DATA ENGINEER")
  })
  ############################################################################## Page 3
  
  output$top_competences_DATA_ANALYST <- renderDataTable({
    top_competences_metier(metier = "DATA ANALYST")
  }, options = list(searching = FALSE, paging = FALSE))
  
  output$top_competences_DATA_SCIENTIST <- renderDataTable({
    top_competences_metier(metier = "DATA SCIENTIST")
  }, options = list(searching = FALSE, paging = FALSE))
  
  output$top_competences_DATA_ENGINEER <- renderDataTable({
    top_competences_metier(metier = "DATA ENGINEER")
  }, options = list(searching = FALSE, paging = FALSE))
  
  output$afc_plot <- renderPlot({
    
    df = df_desc_cat()
    
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

  output$plot_Stat_desc_1 <- renderPlotly({
    df_11 = df_11 %>% filter(libelle_secteur!="NR") %>% arrange(-nb) 
    df_11 = head(df_11,7)
    
    fig <- plot_ly(df_11, x = ~libelle_secteur, y = ~nb, type = 'bar') %>%
      layout(title = "Top 5 des secteurs d'activités",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE,title="Secteur d'activité"),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE,title="%"))
    fig
  })
  
  
})





