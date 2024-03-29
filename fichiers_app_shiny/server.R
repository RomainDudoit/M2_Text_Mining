library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(FactoMineR)
library(factoextra)
library(gplots)
library(graphics)
library(RMySQL)
library(plotly)
library(httr)
library(jsonlite)
library(stringr)
library(RMySQL)
library(utf8)
library(shinyalert)


server = shinyServer(function(input, output) {
  
  eval(parse("mise_a_jour.R",encoding = "UTF-8"))
  
  ############################### FONCTIONS ###############################
  
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
      filter(!word %in% Unaccent(stopwords("french"))) %>%    # Enlever les stopwords francais
      filter(!word %in% c(stopwords_spe,listEntreprise)) %>%  # Enlever les stopwords et les noms des entreprises
      filter(!word %in% setdiff(letters, "r"))                # Enlever les lettres seules sauf "r"
    
    dico = res %>% count(word, sort = TRUE) %>% arrange(-n)
    
    dico_bis = dico[dico$word %in% input$competences,]
    dico_bis$Freq = round((dico_bis$n/sum(dico_bis$n))*100, 2)
    dico_bis = dico_bis %>% dplyr::select(-n)
    colnames(dico_bis) = c("Compétences", "Fréquence d'apparition (en %)")
    head(dico_bis, input$top)
  }
  
  ############################### Mise à jour des données ###############################
  
  observeEvent(input$maj, {
    token=get_token()
    mydb=connect()
    maj=date_last_update(mydb)
    #recupreation des mots cles de l'api 
    for (motcle in c("Data scientist", "Data engineer", "Data analyst"))
      df= data_from_api_to_bdd(mydb,motcle,token, maj)
    print("fin update ")
    dbDisconnect(mydb)
    shinyalert("Done !", "La base de données a bien été mise à jour !", type = "success")
  })
  
  ############################### Liste des stopwords spécifiques ###############################
  
  stopwords_spe = c("data", "donnees", "donnee", "cadre", "profil", "formation", "science", "suivante", "suivantes",
                    "france", "chez", "minimum", "depuis", "jour", "departement", "asie", "pays",
                    "scientists", "scientist", "scientiste", "analyst", "analysts", "engineer", "engineers", "poste",
                    "descriptif", "description", "remuneration", "eur", "ans","recrutement", "salaire",
                    "recherchons", "recherche", "rejoignez", "secteur", "reference", "recrute", "venez",
                    "bnp", "paribas", "fffd", "carrefour","viseo","niveau", "nouveaux", "nouvelles", "mise", "place", "offre","offres",
                    "selon", "alors", "autour", "avant","numero", "toute","etre", "bac", "cdi", "cdd", "travaille", "notamment", "type", "egalement",
                    "quoi","vue", "fr", "bien", "different", "differents","afin", "plus", "etc", "www", "sein","deja", "mieux","ca", 
                    "doit", "donne", "faire", "fait", "jusqu", "bon", "bonnes","metier", "metiers", "travail",
                    "bonne", "tous", "toutes", "re", "ainsi", "aussi", "tant", "travailler", "travaillez", "autres",
                    "sous", "chaque", "personnes", "points", "rh", "carriere","emploi", "plein", "pole", "ci",
                    "compte", "langages", "rejoindre", "tout", "titre", "avoir", "tres", "lors","aujourd", "hui",
                    "skill","skills", "partie", "demande", "group", "massy", "demain", "enfin", "region", "ville", "prime",
                    "produits","produit", "collaborateurs","collaborateur", "entreprise", "environnement","charge","mission", "projet",
                    "outils","groupe","equipe","equipes","real","connaissance","connaissances","competence","competences","qualite","qualites",
                    "estate", "experience","capacite","capacites","necessaire","necessaires","forces","force","quotidien","services","service","etes") 
  
  ############################### Récupération des données ###############################
  
  mydb=connect() # ---------------------------------- df1
  df1 = dbFetch(dbSendQuery(mydb,"SELECT categorie, intitule_offre, description_offre from offre_emploi;"))
  dbDisconnect(mydb)
  Encoding(df1[["intitule_offre"]]) = "UTF-8"
  Encoding(df1[["description_offre"]]) = "UTF-8"
  df1 <- df1[df1$categorie!="AUTRE",]
  
  mydb=connect() # ---------------------------------- dfEntreprise
  dfEntreprise <- dbFetch(dbSendQuery(mydb,"SELECT DISTINCT nom_entreprise from offre_emploi;"))
  dbDisconnect(mydb) 
  Encoding(dfEntreprise[["nom_entreprise"]]) = "UTF-8"
  listEntreprise = chartr("àâäéèêëïîôöùûüÿç", "aaaeeeeiioouuuyc", str_to_lower(unlist(dfEntreprise$nom_entreprise)))

  mydb=connect() # ---------------------------------- df_11 : Nombre d'offre par secteur d'activité et catégorie 
  df_11 = dbGetQuery(mydb,
                     "SELECT offre.categorie,secteur.libelle_secteur,COUNT(*) as nb
                      FROM offre_emploi offre LEFT JOIN secteur_activite secteur
                      ON offre.id_secteur = secteur.id_secteur
                      GROUP BY offre.id_secteur;")
  Encoding(df_11[["libelle_secteur"]]) = "UTF-8"
  dbDisconnect(mydb)
  
  mydb=connect() # ---------------------------------- df_12 : Nombre d'offre par type de contrat (CDI, CDD etc.) et catégorie  
  df_12=dbGetQuery(mydb,
                   "SELECT offre.categorie, COUNT(*) as nb, contrat.type_contrat
                    FROM offre_emploi offre LEFT JOIN contrat
                    ON offre.id_contrat = contrat.id_contrat
                    GROUP BY contrat.type_contrat, offre.categorie;")
  dbDisconnect(mydb)
  
  mydb=connect() # ---------------------------------- df_13 : Nombre d'offre par expérience exigée et catégorie 
  df_13=dbGetQuery(mydb,
                   "SELECT offre.categorie, COUNT(*) as nb, experience.experience_exigee
                    FROM offre_emploi offre LEFT JOIN experience
                    ON offre.id_experience = experience.id_experience
                    GROUP BY experience.experience_exigee, offre.categorie;")
  dbDisconnect(mydb)
  df_13$experience_exigee <- as.factor(df_13$experience_exigee)
  levels(df_13$experience_exigee) <- c("Débutant", "Exgigée", "Souhaitée")
  
  mydb=connect()  # ---------------------------------- df_14 : Nombre d'offre par libelle_rome
  df_14=dbGetQuery(mydb,
                   "SELECT poste.libelle_rome, COUNT(*) as nb
                    FROM offre_emploi offre LEFT JOIN poste
                    ON offre.id_poste = poste.id_poste
                    GROUP BY poste.libelle_rome;")
  dbDisconnect(mydb)
  Encoding(df_14[["libelle_rome"]]) = "UTF-8"
  
  mydb=connect() # ---------------------------------- df_carto : Nombre d'offre par département et catégorie 
  df_carto=dbGetQuery(mydb,
                      "select offre_emploi.categorie, ref_regions.nom_region, ref_departement.nom_departement, Count(*) as nb
                       from ref_regions
                       RIGHT JOIN ref_departement on ref_regions.code_region = ref_departement.code_region
                       RIGHT join ref_communes on ref_departement.code_departement = ref_communes.code_departement
                       RIGHT JOIN offre_emploi on offre_emploi.codeCommune = ref_communes.code_commune
                       group by ref_departement.code_departement, offre_emploi.categorie;")
  dbDisconnect(mydb)
  
  Encoding(df_carto[["nom_region"]]) = "UTF-8"
  Encoding(df_carto[["nom_departement"]]) = "UTF-8"
  df_carto$nom_region=chartr("àâäéèêëïîôöùûüÿç", "aaaeeeeiioouuuyc", df_carto$nom_region)
  df_carto$nom_departement=chartr("àâäéèêëïîôöùûüÿç", "aaaeeeeiioouuuyc",df_carto$nom_departement)
  
  ############################################################################## Page 1 - Statistiques descriptives 
  
  # -------------- Affichage du nombre d'offre par catégorie --------------
  output$value1 <- renderValueBox({
    df= df_12 %>% filter(categorie=="DATA ANALYST")
    valueBox(sum(df$nb), "DATA ANALYST", icon = icon("stats",lib='glyphicon'), color = "green")
  })
  
  output$value2 <- renderValueBox({
    df= df_12 %>% filter(categorie=="DATA SCIENTIST")
    valueBox(sum(df$nb), "DATA SCIENTIST", icon = icon("stats",lib='glyphicon'), color = "blue")
  })
  
  output$value3 <- renderValueBox({
    df= df_12 %>% filter(categorie=="DATA ENGINEER")
    valueBox(sum(df$nb), "DATA ENGINEER", "DATA ENGINEER", icon = icon("stats",lib='glyphicon'), color = "red")
  })
  # -----------------------------------------------------------------------
  
  output$plot_Stat_desc_1 <- renderDataTable({
    df_11 = df_11 %>% filter(libelle_secteur!="NR") %>% filter(categorie %in% input$metier_stat) %>% arrange(-nb) 
    colnames(df_11) = c("Métier","Secteur d'activité","Nombre d'offres")
    df_11 = head(df_11,input$Top_secteur)
  }, options = list(searching = FALSE, paging = FALSE))
  
  output$plot_Stat_desc_2 <- renderPlotly({
    df_12 = df_12 %>% filter(categorie %in% input$metier_stat) # Filtre
    fig <- plot_ly(df_12, labels = ~type_contrat, values = ~nb, type = 'pie') %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$plot_Stat_desc_3 <- renderPlotly({
    df_13 = df_13 %>% filter(categorie %in% input$metier_stat) # Filtre
    fig <- plot_ly(df_13, labels = ~experience_exigee, values = ~nb, type = 'pie') %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$plot_carto <- renderPlotly({
    df_carto = df_carto %>% filter(categorie %in% input$metier_stat) # Filtre
    france <- map_data("france") 
    var <- data.frame(freq=tapply(df_carto$nb, df_carto$nom_departement, sum)) 
    var$var1 <- row.names(var)
    france$nombre_offre <- var$freq[match(france$region,var$var1)]
    
    ggplot(france, aes(x=long, y=lat)) +
      geom_polygon(aes(group=group, text=region, fill=nombre_offre), col="black",lwd=0) +
      scale_fill_continuous(trans = 'reverse')
  })
  
  output$plot_Stat_desc_4 <- renderDataTable({
    df_14 = df_14 %>% arrange(-nb)
    df_14$Freq = round((df_14$nb/sum(df_14$nb))*100, 2)
    df_14 = df_14 %>% dplyr::select(-nb)
    colnames(df_14) = c("Libellé du métier ROME", "Fréquence d'apparition (en %)")
    head(df_14,5)
  }, options = list(searching = FALSE, paging = FALSE))
  
  ############################################################################## Page 2 - Analyse des offres 
  # ------------------------------ Wordcloud ------------------------------
  output$wordcloud_DATA_ANALYST <- renderPlot({
    wordcloud_metier(metier = "DATA ANALYST", df1)
  })
  
  output$wordcloud_DATA_SCIENTIST <- renderPlot({
    wordcloud_metier(metier = "DATA SCIENTIST", df1)
  })
  
  output$wordcloud_DATA_ENGINEER <- renderPlot({
    wordcloud_metier(metier = "DATA ENGINEER", df1)
  })
  # -----------------------------------------------------------------------
  
  output$afc_dep_cat <- renderPlot({
    df_carto = df_carto %>% filter(categorie!="AUTRE")
    tab = xtabs(nb ~ nom_region + categorie, data = df_carto)
    # Calcul de l'AFC + Affichage graphique
    res.ca <- CA(tab, graph = FALSE) 
    fviz_ca_biplot (res.ca, repel = TRUE, title	= "Analyse Factorielle des Correspondances")
  })
  
  ############################################################################## Page 3 - Analyse des compétences
  
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
      filter(!word %in% setdiff(letters, "r"))                # Enlever les lettres seules sauf "r"
    
    dico = res %>% count(word, sort = TRUE) %>% arrange(-n)
    
    # Comptage des termes par document
    compte = res %>% group_by(line, word) %>% summarize(freq=n())
    
    # Matrice termes documents 
    mtd = as.matrix(compte %>% cast_dtm(document = line, term = word, value = freq))
    app_termes = apply(mtd, 2, function(x){sum(x>1)})
    mtd_filtre = as.data.frame(mtd[,app_termes > 10])
    
    # Construction de la table de contingence 
    contingence = aggregate.data.frame(x = mtd_filtre, by = list(df$categorie), sum)
    rownames(contingence) = contingence$Group.1
    contingence = contingence %>% dplyr::select(-Group.1)
    contingence = contingence[, colnames(contingence) %in% input$competences]
    
    # Calcul de l'AFC + Affichage graphique
    res.ca <- CA(contingence, graph = FALSE) 
    fviz_ca_biplot (res.ca, repel = TRUE, title	= "Analyse Factorielle des Correspondances")
  })
  
})
