#-------------------------------Mise à jour des données-----------------------------

get_token <- function (){
  #donnees connexion api
  id_client = "PAR_textminingr_b7458e0fe84fea218e101d411a7f861a49e551f3b9bf18c612a5059bdea3be5b"
  cle_secrete = "5feab49ea00bb939a6224dfb224b13d895b7ea21021a67c6468b5a663f410eab"
  
  # I. Generer un access token (client credentials)
  request_body <- list(grant_type = "client_credentials",
                       client_id = id_client,
                       client_secret = cle_secrete,
                       scope = paste("api_offresdemploiv2", "o2dsoffre", 
                                     paste0("application_",id_client), sep = " "))
  # recuperer le token
  result_auth <- httr::POST("https://entreprise.pole-emploi.fr/connexion/oauth2/access_token",
                            query = list(realm = "/partenaire"),
                            body = request_body,
                            encode = "form")
  auth_JSON = fromJSON(rawToChar(result_auth$content), flatten = TRUE) ; auth_JSON
  token = paste("Bearer ", auth_JSON$access_token) ; token
  return (token)
}

# Insertion des données dans la table poste Fonction appelée par une autre 
insert_into_poste <-function (connexion, id, code_rome, libelle_rome, appellation_libelle){
  query = "insert into poste (id_poste,code_rome, libelle_rome, appellation_libelle) values ("
  query = paste(query,"'",id,"','",code_rome,"',",
                DBI::dbQuoteString(connexion, na_to_null(libelle_rome)),",", 
                DBI::dbQuoteString(connexion, na_to_null(appellation_libelle)),")",sep="")
  execute_requete(connexion, query)
}

# Insertion des données dans la table contrat. Fonction appelée par une autre 
insert_into_contrat <-function (connexion, id, type_contrat, libelle_contrat){
  query = "insert into contrat (id_contrat, type_contrat, libelle_contrat) values ("
  query = paste(query,"'",id,"','",na_to_null(type_contrat),"','",na_to_null(libelle_contrat),"')",sep="")
  execute_requete(connexion, query)
}

# Insertion des données dans la table experience Fonction appelée par une autre 
insert_into_experience <-function (connexion, id,libelle_experience, experience_exigee){
  query = "insert into experience (id_experience,libelle_experience, experience_exigee) values ("
  query = paste(query,"'",id,"','",na_to_null(libelle_experience),"','",na_to_null(experience_exigee),"')",sep="")
  execute_requete(connexion, query)
}

# Insertion des données dans la table activité. Fonction appelée par une autre 
insert_into_secteur_activite <-function (connexion,id,libelle_secteur, secteur_activite){
  query = "insert into secteur_activite (id_secteur,libelle_secteur, secteur_activite) values ("
  query = paste(query,"'",id,"',",
                DBI::dbQuoteString(connexion, na_to_null(libelle_secteur)),",",
                DBI::dbQuoteString(connexion,na_to_null(secteur_activite)),")",sep="")
  execute_requete(connexion, query)
}

# Insertion des données dans la table offre_emploi. Fonction appelée par une autre 
insert_into_Offre_emploi <-function (connexion,id,intitule_offre, description_offre, date_creation,
                                     id_contrat, id_poste,id_experience,id_secteur, categorie, codeCommune,
                                     salaire, nom_entreprise){
  id_offre_emploi = id
  query = "insert into Offre_emploi (intitule_offre, description_offre, categorie,date_creation,id_offre,"
  query = paste(query," id_contrat, id_poste, id_experience,id_secteur, salaire, nom_entreprise, codeCommune) values (")
  query = paste(query,
                DBI::dbQuoteString(connexion, na_to_null(intitule_offre)),",",
                DBI::dbQuoteString(connexion,na_to_null(description_offre)),",",
                DBI::dbQuoteString(connexion, na_to_null(categorie)),
                ",STR_TO_DATE('",date_creation,"','%Y-%m-%dT%T.000Z'),",sep="")
  query = paste(query,"'",id_offre_emploi,"','",id_contrat,"','",id_poste,"','",id_experience,"','",
                id_secteur,"',",
                DBI::dbQuoteString(connexion, na_to_null(salaire)),",",
                DBI::dbQuoteString(connexion, na_to_null(nom_entreprise)),
                sep="")
  cd= na_to_null(codeCommune);
  if( cd=="NULL")
    query = paste(query,",'NR')", sep="")
  else
    query = paste(query, ",'" , cd ,"')", sep="")
  execute_requete(connexion, query)
  
}

# Vérification si l'experience existe déjà dans la base en se basant sur les paramètres de la fonction.
# renvoie son id ou null
# fonction appelée par une autre 
get_experience<- function(connexion, libelle_experience, experience_exigee){
  experience_exigee = utf8_encode(experience_exigee)
  libelle_experience = utf8_encode(libelle_experience)
  req = "select id_experience from experience where "
  req = paste(req,"experience_exigee =",
              DBI::dbQuoteString(connexion, na_to_null(experience_exigee)),sep="")
  req = paste(req," and libelle_experience =",
              DBI::dbQuoteString(connexion, na_to_null(libelle_experience)),sep="")
  return (DBI::dbGetQuery(connexion, req)[1,1])
}

# Vérification si secteur de l'activité existe déjà dans la base en se basant sur les paramètres de la fonction.
# renvoie son id ou null
# fonction appelée par une autre 
get_secteur_activite<- function(connexion, libelle_secteur, secteur_activite){
  
  libelle_secteur = utf8_encode(libelle_secteur)
  secteur_activite = utf8_encode(secteur_activite)  
  req = "select id_secteur from secteur_activite where "
  req = paste(req,"libelle_secteur =",
              DBI::dbQuoteString(connexion, na_to_null(libelle_secteur)),sep="")
  req = paste(req," and secteur_activite =",
              DBI::dbQuoteString(connexion, na_to_null(secteur_activite)),sep="")
  #print(req)
  return (DBI::dbGetQuery(connexion, req)[1,1])
}

# Vérification si l'contrat existe déjà dans la base en se basant sur les paramètres de la fonction.
# renvoie son id ou null
# fonction appelée par une autre 
get_contrat<- function(connexion, type_contrat, libelle_contrat){
  libelle_contrat = utf8_encode(libelle_contrat)
  req = "select id_contrat from contrat where "
  req = paste(req,"type_contrat ='",na_to_null(type_contrat),"'",sep="")
  req = paste(req," and libelle_contrat =",
              DBI::dbQuoteString(connexion, na_to_null(libelle_contrat)),sep="")
  return (DBI::dbGetQuery(connexion, req)[1,1])
}


# Vérification si le poste existe déjà dans la base en se basant sur les paramètres de la fonction.
# renvoie son id ou null
# fonction appelée par une autre 
get_poste<- function(connexion, code_rom){
  req = paste("select id_poste from poste where code_rome='",code_rom,"'", sep="")
  return (DBI::dbGetQuery(connexion, req)[1,1])
}


#Execution de la requête passée en paramère en UTF-8
#Fonction appelée par une autre
execute_requete <-function (connexion, query){
  RMySQL::dbSendQuery(connexion,"SET NAMES utf8mb4;")
  RMySQL::dbSendQuery(connexion,"SET CHARACTER SET utf8mb4;")
  #print(query)
  return (DBI::dbGetQuery(connexion,query))
}


# Vérification si l'id de ligne recupérée de l'APIexiste déjà dans la base (table offre_emploi)
# si oui renvoie l'id sinon renvoie null
# cette fonction permet de ne pas insérer la même offre plusieurs fois dans la base
# quand nous faisons des recherches avec des mots clés diffrents mais qui peuvent amener des 
# resultats en doublons
id_exists<- function(connexion, id){
  rs = RMySQL::dbSendQuery(connexion, paste("select id_offre  from Offre_emploi where id_offre='",id,"'",sep=""))
  data=dbFetch(rs,1)
  count = RMySQL::dbGetRowCount(rs)
  RMySQL::dbClearResult(rs)
  if (count==0){
    return (FALSE)
  }else{
    return (TRUE)
  }
}

# sert à remplacer na et null par NULL dans les requêtes
#si le paramètre est un text, renvoie le text lui même
#si le paramètre est un na, renvoie le text NULL
#si le paramètre est un null, renvoie le text NULL
na_to_null <- function ( text){
  if(is.null(text) )
    return ("NULL")
  if( is.na(text))
    return ("NULL")
  return (text)
}

# fonction qui prend en paramètre un dataframe contenant les résultats des appels API
# pour l'insérer dans la base 
insert_data_int_bdd<- function (connexion,df){
  
  #parcourir le dataframe ligne par ligne
  for(i in 1:nrow(df)) {
    id = df[i,"id"]
    # si la ligne existe déjà dans la base, on passe à la ligne suivante
    # pour plus de détails, voir les commentaires de la fonction id_exists
    if(id_exists(connexion, id)){
      next;
    }
    # vérifier si poste existe 
    # s'il n'existe pas on l'insère 
    id_poste= get_poste(connexion, df[i,"romeCode"])
    if(is.na(id_poste)){
      insert_into_poste (connexion,id, df[i,"romeCode"], df[i,"romeLibelle"],
                         df[i,"appellationlibelle"])
      id_poste=id
    }
    
    # vérifier si contrat existe 
    # s'il n'existe pas on l'insère 
    id_contrat = get_contrat(connexion, df[i,"typeContrat"], df[i,"typeContratLibelle"])
    if(is.na(id_contrat)){
      insert_into_contrat(connexion,id,df[i,"typeContrat"], df[i,"typeContratLibelle"])
      id_contrat=id
    }
    # vérifier si experience existe 
    # s'il n'existe pas on l'insère 
    id_experience = get_experience(connexion, df[i,"experienceLibelle"], df[i,"experienceExige"])
    if(is.na(id_experience)){
      insert_into_experience (connexion,id,df[i,"experienceLibelle"], df[i,"experienceExige"])
      id_experience=id
    }
    #nettoyage: nous avons remarqué des offres sans secteur d'activté.
    #on les insère dans la base avec le mot NR
    secteurActiviteLibelle = df[i,"secteurActiviteLibelle"]
    secteurActivite = df[i,"secteurActivite"]
    if(is.null(secteurActiviteLibelle) ){
      secteurActiviteLibelle="NR"
    }
    else  if(is.na(secteurActiviteLibelle)){
      secteurActiviteLibelle="NR"
    }
    if(is.null(secteurActivite) ){
      secteurActivite="NR"
    }
    else  if( is.na(secteurActivite)){
      secteurActivite="NR"
    }
    
    # vérifier si secteur d'activité existe 
    # s'il n'existe pas on l'insère 
    id_secteur = get_secteur_activite(connexion, secteurActiviteLibelle, secteurActivite)
    if(is.na(id_secteur)){
      insert_into_secteur_activite(connexion, id, secteurActiviteLibelle, secteurActivite)
      id_secteur=id
    }
    #insérer dans la table des faits offre_emploi
    insert_into_Offre_emploi (connexion,id,df[i,"intitule"],  df[i,"description"],df[i,"dateCreation"],
                              id_contrat, id_poste,id_experience,id_secteur,df[i,"categorie"], 
                              df[i,"lieuTravail.commune"],
                              df[i,"salaire.libelle"],df[i,"entreprise.nom"])
  }
}

#---------------------------- fonction finale --------------------------------------------#
# renvoie la date de la dernière mise à jour de la base ou NA
# à utiliser au moment de recupèration des données de l'API
date_last_update<- function(connexion){
  date_last_update=DBI::dbGetQuery(connexion, "select max(date_creation) from offre_emploi")
  maj = date_last_update[[1]]
  if(is.na(maj)){
    print("Date de dernière mise à jour est NULL")
    return (NA)
  }else{
    maj = as.POSIXct(maj, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    maj = format(maj, "%Y-%m-%dT%H:%M:%OSZ")
    print(paste("Date de dernière mise à jour est: ", maj))
    
  }
  return (maj)
}



##---------------------------- requetage API --------------------------------####


Add_categorie <- function(df){
  df$intitule = toupper(df$intitule)
  df$categorie = "AUTRE"
  df$categorie[grep("DATA SCIENTIST", df$intitule)] = "DATA SCIENTIST"
  df$categorie[grep("DATA ANALYST", df$intitule)] = "DATA ANALYST"
  df$categorie[grep("DATA ENGINEER", df$intitule)] = "DATA ENGINEER"
  return(df)
}


#---------------------------- fonction finale --------------------------------------------#
# recupère les données concernant les offres de l'API et les insère dans la base
# si dateMaj est NA, la fonction recupère toutes les données disponibles sinon le diffrence suelement
data_from_api_to_bdd<-function (connexion,mot_cle, token, dateMaj){
  
  #préparer le paramètre 
  mot_cle = str_replace_all(mot_cle," ","+")
  
  # Nous ne pouvons recupérer que 150 ligne du resultat à chaque appel pour l' API
  # le paramètre "range" nous permet de faire plusieurs appels par tranche de 150 ligne à chaque fois
  index_min=0
  index_max=149
  
  # si la date de la dernière mise à jours n'est pas NA, l'ajouter comme paramère dans l'url de l'appel
  suffixurl=""
  if(!is.na(dateMaj)){
    now = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")
    suffixurl=paste("&minCreationDate=",dateMaj,"&maxCreationDate=",now,sep="")
  }
  
  #répeter tant que l'appel de webservice envoie le code 206: qui veut dire que il y a encore des lignes
  # qui correspond à la recherche mais seulement 150 lignes sont retournées à chaque fois pour l'API
  repeat{
    #construction de l'URL 
    url = paste("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=",
                mot_cle,
                "&range=",
                as.character(index_min),
                "-",
                as.character(index_max),
                suffixurl,
                sep=""
    )
    #recupérer les données
    request_data <-httr::GET(url,add_headers(Authorization = token))
    print(url)
    print(status_code(request_data))
    if(status_code(request_data)==206 | status_code(request_data)==200 ){
      #
      df=as.data.frame(fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)$resultats)
      df = Add_categorie(df)
      insert_data_int_bdd(connexion,df)
      #si le code different de 206 ou 200, arrêter la boucle (il n'y plus de résultat ou erreur)
    }else{
      break
    }
    # passer à la tranche suivante
    index_min= index_min+150
    index_max= index_max+150
  }
}

#---------------------------- fonction finale du package mysqlr--------------------------------------------#
fermerConnexion<-function(connexion){
  RMySQL::dbDisconnect(connexion)  
}
