#################################################################################################
#---------------------------------------Projet textmining
#-----------------------------------------Romain Dudoit
#-------------------------------------Fatim-Zahra El Gaouzi
#-----------------------------------------Elisa Frintz

#################################################################################################


#Packages
# install.packages(c("httr", "jsonlite"))
# install.packages('stringr')
# install.packages("RMySQL")
#install.packages("utf8")

library(httr)
library(jsonlite)
library(stringr)
library(RMySQL)
library(utf8)


# Connexion ? la base
connect<-function(user='root', password='root', dbname='textmining', host='127.0.0.1', port=3306){
  mydb = RMySQL::dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host, port=port, encoding = "utf8mb4")
  return (mydb)
}


# Insertion des donn?es dans la table ref_regions. Fonction appel?e par une autre 
insert_into_regions <-function (connexion, nom, code){
  req = "INSERT INTO ref_regions (nom_region, code_region) VALUES ("
  req = paste (req,DBI::dbQuoteString(connexion,nom),",",DBI::dbQuoteString(connexion,code),")")
  #print(req)
  DBI::dbGetQuery(connexion, req)
}

# Insertion des donn?es dans la table ref_departements. Fonction appel?e par une autre 
insert_into_departements <-function (connexion, nom, code, codeRegion){
  req = "INSERT INTO ref_departement (nom_departement, code_departement, code_region) VALUES ("
  req = paste (req,DBI::dbQuoteString(connexion,nom),",",DBI::dbQuoteString(connexion,code),
               ",",DBI::dbQuoteString(connexion,codeRegion),")")
  #print(req)
  DBI::dbGetQuery(connexion, req)
}

# Insertion des donn?es dans la table ref_communes. Fonction appel?e par une autre 
insert_into_communes <-function (connexion,  code, nom, codePostal, codeDepartement){
  
  req = "INSERT INTO ref_communes ( nom_commune, code_commune, code_departement, code_postal) VALUES ("
  req = paste (req,dbQuoteString(connexion,nom),",",
               DBI::dbQuoteString(connexion,code),",",
               DBI::dbQuoteString(connexion,codeDepartement),",",
               DBI::dbQuoteString(connexion,codePostal),")")
  #print(req)
  DBI::dbGetQuery(connexion, req)
}

# Insertion des donn?es dans la table poste Fonction appel?e par une autre 
insert_into_poste <-function (connexion, id, code_rome, libelle_rome, appellation_libelle){
  query = "insert into poste (id_poste,code_rome, libelle_rome, appellation_libelle) values ("
  query = paste(query,"'",id,"','",code_rome,"',",
                DBI::dbQuoteString(connexion, libelle_rome),",", 
                DBI::dbQuoteString(connexion, appellation_libelle),")",sep="")
  execute_requete(connexion, query)
}

# Insertion des donn?es dans la table contrat. Fonction appel?e par une autre 
insert_into_contrat <-function (connexion, id, type_contrat, libelle_contrat){
  query = "insert into contrat (id_contrat, type_contrat, libelle_contrat) values ("
  query = paste(query,"'",id,"','",type_contrat,"','",libelle_contrat,"')",sep="")
  execute_requete(connexion, query)
}

# Insertion des donn?es dans la table experience Fonction appel?e par une autre 
insert_into_experience <-function (connexion, id,libelle_experience, experience_exigee){
  query = "insert into experience (id_experience,libelle_experience, experience_exigee) values ("
  query = paste(query,"'",id,"','",libelle_experience,"','",experience_exigee,"')",sep="")
  execute_requete(connexion, query)
}

# Insertion des donn?es dans la table activit?. Fonction appel?e par une autre 
insert_into_secteur_activite <-function (connexion,id,libelle_secteur, secteur_activite){
  query = "insert into secteur_activite (id_secteur,libelle_secteur, secteur_activite) values ("
  query = paste(query,"'",id,"',",
                DBI::dbQuoteString(connexion, libelle_secteur),",",
                DBI::dbQuoteString(connexion,secteur_activite),")",sep="")
  execute_requete(connexion, query)
}

# Insertion des donn?es dans la table offre_emploi. Fonction appel?e par une autre 
insert_into_Offre_emploi <-function (connexion,id,intitule_offre, description_offre, date_creation,
                                     id_contrat, id_poste,id_experience,id_secteur, categorie, codeCommune,
                                     salaire, nom_entreprise){
  id_offre_emploi = id
  query = "insert into Offre_emploi (intitule_offre, description_offre, categorie,date_creation,id_offre,"
  query = paste(query," id_contrat, id_poste, id_experience,id_secteur, salaire, nom_entreprise, codeCommune) values (")
  query = paste(query,
                DBI::dbQuoteString(connexion, intitule_offre),",",
                DBI::dbQuoteString(connexion,description_offre),",",
                DBI::dbQuoteString(connexion, categorie),
                ",STR_TO_DATE('",date_creation,"','%Y-%m-%dT%T.000Z'),",sep="")
  query = paste(query,"'",id_offre_emploi,"','",id_contrat,"','",id_poste,"','",id_experience,"','",
                id_secteur,"',",
                DBI::dbQuoteString(connexion, salaire),",",
                DBI::dbQuoteString(connexion, nom_entreprise),
                sep="")
  cd= na_to_null(codeCommune);
  if( cd=="NULL")
    query = paste(query,",'NR')", sep="")
  else
    query = paste(query, ",'" , cd ,"')", sep="")
  #print(query)
  execute_requete(connexion, query)
  
}

# V?rification si l'experience existe d?j? dans la base en se basant sur les param?tres de la fonction.
# renvoie son id ou null
# fonction appel?e par une autre 
get_experience<- function(connexion, libelle_experience, experience_exigee){
  experience_exigee = utf8_encode(experience_exigee)
  libelle_experience = utf8_encode(libelle_experience)
  req = "select id_experience from experience where "
  req = paste(req,"experience_exigee =",
              DBI::dbQuoteString(connexion, experience_exigee),sep="")
  req = paste(req," and libelle_experience =",
              DBI::dbQuoteString(connexion, libelle_experience),sep="")
  return (DBI::dbGetQuery(connexion, req)[1,1])
}

# V?rification si secteur de l'activit? existe d?j? dans la base en se basant sur les param?tres de la fonction.
# renvoie son id ou null
# fonction appel?e par une autre 
get_secteur_activite<- function(connexion, libelle_secteur, secteur_activite){
  
  libelle_secteur = utf8_encode(libelle_secteur)
  secteur_activite = utf8_encode(secteur_activite)  
  req = "select id_secteur from secteur_activite where "
  req = paste(req,"libelle_secteur =",
              DBI::dbQuoteString(connexion, libelle_secteur),sep="")
  req = paste(req," and secteur_activite =",
              DBI::dbQuoteString(connexion, secteur_activite),sep="")
  #print(req)
  return (DBI::dbGetQuery(connexion, req)[1,1])
}

# V?rification si l'contrat existe d?j? dans la base en se basant sur les param?tres de la fonction.
# renvoie son id ou null
# fonction appel?e par une autre 
get_contrat<- function(connexion, type_contrat, libelle_contrat){
  libelle_contrat = utf8_encode(libelle_contrat)
  req = "select id_contrat from contrat where "
  req = paste(req,"type_contrat ='",type_contrat,"'",sep="")
  req = paste(req," and libelle_contrat =",
              DBI::dbQuoteString(connexion, libelle_contrat),sep="")
  return (DBI::dbGetQuery(connexion, req)[1,1])
}


# V?rification si le poste existe d?j? dans la base en se basant sur les param?tres de la fonction.
# renvoie son id ou null
# fonction appel?e par une autre 
get_poste<- function(connexion, code_rom){
  req = paste("select id_poste from poste where code_rome='",code_rom,"'", sep="")
  return (DBI::dbGetQuery(connexion, req)[1,1])
}


#Execution de la requ?te pass?e en param?re en UTF-8
#Fonction appel?e par une autre
execute_requete <-function (connexion, query){
  RMySQL::dbSendQuery(connexion,"SET NAMES utf8mb4;")
  RMySQL::dbSendQuery(connexion,"SET CHARACTER SET utf8mb4;")
  #print(query)
  return (DBI::dbGetQuery(connexion,query))
}


# V?rification si l'id de ligne recup?r?e de l'APIexiste d?j? dans la base (table offre_emploi)
# si oui renvoie l'id sinon renvoie null
# cette fonction permet de ne pas ins?rer la m?me offre plusieurs fois dans la base
# quand nous faisons des recherches avec des mots cl?s diffrents mais qui peuvent amener des 
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

# sert ? remplacer na et null par NULL dans les requ?tes
#si le param?tre est un text, renvoie le text lui m?me
#si le param?tre est un na, renvoie le text NULL
#si le param?tre est un null, renvoie le text NULL
na_to_null <- function ( text){
  if(is.null(text) )
    return ("NULL")
  if( is.na(text))
    return ("NULL")
  return (text)
}

# fonction qui prend en param?tre un dataframe contenant les r?sultats des appels API
# pour l'ins?rer dans la base 
insert_data_int_bdd<- function (connexion,df){
  
  #parcourir le dataframe ligne par ligne
  for(i in 1:nrow(df)) {
    id = df[i,"id"]
    # si la ligne existe d?j? dans la base, on passe ? la ligne suivante
    # pour plus de d?tails, voir les commentaires de la fonction id_exists
    if(id_exists(connexion, id)){
      next;
    }
    
    # v?rifier si poste existe 
    # s'il n'existe pas on l'ins?re 
    id_poste= get_poste(connexion, df[i,"romeCode"])
    if(is.na(id_poste)){
      insert_into_poste (connexion,id, df[i,"romeCode"], df[i,"romeLibelle"],
                         df[i,"appellationlibelle"])
      id_poste=id
    }
    
    # v?rifier si contrat existe 
    # s'il n'existe pas on l'ins?re 
    id_contrat = get_contrat(connexion, df[i,"typeContrat"], df[i,"typeContratLibelle"])
    if(is.na(id_contrat)){
      insert_into_contrat(connexion,id,df[i,"typeContrat"], df[i,"typeContratLibelle"])
      id_contrat=id
    }
    
    # v?rifier si experience existe 
    # s'il n'existe pas on l'ins?re 
    id_experience = get_experience(connexion, df[i,"experienceLibelle"], df[i,"experienceExige"])
    if(is.na(id_experience)){
      insert_into_experience (connexion,id,df[i,"experienceLibelle"], df[i,"experienceExige"])
      id_experience=id
    }
    
    #nettoyage: nous avons remarqu? des offres sans secteur d'activt?.
    #on les ins?re dans la base avec le mot NR
    secteurActiviteLibelle = df[i,"secteurActiviteLibelle"]
    secteurActivite = df[i,"secteurActivite"]
    if(is.na(secteurActiviteLibelle))
      secteurActiviteLibelle="NR"
    if(is.na(secteurActivite))
      secteurActivite="NR"
    
    # v?rifier si secteur d'activit? existe 
    # s'il n'existe pas on l'ins?re 
    id_secteur = get_secteur_activite(connexion, secteurActiviteLibelle, secteurActivite)
    if(is.na(id_secteur)){
      insert_into_secteur_activite(mydb, id, secteurActiviteLibelle, secteurActivite)
      id_secteur=id
    }
    
    #ins?rer dans la table des faits offre_emploi
    insert_into_Offre_emploi (connexion,id,df[i,"intitule"],  df[i,"description"],df[i,"dateCreation"],
                              id_contrat, id_poste,id_experience,id_secteur,df[i,"categorie"], 
                              df[i,"lieuTravail.commune"],
                              df[i,"salaire.libelle"],df[i,"entreprise.nom"])
  }
}

#---------------------------- fonction finale --------------------------------------------#
# cr?er la base de donn?es avec les tables 
# les param?tres sont personnalisable avec des valeurs par defaut
# fonction ? utiliser directement par l'utilisateur final 
reset_base_donnes<-function(user='root', password='root', host='127.0.0.1', port=3306, dbname="textmining"){
  
  connexion = dbConnect(MySQL(), user=user, password=password, host=host, port=port)
  execute_requete(connexion,paste("drop database if exists",dbname))
  execute_requete(connexion,paste("create database ",dbname))
  execute_requete(connexion,paste("use  ",dbname))
  fermerConnexion(connexion)
  
  connexion = connect( user, password, dbname,host, port)
  execute_requete(connexion, "SET NAMES utf8mb4")
  execute_requete(connexion, "SET CHARACTER SET utf8mb4")
  execute_requete(connexion, "SET character_set_connection=utf8mb4")
  
  req=""
  req=paste(req,"CREATE TABLE ref_regions(                                  ")
  req=paste(req,"  nom_region VARCHAR(100),                                 ")
  req=paste(req,"  code_region VARCHAR(5),                                ")
  req=paste(req,"  CONSTRAINT PK_ref_regions PRIMARY KEY (code_region)      ")
  req=paste(req,");                                                         ")
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE ref_departement(                              ")
  req=paste(req,"  nom_departement VARCHAR(100),                            ")
  req=paste(req,"  code_departement VARCHAR(6),                             ")
  req=paste(req,"  code_region VARCHAR(5),                                      ")
  req=paste(req,"  CONSTRAINT PK_ref_departement PRIMARY KEY (code_departement), ")
  req=paste(req,"  FOREIGN KEY fk_region  (code_region)                         ")
  req=paste(req,"  REFERENCES ref_regions (code_region)                       ")
  req=paste(req,");                                                             ")
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE ref_communes(  ")
  req=paste(req,"  id_commune INT PRIMARY KEY NOT NULL AUTO_INCREMENT,                 ")
  req=paste(req,"  nom_commune VARCHAR(100),                                   ")
  req=paste(req,"  code_commune VARCHAR(6),                                     ")
  req=paste(req,"  code_departement VARCHAR(5),                                 ")
  req=paste(req,"  code_postal VARCHAR(5),                                     ")
  req=paste(req,"  FOREIGN KEY fk_departements  (code_departement)              ")
  req=paste(req,"  REFERENCES ref_departement (code_departement)                ")
  req=paste(req,");                                                             ")
  execute_requete(connexion,req)
  
  req="ALTER TABLE ref_communes ADD INDEX (code_postal)"
  execute_requete(connexion,req)
  req="ALTER TABLE ref_communes ADD INDEX (code_commune)"
  execute_requete(connexion,req)
  
  
  req=""
  req=paste(req,"CREATE TABLE contrat(                                         ")
  req=paste(req,"  id_contrat VARCHAR(10),               ")
  req=paste(req,"  type_contrat VARCHAR(30),                                   ")
  req=paste(req,"  libelle_contrat VARCHAR(300),                               ")
  req=paste(req,"  CONSTRAINT PK_contrat PRIMARY KEY (id_contrat)              ")
  req=paste(req,");                                                            ")
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE poste(                                           ")
  req=paste(req,"  id_poste VARCHAR(10),                 ")
  req=paste(req,"  code_rome VARCHAR(10),                                      ")
  req=paste(req,"  libelle_rome TEXT,                                          ")
  req=paste(req,"  appellation_libelle TEXT,                                   ")
  req=paste(req,"  CONSTRAINT PK_poste PRIMARY KEY (id_poste)                  ")
  req=paste(req,");                                                            ")
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE experience(                                      ")     
  req=paste(req,"  id_experience VARCHAR(10),            ")       
  req=paste(req,"  libelle_experience TEXT,                                    ")       
  req=paste(req,"  experience_exigee varchar(10),                              ")           
  req=paste(req,"  CONSTRAINT PK_experience PRIMARY KEY (id_experience)        ")       
  req=paste(req,");                                                            ")
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE secteur_activite(                                ")
  req=paste(req,"  id_secteur VARCHAR(10),               ")
  req=paste(req,"  libelle_secteur TEXT,                                       ")
  req=paste(req,"  secteur_activite TEXT,                                      ")
  req=paste(req,"  CONSTRAINT PK_secteur_activite PRIMARY KEY (id_secteur)     ")
  req=paste(req,");                                                            ")
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE Offre_emploi(                                    ")
  req=paste(req,"  id_offre VARCHAR(10),                 ")
  req=paste(req,"  id_contrat VARCHAR(10),                                       ")
  req=paste(req,"  id_poste VARCHAR(10),                                         ")
  req=paste(req,"  id_experience VARCHAR(10),                                    ")
  req=paste(req,"  id_secteur VARCHAR(10),                                       ")
  req=paste(req,"  categorie VARCHAR(60),                                       ")
  req=paste(req,"  date_creation DATETIME,                                     ") 
  req=paste(req,"  intitule_offre VARCHAR(300),                                ")
  req=paste(req,"  description_offre TEXT,                                     ")
  req=paste(req,"  salaire varchar(200),                                     ")
  req=paste(req,"  nom_entreprise varchar(200),                                     ")
  req=paste(req,"  codeCommune varchar(5),                                     ")
  req=paste(req,"  CONSTRAINT PK_offre PRIMARY KEY (id_offre),                 ")
  req=paste(req,"  FOREIGN KEY (id_contrat)                                    ")
  req=paste(req,"  REFERENCES contrat (id_contrat),                            ")
  req=paste(req,"  FOREIGN KEY (id_poste)                                      ")
  req=paste(req,"  REFERENCES poste (id_poste),                                ")
  req=paste(req,"  FOREIGN KEY (id_experience)                                 ")
  req=paste(req,"  REFERENCES experience (id_experience)	,                    ")
  req=paste(req,"  FOREIGN KEY (id_secteur)                                    ")
  req=paste(req,"  REFERENCES secteur_activite (id_secteur)		                 ")
  # probl?me de saisie de code commune: des fois les employeurs mettent le code postal 
  # au lieu de code commune
  #req=paste(req,"  FOREIGN KEY  (codeCommune)                                    ")
  #req=paste(req,"  REFERENCES ref_communes (code_commune)		                 ")
  req=paste(req,")                                                              ")
  
  execute_requete(connexion,req)
  fermerConnexion(connexion)
}

#---------------------------- fonction finale --------------------------------------------#
# renvoie la date de la derni?re mise ? jour de la base ou NA
# ? utiliser au moment de recup?ration des donn?es de l'API
date_last_update<- function(connexion){
  date_last_update=DBI::dbGetQuery(connexion, "select max(date_creation) from offre_emploi")
  maj = date_last_update[[1]]
  if(is.na(maj)){
    return (NA)
  }else{
    maj = as.POSIXct(maj, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    maj = format(maj, "%Y-%m-%dT%H:%M:%OSZ")
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

# fait appel ? l'API des referentiels des r?gion pour alimenter la base
# fonction appel?e par une autre
mise_a_jour_regions<-function (connexion, token){
  url="https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/regions"
  request_data <-httr::GET(url,add_headers(Authorization = token))
  data= jsonlite::fromJSON(httr::content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)
  for(i in 1:nrow(data)) {
    insert_into_regions (connexion, data[i,"libelle"], data[i,"code"])
  }
}

# fait appel ? l'API des referentiels des departements pour alimenter la base
# fonction appel?e par une autre
mise_a_jour_departement<-function (connexion,token){
  url="https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/departements"
  request_data <- httr::GET(url,add_headers(Authorization = token))
  data = jsonlite::fromJSON(httr::content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)
  for(i in 1:nrow(data)) {
    insert_into_departements (connexion, data[i,"libelle"], data[i,"code"], data[i,"region.code"] )
  }
}

# fait appel ? l'API des referentiels des communes pour alimenter la base
# fonction appel?e par une autre
mise_a_jour_communes<-function (connexion, token){
  url="https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/communes"
  request_data <- httr::GET(url,add_headers(Authorization = token))
  data = fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)
  #exclure les departements qui ne sont pas ref?renc?es dans l'API departements 
  data = subset(data, !(codeDepartement %in% c("977","978","979","980","981","982","983","984",
                                               "985","986","987","988","989","975","99")))
  
  for(i in 1:nrow(data)) {
    insert_into_communes (connexion,  data[i,"code"], data[i,"libelle"], 
                          data[i,"codePostal"], data[i,"codeDepartement"])
  }
}

#---------------------------- fonction finale --------------------------------------------#
# ins?re dans la base le ref?rentiel des deparetement, regions et communes
mise_a_jour_referentiel<-function (connexion, token){
  mise_a_jour_regions(connexion,token)
  mise_a_jour_departement(connexion,token)
  mise_a_jour_communes(connexion,token)
}

#---------------------------- fonction finale --------------------------------------------#
# recup?re les donn?es concernant les offres de l'API et les ins?re dans la base
# si dateMaj est NA, la fonction recup?re toutes les donn?es disponibles sinon le diffrence suelement(recupere uniquement les offres ayant une date > max date de creation de la base )
data_from_api_to_bdd<-function (connexion,mot_cle, token, dateMaj){
  
  #pr?parer le param?tre 
  mot_cle = str_replace_all(mot_cle," ","+")
  
  # Nous ne pouvons recup?rer que 150 ligne du resultat ? chaque appel pour l' API
  # le param?tre "range" nous permet de faire plusieurs appels par tranche de 150 ligne ? chaque fois
  index_min=0
  index_max=149
  
  # si la date de la derni?re mise ? jours n'est pas NA, l'ajouter comme param?re dans l'url de l'appel
  suffixurl=""
  if(is.na(dateMaj)){
    print("Date de derni?re mise ? jour est NULL")
  }else{
    print(paste("Derni?re mise ? jour de la base: ",dateMaj, sep=""))
    now = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")
    suffixurl=paste("&minCreationDate=",dateMaj,"&maxCreationDate=",now,sep="")
  }
  
  #r?peter tant que l'appel de webservice envoie le code 206: qui veut dire que il y a encore des lignes
  # qui correspond ? la recherche mais seulement 150 lignes sont retourn?es ? chaque fois pour l'API
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
    #recup?rer les donn?es
    request_data <-httr::GET(url,add_headers(Authorization = token))
    print(url)
    print(status_code(request_data))
    if(status_code(request_data)==206 | status_code(request_data)==200 ){
      #
      df=as.data.frame(fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)$resultats)
      df = Add_categorie(df)
      insert_data_int_bdd(connexion,df)
      #si le code different de 206 ou 200, arr?ter la boucle (il n'y plus de r?sultat ou erreur)
    }else{
      break
    }
    # passer ? la tranche suivante
    index_min= index_min+150
    index_max= index_max+150
  }
  return (df)
}

#---------------------------- fonction finale --------------------------------------------#
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

#---------------------------- fonction finale du package mysqlr--------------------------------------------#
fermerConnexion<-function(connexion){
  RMySQL::dbDisconnect(connexion)  
}

#-------------------execution exemple --------------------------#
#initialisation de la base
reset_base_donnes()
# recuperation du token 
token=get_token()

#ouverture de la connexion 
mydb=connect()
# table region/departement/commune
mise_a_jour_referentiel(mydb, token)
#Mise ? jour de la base
maj=date_last_update(mydb)
#recuperation des mots cles de l'api 
for (motcle in c("Data scientist", "Data engineer", "Data analyst"))
  df= data_from_api_to_bdd(mydb,motcle,token, maj)

# fermeture de la connexion 
fermerConnexion(mydb)

#Reflexion 
#?tapes pour Rshiny (boutons)
#reset_base_donnes(): ? la premi?re connexion ou ? la demande avec un bouton
#mise_a_jour_referentiel(): une seule fois suite ? reset_base_donnees m?me bouton que reset base
# toujours: ? chaque ouverture de l'application:
# - connect(): ouvrir une connexion. ne pas ouvrir beaucoup sinon la base sera satur?e
# - date_last_update(): pour la mise ? jour des offres, recup?rer la derni?re date de MAJ
# - get_token (): pour avoir un token ? chaque fois on fait appel ? une fonction l'utilisant 
# - data_from_api_to_bdd (): pour mettre ? jour les donn?es dans la base ou les ins?rer pour la premi?re fois
# - fermerConnexion(): ? faire ? chaque fois on veut quitter l'application



