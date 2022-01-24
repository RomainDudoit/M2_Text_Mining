# Connexion à la base
connect<-function(user='root', password='root', dbname='textmining', host='127.0.0.1', port=3306){
  mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host, port=port)
  return (mydb)
}

# date_last_update<- function(connexion){
#   date_last_update = fetch(
#     dbSendQuery(connexion, "select max(date_creation) from offre_emploi"),
#     n=1)
#   return (date_last_update[[1]])
# }

# Alimentation de la base
insert_into_poste <-function (connexion, code_rome, libelle_rome, appellation_libelle){
  query = "insert into poste (code_rome, libelle_rome, appellation_libelle) values ("
  query = paste(query,"'",code_rome,"','",sql_text(libelle_rome),"','", sql_text(appellation_libelle),"')")
  execute_requete(connexion, query)
}

insert_into_localisation <-function (connexion,nom_lieu_travail, longitudes_lieu_travail, latitudes_lieu_travail){
  query = "insert into localisation (nom_lieu_travail, longitudes_lieu_travail, latitudes_lieu_travail) values ("
  query = paste(query,"'",nom_lieu_travail,"',",na_to_null(longitudes_lieu_travail),",", na_to_null(latitudes_lieu_travail),")")
  execute_requete(connexion, query)
}

insert_into_contrat <-function (connexion,type_contrat, libelle_contrat){
  query = "insert into contrat (type_contrat, libelle_contrat) values ("
  query = paste(query,"'",type_contrat,"','",libelle_contrat,"')")
  execute_requete(connexion, query)
}

insert_into_experience <-function (connexion,libelle_experience, experience_exigee){
  query = "insert into experience (libelle_experience, experience_exigee) values ("
  query = paste(query,"'",libelle_experience,"','",experience_exigee,"')")
  execute_requete(connexion, query)
}

insert_into_secteur_activite <-function (connexion,libelle_secteur, secteur_activite){
  query = "insert into secteur_activite (libelle_secteur, secteur_activite) values ("
  query = paste(query,"'",sql_text(libelle_secteur),"','",secteur_activite,"')")
  execute_requete(connexion, query)
}

insert_into_Offre_emploi <-function (connexion,intitule_offre, description_offre, date_creation){
  id_localisation = select_max_id(connexion,"id_localisation","localisation")
  id_contrat = select_max_id(connexion,"id_contrat","contrat")
  id_poste = select_max_id(connexion,"id_poste","poste")
  id_experience = select_max_id(connexion,"id_experience","experience")
  id_secteur = select_max_id(connexion,"id_secteur","secteur_activite")
  query = "insert into Offre_emploi (intitule_offre, description_offre, date_creation,id_localisation, id_contrat, id_poste, id_experience,id_secteur) values ("
  query = paste(query,"'",sql_text(intitule_offre),"','",sql_text(description_offre),"','",date_creation,"'")
  query = paste(query,",",id_localisation,",",id_contrat,",",id_poste,",",id_experience,",",id_secteur,")")
  execute_requete(connexion, query)
}

#
select_max_id<-function(connexion,id_table,nom_table){
  rs = dbSendQuery(connexion, paste("select max(",id_table,") from ",nom_table))
  max_id_table = fetch(rs,n=1)
  dbClearResult(rs)
  return (max_id_table[[1]])
}

#Execution de la requete
execute_requete <-function (connexion, query){
  rs <- dbSendQuery(connexion, query)
  dbFetch(rs)
  dbClearResult(rs)
}

#
sql_text <- function ( text){
  return (str_replace_all(text,"'","''"))
}

na_to_null <- function ( text){
  if(is.null(text) )
    return ("NULL")
  if( is.na(text))
    return ("NULL")
  return (text)
}

insert_data_int_bdd<- function (connexion,df){
  for(i in 1:nrow(df)) {
    insert_into_poste (connexion, df[i,"romeCode"], df[i,"romeLibelle"],
                       df[i,"appellationlibelle"])
    insert_into_localisation(connexion,df[i,"lieuTravail"],df[i,"lieuTravail.longitude"], df[i,"lieuTravail.latitude"])
    insert_into_contrat(connexion,df[i,"typeContrat"], df[i,"typeContratLibelle"])
    insert_into_experience (connexion,df[i,"experienceLibelle"], df[i,"experienceExige"])
    insert_into_secteur_activite(mydb,df[i,"secteurActiviteLibelle"],df[i,"secteurActivite"])
    insert_into_Offre_emploi (connexion,df[i,"intitule"], df[i,"description"],df[i,"dateCreation"])
  }
}

#Creation de la base de données 
reset_base_donnes<-function(user='root', password='root', host='127.0.0.1', port=3306, dbname="textmining"){
  connexion = dbConnect(MySQL(), user=user, password=password, host=host, port=port)
  execute_requete(connexion,paste("drop database if exists",dbname))
  execute_requete(connexion,"create database textmining")
  #execute_requete(connexion,"drop table if exists Offre_emploi")
  #execute_requete(connexion,"drop table if exists localisation")
  #execute_requete(connexion,"drop table if exists contrat")
  #execute_requete(connexion,"drop table if exists poste")
  #execute_requete(connexion,"drop table if exists experience")
  #execute_requete(connexion,"drop table if exists secteur_activite")
  dbDisconnect(connexion)
  connexion = connect( user, password, dbname,host, port)
    
  req=""
  req=paste(req,"CREATE TABLE localisation (                                   ")
  req=paste(req,"  id_localisation  MEDIUMINT NOT NULL AUTO_INCREMENT,         ")
  req=paste(req,"  nom_lieu_travail VARCHAR(60),                               ")
  req=paste(req,"  longitudes_lieu_travail REAL,                               ")
  req=paste(req,"  latitudes_lieu_travail REAL,                                ")
  req=paste(req,"  CONSTRAINT PK_localisation PRIMARY KEY (id_localisation)    ")   
  req=paste(req,");                                                            ")     
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE contrat(                                         ")  
  req=paste(req,"  id_contrat MEDIUMINT NOT NULL AUTO_INCREMENT,               ")       
  req=paste(req,"  type_contrat VARCHAR(30),                                   ")       
  req=paste(req,"  libelle_contrat VARCHAR(300),                               ")        
  req=paste(req,"  CONSTRAINT PK_contrat PRIMARY KEY (id_contrat)              ")       
  req=paste(req,");                                                            ")     
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE poste(                                           ")     
  req=paste(req,"  id_poste MEDIUMINT NOT NULL AUTO_INCREMENT,                 ")       
  req=paste(req,"  code_rome VARCHAR(10),                                      ")       
  req=paste(req,"  libelle_rome TEXT,                                          ")       
  req=paste(req,"  appellation_libelle TEXT,                                   ")       
  req=paste(req,"  CONSTRAINT PK_poste PRIMARY KEY (id_poste)                  ")       
  req=paste(req,");                                                            ")     
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE experience(                                      ")     
  req=paste(req,"  id_experience MEDIUMINT NOT NULL AUTO_INCREMENT,            ")       
  req=paste(req,"  libelle_experience TEXT,                                    ")       
  req=paste(req,"  experience_exigee varchar(10),                              ")           
  req=paste(req,"  CONSTRAINT PK_experience PRIMARY KEY (id_experience)        ")       
  req=paste(req,");                                                            ")
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE secteur_activite(                                ")
  req=paste(req,"  id_secteur MEDIUMINT NOT NULL AUTO_INCREMENT,               ")
  req=paste(req,"  libelle_secteur TEXT,                                       ")
  req=paste(req,"  secteur_activite TEXT,                                      ")
  req=paste(req,"  CONSTRAINT PK_secteur_activite PRIMARY KEY (id_secteur)     ")
  req=paste(req,");                                                            ")
  execute_requete(connexion,req)
  
  req=""
  req=paste(req,"CREATE TABLE Offre_emploi(                                    ")
  req=paste(req,"  id_offre MEDIUMINT NOT NULL AUTO_INCREMENT,                 ")
  req=paste(req,"  id_localisation MEDIUMINT,                                  ")
  req=paste(req,"  id_contrat MEDIUMINT,                                       ")
  req=paste(req,"  id_poste MEDIUMINT,                                         ")
  req=paste(req,"  id_experience MEDIUMINT,                                    ")
  req=paste(req,"  id_secteur MEDIUMINT,                                       ")
  req=paste(req,"  date_creation TEXT,                                         ") 
  req=paste(req,"  intitule_offre VARCHAR(300),                                ")
  req=paste(req,"  description_offre TEXT,                                     ")
  req=paste(req,"  CONSTRAINT PK_offre PRIMARY KEY (id_offre),                 ")
  req=paste(req,"  FOREIGN KEY (id_localisation)                               ")
  req=paste(req,"  REFERENCES localisation (id_localisation) ,                 ")
  req=paste(req,"  FOREIGN KEY (id_contrat)                                    ")
  req=paste(req,"  REFERENCES contrat (id_contrat),                            ")
  req=paste(req,"  FOREIGN KEY (id_poste)                                      ")
  req=paste(req,"  REFERENCES poste (id_poste),                                ")
  req=paste(req,"  FOREIGN KEY (id_experience)                                 ")
  req=paste(req,"  REFERENCES experience (id_experience)	,                    ")
  req=paste(req,"  FOREIGN KEY (id_secteur)                                    ")
  req=paste(req,"  REFERENCES secteur_activite (id_secteur)		                 ")
  req=paste(req,")                                                              ")
  execute_requete(connexion,req)
  dbDisconnect(connexion)
}

#show global variables like 'local_infile';
#set global local_infile=true;