# Connexion ? la base
connect<-function(user='root', password='root', dbname='textmining', host='127.0.0.1', port=3306){
  mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host, port=port)
  return (mydb)
}


#Execution de la requete
execute_requete <-function (connexion, query){
  rs <- dbSendQuery(connexion, query)
  data = dbFetch(rs)
  dbClearResult(rs)
  return (data)
}

#Creation de la base de données 
reset_base_donnes<-function(user='root', password='root', host='127.0.0.1', port=3306, dbname="textmining"){
  connexion = dbConnect(MySQL(), user=user, password=password, host=host, port=port)
  #execute_requete(connection,"RESET QUERY CACHE;")
  execute_requete(connexion,paste("drop database if exists",dbname))
  execute_requete(connexion,paste("create database ",dbname))
  #execute_requete(connexion,"drop table if exists Offre_emploi")
  #execute_requete(connexion,"drop table if exists localisation")
  #execute_requete(connexion,"drop table if exists contrat")
  #execute_requete(connexion,"drop table if exists poste")
  #execute_requete(connexion,"drop table if exists experience")
  #execute_requete(connexion,"drop table if exists secteur_activite")
  dbSendQuery(connexion, "SET GLOBAL local_infile = true;")
  dbDisconnect(connexion)
  connexion = connect( user, password, dbname,host, port)
  

  req=""
  req=paste(req,"CREATE TABLE regions(")
  req=paste(req,"  code_insee varchar(50),")
  req=paste(req,"  nom_region  varchar(50),")
  req=paste(req,"  PRIMARY KEY (code_insee)")
  req=paste(req,");")
  execute_requete(connexion,req)
  
  regions = read.csv("regions.csv",sep=";",encoding = "UTF-8")
  dbWriteTable(connexion,name ="regions",regions,append=TRUE,overwrite=FALSE,row.names=FALSE)

  req=""
  req=paste(req,"CREATE TABLE departements(")
  req=paste(req,"  num_dep varchar(3),")
  req=paste(req,"  nom_dep varchar(50),")
  req=paste(req,"  code_region varchar(50),")
  req=paste(req,"  PRIMARY KEY (num_dep),")
  req=paste(req,"  FOREIGN KEY (code_region) REFERENCES regions(code_insee)")
  req=paste(req,");")
  execute_requete(connexion,req)
  
  
  departements = read.csv("departements.csv",sep=";",encoding = "UTF-8")
  dbWriteTable(connexion,name ="departements",departements,row.names=FALSE,append=TRUE,overwrite=FALSE)

  req=""
  req=paste(req,"CREATE TABLE communes(")
  req=paste(req,"  num_commune varchar(5),")
  req=paste(req,"  num_dep varchar(3),")
  req=paste(req,"  nom_ville varchar(3),")
  req=paste(req,"  longitude float,")
  req=paste(req,"  latitude float,")
  req=paste(req,"  PRIMARY KEY (num_commune),")
  req=paste(req,"  FOREIGN KEY (num_dep) REFERENCES departements(num_dep)")
  req=paste(req,");")
  execute_requete(connexion,req)

  communes = read.csv("communes.csv")
  dbWriteTable(connexion,name ="communes",communes,row.names=FALSE,append=FALSE,overwrite=TRUE)
  
  
  req=""
  req=paste(req,"CREATE TABLE Offre_emploi(")
  req=paste(req,"  id_offre VARCHAR(10),")
  req=paste(req,"  date_creation DATE,") 
  req=paste(req,"  num_commune VARCHAR(5),") 
  req=paste(req,"  intitule_offre VARCHAR(300),")
  req=paste(req,"  description_offre TEXT,")
  req=paste(req,"  PRIMARY KEY (id_offre),")
  req=paste(req,"  FOREIGN KEY (num_commune) REFERENCES communes(num_commune)")
  req=paste(req,");")
  execute_requete(connexion,req)


  req=""
  req=paste(req,"CREATE TABLE localisation (                                   ")
  req=paste(req,"  id_localisation  VARCHAR(10),         ")
  req=paste(req,"  nom_lieu_travail VARCHAR(60),                               ")
  req=paste(req,"  longitudes_lieu_travail REAL,                               ")
  req=paste(req,"  latitudes_lieu_travail REAL,                                ")
  req=paste(req,"  CONSTRAINT PK_localisation PRIMARY KEY (id_localisation)    ")
  req=paste(req,");                                                            ")
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
  # 
  # req=""
  # req=paste(req,"CREATE TABLE Offre_emploi(                                    ")
  # req=paste(req,"  id_offre VARCHAR(10),                 ")
  # req=paste(req,"  id_localisation VARCHAR(10),                                  ")
  # req=paste(req,"  id_contrat VARCHAR(10),                                       ")
  # req=paste(req,"  id_poste VARCHAR(10),                                         ")
  # req=paste(req,"  id_experience VARCHAR(10),                                    ")
  # req=paste(req,"  id_secteur VARCHAR(10),                                       ")
  # req=paste(req,"  date_creation TEXT,                                         ") 
  # req=paste(req,"  intitule_offre VARCHAR(300),                                ")
  # req=paste(req,"  description_offre TEXT,                                     ")
  # req=paste(req,"  CONSTRAINT PK_offre PRIMARY KEY (id_offre),                 ")
  # req=paste(req,"  FOREIGN KEY (id_localisation)                               ")
  # req=paste(req,"  REFERENCES localisation (id_localisation) ,                 ")
  # req=paste(req,"  FOREIGN KEY (id_contrat)                                    ")
  # req=paste(req,"  REFERENCES contrat (id_contrat),                            ")
  # req=paste(req,"  FOREIGN KEY (id_poste)                                      ")
  # req=paste(req,"  REFERENCES poste (id_poste),                                ")
  # req=paste(req,"  FOREIGN KEY (id_experience)                                 ")
  # req=paste(req,"  REFERENCES experience (id_experience)	,                    ")
  # req=paste(req,"  FOREIGN KEY (id_secteur)                                    ")
  # req=paste(req,"  REFERENCES secteur_activite (id_secteur)		                 ")
  # req=paste(req,")                                                              ")
  # execute_requete(connexion,req)
  dbDisconnect(connexion)
}

#show global variables like 'local_infile';
#set global local_infile=true;
#CREATE INDEX idx1 ON t1 ((col1 + col2));




# date_last_update<- function(connexion){
#   date_last_update = fetch(
#     dbSendQuery(connexion, "select max(date_creation) from offre_emploi"),
#     n=1)
#   return (date_last_update[[1]])
# }

# Alimentation de la base
insert_into_poste <-function (connexion, id, code_rome, libelle_rome, appellation_libelle){
  query = "insert into poste (id_poste,code_rome, libelle_rome, appellation_libelle) values ("
  query = paste(query,"'",id,"','",code_rome,"','",sql_text(libelle_rome),"','", sql_text(appellation_libelle),"')",sep="")
  execute_requete(connexion, query)
}

#exists_in_localisation()


insert_into_localisation <-function (connexion,id, nom_lieu_travail, longitudes_lieu_travail, latitudes_lieu_travail){
  query = "insert into localisation (id_localisation, nom_lieu_travail, longitudes_lieu_travail, latitudes_lieu_travail) values ("
  query = paste(query,"'",id,"','",nom_lieu_travail,"',",na_to_null(longitudes_lieu_travail),",", na_to_null(latitudes_lieu_travail),")",sep="")
  execute_requete(connexion, query)
}

#contrat_exist(connexion, type_contrat, libelle_contrat){
  
#}
insert_into_contrat <-function (connexion, id, type_contrat, libelle_contrat){
  query = "insert into contrat (id_contrat, type_contrat, libelle_contrat) values ("
  query = paste(query,"'",id,"','",type_contrat,"','",libelle_contrat,"')",sep="")
  execute_requete(connexion, query)
}

insert_into_experience <-function (connexion, id,libelle_experience, experience_exigee){
  query = "insert into experience (id_experience,libelle_experience, experience_exigee) values ("
  query = paste(query,"'",id,"','",libelle_experience,"','",experience_exigee,"')",sep="")
  execute_requete(connexion, query)
}

insert_into_secteur_activite <-function (connexion,id,libelle_secteur, secteur_activite){
  query = "insert into secteur_activite (id_secteur,libelle_secteur, secteur_activite) values ("
  query = paste(query,"'",id,"','",sql_text(libelle_secteur),"','",secteur_activite,"')",sep="")
  execute_requete(connexion, query)
}

insert_into_Offre_emploi <-function (connexion,id,date_creation,num_commune,intitule_offre, description_offre){
  #id_localisation = id
  #id_contrat = id
  #id_poste = id
  #id_experience = id
  #id_secteur = id
  #id_offre_emploi = id
  query = "insert into Offre_emploi (id_offre, date_creation,num_commune,intitule_offre,description_offre) values ("
  query = paste(query,"'",id,"','",date_creation,"','",num_commune,"','",sql_text(intitule_offre),"','",sql_text(description_offre),"')",sep="")
  #query = paste(query,"'",id_offre_emploi,"','",id_localisation,"','",id_contrat,"','",id_poste,"','",id_experience,"','",id_secteur,"')",sep="")
  execute_requete(connexion, query)
}

#
sql_text <- function ( text){
  return (str_replace_all(text,"'","''"))
}

id_exists<- function(connexion, id){
  rs = dbSendQuery(connexion, paste("select id_poste  from poste where id_poste='",id,"'",sep=""))
  data=dbFetch(rs,1)
  count = dbGetRowCount(rs)
  #print("-========>")
  #print(paste("select id_poste  from poste where id_poste='",id,"'",sep=""))
  #print(data)
  #print(count)
  dbClearResult(rs)
  if (count==0){
      return (FALSE)
  }else{
    return (TRUE)
  }
}

na_to_null <- function ( text){
  if(is.null(text) )
    return ("NULL")
  if( is.na(text))
    return ("NULL")
  return (text)
}

insert_data_int_bdd<- function (connexion,df){ # df en provenance de api.R
  for(i in 1:nrow(df)) {
    if(id_exists(connexion, df[i,"id"])){
      next;
    }
    
    #insert_into_poste (connexion,df[i,"id"], df[i,"romeCode"], df[i,"romeLibelle"],df[i,"appellationlibelle"])
    insert_into_localisation(connexion,df[i,"id"],df[i,"lieuTravail.libelle"],df[i,"lieuTravail.longitude"], df[i,"lieuTravail.latitude"])
    insert_into_contrat(connexion,df[i,"id"],df[i,"typeContrat"], df[i,"typeContratLibelle"])
    insert_into_experience (connexion,df[i,"id"],df[i,"experienceLibelle"], df[i,"experienceExige"])
    insert_into_secteur_activite(mydb,df[i,"id"],df[i,"secteurActiviteLibelle"],df[i,"secteurActivite"])
    insert_into_Offre_emploi (connexion,df[i,"id"],df[i,"dateCreation"],df[i,"lieuTravail.commune"],df[i,"intitule"], df[i,"description"])
    
    #insert_into_Offre_emploi (connexion,df[i,"id"],df[i,"dateCreation"],df[i,"lieuTravail.commune"],df[i,"intitule"], df[i,"description"])
    #insert_into_Offre_emploi (connexion,df[i,"id"],df[i,"intitule"], df[i,"description"],df[i,"dateCreation"])
  }
}