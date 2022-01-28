# Connexion à la base
connect<-function(user='root', password='root', dbname='textmining', host='127.0.0.1', port=3306){
  
  mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host, port=port, encoding = "utf8mb4")
  return (mydb)
}

# Alimentation de la base
insert_into_poste <-function (connexion, id, code_rome, libelle_rome, appellation_libelle){
  query = "insert into poste (id_poste,code_rome, libelle_rome, appellation_libelle) values ("
  query = paste(query,"'",id,"','",code_rome,"',",dbQuoteString(connexion, libelle_rome),",", dbQuoteString(connexion, appellation_libelle),")",sep="")
  execute_requete(connexion, query)
}


insert_into_localisation <-function (connexion,id, nom_lieu_travail, longitudes_lieu_travail, latitudes_lieu_travail){
  query = "insert into localisation (id_localisation, nom_lieu_travail, longitudes_lieu_travail, latitudes_lieu_travail) values ("
  query = paste(query,"'",id,"',",dbQuoteString(connexion,nom_lieu_travail),",",na_to_null(longitudes_lieu_travail),",",na_to_null(latitudes_lieu_travail),")",sep="")
  execute_requete(connexion, query)
}


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
  query = paste(query,"'",id,"',",dbQuoteString(connexion, libelle_secteur),",",dbQuoteString(connexion,secteur_activite),")",sep="")
  execute_requete(connexion, query)
}

insert_into_Offre_emploi <-function (connexion,id,intitule_offre, description_offre, date_creation, id_contrat, id_poste,id_localisation,id_experience,id_secteur, categorie){
  id_offre_emploi = id
  query = "insert into Offre_emploi (intitule_offre, description_offre, categorie, date_creation,id_offre,id_localisation, id_contrat, id_poste, id_experience,id_secteur) values ("
  query = paste(query,dbQuoteString(connexion, intitule_offre),",",dbQuoteString(connexion,description_offre),",",dbQuoteString(connexion, categorie),",'",date_creation,"',",sep="")
  query = paste(query,"'",id_offre_emploi,"','",id_localisation,"','",id_contrat,"','",id_poste,"','",id_experience,"','",id_secteur,"')",sep="")
  execute_requete(connexion, query)
  
}

# Vérifier si l'enregistrement est deja dans la base, si oui il recupere le id, sinon il l'insere et recupere son id 
# #get_localisation<- function(connexion,lieuTravail.commune){
#   req = "select id_localisation from localisation where "
#   req = paste(req,"lieuTravail.commune =",dbQuoteString(connexion, lieuTravail.commune),sep="")
#   return (dbGetQuery(connexion, req)[1,1])
# }

get_experience<- function(connexion, libelle_experience, experience_exigee){
  
  experience_exigee = utf8_encode(experience_exigee)
  libelle_experience = utf8_encode(libelle_experience)
  req = "select id_experience from experience where "
  req = paste(req,"experience_exigee =",dbQuoteString(connexion, experience_exigee),sep="")
  req = paste(req," and libelle_experience =",dbQuoteString(connexion, libelle_experience),sep="")
  return (dbGetQuery(connexion, req)[1,1])
}


get_secteur_activite<- function(connexion, libelle_secteur, secteur_activite){
  
  libelle_secteur = utf8_encode(libelle_secteur)
  secteur_activite = utf8_encode(secteur_activite)  
  req = "select id_secteur from secteur_activite where "
  req = paste(req,"libelle_secteur =",dbQuoteString(connexion, libelle_secteur),sep="")
  req = paste(req," and secteur_activite =",dbQuoteString(connexion, secteur_activite),sep="")
  return (dbGetQuery(connexion, req)[1,1])
}
get_contrat<- function(connexion, type_contrat, libelle_contrat){
  
  libelle_contrat = utf8_encode(libelle_contrat)
  req = "select id_contrat, type_contrat, libelle_contrat from contrat where "
  req = paste(req,"type_contrat ='",type_contrat,"'",sep="")
  req = paste(req," and libelle_contrat =",dbQuoteString(connexion, libelle_contrat),sep="")
  return (dbGetQuery(connexion, req)[1,1])
}

get_poste<- function(connexion, code_rom){
  req = paste("select id_poste from poste where code_rome='",code_rom,"'", sep="")
  return (dbGetQuery(connexion, req)[1,1])
}


#Execution de la requete
execute_requete <-function (connexion, query, nbrRow=0){
  dbSendQuery(connexion,"SET NAMES utf8mb4;")
  dbSendQuery(connexion,"SET CHARACTER SET utf8mb4;")
  #print(query)
  rs <- dbSendQuery(connexion, query)
  data = dbFetch(rs, nbrRow)
  dbClearResult(rs)
  return (data)
}

#
# sql_text <- function ( text){
#   return (str_replace_all(text,"'","''"))
# }
#l'offre
id_exists<- function(connexion, id){
  rs = dbSendQuery(connexion, paste("select id_offre  from Offre_emploi where id_offre='",id,"'",sep=""))
  data=dbFetch(rs,1)
  count = dbGetRowCount(rs)
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

insert_data_int_bdd<- function (connexion,df){
  for(i in 1:nrow(df)) {
    id = df[i,"id"]
    if(id_exists(connexion, id)){
      next;
    }
    
    id_poste= get_poste(connexion, df[i,"romeCode"])
    if(is.na(id_poste)){
      insert_into_poste (connexion,id, df[i,"romeCode"], df[i,"romeLibelle"],
                       df[i,"appellationlibelle"])
      id_poste=id
    }
    
    id_localisation = get_localisation(connexion= df[i,"lieuTravail.longitude"], df[i,"lieuTravail.latitude"])
    if(is.na(id_localisation)){
      insert_into_localisation(connexion,id,df[i,"lieuTravail.libelle"],df[i,"lieuTravail.longitude"], df[i,"lieuTravail.latitude"])
      id_localisation=id
    }
    
    id_contrat = get_contrat(connexion, df[i,"typeContrat"], df[i,"typeContratLibelle"])
    if(is.na(id_contrat)){
      insert_into_contrat(connexion,id,df[i,"typeContrat"], df[i,"typeContratLibelle"])
      id_contrat=id
    }
    
    
    id_experience = get_experience(connexion, df[i,"experienceLibelle"], df[i,"experienceExige"])
    if(is.na(id_experience)){
      insert_into_experience (connexion,id,df[i,"experienceLibelle"], df[i,"experienceExige"])
      id_experience=id
    }
    
    id_secteur = get_secteur_activite(connexion, df[i,"secteurActiviteLibelle"], df[i,"secteurActivite"])
    if(is.na(id_secteur)){
      insert_into_secteur_activite(mydb,id,df[i,"secteurActiviteLibelle"],df[i,"secteurActivite"])
      id_secteur=id
    }
    insert_into_Offre_emploi (connexion,id,df[i,"intitule"],  df[i,"description"],df[i,"dateCreation"], id_contrat, id_poste,id_localisation,id_experience,id_secteur,df[i,"categorie"])
  }
}


#Creation de la base de données 
reset_base_donnes<-function(user='root', password='root', host='127.0.0.1', port=3306, dbname="textmining"){
  connexion = dbConnect(MySQL(), user=user, password=password, host=host, port=port)
  execute_requete(connexion,paste("drop database if exists",dbname))
  execute_requete(connexion,paste("create database ",dbname))
  execute_requete(connexion,paste("use  ",dbname))
  #execute_requete(connexion,"drop table if exists Offre_emploi")
  #execute_requete(connexion,"drop table if exists localisation")
  #execute_requete(connexion,"drop table if exists contrat")
  #execute_requete(connexion,"drop table if exists poste")
  #execute_requete(connexion,"drop table if exists experience")
  #execute_requete(connexion,"drop table if exists secteur_activite")
  dbDisconnect(connexion)
  connexion = connect( user, password, dbname,host, port)
  execute_requete(connexion, "SET NAMES utf8mb4")
  execute_requete(connexion, "SET CHARACTER SET utf8mb4")
  execute_requete(connexion, "SET character_set_connection=utf8mb4")
    
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
  req=paste(req,"CREATE TABLE regions(")
  req=paste(req," code_insee varchar(50),")
  req=paste(req," nom_region varchar(50),")
  req=paste(req," PRIMARY KEY (code_insee)")
  req=paste(req,");")
  execute_requete(connexion,req)

  regions = read.csv("regions.csv",sep=";",encoding = "UTF-8")
  dbWriteTable(connexion,name ="regions",regions,append=TRUE,overwrite=FALSE,row.names=FALSE)

  # 
  # 
  # req=""
  # req=paste(req,"CREATE TABLE departements(")
  # req=paste(req," num_dep varchar(3),")
  # req=paste(req," nom_dep varchar(50),")
  # req=paste(req," code_region varchar(50),")
  # req=paste(req," PRIMARY KEY (num_dep),")
  # req=paste(req," FOREIGN KEY (code_region) REFERENCES regions(code_insee)")
  # req=paste(req,");")
  # execute_requete(connexion,req)
  # 
  # 
  # departements = read.csv("departements.csv",sep=",",encoding = "UTF-8")
  # dbWriteTable(connexion,name ="departements",departements,row.names=FALSE,append=TRUE,overwrite=FALSE)
  # 
  # 
  # 
  # req=""
  # req=paste(req,"CREATE TABLE communes(")
  # req=paste(req," num_commune varchar(5),")
  # req=paste(req," num_dep varchar(3),")
  # req=paste(req," nom_ville varchar(3),")
  # req=paste(req," longitude float,")
  # req=paste(req," latitude float,")
  # req=paste(req," PRIMARY KEY (num_commune),")
  # req=paste(req," FOREIGN KEY (num_dep) REFERENCES departements(num_dep)")
  # req=paste(req,");")
  # execute_requete(connexion,req)
  # 
  # 
  # 
  # communes = read.csv("communes.csv")
  # dbWriteTable(connexion,name ="communes",communes,row.names=FALSE,append=FALSE,overwrite=TRUE)
  # 
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
  req=paste(req,"  id_localisation VARCHAR(10),                                  ")
  req=paste(req,"  id_contrat VARCHAR(10),                                       ")
  req=paste(req,"  id_poste VARCHAR(10),                                         ")
  req=paste(req,"  id_experience VARCHAR(10),                                    ")
  req=paste(req,"  id_secteur VARCHAR(10),                                       ")
  req=paste(req,"  categorie VARCHAR(60),                                       ")
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
# reflexion 
#show global variables like 'local_infile';
#set global local_infile=true;
#CREATE INDEX idx1 ON t1 ((col1 + col2));


# date_last_update<- function(connexion){
#   date_last_update = fetch(
#     dbSendQuery(connexion, "select max(date_creation) from offre_emploi"),
#     n=1)
#   return (date_last_update[[1]])
# }
