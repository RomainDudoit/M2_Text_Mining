library(httr)
library(jsonlite)
library(stringi)
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(tidyverse)



rm(list = ls(all = TRUE))

clean_dataframe <- function(df){
  df = apply(df,2, function(x) gsub("Ã©", "é",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã¨", "è",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã¯ ", "ï",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã´", "ô",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã§", "ç",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ãª", "ê",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã¹", "ù",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã¹", "ù",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã‰", "É",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ãˆ", "È",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã€", "À",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã¦ ", "æ",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã¢ ", "â",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Å“", "œ",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Å“", "œ",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Ã", "à",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("Â", " ",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("à®", "î",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("à¢", "â",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("à»", "û",x)) #iso 8859-1 to utf8
  df = apply(df,2, function(x) gsub("[\r\n]", " ", x)) # suppression des retours à la ligne
  df = apply(df,2, function(x) gsub("&bull", " ", x)) # suppression des •
  df = apply(df,2, function(x) gsub("\\s+", " ", x)) # suppression des espaces en trop
  df = apply(df,2, function(x) str_trim(x))
  return (df)
}

experience <- read.csv("experience.csv",encoding = "UTF-8")
colnames(experience) <- c("id_Exp","Exp_exigee","Exp_Libelle")

contrat <- read.csv("contrat.csv",encoding = "UTF-8")
colnames(contrat) <- c("id_Contrat","Type_Contrat","Libelle_Contrat")

regions = read.csv("regions.csv",encoding = "UTF-8")
colnames(regions) <- c("id_Region","CODE_INSEE","NOM_REGION")

departements = read.csv("departements.csv",encoding = "UTF-8")
colnames(departements) <- c("id_DEP","NUMERO_DEP","NOM_DEP","CODE_REGION")

villes = read.csv("villes.csv",encoding = "UTF-8")
colnames(villes) <- c("id_Ville","Num_Dep","Nom_Ville","Longitude","Latitude")
villes$Num_Dep <- as.character(villes$Num_Dep)

dates = read.csv("dates.csv")
dates$Date <- as.Date(dates$Date)


##################################################################################################################


# https://pole-emploi.io/data/documentation/utilisation-api-pole-emploi/generer-access-token
# https://stackoverflow.com/questions/52569432/invalid-content-type-after-a-post-request-with-httr

id_client = "PAR_textminingr_b7458e0fe84fea218e101d411a7f861a49e551f3b9bf18c612a5059bdea3be5b"
cle_secrete = "5feab49ea00bb939a6224dfb224b13d895b7ea21021a67c6468b5a663f410eab"

# I. Générer un access token (client credentials)
request_body <- list(grant_type = "client_credentials",
                     client_id = id_client,
                     client_secret = cle_secrete,
                     scope = paste("api_offresdemploiv2", "o2dsoffre", 
                                   paste0("application_",id_client), sep = " "))

result_auth <- POST("https://entreprise.pole-emploi.fr/connexion/oauth2/access_token",
                    query = list(realm = "/partenaire"),
                    body = request_body,
                    encode = "form")

auth_JSON = fromJSON(rawToChar(result_auth$content), flatten = TRUE)
token = paste("Bearer ", auth_JSON$access_token) 

# II. Requêter une API 
# Méthodologie : 
#    - request contient du code hexadecimal (https://cryptii.com/pipes/hex-to-text) 
#    - pour transformer cette requete en dataframe, il faut convertir l'hexadecimal en caractere,
#      ce qui donne du JSON, puis convetir ce format JSON en r


# Colonnes à garder (certains champs ne sont pas directement accessibles car des dataframes sont contenus le df initial)
colstokeep = c("intitule","description","dateCreation","lieuTravail","romeCode","romeLibelle","appellationlibelle","entreprise","typeContrat","typeContratLibelle","natureContrat","experienceExige","experienceLibelle","secteurActivite","secteurActiviteLibelle") 

colstoremove = c("lieuTravail.longitude","lieuTravail.latitude","lieuTravail.commune","lieuTravail.codePostal","entreprise.logo","entreprise.description","entreprise.entrepriseAdaptee","entreprise.url","entreprise.logo","typeContratLibelle","natureContrat","experienceLibelle")


# Recherches des offres de data scientist en France
url = "https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data+scientist&paysContinent=01"
first_index = 0
last_index = 149
nb_page = 10

df_data_scientist <- data.frame(row.names = NULL)

for (i in 1:nb_page){
  url = paste(url,"&range=",as.character(first_index),"-",as.character(last_index),sep="")
  request_data_scientist =GET(url,add_headers(Authorization = token))
  df = fromJSON(rawToChar(request_data_scientist$content))$resultats
  df = df[,colstokeep]
  df <- as.data.frame(clean_dataframe(df))
  df_data_scientist <- rbind(df_data_scientist,df)

  first_index = first_index+150
  last_index = last_index+150
}


##################################################################################################################################

# Filtrage des données à garder
df_data_scientist = df_data_scientist[!names(df_data_scientist) %in% colstoremove]


# Transformation des données
df_data_scientist <- separate(df_data_scientist,lieuTravail.libelle,c('Departement','Ville'),sep = " - ")
df_data_scientist$Departement = gsub(" ", "", df_data_scientist$Departement, fixed = TRUE)

for (i in 1:nrow(df_data_scientist)){
  if (!grepl('^[[:digit:]]',df_data_scientist$Departement[i])){
    df_data_scientist$Ville[i] = df_data_scientist$Departement[i]
    df_data_scientist$Departement[i] = ""
  }
}

df_data_scientist$Ville = gsub("\\s*\\([^\\)]+\\)","",df_data_scientist$Ville) 
df_data_scientist$Ville = gsub(' [[:digit:]]+', '', df_data_scientist$Ville) # Suppression des arrondissements
df_data_scientist$Ville[df_data_scientist$Ville=="Paris"] <- "PARIS" # Convertion en Uppercase
df_data_scientist$Ville = gsub("France", '', df_data_scientist$Ville) # Remplace France par rien 
df_data_scientist$Ville = gsub("\\b[a-z]+\\b", '', df_data_scientist$Ville)

df_data_scientist[grep("[[:alpha:]][a-z]+|[a-z][[:alpha:]]+",df_data_scientist$Ville),"Ville"] <- "" # Remplace la ville par rien si en lowercase
df_data_scientist$Ville = gsub("D ", 'D\'', df_data_scientist$Ville) # ajoute un ' après le D
df_data_scientist$Ville = gsub("L ", 'L\'', df_data_scientist$Ville) # ajoute un ' après le L
df_data_scientist$Ville = gsub(" ", '-', df_data_scientist$Ville) # remplace les espaces par -


df_data_scientist$dateCreation = as.Date(df_data_scientist$dateCreation) # conversion des dates en date

# Suppression des villes avec des valeurs manquantes
df_data_scientist <- df_data_scientist[-which(df_data_scientist$Ville == ""), ]
df_data_scientist <- df_data_scientist[!is.na(df_data_scientist$Ville), ]

df_data_scientist <- rowid_to_column(df_data_scientist, "ID")

df_data_scientist2 <- df_data_scientist


# Ajout de l'identifiant de la ville
df_data_scientist2 <- left_join(df_data_scientist2,villes,by=c("Ville"="Nom_Ville","Departement"="Num_Dep"))[,-c(16,17)]

# Ajout de l'identifiant de l'experience
df_data_scientist2 <- left_join(df_data_scientist2,experience,by=c("experienceExige"="Exp_exigee"))[,-c(17)]

# Ajout de l'identifiant du contrat
df_data_scientist2 <- left_join(df_data_scientist2,contrat,by=c("typeContrat"="Type_Contrat"))[,-c(18)]

# Ajout de l'identifiant de date
df_data_scientist2 <- left_join(df_data_scientist2,dates,by=c("dateCreation"="Date"))[,-c(19,20)]


df_data_scientist2 = df_data_scientist2[!names(df_data_scientist2) %in% c("Departement","Ville","typeContrat","experienceExige","romeCode","romeLibelle","appellationlibelle","secteurActivite","secteurActiviteLibelle","Libelle_Contrat","entreprise.nom")]
df_data_scientist2 = df_data_scientist2[,c("ID","id_date","id_Ville","id_Exp","id_Contrat","intitule","description")]

lapply(df_data_scientist2,class)


write.csv(df_data_scientist2,"C:/Users/Romain/Documents/GitHub/M2_Text_Mining/df_data_scientist.csv", row.names = FALSE,fileEncoding = "UTF-8")





db <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
#dbDisconnect(mydb)
#unlink("my-db.sqlite")

dbWriteTable(db, "data_engineer", df_data_engineer2)
dbWriteTable(db, "data_engineer", df_data_scientist)


# Exportation des dataframes. 
#write.csv(df_data_engineer2,"C:/Users/rodud/Documents/GitHub/M2_Text_Mining/df_data_engineer.csv", row.names = FALSE)
#write.csv(df_data_scientist,"C:/Users/rodud/Documents/GitHub/M2_Text_Mining/df_data_scientist.csv", row.names = FALSE)
