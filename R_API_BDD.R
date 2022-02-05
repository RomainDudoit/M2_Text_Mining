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
source("DB.R")
source("api.R")

#initialisation de la base
reset_base_donnes()
# recuperation du token 
token=get_token()

#ouverture de la connexion 
mydb=connect()

mise_a_jour_referentiel(mydb, token)

#recupeation des mots cles de l'api 
for (motcle in c("Data scientist", "Data engineer", "Data analyst"))
  df= data_from_api_to_bdd(mydb,motcle,token)

# fermeture de la connexion 
dbDisconnect(mydb)


#url="https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/communes"
#url="https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/departements"
#request_data=GET(url,add_headers(Authorization = token))
#data= fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)
#print(data)
#write.csv( data.frame(df),"C:\\Users\\PC-Abdel\\Desktop\\TEXTM\\db.csv", row.names = FALSE, col.names = FALSE)


# rs = dbSendQuery(mydb,"SELECT categorie, intitule_offre, description_offre from offre_emploi;")
# df_textmining = dbFetch(rs)
# Encoding(df_textmining[["intitule_offre"]]) = "UTF-8"
# Encoding(df_textmining[["description_offre"]]) = "UTF-8"
# 
# df_textmining = df_textmining[df_textmining$categorie != "AUTRE",]
# table(df_textmining$categorie)
# 
# 
# write.csv(data.frame(df_textmining),"df_textmining.csv")
# 


# ÃƒÂ  retravailler 
#maj=date_last_update(mydb)
#if(is.na(maj)){
#  print("aucune date n'a été trouvé")
#}else{
#  print(maj)
#}
#now = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")
#now
#maj




# dateCreation
#paste(strptime(as.character(row[9]), tz = tz, format = "%Y-%m-%dT%H:%M:%OSZ"), collapse = ", "), # dateActualisation

#url = "https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data+scientist"

#print(url)

#request_data=GET(url,add_headers(Authorization = token))
#print(request_data)
#df=as.data.frame(fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)$resultats)
#print(df)

