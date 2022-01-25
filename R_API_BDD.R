#Packages
# install.packages(c("httr", "jsonlite"))
# install.packages('stringr')
# install.packages("RMySQL")
setwd("~/M2_Text_Mining-dev_Fatim-Zahra")
library(httr)
library(jsonlite)
library(stringr)
library(RMySQL)
source("DB.R")
source("api.R")

#initialisation de la base
reset_base_donnes()
# recuperation du token 
token=get_token()
#ouverture de la connexion 
mydb=connect()
#recupeation des mots cles de l'api 
for (motcle in c("Data scientist", "Data engineer", "Data analyst"))
  df= data_from_api_to_bdd(mydb,motcle,token)

# à retravailler 
#maj=date_last_update(mydb)
#if(is.na(maj)){
#  urlavecdate<-paste("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data+engineer&&dateCreation=",maj)
#}else{
#  urlavecdate<-"https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data+engineer&&dateCreation="
#}

# fermeture de la connexion 
dbDisconnect(mydb)

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


