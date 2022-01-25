#Packages
#install.packages(c("httr", "jsonlite"))
#install.packages('stringr')
#install.packages("RMySQL")
library(httr)
library(jsonlite)
library(stringr)
library(RMySQL)
source("DB.R")
source("api.R")

lapply(dbListConnections(MySQL()), dbDisconnect)


reset_base_donnes() # DB.R : #création de la base avec insertion des régions et des départements 

# recuperation du token 
token=get_token()#ouverture de la connexion
mydb=connect()
#recupeation des mots cles de l'api 
for (motcle in c("Data scientist", "Data engineer", "Data analyst")){
  df= data_from_api_to_bdd(mydb,motcle,token)
}

# fermeture de la connexion 
dbDisconnect(mydb)






# ? retravailler 
#maj=date_last_update(mydb)
#if(is.na(maj)){
#  urlavecdate<-paste("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data+engineer&&dateCreation=",maj)
#}else{
#  urlavecdate<-"https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data+engineer&&dateCreation="
#}



#write.csv( data.frame(df),"C:\\Users\\PC-Abdel\\Desktop\\TEXTM\\db.csv", row.names = FALSE, col.names = FALSE)









