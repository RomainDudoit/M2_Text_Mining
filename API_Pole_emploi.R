rm(list = ls(all = TRUE))


clean_dataframe <- function(df){
  df = apply(df,2, function(x) gsub("Ã©", "é",x))
  df = apply(df,2, function(x) gsub("Ã¨", "è",x))
  df = apply(df,2, function(x) gsub("Ã¯ ", "ï",x))
  df = apply(df,2, function(x) gsub("Ã´", "ô",x))
  df = apply(df,2, function(x) gsub("Ã§", "ç",x))
  df = apply(df,2, function(x) gsub("Ãª", "ê",x))
  df = apply(df,2, function(x) gsub("Ã¹", "ù",x))
  df = apply(df,2, function(x) gsub("Ã¹", "ù",x))
  df = apply(df,2, function(x) gsub("Ã‰", "É",x))
  df = apply(df,2, function(x) gsub("Ãˆ", "È",x))
  df = apply(df,2, function(x) gsub("Ã€", "À",x))
  df = apply(df,2, function(x) gsub("Ã", "à",x))

  return (df)
}



#Sys.setlocale("LC_ALL","French")

#setwd("~/Documents/M2_SISE/Text_Mining/Projet")

library(httr)
library(jsonlite)
library(stringi)


id_client = "PAR_textminingr_b7458e0fe84fea218e101d411a7f861a49e551f3b9bf18c612a5059bdea3be5b"
cle_secrete = "5feab49ea00bb939a6224dfb224b13d895b7ea21021a67c6468b5a663f410eab"

# https://pole-emploi.io/data/documentation/utilisation-api-pole-emploi/generer-access-token
# https://stackoverflow.com/questions/52569432/invalid-content-type-after-a-post-request-with-httr

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

result_auth
content(result_auth)

auth_JSON = fromJSON(rawToChar(result_auth$content), flatten = TRUE) ; auth_JSON
token = paste("Bearer ", auth_JSON$access_token) ; token

# II. Requêter une API 
# Méthodologie : 
#    - request contient du code hexadecimal (https://cryptii.com/pipes/hex-to-text) 
#    - pour transformer cette requete en dataframe, il faut convertir l'hexadecimal en caractere,
#      ce qui donne du JSON, puis convetir ce format JSON en r


#Colonnes à garder
colstokeep = c("intitule","description","dateCreation","lieuTravail","romeCode","romeLibelle","appellationlibelle","typeContrat","typeContratLibelle","natureContrat","experienceExige","experienceLibelle","secteurActivite","secteurActiviteLibelle") 

# Data scientist
request_data_scientist = GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data+scientist", 
                             add_headers(Authorization = token))
df_data_scientist = fromJSON(rawToChar(request_data_scientist$content))$resultats[,colstokeep]


# Data engineer
request_data_engineer = GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data+engineer", 
                            add_headers(Authorization = token))
df_data_engineer = fromJSON(rawToChar(request_data_engineer$content))$resultats[,colstokeep]

df_data_engineer <- as.data.frame(clean_dataframe(df_data_engineer))
df_data_scientist <- as.data.frame(clean_dataframe(df_data_scientist))




