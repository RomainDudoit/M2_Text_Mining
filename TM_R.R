
rm(list = ls(all = TRUE))
setwd("~/Documents/M2_SISE/Text_Mining/Projet")
library(httr)
library(jsonlite)
library(stringr)
library(tm)

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

# Data scientist
request_data_scientist = GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data+scientist", 
                             add_headers(Authorization = token))
df = fromJSON(rawToChar(request_data_scientist$content))$resultats

df1 = df[1:10,c("id","intitule","description")]

# df1$description = gsub("\n", " ", df1$description)
# df1$description = gsub("[[:punct:]]", " ", df1$description)
# df1$description = gsub("[[:digit:]]", " ", df1$description)

mycorpus <- VCorpus(x = VectorSource(df1$description),
                      readerControl = list(reader=readPlain,
                                           language="fr"))

mycorpus <- tm_map(mycorpus,removePunctuation)
mycorpus <- tm_map(mycorpus,removeNumbers) # retrait des nombres
mycorpus <- tm_map(mycorpus, removeWords, stopwords("french")) # retrait des stopwords 
mycorpus <- tm_map(mycorpus,stripWhitespace) # retirer les espaces en trop (s'il en reste encore)

MatTermDoc <- DocumentTermMatrix(mycorpus)
print(MatTermDoc)
mat.valeurs <- as.matrix(MatTermDoc)

# La transformer en data.frame
dfTermDoc <- as.data.frame(mat.valeurs)

#####################################################

msgClean = gsub("\n", " ", df1$description)
msgClean = gsub("[[:punct:]]", " ", msgClean)
msgClean = gsub("[[:digit:]]", " ", msgClean)


names(msgClean) <- seq(1,length(msgClean),1)
print(msgClean[1]) #contr?le

corpus <- Corpus(VectorSource(msgClean))
print(corpus)

#retrait des ponctuations
corpus <- tm_map(corpus,removePunctuation)
#retrait des nombres
corpus <- tm_map(corpus,removeNumbers)
#retrait des stopwords (mots outils)
corpus <- tm_map(corpus,removeWords,stopwords("french"))
#retirer les espaces en trop (s'il en reste encore)
corpus <- tm_map(corpus,stripWhitespace)
#vérification avec le document n°8
print(corpus[[8]]$content)


#################################

mots = strsplit(vec.desc, " ") ; mots

corpus = unlist(df1$description) ; corpus[1]

?gsub
########################################
class(textes.bis)
textes.bis = corpus

#chargement de la librairie
library(tm)

#faire savoir que chaque ligne du vecteur est un document
reuters <- Corpus(VectorSource(textes.bis))

#premi?re version de la DTM
mdt.1 <- DocumentTermMatrix(reuters)
print(mdt.1)

#nettoyage
reuters <- tm_map(reuters, removePunctuation)
reuters <- tm_map(reuters, content_transformer(tolower))
reuters <- tm_map(reuters, removeWords, stopwords("french"))  
reuters <- tm_map(reuters, removeNumbers)
reuters <- tm_map(reuters, stripWhitespace)

#seconde version
mdt.2 <- DocumentTermMatrix(reuters)
print(mdt.2)






############################################







mots = strsplit(corpus, " ") ; mots

documents <- Corpus(VectorSource(corpus))

documents = tm_map(documents, removeWords, stopwords('french')) #remove stopwords



documents <- DocumentTermMatrix(reuters)


vec.desc = tolower(vec.desc) ; vec.desc

mots = strsplit(vec.desc, " ") ; mots

stopwords("french")

reuters <- Corpus(VectorSource(vec.desc))
mdt.1 <- DocumentTermMatrix(reuters)
print(mdt.1)

documents = tm_map(documents, removeWords, stopwords('english')) #remove stopwords

reuters <- tm_map(reuters, removePunctuation)
reuters <- tm_map(reuters, content_transformer(tolower))
reuters <- tm_map(reuters, removeWords, stopwords("french"))  
reuters <- tm_map(reuters, removeNumbers)
reuters <- tm_map(reuters, stripWhitespace)


df1$description[1]


for (i in stopwords("french")){
  df1$description = gsub(i, " ", df1$description, fixed = TRUE)
}

df1$description[1]
