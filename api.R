
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
  result_auth <- POST("https://entreprise.pole-emploi.fr/connexion/oauth2/access_token",
                      query = list(realm = "/partenaire"),
                      body = request_body,
                      encode = "form")
  auth_JSON = fromJSON(rawToChar(result_auth$content), flatten = TRUE) ; auth_JSON
  token = paste("Bearer ", auth_JSON$access_token) ; token
  return (token)
}


clean_dataframe <- function(df){
  df = apply(df,2, function(x) gsub("[\r\n]", " ", x)) # suppression des retours ? la ligne
  df = apply(df,2, function(x) gsub("&bull", " ", x)) # suppression des ?
  df = apply(df,2, function(x) gsub("\\s+", " ", x)) # suppression des espaces en trop
  df = apply(df,2, function(x) str_trim(x))
  return (df)
}


colstokeep = c("id","intitule","description","dateCreation","romeCode","romeLibelle","appellationlibelle","typeContrat","typeContratLibelle","experienceExige","experienceLibelle","secteurActivite","secteurActiviteLibelle","lieuTravail.libelle","lieuTravail.latitude","lieuTravail.longitude","lieuTravail.commune","lieuTravail.codePostal","entreprise.nom")         



data_from_api_to_bdd <-function(connexion,mot_cle, token){
  
  #preparer le parametrage
  mot_cle = str_replace_all(mot_cle," ","+")
  # range
  index_min=0
  index_max=149
  
  #r?peter tant que l'appel de webservice envoie le code 206
  repeat{
    url = paste("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=",
                mot_cle,
                "&range=",
                as.character(index_min),
                "-",
                as.character(index_max), sep=""
                )
    
    print(url)
    
    request_data=GET(url,add_headers(Authorization = token))
    
    if(status_code(request_data)==206 | status_code(request_data)==200 ){
      
      df=as.data.frame(fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)$resultats)
      df = df[,colstokeep]
      df <- separate(df,lieuTravail.libelle,c('departement','ville'),sep = " - ")
      df$departement = gsub(" ", "", df$departement, fixed = TRUE)
      
      for (i in 1:nrow(df)){
        if (!grepl('^[[:digit:]]',df$departement[i])){
          df$ville[i] = df$departement[i]
          df$departement[i] = ""
        }
      }
      
      df$ville = gsub("\\s*\\([^\\)]+\\)","",df$ville) 
      df$ville = gsub(' [[:digit:]]+', '', df$ville) # Suppression des arrondissements
      df$ville[df$ville=="Paris"] <- "PARIS" # Convertion en Uppercase
      df$ville = gsub("France", '', df$ville) # Remplace France par rien 
      df$ville = gsub("\\b[a-z]+\\b", '', df$ville)
      
      
      df[grep("[[:alpha:]][a-z]+|[a-z][[:alpha:]]+",df$ville),"ville"] <- "" # Remplace la ville par rien si en lowercase
      df$ville = gsub("D ", 'D\'', df$ville) # ajoute un ' apr?s le D
      df$ville = gsub("L ", 'L\'', df$ville) # ajoute un ' apr?s le L
      df$ville = gsub(" ", '-', df$ville) # remplace les espaces par -
      
      df$dateCreation = as.Date(df$dateCreation) # conversion des dates en date
      
      # Suppression des villes avec des valeurs manquantes
      df <- df[-which(df$ville == ""), ]
      df <- df[!is.na(df$ville), ]
      
      print(colnames(df))
      
      #communes = read.csv("communes.csv")
      
      #df2 <- df
      #df2 <- merge(x=df2,y=communes[,c("Num_commune")],by.x="lieuTravail.commune",by.y="Num_commune")
      # <- left_join(df2,communes,by=c("ville"="Nom_Ville","Departement"="Num_Dep"))[,-c(16,17)]
      
      #print(colnames(df))
      

      #colstoremove = c("lieuTravail.longitude","lieuTravail.latitude","lieuTravail.commune","lieuTravail.codePostal","entreprise.logo","entreprise.description","entreprise.entrepriseAdaptee","entreprise.url","entreprise.logo","typeContratLibelle","natureContrat","experienceLibelle")
      
      #print(df)
      
      #df = clean_dataframe(df)
      
      
      insert_data_int_bdd(connexion,df)
      #si le code different de 206 ou 200, arr?ter la boucle
    }else{
      break
    }
    index_min= index_min+150
    index_max= index_max+150
  }
  return (df)
}
# 
# #recuperer les donnees de l'api N'EST PLUS UTILISEE
# get_data_from_api<-function (mot_cle, token){
#   
#   #preparer le param
#   mot_cle = str_replace_all(mot_cle," ","+")
#   index_min=0
#   index_max=149
#   url = paste("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=",mot_cle,sep="")
#     
#   print(url)
#   request_data=GET(url,add_headers(Authorization = token))
#   df=as.data.frame(fromJSON(rawToChar(request_data$content))$resultats[,colstokeep])
#   
#   request_data=GET(url,add_headers(Authorization = token))
#   df=as.data.frame(fromJSON(rawToChar(request_data$content))$resultats[,colstokeep])
#   return (as.data.frame(clean_dataframe(df)))
# }



