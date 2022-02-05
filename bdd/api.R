
Add_categorie <- function(df){
  df$intitule = toupper(df$intitule)
  df$categorie = "AUTRE"
  df$categorie[grep("DATA SCIENTIST", df$intitule)] = "DATA SCIENTIST"
  df$categorie[grep("DATA ANALYST", df$intitule)] = "DATA ANALYST"
  df$categorie[grep("DATA ENGINEER", df$intitule)] = "DATA ENGINEER"
  return(df)
}

#insertion régions
mise_a_jour_regions<-function (connexion, token){
  url="https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/regions"
  request_data=GET(url,add_headers(Authorization = token))
  data= fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)
  for(i in 1:nrow(data)) {
    insert_into_regions (connexion, data[i,"libelle"], data[i,"code"])
  }
}

#insertion departements
mise_a_jour_departement<-function (connexion,token){
  url="https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/departements"
  request_data=GET(url,add_headers(Authorization = token))
  data = fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)
  for(i in 1:nrow(data)) {
    insert_into_departements (connexion, data[i,"libelle"], data[i,"code"], data[i,"region.code"] )
  }
}

#insertion communes
mise_a_jour_communes<-function (connexion, token){
  url="https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/communes"
  request_data=GET(url,add_headers(Authorization = token))
  data = fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)
  data = subset(data, !(codeDepartement %in% c("977","978","979","980","981","982","983","984",
                                               "985","986","987","988","989","975","99")))
  
  for(i in 1:nrow(data)) {
      insert_into_communes (connexion,  data[i,"code"], data[i,"libelle"], 
                            data[i,"codePostal"], data[i,"codeDepartement"])
  }
}

get_date_from_url<- function(url){
  r <- GET(url)
  return (fromJSON( content(r, as="text")))
}


mise_a_jour_referentiel<-function (connexion, token){
  mise_a_jour_regions(connexion,token)
  mise_a_jour_departement(connexion,token)
  mise_a_jour_communes(connexion,token)
}

data_from_api_to_bdd<-function (connexion,mot_cle, token){

  #preparer le parametrage
  mot_cle = str_replace_all(mot_cle," ","+")
  # range
  index_min=0
  index_max=149
  
  #répeter tant que l'appel de webservice envoie le code 206
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
    print(status_code(request_data))
    if(status_code(request_data)==206 | status_code(request_data)==200 ){
      #
      df=as.data.frame(fromJSON(content(request_data,as="text", encoding = "UTF-8"), flatten =TRUE)$resultats)
      df = Add_categorie(df)
      insert_data_int_bdd(connexion,df)
      #si le code different de 206 ou 200, arrêter la boucle
    }else{
      break
    }
    index_min= index_min+150
    index_max= index_max+150
  }
  return (df)
}


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

