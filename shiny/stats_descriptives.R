eval(parse("functions.R", encoding="UTF-8"))


library(plotly)


##################################################### Répartition des secteurs d'activité

mydb=connect() # Ouverture de la connexion
df_11 = dbGetQuery(
  mydb,
  "SELECT offre.categorie, COUNT(*) as nb, secteur.libelle_secteur 
FROM offre_emploi offre LEFT JOIN secteur_activite secteur
ON offre.id_secteur = secteur.id_secteur
GROUP BY offre.id_secteur;")
Encoding(df_11[["libelle_secteur"]]) = "UTF-8"
dbDisconnect(mydb)

df_11 = df_11 %>% filter(libelle_secteur!="NR") %>% arrange(-nb) 
df_11 = head(df_11,7)

fig <- plot_ly(df_11, x = ~libelle_secteur, y = ~nb, type = 'bar')
fig <- fig %>% layout(title = "Top 5 des secteurs d'activités",
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE,title="Secteur d'activité"),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE,title="%"))

fig

plotlyOutput('plot_Stat_desc_1')
output$plot_Stat_desc_1 <- renderPlotly({
  df_11 = df_11 %>% filter(libelle_secteur!="NR") %>% arrange(-nb) 
  df_11 = head(df_11,7)
  
  fig <- plot_ly(df_11, x = ~libelle_secteur, y = ~nb, type = 'bar') %>%
    layout(title = "Top 5 des secteurs d'activités",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE,title="Secteur d'activité"),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE,title="%"))
  fig
})

##################################################### Répartition des types de contrat (par catégorie)

mydb=connect() # Ouverture de la connexion
df1=dbGetQuery(
  mydb,
  "SELECT offre.categorie, COUNT(*) as nb, contrat.type_contrat
FROM offre_emploi offre LEFT JOIN contrat
ON offre.id_contrat = contrat.id_contrat
GROUP BY contrat.type_contrat, offre.categorie;")

dbDisconnect(mydb)

df1 = df1 %>% count(type_contrat) %>% arrange(-nb) 
#df1 = head(df1,7)

fig <- plot_ly(df1, labels = ~type_contrat, values = ~nb, type = 'pie')
fig <- fig %>% layout(title = "Répartition des types de contrat (par catégorie)",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig


##################################################### Répartition des types de contrat (par catégorie)


mydb=connect() # Ouverture de la connexion
df1=dbGetQuery(
  mydb,
  "SELECT offre.categorie, COUNT(*) as nb, contrat.type_contrat
FROM offre_emploi offre LEFT JOIN contrat
ON offre.id_contrat = contrat.id_contrat
GROUP BY contrat.type_contrat, offre.categorie;")

dbDisconnect(mydb)

df1 = df1 %>% count(type_contrat) %>% arrange(-nb) 
#df1 = head(df1,7)

fig <- plot_ly(df1, labels = ~type_contrat, values = ~nb, type = 'pie')
fig <- fig %>% layout(title = "Répartition des types de contrat (par catégorie)",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig



##################################################### Répartition des expériences exigées par catégorie


mydb=connect() # Ouverture de la connexion
df1=dbGetQuery(
  mydb,
  "SELECT offre.categorie, COUNT(*) as nb, experience.experience_exigee
FROM offre_emploi offre LEFT JOIN experience
ON offre.id_experience = experience.id_experience
GROUP BY experience.experience_exigee, offre.categorie;")

dbDisconnect(mydb)

df1$experience_exigee <- as.factor(df1$experience_exigee)
levels(df1$experience_exigee) <- c("Débutant", "Exgigée", "Souhaitée")

fig <- plot_ly(df1, labels = ~experience_exigee, values = ~nb, type = 'pie')
fig <- fig %>% layout(title = "Répartition des expériences exigées par catégorie",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

############################################################ Afc entre catégorie et libellé code rome
mydb=connect() # Ouverture de la connexion
df1=dbGetQuery(
  mydb,
  "select offre.categorie,  poste.libelle_rome, count(*) as nb
from offre_emploi offre LEFT JOIN poste
ON offre.id_poste = poste.id_poste
group by poste.libelle_rome, offre.categorie;")

dbDisconnect(mydb)
