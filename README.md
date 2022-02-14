# Projet Text Mining - Master 2 SISE 
* Romain Dudoit
* Fatim-Zahra El Gaouzi
* Elisa Frintz

## Contexte et objectifs du projet

Dans le cadre de notre cours de __text mining__, nous avons voulu analyser différents métiers autour de la data qui sont très souvent confondus dans l’imaginaire collectif. En effet, le métier de Data scientist a longtemps été le métier de référence dans le domaine de la data. Pourtant, il existe de plus en plus métiers différents qui permettent de mieux spécifier les rôles et besoins de chacun. Parmi ces nombreux noms de métiers, nous avons voulu nous concentrer sur trois d’entre eux : __Data Scientist__, __Data Analyst__ et __Data Engineer__. 

Pour cela, nous avons utilisé l’__API de pôle emploi__ qui regroupe un certain nombre d’offres d’emploi en France. Afin de ne pas perdre les données des offres d’emploi qui pourrait être retirées, nous avons construit une base de données afin de les stocker dans un serveur __MySQL__. Nous pouvons également mettre à jour cette base de données et en ajoutant les nouvelles offres disponibles.

Pour présenter nos résultats, nous avons construit une interface à l’aide de __Rshiny__. Celle-ci se compose de trois onglets ainsi que d’un bouton pour mettre à jour la base de données. Elle se base directement sur la base de données qui doit avoir été installée et construite en amont (à l’aide d’un script.R fournit). Nous avons ainsi pu analyser les différentes offres d’emploi afin d’en tirer des informations pertinentes quant à leurs __similarités__ et leurs __différences__.

### Page n°1 : Statistiques descriptives
![p1_1](https://user-images.githubusercontent.com/65174929/153760895-979d3d5c-26af-4876-a075-1d3d5f99ac10.png)
![p1_2](https://user-images.githubusercontent.com/65174929/153760904-f745d7a0-4314-40d3-bcf3-0768b8a9e578.png)

### Page n°2 : Analyse des offres
![page2](https://user-images.githubusercontent.com/65174929/153760845-9a600a08-c2fd-4a7d-a849-c1316c629a97.png)

### Page n°3 : Analyse des compétences
![page3](https://user-images.githubusercontent.com/65174929/153760852-042a7ce9-6a96-46ac-a5c3-4eb87ea33f37.png)

### Bouton de mise à jour de la base de données
![miseajour](https://user-images.githubusercontent.com/65174929/153760863-e409ecb5-5485-49a9-adcb-1c699b73c429.png)


