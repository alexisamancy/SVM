# Implémentation des SVM dans le cadre de détection de fraude sur des cartes de crédit. 

<p style="text-align:justify";>
Cette application a été développée par [**Sabrina MAZLOUM**](https://www.linkedin.com/in/sabrina-mazloum-26359717a/) et [**Alexis AMANCY**](https://www.linkedin.com/in/alexis-amancy-a50901133/). Elle a pour but de montrer l'implémentation des **SVM** (Support Vector Machine) dans le cadre de **détection de fraude** sur des cartes de crédit. Les données utilisées pour cette démonstration sont retrouvables [ici](https://www.kaggle.com/mlg-ulb/creditcardfraud").    
</p>

## Les données 

<p style="text-align:justify;>
Le jeu de données contient des transactions effectuées par cartes de crédit en Septembre 2013 par des européens. Il présente des transactions produitent sur deux jours. Il y a 284 807 observations et 492 sont frauduleuses. La base de données est grandement déséquilibrée, la classe positive (fraude) représente seulement 0.172% de la totalité des transactions. 
</p>
<p style="text-align:justify;>
Le jeu de données contient uniquement des variables quantitatives, dont 28 (notées V1, V2, ..., V28) issues d'une transformation PCA en raison de leur anonymisation. Nous disposons également de la variable 'Time' qui représente le temps écoulé en secondes entre chaque transactions et la première transaction dans la base, ainsi que la variable 'Amount' qui représente le montant de la transaction. Notre variable de réponse est la variable 'Class', elle prend les valeurs 1 en cas de fraud et 0 sinon.
</p>

<p style="text-align:justify";>
Ce projet a pour but de déployer un démonstrateur en ligne via RShiny à l'aide de Travis,   
montrant l'implémentation des **machines à vecteurs de support** dans une logique de détection de fraude.   
Il est réalisé dans le cadre d'un cours enseigné par [Christophe Hurlin](https://sites.google.com/view/christophe-hurlin/home?authuser=0llllllllll), professeur au [Master ESA](https://www.univ-orleans.fr/deg/masters/ESA/) d'Orléans, et soutenu par 
[Jérémy Dudek](crest.fr/pagesperso.php?user=3223), Responsable de la Recherche chez YOMONI. Nous les remercions vivement. 
</p>
