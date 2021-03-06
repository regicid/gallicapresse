# Notice de Gallicapresse

- Gallicapresse est un programme représentant graphiquement la structure des résultats de recherche réalisées dans le corpus de presse de [Gallica](https://gallica.bnf.fr/).
- Il vient en complément de l'outil [Gallicagram](http://gallicagram.hopto.org:3838/gallicagram_app/) et permet ainsi la maîtrise du corpus sous-jacent.
- Développé par [Benjamin Azoulay](https://github.com/benjyazoulay/) et [Benoît de Courson](https://regicid.github.io/), il est intégralement rédigé en langage [R](https://www.r-project.org/) et présente une interface graphique interactive [Shiny](https://shiny.rstudio.com/).
- Les données produites et les graphiques interactifs sont téléchargeables par l’utilisateur. Le [code source](https://github.com/regicid/gallicapresse) de Gallicapresse est libre d'accès et de droits.


## Extraction

- Gallicapresse procède à l’extraction des données de recherche renvoyées par l’[API de recherche de Gallica](https://api.bnf.fr/fr/api-gallica-de-recherche). Il est paramétré pour des recherches à l’intérieur de bornes chronologiques définies par l’utilisateur. Les corpus sont restreints aux documents en français océrisés (mode texte disponible).
- Gallicapresse compile ensuite les données html sous la forme d'un tableau de données CSV exploitable.
- Le programme nettoie certaines données (ville de publication, titre de presse d'origine) pour les lisser et les harmoniser.

## Options de recherche
- L'utilisateur ne peut entrer qu'une seule requête dans le champ correspondant. 
- La requête conditionnelle "OU" est disponible et s'effectue avec le signe "+" placé entre deux termes.

## Données représentées
- Gallicapresse représente la distribution chronologique des résultats de recherche selon deux variables d'intérêt : la ville de publication du journal et le titre de presse d'origine.
- Gallicapresse affiche aussi dans deux diagrammes distincts les valeurs les plus importantes : principaux titres de presse d'origine ; principales villes de publication des numéros retournés par la recherche.
- Gallicapresse affiche par défaut ces résultats en valeur absolue. Une option permet l'affichage de ces mêmes analyses en valeurs relatives.

## Corpus
- Se reporter à la notice de [Gallicagram](http://gallicagram.hopto.org:3838/gallicagram_app/)
