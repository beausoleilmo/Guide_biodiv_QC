# Guide de la biodiversité du Québec par région (GBQR)

Guide de biodiversité du Québec

- Téléchargement et préparation automatique des données GBIF pour le Québec (totalisant plus de 48 millions d'occurrence en 2026).
- Combinaison de diverses liste de noms d'espèce en français. Les données sont compléter manuellement. 

## Structure 

- `./scripts/99.guide/control.R` : Génère le document `.html`
- `./scripts/99.guide/guide_regs.qmd` : document principal et construit le guide en `.html`

## Ajout de modes nuit et jour 
le fichier header.html permet une installation du Guide sur iPhone avec un logo et permet la fonctionnalité du mode nuit/jour pour une navigation plus agréable selon la préférence de l'utilisateur.  

## Erreurs connues

Si erreur lors de la compilation : "Error: pandoc document conversion failed with error 9", pandoc manque de mémoire. Cela peut prendre 52GB de mémoire (avec SWAP)

Les espaces dans le code HTML sont importantes. 

## Références 

- Utilisation du modeule jour/nuit de https://github.com/glauberramos/inat
