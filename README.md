# Guide_biodiv_QC
Guide de biodiversité du Québec

- Les données GBIF sont automatiquement téléchargé pour le Québec 
- Liste d'espèce avec noms standardisé au Québec n'existe pas. Il faut créer 
cette liste avec un ensemble de données publiques puis compléter manuellement. 

## Structure : 

- `./scripts/control.R` : Générer le document
- `./scripts/guide_regs.qmd` : document principal et construit le guide en `.html`

## Ajout de modes nuit et jour 
le fichier header.html contient des 

## Erreurs connues

Si erreur : "Error: pandoc document conversion failed with error 9", pandoc manque de mémoire. Cela peut prendre 52GB de mémoire (avec SWAP)

Les espaces dans le code HTML sont importantes. 

## 
- Utilisation du modeule jour/nuit de https://github.com/glauberramos/inat
