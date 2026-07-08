# Guide_biodiv_QC
Guide de biodiversité du Québec

## Suppositions 
- Les données sont produites à partir du blogue Évologie

## Structure : 
./scripts/control.R : Fait originallement pour controler des paramètres d'affichage (nb colonnes) et faire des tests pour la représentation des données
./scripts/guide_regs.qmd : construit le guide en `.html`

## Erreurs connues

Si l'erreur : "Error: pandoc document conversion failed with error 9", c'est qu'il y a trop de photos

Les espaces dans le code HTML sont importantes. 