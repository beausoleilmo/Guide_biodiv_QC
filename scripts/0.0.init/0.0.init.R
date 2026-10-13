## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Charge les progiciels
#   --> Corrige priorité de fonction (?) pour la documentation des fonctions
#   --> Charge CRS pour le projet
#   -->

## ____________####
## Prépare l'environnement --------

# Vérification si pak est installer
# Permet d'installer les progiciels et leur dépendance rapidement
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# Installation de progiciel développer pour le blogue
pak::pak("devtools")
# Installation
# devtools::install_github(
#   repo = "beausoleilmo/evolovr",
#   force = TRUE
#   )
message("Charge evolovr")
# library(evolovr)
devtools::load_all("../evolovr")

# Localement, si le proficiel est cloné de GitHub directement.
# devtools::load_all(path = "../evolovr/")

message("Charge les libraries")
libs_v <- c(
  # Manipulation
  "dplyr", # Manipulation et transformation de données (tableaux)
  "dbplyr", # Traduction du code dplyr en requêtes SQL
  "tidyr", # Nettoyage et restructuration des données (format long/large)
  # Carto
  "sf", # Simple feature : données spatiales
  "terra", # Données spatiales matricielles
  "h3jsr", # Grille H3
  "mapview", # Cartes interactives
  "lwgeom", # Préparation de polygon pour filtre GBIF
  # Graphiques
  "ggplot2", # Graphiques
  "treemap", # Graphiques pour compte des espèces
  "plotly", # Diagrammes interactifs
  "tidyterra", # Graphiques ggplot avec raster
  "scales", # Pour les 'échelles' de ggplot
  # Utilitaires
  "readxl", # Lire des fichiers Excel (SVP à éviter...)
  "writexl", # Exporter un Excel...
  "readr", # Lire des données dans R
  "stringr", # Manipulation de chaînes de caractères
  "glue", # Interpolation de chaînes de caractères ("coller" des variables)
  "tictoc", # Minuteur
  "docstring", # Création de documentation directement dans les fonctions R
  "purrr", # Programmation fonctionnelle et manipulation de listes/vecteurs
  "janitor", # pour nettoryer les noms de colonnes
  "tibble", # Tables dans R
  # biodiversité
  "rgbif", # Intéragir avec GBIF (API)
  "rebird",
  "rcol", # Connection avec le catalogue of life (COL)
  "taxize", # Taxonomie https://www.r-bloggers.com/2011/11/use-case-combining-taxize-and-rgbif/
  "rinat", # Intéragir avec iNaturalist (API)
  # Lecture données et gestion fichiers
  "duckdb", # Lire fichiers données volumineux
  "duckspatial", # Lire des données spatiales et manipulations facilement
  "arrow", # lire des .parquet en data.frame
  "sfarrow", # Lire des geoparquets
  "DBI", # Gestion des connexions BD
  "jsonlite", # Gestion du format JSON
  "zip", # Décompresser des fichiers
  "xml2", # manipualtion xml
  "rvest", # moissonnage
  "httr2" # Envoi de requêtes HTTP et interactions avec des API Web
)
# Install au besoin
missing_libs <- libs_v[!(libs_v %in% installed.packages()[, "Package"])]
if (length(missing_libs) > 0) {
  # brew install apache-arrow
  # install.packages("arrow")
  pak::pak(missing_libs)
}

# Charge toutes les librairies sans message ou sortie
suppressWarnings(
  suppressPackageStartupMessages(
    lapply(libs_v, library, character.only = TRUE)
  )
) |>
  invisible()

# S'assurer que la documentation peut être vue avec ?
`?` <- docstring::`?`

# Charge le CRS du projet
message("Charge : CRS projet")
projetCRS <- 32198

message("Charge les fonctions")

# Fonction pour trouver l'extension des images iNaturalist
source("scripts/0.0.init/fonctions.R")

message("Fait!")
