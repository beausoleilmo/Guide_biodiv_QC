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
  "sf", # Simple feature
  "h3jsr", # Grille H3
  "mapview", # Cartes interactives
  "lwgeom", # Préparation de polygon pour filtre GBIF
  # Graphiques
  "ggplot2", # Graphiques
  "treemap", # Graphiques pour compte des espèces
  "plotly", # Diagrammes interactifs
  # Utilitaires
  "stringr", # Manipulation de chaînes de caractères
  "glue", # Interpolation de chaînes de caractères ("coller" des variables)
  "tictoc", # Minuteur
  "docstring", # Création de documentation directement dans les fonctions R
  "purrr", # Programmation fonctionnelle et manipulation de listes/vecteurs
  # biodiversité
  "rinat",
  "rebird",
  # Lecture données et gestion fichiers
  "duckdb", # Lire fichiers données volumineux
  "duckspatial", # Lire des données spatiales et manipulations facilement
  "arrow", # lire des .parquet en data.frame
  "sfarrow", # Lire des geoparquets
  "DBI", # Gestion des connexions BD
  "rgbif", # Intéragir avec GBIF (API)
  "rinat", # Intéragir avec iNaturalist (API)
  "jsonlite", # Gestion du format JSON
  "zip", # Décompresser des fichiers
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

# message("Charge les fonctions")

# Préparation environnement duckdb
# source(
#   file = file.path(
#     incub,
#     "scripts/0.init",
#     "db_connection_fun.R")
# )

# Fonction pour lire les données
# source(
#   file = file.path(
#     incub,
#     "scripts/0.init",
#     "transforme_gbif_parquet.R")
# )

# Fonction pour joindre les données ADMIN et H3 au parquet
# source(
#   file = file.path(
#     incub,
#     "scripts/0.init",
#     "GBIF_jointure_admin_h3.R")
# )

# Fonction pour algorithme pour proportion de choix d'espèces
# source(
#   file = file.path(
#     incub,
#     "scripts/0.init",
#     "more_prec_decay.R")
# )

# Toutes les fonctions du projet
# source(
#   file = file.path(
#     "~/Github_proj/evologie",
#     "posts/guide_biodiv_qc/2025_05_24_Guide_biodiv_qc",
#     "scripts/00_init",
#     "functions.R"
#   )
# )

message("Fait!")
