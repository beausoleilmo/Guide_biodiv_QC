## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> script de pipeline roule DANS L'ORDRE les différentes
#       manipulations nécessaires.
#

# Préparation de l'environnement, paramètres, résolutions.
source("./scripts/99.pipeline/99.0_prep.R")
# Données GBIF
source("./scripts/99.pipeline/99.1_obtenir_donnees_GBIF.R")

# Préparation des données admin (régions) et eBird
if (FALSE) {
  # Préparation des données
  source("./scripts/99.pipeline/99.1_prep_aux_data.R")
}
# Charge les données auxiliaires (régions et eBird hotspots)
source("./scripts/99.pipeline/99.2_charge_donnees.R")

# NOTE :
# La résolution (res_fine et res_parent) est prise en
# charge dans 99.0_prep.R qui affecte plusieurs scripts.

message("--> Charge les données avec duckdb")
## Ouverture de Connection duckdb
source(
  file = file.path(
    "./scripts/5.1.prep_data.R"
  )
)

message("--> Cartographie H3 et NB observations")
# NOTE : si les données sont aggrégées à l'échelle H3 res = 10 avec
# les municipalités, si on prend une résolution parent, et qu'on
# groupe les résultats par la grille H3 ET les municipalités, on aura des
# doublons de grille H3.
source(
  file = file.path(
    "./scripts/5.3.carte.R"
  )
)

message("--> Visualisation NB observations")
source(
  file = file.path(
    "./scripts/6.1.graphique_treemap_static.R"
  )
)

message("--> Visualisation NB observations interactive")
source(
  file = file.path(
    "./scripts/6.2.graphique_treemap_interactif.R"
  )
)

# DÉPLIANT!!! -------------------------------------------------------------
message("--> Obtient compte d'espèces par région")
source(
  file = file.path(
    "./scripts/7.selection_especes_region.R"
  )
)


# message("--> ")
# source(
#   file = file.path(
#     incub, "scripts/7.selection_especes.R"
#   ))

# message("--> Extraire le nom des espèces GBIF")
# source(
#   file = file.path(
#     incub, "scripts/espece_match_gbif.R"
#   ))

message("--> ")
source(
  file = file.path(
    "./scripts/",
    "8.1.iNaturalist_liens_pages_especes.R"
  )
)


# Étape manuelle pour aller chercher les noms Français de préférence pour le Québec
source(
  file = file.path(
    "./scripts/",
    "8.2.iNaturalist_especes_noms_commun.R"
  )
)

# 9 pour télécharger les nouvelles photos
# utils/10.reduce_image_size.sh pour réduire la taille des images.
# 99.guide
# Aller dans projet Github "Guide_biodiv_Qc"
# --> https://github.com/beausoleilmo/Guide_biodiv_QC
