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
# NOTE : besoin d'internet pour MAJ des progiciels
source("./scripts/99.pipeline/99.0_prep.R")
# Données GBIF
source("./scripts/99.pipeline/99.1_obtenir_donnees_GBIF.R")

# Préparation des données admin (régions), eBird et GRHQ
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
## Joindre les données GBIF et listes de noms français (en connection duckdb)
source(
  file = file.path(
    "./scripts/5.1.prep_data.R"
  )
)

# Statistiques regroupées taxonomiquement et spatialement
source(
  file = file.path(
    "./scripts/5.2.Exploration_compte_gbif.R"
  )
)

message("--> Carte d'observation dues données GBIF au Québec")
source(
  file = file.path(
    "./scripts/exploration_donnees/carte_observations_GBIF_quebec.R"
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

# Exploration des données sous forme de 'treemaps'
# Permet de voir la distribution des espèces selon:
#  --> Le nombre d'observations
#  --> Le nombre d'espèces dans chaque groupe taxonomique
message("--> Visualisation NB observations")
source(
  file = file.path(
    "./scripts/6.1.graphique_treemap_static.R"
  )
)

# Treemap interactif
message("--> Visualisation NB observations interactive")
source(
  file = file.path(
    "./scripts/6.2.graphique_treemap_interactif.R"
  )
)

# DÉPLIANT!!! -------------------------------------------------------------
message("--> Obtient compte d'espèces par région")
# NOTE : la sélection d'espèce se fait de manière à REPRÉSENTER différents
# groupes taxonomiques.
# Cela permet de réduire le nombre de requête à faire pour iNaturalist
source(
  file = file.path(
    "./scripts/7.selection_especes_region.R"
  )
)

# message("--> Compte d'espèce totales")
# source(
#   file = file.path(
#     incub, "scripts/7.selection_especes.R"
#   ))

# message("--> Extraire le nom des espèces GBIF")
# À partir des listes d'espèces de différentes sources
# Extraire les noms franâis et exporter en parquet
# la liste d'espèces est ensuite passé dans `rgbif::name_backbone_checklist`
# pour avoir des données 'standardisé'.
# NOTE : un filtre sur la 'confiance des données' avec l'API de GBIF fait
# que ce ne sont pas toutes les espèces qui se retrouvent dans les données
# finales.
#
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
# source(file.path("scripts/9.Photos_iNaturalist.R"))

# utils/10.reduce_image_size.sh pour réduire la taille des images.
# Rendre le script exécutable
system2(
  command = "chmod",
  args = c("+x", normalizePath("scripts/utils/10.reduce_image_size.sh"))
)

# Afficher l'aide et description de la fonction
system2(
  command = "bash",
  args = c(
    "scripts/utils/10.reduce_image_size.sh",
    "--help"
  )
)
# Exécution de la réduction de la taille des photos
system2(
  command = "bash",
  args = c(
    "scripts/utils/10.reduce_image_size.sh",
    "output/images/main_page_image",
    "output/images/main_page_image2/"
  )
)
# 99.guide : Exécuter le script qui bâtit le guide
source(file = file.path("./scripts/99.guide/control.R"))
