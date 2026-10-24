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

## ____________####
## Prépare l'environnement --------

message("Prépare l'environnement du projet")
# Accès au projet
# incub <- file.path("Incubateur/2025_05_24_Guide_biodiv_qc")

# Liste d'espèces avec Type_FR
# source(
#   file.path(
#   "Incubateur/2025_05_24_Guide_biodiv_qc",
#   "scripts/0.1_charge_donnees_sp_nm.R"
# ))
# Initialise les progiciels, scripts et fonctions
source(
  file = file.path(
    "scripts/0.0.init/0.0.init.R"
  )
)

# Charge les modules du pipeline (création des données) et
# établir la résolution pour les calculs
source(
  file = file.path(
    "scripts/0.0.init/0.0.1.parametre_pipeline.R"
  )
)
