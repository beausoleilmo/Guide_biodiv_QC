## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Définition de fonction supplémentaire pour exécuter les scripts
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-27
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Extraire les informations des espèces sur
#       d'iNaturalist (liens photos) pour le guide

## ____________####
## Prépare l'environnement --------

# # Pour le Québec au complet
# source(
#   file = file.path(
#     "scripts/partie_2/7.selection_especes.R"
#   )
# )
#
# # Pour chaque région
# source(
#   file = file.path(
#     "scripts/partie_2/7.selection_especes_region.R"
#   )
# )

# ===========================
# inclure les données de wild species canada?
# Importer les noms d'espèces en français
# --> scripts/partie_2/espece_match_gbif.R
esp_sc_gbif <- read.csv(
  file = file.path(
    "output",
    "esp_noms_gbif.csv"
  )
)
# ===========================

# Obtient TOUTES les espèces (13799).
# BEAUCOUP trop de données à passer à l'API d'iNaturalist
especes_tout_gbif <- gbif_prep_type_fr_h3 |>
  count(
    scientificName,
    nom_sci,
    Type_FR,
    kingdom,
    class,
    order,
    family,
    nom_fr,
    nom_en
  ) |>
  collect() |>
  mutate(sp2w = stringr::word(scientificName, 1, 2))

# Distribution du nombre d'observation par espèce
# dev.off() # Pour reset après les treemaps
# hist(especes_tout_gbif |> filter(n>1) |> pull(n))

# Explore combien d'espèce reste après filtre
especes_tout_gbif |>
  filter(n > 200) |>
  distinct(
    scientificName,
    nom_sci,
    Type_FR,
    kingdom,
    class,
    order,
    family,
    nom_fr,
    nom_en
  )


fichier_sortie_noms <- file.path(
  "output/sp_list_fil_iNat.csv"
)

if (file.exists(fichier_sortie_noms)) {
  ## importer données si elles existent --------
  sp_nom_exist <- read.csv2(
    file = fichier_sortie_noms
  )

  sp_deja_fait <- sp_nom_exist |> pull(nom_sp)
  # Filtrer les noms d'espèces
  # select_sp_tout_reg vient de 7.selection_especes_region.R
  sp_list <- select_sp_tout_reg |>
    # Retirer les espèces déjà fait
    filter(!(species %in% sp_deja_fait)) |>
    dplyr::distinct(species)
} else {
  # Liste d'espèces prêt pour le guide
  sp_list <- # Espèces unique
    select_sp_tout_reg |>
    dplyr::distinct(species)
}

message(sprintf("Nombre de lignes %s", nrow(sp_list)))

### Extraire données d'espèces (ID) iNaturalist ----------------------------------
if (nrow(sp_list) == 0) {
  warning("Pas de ligne à extraire de l'information!")
  sp_list_fil <- select_sp_tout_reg
} else {
  sp_rec <- NULL
  max_it <- nrow(sp_list)
  attendre_i <- 10
  nb_requete <- 10
  tail(sp_rec)

  message("Extraire ID espèces iNaturalist")
  # Si erreur, attrapé par TryCatch!
  for (sp_idx in 1:max_it) {
    message(
      sprintf(
        "%s/%02d (%02d %%)",
        formatC(sp_idx, width = 3, flag = " "),
        max_it,
        round(sp_idx / max_it * 100, 0)
      )
    )
    # Tous les 'nb_requete' requêtes, attendre 'attendre_i' secondes
    # Pour ne pas trop en demander au serveur de iNaturalist
    if (sp_idx %% nb_requete == 0) {
      message(sprintf(
        fmt = "Terminé %s. Attendre %s s.",
        sp_idx,
        attendre_i
      ))
      Sys.sleep(time = attendre_i)
    }
    # Nom de l'espèce
    nom_sp <- sp_list[sp_idx, ] |> pull(species)
    # Obtenir information taxon de iNaturalist voir ?getTaxonInfo
    # sptmp = getTaxonInfo(taxon = nom_sp, wait = attendre_i)
    sptmp <- evolovr::get_taxon_info(
      nom_latin = nom_sp,
      attend = attendre_i
    )

    sp_rec <- dplyr::bind_rows(
      sp_rec,
      data.frame(nom_sp, as.data.frame(sptmp))
    )
  } # Fin de la boucle "sp_idx"

  # Certaines colonnes sont des data.frames.
  # Il faut les 'défaire' avec unnest. Il
  sp_rec_out <- sp_rec |>
    dplyr::select(-c(ancestor_ids)) |>
    # Toutes ces colonnes doivent dénichées
    unnest_wider(
      col = c(
        default_photo,
        flag_counts,
        conservation_status
      ),
      names_sep = "_"
    ) |>
    unnest_wider(col = c(default_photo_original_dimensions), names_sep = "_") |>
    unnest_wider(default_photo_flags, names_sep = "_") |>
    dplyr::select(-c(default_photo_flags_1))

  if (file.exists(fichier_sortie_noms)) {
    # ajouter les nouvelles données à celles existantes
    sp_rec_out <- sp_nom_exist |>
      bind_rows(sp_rec_out)
  }

  # Ajoute l'ID d'espèce
  # Pas le même que ID D'OBSERVATION!
  sp_list_fil <- select_sp_tout_reg |>
    dplyr::left_join(
      y = sp_rec_out,
      by = dplyr::join_by(
        species == nom_sp
      ),
      relationship = "many-to-many"
    )

  ## ____________####
  message("Exportation données 'sp_list_fil_iNat.csv'")
  ## Exporter données --------
  readr::write_excel_csv2(
    x = sp_rec_out,
    file = file.path(
      "output/sp_list_fil_iNat.csv"
    )
  )
}
