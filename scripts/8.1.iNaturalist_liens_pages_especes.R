## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Définition de fonction supplémentaire pour exécuter les scripts
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-27
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Extraire les informations des espèces
#       d'iNaturalist (liens photos) pour le guide
# NOTE :
# --> Le guide n'inclut pas TOUTES les espèces.
# Une sélection d'espèces est faite avec
# 7.selection_especes.R permettant de garder un nombre d'espèce
# par groupe taxonomique (e.g., pour les oiseaux, ont veut pas juste des
# espèces de passereaux, on veut aussi les autres familles d'oiseaux,
# donc la sélection d'espèces. La sélection d'espèces
# se fait au niveau taxonomique de la famille. Pour d'autres groupe
# d'organismes, la sélection se fait selon d'autres niveaux taxonomiques)

message("Création de l'objet `sp_list_fil` : jointure des données iNaturalist")

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
# esp_sc_gbif <- read.csv(
#   file = file.path(
#     "output",
#     "esp_noms_gbif.csv"
#   )
# )

# Lire les données d'espèces de GBIF
# out_liste <- "output/biodiv/liste_especes"
# esp_sc_gbif <- arrow::read_parquet(
#   file = file.path(
#     out_liste,
#     "esp_noms_gbif_maj.parquet"
#   )
# )
#
# ===========================

# Obtient TOUTES les espèces (31427).
# BEAUCOUP trop de données à passer à l'API d'iNaturalist
especes_tout_gbif <- gbif_prep_type_fr_h3 |>
  dplyr::count(
    scientificName,
    nom_sci,
    speciesKey,
    Type_FR,
    kingdom,
    phylum,
    class,
    order,
    family,
    genus,
    nom_fr,
    nom_en
  ) |>
  dplyr::collect() |>
  dplyr::mutate(sp2w = stringr::word(scientificName, 1, 2))

# Distribution du nombre d'observation par espèce
# dev.off() # Pour reset après les treemaps
# hist(especes_tout_gbif |> filter(n>1) |> pull(n))

# Explore combien d'espèce reste après filtre
especes_tout_gbif |>
  dplyr::filter(n > 200) |>
  dplyr::distinct(
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
# ====================================================

# NOTE : La création du fichier de données iNaturalist (pour extraire les
# données de références pour les espèces sur iNaturalist) est fait en
# plusieurs fois selon l'addition de nouvelles espèces. Donc, un fichier
# des données collectées sur iNaturalist est exporté une fois puis mis
# à jour à chaque fois que le code tourne. Les espèces déjà extraitent sont
# exclus de la requête envoyé à iNaturalist.
#

# Nom du fichier à créer
fichier_sortie_noms <- file.path(
  "output",
  "sp_list_fil_iNat.csv"
)

# Si le fichier existe
if (file.exists(fichier_sortie_noms)) {
  message(sprintf(
    "Fichier existe. 
Lecture de : %s",
    fichier_sortie_noms
  ))

  ## importer données si elles existent --------
  sp_nom_exist <- read.csv2(
    file = fichier_sortie_noms
  ) |>
    arrange(nom_sp)

  # Obtenir la liste des noms
  sp_deja_fait <- sp_nom_exist |>
    dplyr::pull(nom_sp)
  # Filtrer les noms d'espèces
  # --> 7.selection_especes_region.R (select_sp_tout_reg) :
  sp_list <- select_sp_tout_reg |>
    # Retire les espèces dupliquées (puisque même espèce peut se
    # retrouver dans différentes régions)
    dplyr::distinct(species) |>
    # Retirer les espèces déjà fait
    dplyr::filter(!(species %in% sp_deja_fait))
} else {
  # Liste d'espèces prêt pour le guide
  sp_list <- # Espèces unique
    select_sp_tout_reg |>
    dplyr::distinct(species)
}

# Messages du nombre de lignes dans chaque tableau
message(
  sprintf(
    "
Nombre de lignes données déjà extraites d'iNat : %s
Nombre de lignes à extraire d'iNaturalist : %s
Nomvre de lignes d'espèces différentes sélectionnés : %s",
    nrow(sp_nom_exist),
    nrow(sp_list),
    nrow(
      select_sp_tout_reg |>
        dplyr::distinct(species)
    )
  )
)

head(select_sp_tout_reg)

### Extraire données d'espèces (ID) iNaturalist ----------------------------------
if (nrow(sp_list) == 0) {
  message("Pas de ligne à extraire de l'information!")
  # TODO : pas utilisé comme tableau. Retirer?
  message(
    "Création de l'objet sp_list_fil: 
jointure du tableau des espèces sélectionnées et des espèces d'iNaturalist"
  )
  # Garder le jeu de données
  sp_list_fil <- select_sp_tout_reg |>
    dplyr::left_join(
      y = sp_nom_exist |> distinct(nom_sp, .keep_all = TRUE),
      by = dplyr::join_by(
        species == nom_sp
      ),
      relationship = "many-to-many"
    )
  # la jointure devrait fournir le même nombre de point de données
  # après la jointure
  message(sprintf(
    "Nombre de lignes après jointure : %s 
Nombre de lignes originales :     %s",
    nrow(sp_list_fil),
    nrow(select_sp_tout_reg)
  ))
} else {
  message("Extraction des espèces manquantes")
  # Paramètres
  sp_rec <- NULL
  max_it <- nrow(sp_list)
  attendre_i <- 10
  nb_requete <- 10

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
    # Obtenir information taxon de iNaturalist voir evolovr (avant ?getTaxonInfo)
    # sptmp = getTaxonInfo(taxon = nom_sp, wait = attendre_i)
    sptmp <- evolovr::get_taxon_info(
      nom_latin = nom_sp,
      attend = attendre_i
    )
    # combiner les données
    sp_rec <- dplyr::bind_rows(
      sp_rec,
      data.frame(nom_sp, as.data.frame(sptmp))
    )
  } # Fin de la boucle "sp_idx"

  # Post process les données extraites de iNaturalist
  # NOTE : Certaines colonnes sont des data.frames.
  # Il faut les 'défaire' avec unnest.
  sp_rec_out <- sp_rec |>
    dplyr::select(-c(ancestor_ids)) |>
    # Toutes ces colonnes doivent dénichées (unnest)
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
    message("Joindre les données iNaturalist aux données déjà présentes")
    # ajouter les nouvelles données à celles existantes
    sp_rec_out <- sp_nom_exist |>
      bind_rows(sp_rec_out)
  }

  # TODO : chercher les noms d'espèces avec taxonomie de GBIF? Bonne idée?
  # message("Extraire les noms avec l'API de GBIF (rgbif)")
  # tictoc::tic("Extraction de la taxonomie GBIF")
  # # Utilisation version 3.9.0
  # df_matches <- rgbif::name_backbone_checklist(
  #   name_data = listes_especes_api_gbif,
  #   verbose = TRUE # Montre les alternatifs
  # )
  # tictoc::toc()

  # Ajoute l'ID d'espèce
  # Pas le même que ID D'OBSERVATION!
  # # NOTE : C'est le nom_sp qui a été utilisé pour faire la recherche d'espèce
  # sur iNaturalist. Donc, jointure basé sur ce nom d'espèce
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
  ## # NOTE : sp_rec_out contient les données exportés auparavant et les
  ## NOUVELLES données trouvées.
  readr::write_excel_csv2(
    x = sp_rec_out,
    file = file.path(
      "output",
      "sp_list_fil_iNat.csv"
    )
  )
}
