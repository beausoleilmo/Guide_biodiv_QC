## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Définition de fonction supplémentaire pour exécuter les scripts
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-28
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Extraire NOMS en français des espèces provenant d'iNaturalist pour le guide
#   --> À FAIRE : étape manuelle d'ajouter les espèces du fichier sp_nm_df_MOD2.csv -> sp_nm_df_MOD.csv

## ____________####
## Prépare l'environnement --------

# Pour chaque région
# Requis : 5.1.prep_data.R
source(
  file = file.path(
    "scripts/7.selection_especes_region.R"
  )
)
# données 8.1, regarde tous les noms français manquant
# NOTE : le filtre de >200 est pour SIMULER approximativement ce que
# le script 7.selection_especes_region.R fait. Il sélectionne un PETIT
# nombre d'espèce pour en extraire le nom français et éviter de
# surcharger l'API de iNaturalist. En réalité, le guide ne contiendra
# pas toutes les espèces : juste un nombre qui a du sens pour construire la
# page du guide (pas trop d'espèce pour ne pas avoir trop de données à
# charger pour le guide)
#

# NOTE : especes_tout_gbif vient de 8.1.iNaturalist_...R
# especes_tout_gbif |>
#   filter(n > 200) |>
#   select(scientificName, nom_sci, speciesKey, nom_fr) |>
#   filter(is.na(nom_fr))

# Lire la liste d'espèces et des liens vers les photos iNaturalist
# obtenue avec getTaxonInfo (evolovr::get_taxon_info())
# Voir script 8.1
sp_liste_inat_photos_liens <- read.csv2(
  file = file.path(
    "output",
    "sp_list_fil_iNat.csv"
  )
)

# ===========================
# inclure les données de wild species canada?
# Importer les noms d'espèces en français
# esp_sc_gbif <- read.csv(
#   file = "output/esp_noms_gbif.csv"
# )
out_liste <- "output/biodiv/liste_especes"

# Lecture des données de la liste des espèces combinées
esp_sc_gbif <- arrow::read_parquet(
  file = file.path(
    out_liste,
    "esp_noms_gbif_maj.parquet"
  )
)

esp_sc_gbif |>
  count(rank)

# Compte le nombre d'espèces où le nom français est NA
# Utilisé pour filtrer les espèces à ne
esp_filt <- esp_sc_gbif |>
  filter(
    rank == "SPECIES",
    is.na(nom_commun_en_francais)
  ) |>
  dplyr::select(
    scientificName,
    canonicalName,
    nom_commun_en_francais,
    Type_FR
  )


esp_filt |> count(Type_FR)
nrow(esp_filt)
nrow(sp_liste_inat_photos_liens)

esp_filt |>
  filter(canonicalName %in% sp_liste_inat_photos_liens$nom_sp)

# ===========================

### Téléchargement des images 'Typiques' de iNaturalist ---------------------

# Fichier des noms d'espèces avec noms français provenant de iNaturalist
# L'exportation de ce fichier s'est fait en plusieurs étapes (i.e., plusieurs
# itération; au début, seulement quelques animaux, mais maintenant inclusion
# des plantes et des champignons; au lieu de toujours rouler le script à
# nouveau, il était plus judicieux de stocker les informations déjà récoltés
# et d'ajouter les nouvelles espèces au besoin)
# À chaque fois que de nouvelles espèces sont ajoutées,
# on réexporte le fichier avec les nouvelles espèces.
# NOTE : des modifications MANUELLES ont été faites pour choisir quelques
# noms français plus approprié pour le Québec.
fichier_sp_nom <- file.path(
  "output",
  "sp_nm_df_MOD.csv"
)

# Retirer les espèces dupliquées
# if (file.exists(fichier_sp_nom)) {
#   message("Retirer les espèces dupliquées")
#   sp_nm_df_mod <- read.csv(file = fichier_sp_nom)
#   sp_nm_df_mod_distinct_sp <- sp_nm_df_mod |>
#     distinct(species, .keep_all = TRUE)
#   # Exporter les données sans les espèces dupliquées
#   write.csv(
#     x = sp_nm_df_mod_distinct_sp,
#     file = file.path(
#       "output",
#       "sp_nm_df_MOD_deduplicate.csv"
#     ),
#     row.names = FALSE
#   )
# }
#

# Si le fichier existe exécuter
if (file.exists(fichier_sp_nom)) {
  message("Filtrer les espèces pour extraire ce qui manque")

  # Importer le fichier des noms d'espèces extrait de iNaturalist
  sp_nm_df_mod <- read.csv(file = fichier_sp_nom)

  ## Vérification si espèces à ajouter
  ## Les espèces à faire sont celles qui sont dans le tableau  sp_liste_inat_photos_liens
  ## provenant du scrip 8.1. Si les espèces ont déjà été cherchées sur
  ## iNaturalist et exporté dans sp_nm_df_mod, on les retire de sp_liste_inat_photos_liens
  ## pour ne pas exécuter les espèces déjà exportés. Cela assume qu'il n'y a
  ## pas de mise à jour pertinantes (alors qu'il pourrait en avoir sur
  ## iNaturalist). L'idée ici est de limiter les requêtes envoyées sur l'API
  ## de iNaturalist.
  ## Il se peut que TOUTES les espèces aient été déjà extraitent. Dans ce cas,
  ## le filtre va retourner 0 ligne.
  sp_tab_a_faire <- sp_liste_inat_photos_liens |>
    # Retirer les espèces déjà extraitent
    filter(!(nom_sp %in% sp_nm_df_mod$species)) |>
    distinct()

  # Test s'il reste des rangées
  if (nrow(sp_tab_a_faire) == 0) {
    message(
      "/!\\ Pu de données dans le tableau... bravo Sylvain! 
Le filtre a tout enlevé!"
    )
  }
} else {
  # Si le fichier n'existe pas, il faut exécuter pour TOUTES les espèces
  message("Utiliser sp_liste_inat_photos_liens")
  sp_tab_a_faire <- sp_liste_inat_photos_liens
}


# Liste des noms français de noms d'espèces provenant d'iNaturalist
sp_nm_df <- NULL
attente_i <- 10
nb_requete <- 10 # Chaque nb_requete requête, attendre attente_i secondes

for (sp_nm_idx in 1:nrow(sp_tab_a_faire)) {
  # S'il n'y a plus de données après le filtre, 'break' la boucle for.
  if (nrow(sp_tab_a_faire) == 0) {
    message(
      "Pu de données dans le tableau... bravo Sylvain! 
       Je peux rien faire... 
       donne moi des espèces en latinnn 
       Siouplaittt!
       Oueilldont!"
    )
    break
  }
  message(
    sprintf(
      "%03d/%03d (%02d %%)",
      sp_nm_idx,
      nrow(sp_tab_a_faire),
      round(sp_nm_idx / nrow(sp_tab_a_faire) * 100, 0)
    )
  )

  sp_nm <- sp_tab_a_faire$nom_sp[sp_nm_idx]

  # Attente pour ne pas surcharger le serveur
  Sys.sleep(time = 0.1)

  # Fonction personnelle voir ?readTaxonFR
  fr_nm <- evolovr::get_try_taxon_name(
    taxon_id = sp_tab_a_faire$id[sp_nm_idx],
    lang = "french",
    # Si problème dans CETTE fonction, va attendre 'attente_i'
    attend = attente_i
  )
  #readTaxonFR(
  # taxon = sp_tab_a_faire$id[sp_nm_idx],
  # wait = attente_i)

  # Corriger la case des noms d'organismes ---
  # -- Mettre la première lettre en majuscule
  # -- Pattern: Find a lowercase letter ([a-z]) that is preceded by
  # -- either the start of the string (^) OR a semicolon (;)
  fr_nm <- gsub(
    x = fr_nm,
    pattern = "(^|;)([a-z])",
    replacement = "\\1\\U\\2",
    perl = TRUE
  )

  # Ajouter l'espèces recherché et le résultat
  sp_nm_df <- rbind(
    sp_nm_df,
    data.frame(
      species = sp_nm,
      fr_nm = fr_nm
    )
  )

  # Tous les 'nb_requete' requêtes, attendre 'attente_i' secondes
  # Pour ne pas trop en demander au serveur de iNaturalist
  if (sp_nm_idx %% nb_requete == 0) {
    message(sprintf(
      fmt = "Fait %s. Attend %s s",
      sp_nm_idx,
      attente_i
    ))
    Sys.sleep(time = attente_i)
  }
}

# Si les données sont non NULL, exporter le CSV de la recherche de l'API
# d'iNaturalist
if (!is.null(sp_nm_df)) {
  # Exporter le tableau des noms Latin + noms français d'iNaturalist
  readr::write_excel_csv2(
    x = sp_nm_df,
    file = file.path(
      "output/sp_nm_df.csv"
    )
  )
}

# =======================++++=========
# =======================++++=========
# =======================++++=========
message("Lire les données VASCAN")
vascanqc <- readr::read_delim(
  file = file.path(
    "data/raw/biodiv/listes_especes/",
    "vascan_qc_2026_06_20.txt"
  ),
  delim = "\t",
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  mutate(
    vernacular_fr = gsub(
      x = vernacular_fr,
      pattern = "(^)([a-z])",
      replacement = "\\1\\U\\2",
      perl = TRUE
    )
  ) |>
  dplyr::select(scientific_name, vernacular_fr)

mycoliste <- readxl::read_xlsx(
  path = "data/raw/biodiv/listes_especes/Mycoliste.xlsx",
  skip = 3
) |>
  janitor::clean_names()
# =======================++++=========
# =======================++++=========
# =======================++++=========
# =======================++++=========

## Objectif :
## Cette partie RECONSTRUIT le tableau des espèces au complet
## pour être remis dans le tableau `sp_nm_df`.
## Donc, combinaison de tous les tableaux pour remplacer complètement.

# Si le fichier n'existe pas, ou si sp_nm_df == NULL ne pas exécuter
if (file.exists(fichier_sp_nom) & !is.null(sp_nm_df)) {
  message(
    "Exporter les données (sp_nm_df_MOD2.csv) et 
mettre à jour 'manuellement' sp_nm_df_MOD.csv choisir les noms du Québec"
  )

  # Combine les données iNaturalist, VASCAN, mycoliste, GBIF.
  # Tableau de données des noms français.
  # NOTE : tenter d'utiliser `info <- rcol::col_usage_info(id = id)`
  # À partir des noms de iNaturalist (species, fr_nm), compléter pour avoir
  # les noms français du Québec avec des listes d'espèces contenant des noms
  # Français
  sp_nm_qc <- sp_nm_df |>
    # Ajouter une colonne pour connaître l'origine des données
    dplyr::mutate(origine_liste = "iNat") |>
    # ---------------------------------------------------------
    # Ajouter à la liste existante (voir fichier_sp_nom)
    bind_rows(
      sp_nm_df_mod |> dplyr::mutate(origine_liste = "old_sp_nm_df_mod")
    ) |>
    # Ajout de la colonne de nom d'espèces en français (Québec), s'il n'y a
    # qu'un seul nom
    mutate(
      # Création de nom_commun_qc : s'il y a plusieurs noms d'espèces, mette ""
      nom_commun_qc = ifelse(
        test = grepl(pattern = ';', x = fr_nm),
        yes = "",
        no = fr_nm
      ),
      # Si pas de nom d'espèce, on doit le trouver à la main (note = 1)
      find.tect = ifelse(
        test = nom_commun_qc == "" | is.na(nom_commun_qc),
        yes = 1,
        no = 0
      )
    ) |>
    # Ajout des données vascan ------------------------
    left_join(
      y = vascanqc,
      by = join_by(species == scientific_name)
    ) |>
    # combiner les noms français si le nom vernaculaire se retrouve dans
    # le tableau VASCAN
    mutate(
      nom_commun_qc = coalesce(na_if(nom_commun_qc, ""), vernacular_fr)
    ) |> # Retirer les colonnes coalesce
    dplyr::select(
      -c(vernacular_fr)
    ) |>
    # Noms standardisés ----------------------------------------
    # Voir script 8.1.iNaturalist_...
    # Ajout des données avec les noms préparé GBIF, puis les noms français
    # au données de noms vernaculaire français.
    left_join(
      y = especes_tout_gbif |>
        dplyr::select(nom_sci, nom_fr, speciesKey),
      by = join_by(species == nom_sci)
    ) |>
    mutate(
      nom_commun_qc = coalesce(na_if(nom_commun_qc, ""), nom_fr)
    ) |>
    dplyr::select(-nom_fr) |>
    # Ajoute le nom des champginons mycoliste ---------------------------
    left_join(
      y = mycoliste |>
        select(nom_latin, nom_francais),
      by = join_by(species == nom_latin)
    ) |>
    # Combiner les noms français si le nom vernaculaire est dans mycoliste
    mutate(nom_commun_qc = dplyr::coalesce(nom_commun_qc, nom_francais)) |>
    select(-nom_francais) |>
    arrange(find.tect) |>
    mutate(
      is_duplicate = duplicated(species) | duplicated(species, fromLast = TRUE)
    )

  # Trouver les espèces avec des noms dupliqué
  especes_dupliquees <- sp_nm_qc |>
    # distinct(species) |>
    group_by(species) |>
    count() |>
    filter(n > 1)

  sp_filt_complet <- sp_nm_qc |>
    # pour ordonner les résultats priorisant les valeurs de nom_commun_qc
    # qui ne sont pas NA.
    dplyr::arrange(species, is.na(nom_commun_qc)) |>
    # distinct garde la première instance des donnée
    dplyr::distinct(species, .keep_all = TRUE) |>
    # filter(!is.na(nom_commun_qc)) |>
    dplyr::select(-is_duplicate) |>
    arrange(!is.na(nom_commun_qc), is.na(fr_nm)) |>
    # Formation des noms français automatiquement,
    # NOTE : si le nom français n'est pas trouvé, mettre
    # le nom latin entre paranthèse...
    mutate(
      nom_commun_qc = if_else(
        condition = is.na(fr_nm),
        # Nom latin entre ( )
        true = paste0("(", species, ")"),
        # Garder le nom commun
        false = nom_commun_qc
      ),
      # Correction : si les noms d'espèces (latin) sont les même que les
      # noms d'espèces communes, mettre le latin entre ( )
      nom_commun_qc = if_else(
        condition = species == nom_commun_qc,
        true = paste0("(", species, ")"),
        false =
      ),
      species_sep = stringr::str_split_i(fr_nm, ";", 1),
      nom_commun_qc = coalesce(na_if(nom_commun_qc, ""), species_sep)
    )

  nrow(sp_nm_qc)
  nrow(sp_filt_complet)
  # Exporter cette nouvelle liste préremplie avec les noms d'espèce
  # Par contre, certains noms d'espèces sont encore NA ou "". Il faut
  # donc compléter à la main
  readr::write_excel_csv(
    x = sp_filt_complet,
    file = file.path(
      "output",
      # à modifier manuellement et remplacer "sp_nm_df_MOD.csv"
      # (optionel) Noter les noms cherché à la main
      "sp_nm_df_MOD2.csv"
      # -> Champignon https://www.mycoquebec.org
      # -> Lichen https://lichens.quebec/especes/
      # -> Bryophytes https://www.societequebecoisedebryologie.org/Bryoquel_presentation.html
      # -> Tout https://explorer.natureserve.org
      # -> Algues : https://fourchettebleue.ca/produits/main-de-mer-palmee/
      # -> Poissons : https://www.fishbase.se/summary/Upeneus_parvus.html
    ),
    quote = "needed"
  )
}

## Exploration :
## NOTE : Est-ce possible de prendre le nom des espèces ici et de les matcher
## avec les noms de GBIF pour voir si notre tableau d'espèces avec nom
## communs (voir espece_match_gbif.R) et ansi compléter les noms français
## manquants
##
## NOTE : après inspection, ne semble pas aider beaucoup...
##
# sp_filt_complet |> head()
#
# # 130 sec
# tictoc::tic("Extraction de la taxonomie GBIF")
# # Utilisation version 3.9.0
# gbif_matches_inat_sp <- rgbif::name_backbone_checklist(
#   name_data = sp_filt_complet |> select(scientificName = species),
#   verbose = TRUE # Montre les alternatifs
# )
# tictoc::toc()
#
# nom_gbif_haute_confiance =  gbif_matches_inat_sp |>
#   dplyr::filter(
#     # Si le speciesKey est NA, c'est que ce le niveau espèce n'est pas atteint
#     !is.na(speciesKey),
#     # filtre selon la confiance pour retirer ce qui est vraiment pas précis
#     # comme nom. à 50%, il y avait beaucoup de faux positifs
#     confidence >= 80,
#     # Retirer les noms alternatifs
#     # !is_alternative,
#     rank %in% c("FROM", "SPECIES", "SUBSPECIES", "SUBVARIETY", "VARIETY")
#   ) |>
#   dplyr::mutate(
#     # Vérification que les noms canoniques sont les mêmes que les noms verbatim
#     check_canonName_verbatimName = canonicalName == verbatim_name
#   ) |>
#   group_by(speciesKey) |>
#   # si le nombre est plus que 1 dans le groupe, c'est un duplicat
#   mutate(is_duplicate = n() > 1) |>
#   # 1. Arrange by priorities within each speciesKey group
#   arrange(
#     # Priority 1: Rows where BOTH are TRUE come first (descending puts TRUE before FALSE)
#     desc(is_duplicate & check_canonName_verbatimName),
#     # Priority 2: If none match Priority 1, sort by highest confidence
#     desc(confidence),
#     .by_group = TRUE
#   ) |>
#   # 2. Pick the top row for each speciesKey
#   slice(1) |>
#   mutate(is_duplicate_2 = n() > 1) |>
#   ungroup()
#
# sp_inat_gbif = sp_filt_complet  |>
#   dplyr::select(-speciesKey) |>
#   left_join(
#     y = nom_gbif_haute_confiance,
#     by = join_by(
#       species == verbatim_name
#     )
#   )  |>
#     left_join(
#       y = especes_tout_gbif |>
#         dplyr::select(nom_sci, nom_fr, speciesKey),
#       by = join_by(speciesKey)
#     )  |>
#   distinct(species, .keep_all = TRUE)
#
#
#   readr::write_excel_csv(
#     x = sp_inat_gbif,
#     file = file.path(
#       "output",
#       "sp_nm_df_comparaison_inat_gbif.csv"
#     ),
#     quote = "needed"
#   )
######################

# NOTE : Cette section était pour faciliter l'écriture à la main des noms
# d'espèces en français à ajouter. l'approche maintenant est
#  --> de prendre le premier nom français existant si plusieurs existent
#  --> de prendre le nom latin et le mettre entre ( ) pour ne pas avoir
#      à chercher plein de noms supplémentaires.

# Si le fichier n'existe pas ou que sp_nm_df est NULL ne pas exécuter

if (file.exists(fichier_sp_nom) & !is.null(sp_nm_df)) {
  message("Filtrer les espèces pour extraire ce qui manque")
  # Importer le fichier
  # sp_nm_df_mod <- read.csv(fichier_sp_nom)

  # Retrait des espèces déjà présentes dans le fichier original (sp_nm_df_mod)
  # pour éviter les duplicats
  sp_a_ajouter <- sp_filt_complet |>
    filter(is.na(nom_commun_qc))

  # Corrige les NA dans la colonne nom_commun_qc
  sp_a_ajouter_coal <- sp_a_ajouter |>
    mutate(
      nom_commun_qc = na_if(nom_commun_qc, "NA"),
      nom_commun_qc = na_if(nom_commun_qc, "<NA>")
    )

  # Exporter les données simplifiées
  readr::write_excel_csv(
    x = sp_a_ajouter_coal,
    file = file.path(
      "output",
      # à modifier manuellement et remplacer "sp_nm_df_MOD.csv"
      # (optionel) Noter les noms cherché à la main
      "sp_nm_df_MOD3_simple.csv"
    ),
    quote = "needed"
  )
}


atomiser_colonne_nom <- FALSE
if (atomiser_colonne_nom) {
  ## --> finalement, je ne pense pas fonctionner comme cela.
  ## --> Je crois que de mettre dans des fichiers MOD et ajouter
  ## manuellement est le plus rapide
  ## --> sinon, connecter avec les liens de recherche de nom
  ## d'espèces (voir si API existe)
  # Tableau de noms d'espèces :
  #  séparation de la colonne fr_nm pour que chaque élément séparé par ';'
  # se retrouve sur une nouvelle ligne.
  sp_nm_df_long <- sp_nm_df |>
    separate_longer_delim(
      cols = fr_nm,
      delim = ";"
    ) |>
    # groupe par espèce, mais pas par nom fr (comptte doublons)
    group_by(species) |>
    mutate(
      # compte le nombre de duplicat de noms d'espèces
      n = n(),
      # Préférence d'utilisation d'un nom
      preference = ifelse(
        # Si le nom est 1 seule fois présent,
        test = n == 1,
        # mettre 1, sinon 0 et doit être choisi manuellement
        yes = 1,
        no = 0
      )
    ) |>
    ungroup()
}

# Si null, joindre avec les données déjà exportés auparavant "sp_nm_df_mod"
# S ile jeu de données sp_nm_df n'est pas NULL, joindre les données déjà
# exportés et les données extraite avec les données de liens vers les photos
if (!is.null(sp_nm_df)) {
  nom_esp_qc_taxon_info <- sp_nm_df |>
    bind_rows(sp_nm_df_mod) |>
    left_join(
      y = sp_liste_inat_photos_liens,
      by = join_by(species == nom_sp)
    )
} else {
  # Si les données sp_nm_df sont NULL, joindre les données de noms français
  # avec les données de liens avec photos
  nom_esp_qc_taxon_info <- sp_nm_df_mod |>
    left_join(
      y = sp_liste_inat_photos_liens,
      by = join_by(species == nom_sp)
    )
}

message(
  "Création objet sp_list_nom_fr : liste d'espèce provenant de GBIF
avec les informations provenant d'iNaturalist (liens et nom français)"
)
# Joindre la liste d'espèces et les noms français des espèces
# sp_list_nom_fr = select_sp_tout |> # Ancienne version sans les régions
# Si on prend TOUTES les données GBIF, il y aura BEAUCOUP d'espèces SANS liens iNat puisque nous avons fait une SÉLECTION des espèces
# sp_list_nom_fr <- gbif_prep_type_fr_h3 |>
# combiner la liste SÉLECTIONNÉE d'espèces par région et la liste des
# espèces extrait d'iNaturalist
sp_list_nom_fr <- select_sp_tout_reg |>
  # Obtenir la taxonomie
  distinct(
    kingdom,
    phylum,
    class,
    `order`,
    family,
    species,
    scientificName,
    speciesKey,
    nom_fr,
    nom_en,
    Type_FR
  ) |>
  # Mettre les données en mémoire
  collect() |>
  # Pas vraiemnt besoin de cela
  # left_join(
  #   y = sp_liste_inat_photos_liens,
  #   by =  join_by(species == nom_sp),
  #   relationship = "many-to-many"
  # ) |>
  # Noms en français! (par contre, des noms multiples sont aussi intégrés)
  left_join(
    y = nom_esp_qc_taxon_info |> select(-speciesKey),
    by = join_by(species),
    relationship = "many-to-many"
  ) |>
  dplyr::distinct(species, .keep_all = TRUE)
colnames(nom_esp_qc_taxon_info)
colnames(sp_list_nom_fr)
message(sprintf(
  "Nombre de lignes de la liste des noms et liens iNaturalist : %s",
  nrow(sp_list_nom_fr)
))

## ____________####
## Exporter données --------
message("Exportation de sp_list_fil_iNat_nomFR.csv")
readr::write_excel_csv2(
  x = sp_list_nom_fr,
  file = file.path(
    "output",
    "sp_list_fil_iNat_nomFR.csv"
  )
)
#
# readr::write_excel_csv2(
#   x = sp_nm_df_long,
#   file = file.path(
#     "output/sp_nm_df_long.csv"
#   )
# )
