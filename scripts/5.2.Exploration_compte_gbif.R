## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
# --> Suite du script 5.1.prep_data.R
# --> Application de filtre pour calculer des statistiques spatiales
# TODO : meilleure explication de ce qui se passe ici.

## ____________####
## Prépare l'environnement --------

# Compte le nombre d'observation pour chaque espèce dans les données GBIF
sp_count_names <- gbif_prep_type_fr_h3 |>
  count(
    scientificName,
    speciesKey,
    nom_sci,
    Type_FR,
    kingdom,
    class,
    order,
    family,
    nom_fr,
    nom_en
  ) |>
  arrange(desc(n)) |>
  collect()

# Les Type_FR ne devraient pas avoir de NA!
# type_fr_empty = gbif_prep_type_fr_h3 |>
#   filter(is.na(Type_FR)) |>
#   distinct(kingdom, phylum,  class,  order,  family,  genus, species)

# Vérification que tous les Type_FR sont présents (donc si tout est OK
# le tableau devrait être vide pour les  is.NA(Type_FR)!)
gbif_prep_type_fr_h3 |>
  group_by(kingdom, class, order, family, Type_FR) |>
  filter(is.na(Type_FR), !is.na(class)) |>
  count() |>
  ungroup() |>
  # pivot_longer(
  #   cols = c(class, order, family),  names_to = "Rank", values_to = "value")
  collect()

message("NB observation unique espèce :\ncount_sp")
# NOMBRE *D'OBSERVATION*  POUR CHAQUE ESPÈCES
count_sp <- gbif_prep_type_fr_h3 |>
  group_by(
    kingdom,
    phylum,
    class,
    `order`,
    family,
    genus,
    scientificName,
    nom_sci,
    nom_en,
    nom_fr,
    Type_FR,
    speciesKey
  ) |>
  summarise(
    n = n(),
    .groups = "drop"
  ) |>
  # Matérialiser les résultats
  collect() |>
  # Ordonne les données après
  arrange(
    kingdom,
    phylum,
    class,
    `order`,
    family,
    scientificName,
    speciesKey,
    desc(n)
  ) |>
  # Ajoute la colonne des noms d'espèces binomiaux
  mutate(
    sp2w = scientificName |>
      str_remove_all("\\s*\\([^)]+\\)") |> # Remove parentheticals like (Danaus) or (Linnaeus, 1758)
      word(1, 2)
  ) |>
  # Si nom_fr NA, mettre ''
  mutate(nom_fr_clean = coalesce(nom_fr, "")) |>
  tidyr::unite(
    "label",
    sp2w,
    nom_fr_clean,
    sep = "\n",
    remove = FALSE
  ) |>
  mutate(label = stringr::str_trim(label)) |>
  select(-nom_fr_clean)

## Compte hiérarchique (passe d'espèce à famille)
# du nombre d'espèces unique dans chaque famille
hierarchy_counts <- count_sp |>
  select(-c(n)) |>
  group_by_all() |>
  count() |>
  mutate(
    pa = 1
  ) |>
  ungroup()


message("NB observation unique espèce par région \n--> count_sp_reg")
# NOMBRE *D'OBSERVATION* POUR CHAQUE ESPÈCES PAR RÉGION
count_sp_reg <- gbif_prep_type_fr_h3 |>
  group_by(
    kingdom,
    phylum,
    class,
    `order`,
    family,
    genus,
    scientificName,
    nom_sci,
    nom_en,
    nom_fr,
    Type_FR,
    species,
    speciesKey,
    # RÉGION
    MUS_NM_REG
  ) |>
  summarise(
    n = sum(!is.na(speciesKey), na.rm = TRUE), # Les valeurs manquantes sont retirées en SQL
    .groups = "drop"
  ) |>
  # Correction de classe
  mutate(
    pa = 1
  ) |>
  # Matérialiser les résultats
  collect() |>
  mutate(
    sp2w = scientificName |>
      str_remove_all("\\s*\\([^)]+\\)") |> # Remove parentheticals like (Danaus) or (Linnaeus, 1758)
      word(1, 2)
  ) |>
  # Si nom_fr NA, mettre ''
  mutate(nom_fr_clean = coalesce(nom_fr, "")) |>
  tidyr::unite(
    "label",
    sp2w,
    nom_fr_clean,
    sep = "\n",
    remove = FALSE
  ) |>
  mutate(label = stringr::str_trim(label)) |>
  select(-nom_fr_clean)


### Sommaire des données GBIF selon parent H3 ------

message(
  "Sommaire NB observations par h3_parent des données GBIF (complètes -> familles)"
)
# Sommaire des observation GBIF (compte par groupe)
# La résolution du sommaire est déterminé par
# - la précision taxonomique (e.g., famille)
# - la précision de région (e.g., municipalité)
# - la précision de la grille h3 (à résolution 10, on est plus petit que munici)
# NOTE : puisque nous changeons la résolution H3 (recherche d'un parent),
#        une observation fait dans une Municipalité peut maintenant se retrouver
#        dans 2 ou + polygones administratif "superposés".
#        I.e., une observation fait entre Montréal et Westmount, à résoltuion
#        10, il se pourrait que le point puisse être séparaé entre
#        les régions, mais qu'a résolution 9, le polygone H3 couvre les
#        deux régions ce qui empêche de connaître d'à quelle région
#        administrative le point appartient. C'est la limite d'utiliser
#        une grille standardisé.
#        Solution potentielle : Région avec + d'observations
#        L'hexagone 617761314074787839 contient 1 Charadriiformes Alcidae de
#        Montréal, alors que le reste des observations sont de Longueuil.
#        Dans ce cas, on trouve quelle région à le + d'observations et on
#        combine dans la région dominante.
gbif_prep_query <- gbif_prep_type_fr_h3 |>
  # Trouve la résolution parent au besoin
  # (En fonction de ce qui a été préparé avant)
  mutate(
    h3_parent = h3_cell_to_parent(h3_cell, res_parent),
    # Ajoute la résolution au tableau
    res_parent = res_parent
  ) |>
  # Trouve les régions admin où il y a le plus de H3 pour les données admin
  group_by(
    h3_parent,
    MUS_NM_MUN,
    MUS_NM_MRC,
    MUS_NM_REG
  ) |>
  # Compte le nb d'observation dans chaque région admin
  mutate(
    poids_admin = n()
  ) |>
  ungroup() |>
  # Pour chaque h3_parent on organise les colonnes en fonction du 'poids_admin'
  # donc le maximum d'observation pour une région.
  # Autrement dit, donne un rang aux régions administratives et place
  # celle qui a un + grand poids en premier
  group_by(h3_parent) |>
  mutate(
    rang_admin = row_number(desc(poids_admin))
  ) |>
  # Extraire l'administration gagnante pour chaque ligne du H3
  mutate(
    MUN_DOMINANTE = max(ifelse(rang_admin == 1, MUS_NM_MUN, NA), na.rm = TRUE),
    MRC_DOMINANTE = max(ifelse(rang_admin == 1, MUS_NM_MRC, NA), na.rm = TRUE),
    REG_DOMINANTE = max(ifelse(rang_admin == 1, MUS_NM_REG, NA), na.rm = TRUE),
    # vecteur de différence
    MUN_Diff = MUS_NM_MUN != MUN_DOMINANTE
  ) |>
  ungroup() |>
  # Préparer l'aggrégation finale en utilisant les colonnes DOMINANTES.
  #    Toutes les observations de toutes les municipalités d'un même H3
  #    vont maintenant fusionner sous ces trois colonnes.
  group_by(
    # Précision taxonomique
    Type_FR,
    kingdom,
    phylum,
    class,
    `order`, # SQL a des mots réservés qui doivent être "échappés"
    family, # <- PRÉCISION TAXONOMIQUE
    # Grille h3
    h3_parent, # <- PRÉCISION H3
    res_parent,
    # Référence administrative (en renommant les colonnes)
    MUS_NM_MUN = MUN_DOMINANTE, # <- PRÉCISION ADMIN
    MUS_NM_MRC = MRC_DOMINANTE,
    MUS_NM_REG = REG_DOMINANTE
  ) |>
  # Somme de toutes les observations dans le groupe
  summarise(
    n = n(),
    .groups = "drop"
  ) |>
  # calcul ln des observations
  mutate(
    log_n = log(n + 1),
    # Utilise sql() pour utiliser une fonction spatiale (H3)
    # dans DuckDB pour la colonne h3_parent
    wkt_geom = sql(
      sprintf(
        "ST_AsText(h3_cell_to_boundary_wkt(%s)::GEOMETRY)",
        "h3_parent"
      )
    )
  )


# Sommaire par groupe taxonomique ---------------------------------------------
message(
  "Sommaire des observations des données GBIF (groupe taxonomique kingdom et Type_FR)"
)
# Sommaire selon un groupe taxonomique particulier et une sélection de région
# Règne à sélectionner
KINGDOM_SEL <- "Animalia"
# Type d'organisme
TYPE_FCT_LIST <- c("Oiseaux", "Mammifères")


# Query using dplyr syntax
# Pour toutes les régions
gbif_somme_query <- gbif_prep_query |>
  # Remplacer des noms pour l'affichage
  mutate(
    reformat_taxo = case_when(
      kingdom == "Fungi" ~ "Champignons",
      kingdom == "Plantae" ~ "Plantes",
      phylum == "Arthropoda" ~ "Bobittes",
      class == "Amphibia" ~ "Amphibiens",
      class == "Aves" ~ "Oiseaux",
      class == "Mammalia" ~ "Mammifères",
      order == "Squamata" ~ "Couleuvres",
      order == "Testudines" ~ "Tortues",
      Type_FR == "Poissons" ~ "Poissons",
      .default = class
    )
  ) |>
  # filter(
  #   kingdom %in% KINGDOM_SEL,
  #   Type_FR %in% TYPE_FCT_LIST
  # ) |>
  group_by(
    Type_FR,
    kingdom,
    reformat_taxo,
    h3_parent,
    MUS_NM_MUN,
    MUS_NM_MRC,
    MUS_NM_REG,
    wkt_geom
  ) |>
  summarise(
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    log_n = log(n) # dbplyr translates log() to LN() in DuckDB
  )
