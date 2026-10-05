## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
# --> À différentes résolutions H3, Compte
# --> Exécution automatique des scripts dudckdb pour compter (exportés en Parquet)

## ____________####
## Prépare l'environnement --------

# --- Démarre duckdb avec librairies nécessaires ---
con <- evolovr::setup_duckdb()

message(sprintf("Résolution originale %s, parent %s", res_fine, res_parent))

# Résolution désirée "res_parent"
# NOTE :
# -> chiffre plus petit, la résolution est plus grossière
# -> chiffre plus petit = l'ID H3 parent!
# -> Variable réutilisé dans : 5.1.prep_data.R
# -> Le maximum possible est la résolution qui a été choisie dans :
#   file.path(
#     incub, "scripts/partie_2/2.transf_donn/2.transform_gbif_data_admin_region.R")
#   )

## Sommaire biodiv Grille H3 -----------------------------------------------

## Charge et prépare données GBIF + H3 -----------------------------------------------
# source(file.path(incub, "scripts/partie_2/01_load_gbif_data.R"))

# Define paths (using your local paths)

# Précalculé avec scripts/partie_2/2.transf_donn/2.duckdb_data_prep.sh (prend 15 min; archivé)
# gb_path      <- file.path(incub, 'data/partie_2/biodiv/gbif_data/gbif_prep_h3.parquet')

# gbif_prep_h3_res_10.parquet (gbif_prep_h3_highres.parquet) préparé avec scripts/2.transf_donn/2.transform_gbif_data_admin_region.R
# (long, prend 20 min environ pour région admin, 5 min jointure pour GBIF)
gb_path <- file.path(
  'output/biodiv/gbif_data/',
  # "gbif_prep_h3_highres.parquet" # avant
  sprintf("gbif_prep_h3_res_%s.parquet", res_fine)
)

# voir scripts/espece_match_gbif.R
# TODO : REMPLACER le fichier avec le XLSX des noms d'espèces
# TODO : Extraire le nom des espèces de la liste GBIF et trouver ceux qui ne
# sont pas dans la liste de noms d'espèces (comme cela, on va voir ceux
# qu'on n'a pas d'information et donc qu'une correction est nécessaire)
spnm_path <- file.path('output/esp_noms_gbif.csv')
# Ce fichier a été créé manuellement avec colonnes
# --> Rank, value, Type_FR_Mapped,sci_group
# Avant la correction se faisant directement avec duckdb.
# Par contre, ça devenait un code super long à maintenir et super difficile
# à suivre. Donc, le code a été mis dans un tableau puis complété pour
# faire une jointure et un coalesce sur les colonnes. De cette manière,
# l'ajout de données pour modifier la colonne 'Type_FR' se fait en
# ajoutant une nouvelle ligne dans le tableau CSV.
# Correction de valeurs Type_FR avec IA. Voici la requête :
# -------------------------------------------
# from this list of correspondance as a base
# corresp_table_class_orderType_FR.csv,
# fill the Type_FR of species_list_empty_type_fr.csv.
# Be as accurate taxonomically as possible.
# Provide a csv that can be downloaded
# -------------------------------------------
type_fr_path <- file.path(
  'data/raw/biodiv/more/corresp_table_class_orderType_FR.csv'
)

# Reference the parquet and CSVs as if they were tables
gb_tbl <- tbl(src = con, from = sprintf("read_parquet('%s')", gb_path))
spnm_tbl <- tbl(src = con, from = sprintf("read_csv_auto('%s')", spnm_path))
mapping_tbl <- tbl(
  src = con,
  from = sprintf("read_csv_auto('%s')", type_fr_path)
)

# tbl(con, "read_parquet('InCubateur/2025_05_24_Guide_biodiv_qc/data/partie_2/biodiv/gbif_data/gbif_raw_new.parquet')")

# Ajouter colonne Type_FR pour faire une référence taxonomique dans le guide
# Correction de certains niveaux taxonomiques
# Prepare mapping slices for specific ranks
m_famil <- mapping_tbl |>
  filter(Rank == "Family") |>
  select(Value, family_match = Type_FR_Mapped)
m_class <- mapping_tbl |>
  filter(Rank == "Class") |>
  select(Value, class_match = Type_FR_Mapped)
m_order <- mapping_tbl |>
  filter(Rank == "Order") |>
  select(Value, order_match = Type_FR_Mapped)
m_kingdom <- mapping_tbl |>
  filter(Rank == "Kingdom") |>
  select(Value, kingdom_match = Type_FR_Mapped)


### Correction des noms Type_FR ------

# Exploration poissons
# spnm_tbl |>
#  filter(Type_FR == "Poissons") |>
#  select(species, Type_FR, phylum, class, `order`, family)

message("Correction des noms et Type_FR")
# Ajout des noms d'espèce et Type_FR corrigés (depuis mapping_tbl)
gbif_prep_type_fr_h3 <- gb_tbl |>
  # Match le nom des espèces avec le Type_FR
  left_join(
    y = spnm_tbl |>
      select(
        scientificName,
        nom_sci = scientific_name_nom_scientifique,
        nom_en = english_common_name,
        nom_fr = nom_commun_en_francais,
        # Faire un Type_FR temporaire qu'on va utiliser dans Coalesce
        # (colonne de base pour coalesce)
        spnm_type = Type_FR
      ),
    by = join_by(
      scientificName == scientificName
    )
  ) |>
  ### Correction des Type_FR
  # Joindre pour ajouter la colonne de Famille modifiée
  left_join(m_famil, by = c("family" = "Value")) |>
  # Joindre pour ajouter la colonne de Classe modifiée
  left_join(m_class, by = c("class" = "Value")) |>
  # Joindre pour ajouter la colonne de Ordre modifiée
  left_join(m_order, by = c("order" = "Value")) |>
  # Joindre pour ajouter la colonne de Règne modifiée
  left_join(m_kingdom, by = c("kingdom" = "Value")) |>
  # 5. Coalesce logic (Priority: Species > Order > Class > Kingdom)
  mutate(
    # Coalesce dans l'ordre pour garder le plus précis possible, sinon prendre 'spnm_type' si rien d'autre
    Type_FR = coalesce(
      order_match,
      family_match,
      class_match,
      kingdom_match,
      spnm_type # Type de base
    )
  ) |>
  # Correction taxonomique (classe, ordre, espèces )
  mutate(
    ## -- Classe
    # Reclassification nécessaire pour éviter d'avoir
    # des duplicats dans class et ordre
    class = if_else(
      class == "Diplura", # mauvaise classification
      true = "Entognatha",
      false = class
    ),
    ## -- Ordre
    # treemap ne fonctionne pas s'il y a des NA dans 'order'.
    order = ifelse(class == "Squamata", yes = "Couleuvres", no = order),
    order = ifelse(class == "Testudines", yes = "Tortues", no = order),
    ## -- Espèces
    # Espèce de paruline renommée en 2025!
    species = ifelse(
      species == "Setophaga petechia",
      yes = "Setophaga aestiva",
      no = species
    ),
  ) |>
  # Retirer les colonnes temporaires pour faire la jointure
  dplyr::select(
    -c(spnm_type, family_match, class_match, order_match, kingdom_match)
  )

sp_count_names <- gbif_prep_type_fr_h3 |>
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
  # pivot_longer(cols = c(class, order, family),  names_to = "Rank", values_to = "value")
  collect()

message("NB observation unique espèce (en mémoire)\ncount_sp")
# NOMBRE *D'OBSERVATION* UNIQUE POUR CHAQUE ESPÈCES
count_sp <- gbif_prep_type_fr_h3 |>
  group_by(
    kingdom,
    phylum,
    class,
    `order`,
    family,
    species
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
    species,
    desc(n)
  )

## Compte hiérarchique (passe d'espèce à famille)
# du nombre d'espèces unique dans chaque famille
hierarchy_counts <- count_sp |>
  select(-c(n, species)) |>
  group_by_all() |>
  count() |>
  mutate(
    pa = 1
  ) |>
  ungroup()


message("NB observation unique espèce par région \ncount_sp_reg")
# NOMBRE *D'OBSERVATION* UNIQUE POUR CHAQUE ESPÈCES PAR RÉGION
count_sp_reg <- gbif_prep_type_fr_h3 |>
  group_by(
    kingdom,
    phylum,
    class,
    `order`,
    family,
    Type_FR,
    species,
    # RÉGION
    MUS_NM_REG
  ) |>
  summarise(
    n = sum(!is.na(species), na.rm = TRUE), # Les valeurs manquantes sont retirées en SQL
    .groups = "drop"
  ) |>
  # Correction de classe
  mutate(
    pa = 1
  ) |>
  # Matérialiser les résultats
  collect()

grid_resolution <- 0.1 # Smaller number = higher resolution grid


gbif_prep_type_fr_h3 |> select(gbifID, geometry) |> head()

compte_pts <- gbif_prep_type_fr_h3 |> count() |> collect() |> pull()
binned_data <- gbif_prep_type_fr_h3 |>
  mutate(
    grid_lon = round(decimalLongitude / grid_resolution) * grid_resolution,
    grid_lat = round(decimalLatitude / grid_resolution) * grid_resolution
  ) |>
  group_by(grid_lon, grid_lat) |>
  summarise(point_count = n(), .groups = "drop") |>
  collect()

# Carte du Québec avec les observations aggrégées
ggplot(
  data = binned_data,
  aes(x = grid_lon, y = grid_lat, fill = point_count)
) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", trans = "log10") +
  labs(
    title = "DuckDB-Aggregated 2D Density Map",
    subtitle = sprintf("%s Points", compte_pts),
    fill = "Count (Log Scale)"
  ) +
  theme_minimal()


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
      class == "Amphibia" ~ "Amphibiens",
      class == "Squamata" ~ "Couleuvres",
      class == "Testudines" ~ "Tortues",
      class == "Mammalia" ~ "Mammifères",
      kingdom == "Fungi" ~ "Champignons",
      kingdom == "Plantae" ~ "Plantes",
      Type_FR == "Poissons" ~ "Poissons",
      phylum == "Arthropoda" ~ "Bobittes",
      class == "Aves" ~ "Oiseaux",
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

# Cleanup
# dbDisconnect(con, shutdown = TRUE)
