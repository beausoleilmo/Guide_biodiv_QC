## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
# --> Préparation des données GBIF avec résolutions pour H3
# --> Correction d'erreurs taxonomiques (noms manquants, changement taxons)
# --> Exploration des données (Compte, etc.)
# --> La jointure des données était auparavant faite avec le nom 'latin' des
#     espèces. Par contre, cela cause problème puisque les données GBIF ont
#     des synonymes qui changent dans le temps et qui peuvent biaiser les
#     comptes d'espèces. L'utilisation de speciesKey est supérieur puisque
#     les données de noms d'espèces de GBIF (API) et le jeu de données
#     téléchargé pour le Québec peuvent se joindre même s'il existe plusieurs
#     noms synonymes pour la même espèce.
#     Une liste de noms français doit simplement être jointe aux données
#     de noms d'espèces extraites avec les noms d'espèces de GBIF (API).

# Résolution désirée "res_parent"
# NOTE :
# -> chiffre plus petit, la résolution est plus grossière
# -> chiffre plus petit = l'ID H3 parent!
# -> Variable réutilisé dans : 5.1.prep_data.R
# -> Le maximum possible est la résolution qui a été choisie dans :
#   file.path(
#     "scripts/partie_2/2.transf_donn/2.transform_gbif_data_admin_region.R")
#   )

## ____________####
## Prépare l'environnement --------

# --- Démarre duckdb avec librairies nécessaires ---
con <- evolovr::setup_duckdb()

message(
  sprintf(
    "Résolution originale %s, parent %s",
    res_fine,
    res_parent
  )
)

## Charge et prépare données GBIF + H3 -----------------------------------------------

# Définit les chemins d'accès aux fichiers

# gbif_prep_h3_res_10.parquet (gbif_prep_h3_highres.parquet) préparé avec
# scripts/2.transf_donn/2.transform_gbif_data_admin_region.R
# (long, prend 20 min environ pour région admin, 5 min jointure pour GBIF)
gb_path <- file.path(
  'output/biodiv/gbif_data/',
  # "gbif_prep_h3_highres.parquet" # avant
  sprintf("gbif_prep_h3_res_%s.parquet", res_fine)
)

# TODO :
#   Extraire le nom des espèces de la liste GBIF et trouver ceux qui ne
# sont pas dans la liste de noms d'espèces (comme cela, on va voir ceux
# qu'on n'a pas d'information et donc qu'une correction est nécessaire)
# spnm_path <- file.path('output/esp_noms_gbif.csv')
# NOTE : voir scripts/espece_match_gbif.R
spnm_path <- file.path(
  "output/biodiv/liste_especes",
  "esp_noms_gbif_maj.parquet"
)

# Le fichier `corresp_table_class_orderTypeFR.csv` a été créé manuellement
# avec colonnes
# --> Rank, value, Type_FR_Mapped, sci_group
# Avant, la correction se faisant directement avec duckdb en SQL, lorsqu'il
# n'y avait que quelques espèces à prendre en compte.
# Par contre, plus le nombre d'espèces à corriger grandissait, ça
# devenait un code super long à maintenir et super difficile
# à suivre.
# Donc, le code a été mis dans un tableau puis complété pour
# faire une jointure et un coalesce sur les colonnes.
# De cette manière,
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
# TODO : ajouter les infos de https://www.inaturalist.org/taxa ?
type_fr_path <- file.path(
  'data/raw/biodiv/more',
  "corresp_table_class_orderType_FR.csv"
)

# Cherge données parquet et CSVs
# GBIF
gb_tbl <- tbl(
  src = con,
  from = sprintf("read_parquet('%s')", gb_path)
)
# Données de noms d'espèces GBIF_API-Nom_francais
spnm_tbl <- tbl(
  src = con,
  from = sprintf("read_parquet('%s')", spnm_path)
)
# Table des noms de Type_FR à corriger
mapping_tbl <- tbl(
  src = con,
  from = sprintf("read_csv_auto('%s')", type_fr_path)
)

# Ajouter colonne Type_FR pour faire une référence taxonomique dans le guide
# Correction de certains niveaux taxonomiques
# Prepare mapping slices for specific ranks
m_famil <- mapping_tbl |>
  dplyr::filter(Rank == "Family") |>
  dplyr::select(Value, family_match = Type_FR_Mapped)
m_class <- mapping_tbl |>
  dplyr::filter(Rank == "Class") |>
  dplyr::select(Value, class_match = Type_FR_Mapped)
m_order <- mapping_tbl |>
  dplyr::filter(Rank == "Order") |>
  dplyr::select(Value, order_match = Type_FR_Mapped)
m_phylum <- mapping_tbl |>
  dplyr::filter(Rank == "Phylum") |>
  dplyr::select(Value, phylum_match = Type_FR_Mapped)
m_kingdom <- mapping_tbl |>
  dplyr::filter(Rank == "Kingdom") |>
  dplyr::select(Value, kingdom_match = Type_FR_Mapped)


### Correction des noms Type_FR ------

# Exploration poissons
# spnm_tbl |>
#  filter(Type_FR == "Poissons") |>
#  select(species, Type_FR, phylum, class, `order`, family)

message("Correction des noms et Type_FR")
# Ajout des noms d'espèce et Type_FR corrigés (depuis mapping_tbl)
gbif_prep_type_fr_h3 <- gb_tbl |>
  # Match le nom des espèces, en français et avec le Type_FR
  dplyr::left_join(
    y = spnm_tbl |>
      dplyr::select(
        sciName_GBIF = scientificName,
        nom_sci = nom_scientifique,
        nom_en = english_common_name,
        nom_fr = nom_commun_en_francais,
        # Faire un Type_FR temporaire qu'on va utiliser dans Coalesce
        # (colonne de base pour coalesce)
        spnm_type = Type_FR,
        speciesKey
      ),
    by = dplyr::join_by(
      # Avant, utilisation des NOMS latins pour faire la jointure.
      # Cela amène un problème de taille : les synonymes.
      # L'utilisation de speciesKey est SUPÉRIEUR pour résoudre les
      # noms d'espèces synonymes.
      # scientificName == scientificName # Ne pas utiliser les noms directement
      speciesKey == speciesKey
    )
  ) |>
  ### Correction des Type_FR
  # Joindre pour ajouter la colonne de Famille modifiée
  dplyr::left_join(m_famil, by = c("family" = "Value")) |>
  # Joindre pour ajouter la colonne de Classe modifiée
  dplyr::left_join(m_class, by = c("class" = "Value")) |>
  # Joindre pour ajouter la colonne de Ordre modifiée
  dplyr::left_join(m_order, by = c("order" = "Value")) |>
  # Joindre pour ajouter la colonne de Embramchement modifiée
  dplyr::left_join(m_phylum, by = c("phylum" = "Value")) |>
  # Joindre pour ajouter la colonne de Règne modifiée
  dplyr::left_join(m_kingdom, by = c("kingdom" = "Value")) |>
  # 5. Coalesce logic (Priority: Species > Order > Class > Kingdom)
  dplyr::mutate(
    # Coalesce dans l'ordre pour garder le plus précis possible,
    # sinon prendre 'spnm_type' si rien d'autre
    Type_FR = dplyr::coalesce(
      order_match,
      family_match,
      class_match,
      phylum_match,
      kingdom_match,
      spnm_type # Type de base
    )
  ) |>
  # Correction taxonomique (classe, ordre, espèces )
  dplyr::mutate(
    ## -- Classe
    # Reclassification nécessaire pour éviter d'avoir
    # des duplicats dans class et ordre
    class = dplyr::if_else(
      class == "Diplura", # mauvaise classification
      true = "Entognatha",
      false = class
    ),
    ## -- Ordre
    # treemap ne fonctionne pas s'il y a des NA dans 'order'.
    # order = ifelse(class == "Squamata", yes = "Couleuvres", no = order),
    # order = ifelse(class == "Testudines", yes = "Tortues", no = order),
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
    -c(
      spnm_type,
      family_match,
      class_match,
      order_match,
      phylum_match,
      kingdom_match
    )
  )

# Cleanup
# dbDisconnect(con, shutdown = TRUE)
