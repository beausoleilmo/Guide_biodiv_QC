base::cat(
  "=========================================================\n",
  "Initialisation : 
  -> Utiliser fichier GBIF Parquet et appliquer filtres \n",
  "Description    : 
  -> Prépare grille H3-administrative (evolovr::prep_h3_admin)
  -> Lecture des données GBIF
  -> '.log' calcul sommaire (nb lignes après filtres)
  evolovr::generate_gbif_log
  \n",
  "=========================================================\n\n",
  sep = ""
)
# devtools::load_all(path = "../evolovr/")

# Création de dossier pour la sortie des fichiers
mapply(
  FUN = dir.create,
  file.path(
    sprintf("output/%s", c("biodiv/gbif_data", "admin"))
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

# --- Configuration ---
cat(sprintf("Résolution pour grille H3 = %s", res_fine), fill = TRUE)
paths <- list(
  # Importation
  gbif_raw = file.path(
    "data/raw",
    "biodiv/gbif_data",
    "occurrence_trans.parquet"
    # "gbif_raw_new.parquet"
  ), # était gbif_prep_all.parquet

  in_admin_shp = file.path(
    "data/raw",
    "decoupages_administratifs_1_20000_format_SHP",
    "munic_s.shp"
  ),

  # Exportation
  out_path_log = file.path(
    "output/biodiv/gbif_data",
    "occurrence_trans.log"
  ),

  out_admin_pq = file.path(
    "data/mod", "munic_s.parquet"),
  # out_admin_pq = file.path(
  #   "output",
  #   "admin",
  #   sprintf("admin_mun_res_%s.parquet", res_fine)
  # ),

  out_gbif_pq = file.path(
    "output/biodiv/gbif_data",
    sprintf("gbif_prep_h3_res_%s.parquet", res_fine)
  )
)


message(" --> .log des données GBIF originales")
# Génère des fichiers de sommaire sur le nombre de rangées et
# statistiques avec duckdb
# evolovr::
generate_gbif_log(
  con = NULL,
  path_in = paths$gbif_raw,
  path_log = paths$out_path_log, 
  notes = "Fichier brute des données GBIF avec polygone grossier."
)


message(" --> Prépare la table admin-H3 : prep_h3_admin()")
# NOTE : Changement de stratégie : 
# on va ajouter l'intersection spatiale des municipalités directement avec GBIF 
if (!file.exists(paths$out_admin_pq)) {
  # 910-2500 sec à 10L (15-45 min)
  # Inputs : 1. données in_admin_shp
  evolovr::prep_h3_admin(
    con = NULL,
    config = paths,
    res = res_fine
  )
}

message(" --> Prépare GBIF avec admin + h3 : GBIF_join_admin()")

# NOTE : jointure (inner join) des données admin
# NOTE : 1184 s : Filtration des données, jointure ('filtre' inner)
# admin, exportation du fichier
# ANCIENNEMENT : une grille entre admin et H3
# Entre 620 et 920 sec, 10 min et 15 min (Sans précalcul de grille admin)
# 180-300 sec (3-5 min) Avec précalcul de la grille H3 avec les régions
# administratives (selon grille 10L)
# evolovr::
join_gbif_admin(
  con = NULL,
  config = paths,
  res = res_fine,
  # NOTE :
  # Considère mettre "PRESERVED SPECIMEN" pour inclure les observations
  # des herbiers
  # Voir :
  # https://www.gbif.org/occurrence/search?datasetKey=2fd02649-fc08-4957-9ac5-2830e072c097
  basisRec = c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION", "PRESERVED_SPECIMEN"),
  taxRank = c("SPECIES", "SUBSPECIES", "VARIETY"),
  kingdm = c("Chromista", "Fungi", "Plantae", "Animalia"),
  coordUncertainM = 200
)

# Génère des fichiers de sommaire sur le nombre de rangées et
# statistiques avec duckdb
evolovr::generate_gbif_log(
  con = NULL,
  # Notez que le nouveau fichier de GBIF à prendre en compte
  # est celui de la sortie précédante `out_gbif_pq`.
  path_in = paths$out_gbif_pq,
  path_log = file.path(
    "output/biodiv/gbif_data/",
    sprintf("gbif_prep_h3_res_%s.log", res_fine)
  ),
  notes = 
  "Fichier de données transformé
(filtre basisOfRecord, occurrenceStatus, taxonRank, 
coordinateUncertaintyMeters")
)
