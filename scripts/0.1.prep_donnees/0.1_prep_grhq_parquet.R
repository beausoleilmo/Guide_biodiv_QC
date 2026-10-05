# Charge les données téléchargés du GRHQ
#
library(duckdb)
library(DBI)
library(glue)
library(dplyr)
library(purrr)

# Lecture des données téléchargés de 0.0_prep_hydro_grhq.R
gdb_fichiers <- list.files(
  path = "/Volumes/g_magni/data/DonneeQuebec/hydro/donnees_qc_downloads",
  pattern = ".gdb$",
  full.names = TRUE,
  include.dirs = TRUE,
  recursive = TRUE
)

# Extraire le nom de toutes les couches et filtrer pour garder "RH_X"
couches_fichiers <- lapply(gdb_fichiers, function(x) {
  sf::st_layers(x) |> as.data.frame()
}) |>
  # Ajouter le nom des fichier pour la list
  setNames(gdb_fichiers) |>
  # Combiner toutes les lignes
  bind_rows(.id = "nom_fichier") |>
  dplyr::select(-crs) |>
  # Filtre seulement les couches voulues
  filter(name %in% c("RH_S", "RH_L"))

# Exploration des nombres de colonnes dans les Fichiers
# Nous voulons prendre le nombre MINIMAL de colonnes en assumant que
# tous les jeux de données ont les mêmes noms
couches_col <- couches_fichiers |>
  group_by(name) |>
  count(fields) |>
  slice_min(order_by = fields)


# noms des colonnes pour chaque couche de données
# RH_L
nom_couche_rhl <- couches_fichiers |>
  filter(
    name == "RH_L",
    fields ==
      couches_col |>
        filter(name == "RH_L") |>
        pull(fields)
  ) |>
  slice_head(n = 1) |>
  pull(nom_fichier)

# RH_S
nom_couche_rhs <- couches_fichiers |>
  filter(
    name == "RH_S",
    fields ==
      couches_col |>
        filter(name == "RH_S") |>
        pull(fields)
  ) |>
  slice_head(n = 1) |>
  pull(nom_fichier)


head(couches_fichiers)

# Obtenir le nom des colonnes seulement
header_rhl <- sf::st_read(
  nom_couche_rhl,
  query = sprintf("SELECT * FROM %s LIMIT 0", "RH_L")
)
header_rhs <- sf::st_read(
  nom_couche_rhs,
  query = sprintf("SELECT * FROM %s LIMIT 0", "RH_S")
)


# Connection DuckDB & Extension
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
# Extensions spatiales
dbExecute(con, "INSTALL spatial;LOAD spatial;")

# Liste des noms de fichiers
gdb_files <- couches_fichiers |>
  pull(nom_fichier) |>
  unique()

# Define target layer and schema template
for (layer_name_i in c("RH_L", "RH_S")) {
  message(layer_name_i)
  tictoc::tic("Pipeline")
  # Extract expected columns directly from header_rhl (ignoring geometry column name variation if needed)
  if (layer_name_i == "RH_L") {
    target_cols <- colnames(header_rhl)
    tab_nom <- DBI::dbQuoteIdentifier(con, "combined_rhl")
  }
  if (layer_name_i == "RH_S") {
    target_cols <- colnames(header_rhs)
    tab_nom <- DBI::dbQuoteIdentifier(con, "combined_rhs")
  }

  # Exclude spatial 'Shape' or 'geom' column if handling purely attribute/GDAL spatial reads,
  # or keep explicitly as SQL columns.
  cols_sql <- paste(DBI::dbQuoteIdentifier(con, target_cols), collapse = ", ")

  # Construire la requête de l'UNION ALL
  build_select_query <- function(
    file_path,
    layer,
    cols_select_sql,
    crs_str = #'PROJCS["NAD83 / Quebec Lambert",GEOGCS["NAD83",DATUM["North_American_Datum_1983",SPHEROID["GRS 1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic_2SP"],PARAMETER["standard_parallel_1",60],PARAMETER["standard_parallel_2",44],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-70],PARAMETER["false_easting",700000],PARAMETER["false_northing",0],UNIT["metre",1]]'
    "EPSG:32198" # L'utilisation seulement de EPSG causait duckdb de mal exporter les données de géométrie
  ) {
    safe_path <- DBI::dbQuoteString(con, file_path)
    safe_layer <- DBI::dbQuoteString(con, layer)
    safe_crs <- DBI::dbQuoteString(con, crs_str)

    sprintf(
      "SELECT %s, 
      ST_SetCRS(Shape, %s) AS geometry 
      FROM ST_Read(%s, layer = %s)",
      cols_select_sql,
      safe_crs,
      safe_path,
      safe_layer
    )
  }

  # Combine all file queries with UNION ALL
  union_query <- gdb_files |>
    map_chr(~ build_select_query(.x, layer_name_i, cols_sql)) |>
    paste(collapse = "\nUNION ALL\n")

  # 4. Create a Lazy DuckDB Table or Materialize Result
  # Option A: Create a lazy view inside DuckDB for fast out-of-core queries
  dbExecute(
    con,
    sprintf(
      "CREATE OR REPLACE VIEW %s AS %s",
      tab_nom,
      union_query
    )
  )

  # Query the combined dataset directly in DuckDB
  # combined_db <- tbl(con, "combined_rhl")

  # Change le nom de sortie du fichier GRHQ pour S surfaces et L lignes.
  out_file <- sprintf(
    "/Volumes/g_magni/data/DonneeQuebec/hydro/grhq_%s.parquet",
    tolower(layer_name_i)
  )

  # Requête pour COPIER les résultats en parquet
  # NOTE : GEOPARQUET_VERSION semble nécessaire pour avoir une géométrie
  # compatible avec QGIS.
  query <- glue::glue_sql(
    "
  COPY {tab_nom} 
  TO {out_file} 
  (FORMAT PARQUET, COMPRESSION ZSTD, 
  GEOPARQUET_VERSION 'NONE',  -- Sinon la colonne de géométrie ne s'exporte pas bien.
  ROW_GROUP_SIZE 100000)  
",
    .con = con
  )

  message("Exportation des données")
  DBI::dbExecute(con, query)
  tictoc::toc()
  # CREATE OR REPLACE VIEW crhl AS FROM 'grhq_rh_l.parquet' limit 10;
  # FROM crhl LIMIT 10;

  # Clean up connection when finished
  # dbDisconnect(con, shutdown = TRUE)
}


# Charge données surfaciques
hq <- duckspatial::ddbs_open_dataset(
  path = "/Volumes/g_magni/data/DonneeQuebec/hydro/grhq_rh_s.parquet"
)

hq
## Emprise Québec ----------------------------------------------------
# Prépare l'emprise spatiale du Québec
bbox_qc <- sf::st_bbox(
  c(
    xmin = -830291.429999985,
    ymin = 117964.150000002,
    xmax = 783722.440000005,
    ymax = 721304.835203388
  ),
  crs = sf::st_crs(32198)
) |>
  sf::st_as_sfc() |>
  sf::st_as_sf()

dir.create(path = file.path(tempdir(), "duckdb/temp"), recursive = TRUE)
aire_seuil <- 1e6
# Simplification des données spatiales pour des calculs plus efficace
# L'objectif est de faire des cartes pas pire. Pas des calculs super précis.
hq_sel <- hq |>
  # Ne garder que les formes avec une aire supérieure
  dplyr::filter(SHAPE_Area > aire_seuil) |>
  # Simplification des polygones. 120 est super simplifié
  duckspatial::ddbs_simplify(tolerance = 120) |>
  duckspatial::ddbs_transform(y = paste0("EPSG:", projetCRS)) |>
  duckspatial::ddbs_crop(y = bbox_qc)

# Taille de notre table d'attributs
# count(hq_sel)

## ____________####
## Exporter les données --------

# Exportation des données pour un accès plus rapide
duckspatial::ddbs_write_dataset(
  data = hq_sel,
  path = "data/mod/grhq_rhs_simple.parquet",
  overwrite = TRUE
)
