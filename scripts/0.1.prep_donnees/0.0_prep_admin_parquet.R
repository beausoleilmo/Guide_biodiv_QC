## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Création du fichier administratif en parquet
suppressMessages(
  {
    require(sf)
    require(sfarrow)
    require(duckspatial)
  }
)

#   --> Transforme les données originale des régions du Québec avec le bon CRS
#   @source :
#   https://www.donneesquebec.ca/recherche/dataset/decoupages-administratif
message("transforme données région")

# Lire données spatiales en sf
muni <- sf::st_read(
  dsn = file.path(
    "data/raw",
    "decoupages_administratifs_1_20000_format_SHP/munic_s.shp"
  ),
  quiet = TRUE
)
muni <- duckspatial::ddbs_open_dataset(
  path = file.path(
    "data/raw",
    "decoupages_administratifs_1_20000_format_SHP/munic_s.shp"
  )
)

# 15 s
tictoc::tic("Union admin du Québec")
muni_union <- duckspatial::ddbs_union(muni)
# muni_union  |>  st_as_sf()
tictoc::toc()

# Exportation des données en parquet
message("Exporation municipalités en .parquet")
duckspatial::ddbs_write_dataset(
  data = muni,
  path = file.path(
    "data/mod",
    "munic_s.parquet"
  ),
  parquet_compression = "zstd",
  options = list(COMPRESSION_LEVEL = 12),
  overwrite = TRUE
)

duckspatial::ddbs_write_dataset(
  data = muni_union,
  path = file.path(
    "data/mod",
    "munic_union_s.parquet"
  ),
  parquet_compression = "zstd",
  options = list(compression_level = 12),
  overwrite = TRUE
)


### Préparer carte des MRCs du Québec en parquet
### Avant, c'était dans le guide directement
region_noms <- tibble::tribble(
  ~MUS_CO_REG , ~MUS_NM_REG                     , ~MUS_NM_REG_short ,
  "01"        , "Bas-Saint-Laurent"             , "Bas-St-Laurent"  ,
  "02"        , "Saguenay–Lac-Saint-Jean"       , "Saguenay–LSJ"    ,
  "03"        , "Capitale-Nationale"            , "C.-Nationale"    ,
  "04"        , "Mauricie"                      , "Mauricie"        ,
  "05"        , "Estrie"                        , "Estrie"          ,
  "06"        , "Montréal"                      , "Montréal"        ,
  "07"        , "Outaouais"                     , "Outaouais"       ,
  "08"        , "Abitibi-Témiscamingue"         , "Abitibi-T"       ,
  "09"        , "Côte-Nord"                     , "Côte-N."         ,
  "10"        , "Nord-du-Québec"                , "Nord-Qc"         ,
  "11"        , "Gaspésie–Îles-de-la-Madeleine" , "Gaspésie–ÎDLM"   ,
  "12"        , "Chaudière-Appalaches"          , "Chaudière-A."    ,
  "13"        , "Laval"                         , "Laval"           ,
  "14"        , "Lanaudière"                    , "Lanaudière"      ,
  "15"        , "Laurentides"                   , "Laurentides"     ,
  "16"        , "Montérégie"                    , "Montérégie"      ,
  "17"        , "Centre-du-Québec"              , "Centre-Qc"
) |>
  mutate(
    code_reg_small = paste(MUS_CO_REG, MUS_NM_REG_short, sep = " ")
  )


runion_raw <- muni |>
  duckspatial::ddbs_transform(y = "EPSG:32198") |>
  duckspatial::ddbs_union_agg(by = c("MUS_NM_REG", "MUS_CO_REG")) |>
  dplyr::mutate(
    code_reg = paste(MUS_CO_REG, MUS_NM_REG, sep = " ")
  ) |>
  left_join(
    y = region_noms,
    by = join_by(MUS_CO_REG, MUS_NM_REG),
    # La table tribble doit être de la même source que ddbs
    copy = "temp-table"
  )


duckspatial::ddbs_write_dataset(
  data = runion_raw,
  path = file.path(
    "data/mod",
    "mrc_union_s.parquet"
  ),
  parquet_compression = "zstd",
  options = list(compression_level = 12),
  overwrite = TRUE
)

# # NOTE :
# # NE PAS EXTRAIRE TOUS LES POLYGONES DU QUÉBEC À HAUTE RÉSOLUTION
# # C'est beaucoup trop gros et lent.
# # L'idée est de trouver les INDICES des polygones H3 puis de faire les calculs
# # Une fois les statistiques calculées, il y aura BEAUCOUP moins de lignes
# # à transformer en polygone
# #
# #### Jointure avec H3
# con <- evolovr::setup_duckdb()
# admin <- file.path("data/mod", "munic_s.parquet")
#
#   colsAdmin = c(
#     "MUS_NM_MUN",
#     "MUS_NM_MRC",
#     "MUS_NM_REG"
#   )
# # Définition des variables de base
# res <- 7   # Choisissez la résolution H3 désirée (ex: 6, 7 ou 8)
#
# # ==============================================================================
# # ÉTAPE 1 : UNION GLOBALE DU QUÉBEC & REPROJECTION
# # ==============================================================================
# # On agrège toutes les entités en une seule surface globale (ST_Union_Agg)
# quebec_union_query <- dplyr::tbl(
#   src = con,
#   from = dbplyr::sql(
#     glue::glue_sql(
#       "WITH raw_geom AS (
#          SELECT ST_Transform((geometry), 'EPSG:4269', 'EPSG:4326') AS geom_4326
#          FROM read_parquet({admin})
#        )
#        SELECT ST_Union_Agg(geom_4326) AS qc_boundary FROM raw_geom",
#       .con = con
#     )
#   )
# )
#
# # Récupération du SQL de l'union
# union_sql <- dbplyr::remote_query(quebec_union_query)
#
#
# # ==============================================================================
# # ÉTAPE 2 : GÉNÉRATION ET EXPLOSION DE LA GRILLE H3
# # ==============================================================================
# # On utilise la géométrie unifiée pour couler les hexagones H3
# h3_quebec_grid_query <- glue::glue(
#   "WITH grid_array AS (
#      SELECT unnest(
#        h3_polygon_wkt_to_cells_experimental_string(
#          ST_AsText(qc_boundary),
#          {as.integer(res)},
#          'overlap'
#        )
#      ) AS h3_cell
#      FROM ({union_sql})
#    )
#    SELECT DISTINCT h3_cell FROM grid_array WHERE h3_cell IS NOT NULL"
# )
#
#
# # ==============================================================================
# # ÉTAPE 3 : EXPORTATION DU FICHIER DE COUVERTURE POUR INSPECTION
# # ==============================================================================
# output_qc_h3_file <- "data/mod/quebec_h3_coverage.parquet"
#
# # Exportation ultra-rapide native DuckDB
# DBI::dbExecute(
#   con,
#   glue::glue("COPY ({h3_quebec_grid_query}) TO '{output_qc_h3_file}' (FORMAT PARQUET);")
# )
#
# message("Succès ! La grille H3 unifiée couvrant le Québec a été exportée dans : ", output_qc_h3_file)
