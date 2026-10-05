
message("-> prep_h3_admin()")
# --- Préparation des données Administratives ---
prep_h3_admin <- function(con = NULL, config, res = 10L) {
  #' Ajout des régions admins aux données GBIF
  #' @md
  #' @param con Connection à un pilote \code{duckdb()}
  #' @param config Liste avec les chemins d'accès au minimum 
  #' \code{admin_shp} (mis en mémoire avec \code{ST_Read}) et 
  #' \code{out_admin_pq} qui exporte un fichier ".parquet"
  #' @param res "integer": nombre déterminant la résolution H3 e.g., \code{9L}
  #'
  #' @description
  #' Intersection de polygones administratifs (e.g. municipalités) avec 
  #' la grille H3 à fine échelle (e.g., résolution 10L). 
  #' 
  #' L'ajout des H3, aux données administratives, crée une grille standardisée
  #' utile pour en faire référence avec des données GBIF
  #' @section Utilisation potentielle : 
  #' Permet de faire une grille standardiséee (e.g., H3), mais informée par une
  #' variable spatiale (polygones). 
  #' Pour faire de la cartographie de points qui 
  #' se présenterait en unité spatiale standardisée. 
  #' 
  #' @details
  #' Connexion à \code{duckdb} et trouver les indices H3 à une résolution \code{res}.
  #' Utiliser \code{transmute} pour garder que les noms '\code{MUS_NM_*}' et 
  #' l'indice H3. Copie du fichier selon une préférence 
  #' choisie dans le \code{config}.
  #' 
  #' Pour donner un ordre de grandeur : 
  #' Resolution: 4, 
  #' "842baa5ffffffff" est un polygone qui 
  #' engloble aisément Montréal et Laval (aire ~1930 km2), 
  #' 
  #' Resolution: 7, 
  #' "872baa441ffffff" englobe le Mont-Royal (aire ~5.6 km2)
  #' 
  #' Resolution: 9, 
  #' "872baa441ffffff" est plus petit que le Stade-Olympic (aire ~114490 m2)
  #' 
  #' Resolution: 10, "8a2baa46a50ffff" englobe la salle Wilfrid-Pelletier et 
  #' la maison symphonique (aire ~16345 m2)
  #' 
  #' @returns
  #' Exportation de données \code{out_admin_pq}. 
  #' 
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' # --- Configuration ---
  #' con = setup_duckdb()
  #' folder = "test_path"
  #' paths <- list(
  #'   # Importation 
  #'   admin_shp  = file.path(folder, "munic_s.shp"),
  #'   # Exportation 
  #'   out_admin_pq  = file.path(folder, "admin_mun.parquet")
  #' )
  #' res = 10L
  #' # --- Calcul---
  #'  
  #' prep_h3_admin(con, config = paths, res = res)
  #' }
  #' 
  if(is.null(con)) {
    con = setup_duckdb()
    on.exit(expr = DBI::dbDisconnect(con, shutdown = TRUE))
  }
  # Prépare les données de régions administratives 
  admin_qc_tbl <- dplyr::tbl(
    src = con, 
    from = glue::glue_sql("ST_Read({config$admin_shp})", .con = con)
  )|>
    dplyr::select(MUS_NM_MUN, MUS_NM_MRC, MUS_NM_REG, geom) |>
    dplyr::mutate(
      # ST_Transform a besoin de extension spatiale de duckdb
      geom = ST_Transform(geom, 'EPSG:4269', 'EPSG:4326')
    )
  
  # Préparation de la table région admin en trouvant les IDs des polygones
  # H3 cells (Résolution XX)
  admin_h3_indexed <- admin_qc_tbl |>
    dplyr::mutate(
      # Création d'une 'liste' de cells (liste d'hexagones couvrant le polygone)
      cells = h3_polygon_wkt_to_cells(ST_AsText(geom), as.integer(res))
    ) |>
    # Utilisation d'une subquery/transmute pour faire une opération (un peu 
    # comme mutate), mais qui ne garde que les colonnes désirées. Avec unnest 
    # DuckDB's unnest() on prend les listes d'hexagone et on les reporte sur des 
    # nouvelles lignes. Donc chaque MUS_NM_* se retrouve avec 1 ID de 
    # la grille H3
    dplyr::transmute(
      h3_cell = unnest(cells),
      # Garde colonnes d'indicateur de région (pour la jointure basée sur H3)
      MUS_NM_MUN, 
      MUS_NM_MRC,
      MUS_NM_REG
    )  
  
  # Execute l'exportation
  message("Exécute la jointure spatiale et exporte en Parquet...")
  
  # Requête SQL 
  raw_dbplyr_query <- DBI::SQL(dbplyr::remote_query(admin_h3_indexed))
  
  export_sql <- glue::glue_sql(
    "COPY ({raw_dbplyr_query}) 
     TO {config$out_admin_pq} 
     (FORMAT PARQUET);", 
    .con = con
  )
  
  tictoc::tic()
  DBI::dbExecute(con, export_sql)
  tictoc::toc()
  
  message(glue("Réussi: Données ici --> {config$out_admin_pq}"))
  
}; docstring::docstring(prep_h3_admin)















message("-> GBIF_join_admin()")
GBIF_join_admin <- function(con = NULL, config, res = 10L) {
  # --- Préparation des données GBIF ---
  #' Jointure données GBIF filtrées et régions admin VIA cells de la grille H3  
  #'
  #' @param con Connection duckdb
  #' @param config Liste avec les chemins d'accès au minimum 
  #' \code{out_admin_pq} (voir \code{prep_h3_admin()}), 
  #' \code{gbif_raw} et \code{out_Gbif_pq}
  #' @param res "integer": nombre déterminant la résolution H3 e.g., \code{9L}
  #'
  #' @returns NULL. Exportation de données \code{out_Gbif_pq}. 
  #' 
  #' @export
  #' @section Résolution la plus fine:
  #' Permet de trouver le parent d'une résolution donnée après conversion.
  #'
  #' @note
  #' L'identifiant retourné est un entier de type `uint64`. Comme R ne gère pas
  #' nativement le 64-bit de manière précise, cet ID est généralement représenté
  #' sous forme de chaîne de caractères (`character`).
  #' E.g. en SQL dans duckdb avec l'extension h3 
  #' "SELECT h3_latlng_to_cell(37.7887987, -122.3931578, 9) as h3;"
  #'  retourne "617700169991847935" qui est un "uint64" . 
  #' @examples  
  #' \dontrun{
  #' # --- Configuration ---
  #' con = setup_duckdb()
  #' folder = "test_path"
  #' paths <- list(
  #'   # Importation 
  #'   gbif_raw   = file.path(folder, "gbif_prep_all.parquet"),
  #'   # Exportation 
  #'   out_admin_pq  = file.path(folder, "admin_mun.parquet"),
  #'   out_Gbif_pq  = file.path(folder, "gbif_prep_h3_res_10.parquet")
  #' )
  #' res = 10L
  #' # --- Calcul---
  #'  
  #' GBIF_join_admin(con, config = paths, res = res)
  #' }
  
  if(is.null(con)) {
    con = evolovr::setup_duckdb()
    on.exit(
      expr = evolovr::discon_duckdb()
      )
  }
  
  # Lire les données administratives précalculées 
  admin_h3_idx_precalc <- dplyr::tbl(
    src = con, 
    from = glue::glue("read_parquet('{config$out_admin_pq}')")
  )
  
  # Connection aux données GBIF
  gb_tbl <- dplyr::tbl(
    src = con, 
    glue::glue("read_parquet('{config$gbif_raw}')")
    )
  
  # gb_tbl |> count()
  
  # Pipeline de transformation des données GBIF
  pipeline <- gb_tbl |>
    # Filtrer l'information 
    filter(
      !is.na(species),
      basisofrecord == "HUMAN_OBSERVATION",
      countrycode == "CA",
      stateprovince %in% c("Quebec", "Québec", "Qc") | is.na(stateprovince),
      taxonrank %in% c("SPECIES", "SUBSPECIES", "VARIETY"),
      kingdom %in% c("Chromista", "Fungi", "Plantae", "Animalia"),
      coordinateuncertaintyinmeters <= 200 | is.na(coordinateuncertaintyinmeters)
    ) |>
    # Forme les données spatiales et l'ID H3 
    mutate(
      # geometry = ST_Point(decimallongitude, decimallatitude),
      # h3_cell = h3_latlng_to_cell(decimallatitude, decimallongitude, res)
      h3_cell = h3_latlng_to_cell(ST_Y(geometry), ST_X(geometry), res)
    ) |>
    # Jointure Régions admin VIA cell H3 
    inner_join(
      admin_h3_idx_precalc, 
      by = "h3_cell")
  
  # Exportation des données en GeoParquet
  message("Exécution de la jointure ID H3 et exportation en Parquet...")
  
  export_sql <- paste0(
    "COPY (", dbplyr::remote_query(pipeline), ") ",
    "TO ", 
    glue::glue_sql("{config$out_Gbif_pq}", .con = con), 
    " (FORMAT parquet, COMPRESSION 'zstd', COMPRESSION_LEVEL 4)"
  )
  
  tictoc::tic()
  dbExecute(con, export_sql)
  tictoc::toc()
  
  
  message(glue("Réussi: Données ici --> {config$out_Gbif_pq}"))
}
# docstring::docstring(GBIF_join_admin)
