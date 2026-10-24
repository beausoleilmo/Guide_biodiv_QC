## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   -->  données points ebirds
#   NOTE : besoin de la EBIRD_KEY dans .Renviron
# @source : extraction directe avec `rebird`

suppressMessages(
  {
    require(sf)
    require(dplyr)
    require(rebird)
  }
)

message("Charge et exporte : 'ebird_hp_sf' (points d'observations eBird)")
# Obtenir les données d'eBird
# list_region <- rebird::ebirdsubregionlist("subnational1", "CA")
hspt_ebirds <- rebird::ebirdhotspotlist(regionCode = "CA-QC")
# evolovr::download_ebird_hotspots(export_path = "data/raw/biodiv/ebird")

ebird_hp_sf <- hspt_ebirds |>
  # Mettre tableau en format spatial
  sf::st_as_sf(coords = c('lng', 'lat')) |>
  # Choisir le CRS
  sf::st_set_crs(value = 4326) |>
  # Formatter la colonne de date
  dplyr::mutate(
    date_obs_recent = as.POSIXct(
      latestObsDt,
      format = "%Y-%m-%d %H:%M",
      tz = Sys.timezone()
    )
  ) |>
  # Projection du jeu de données selon le CRS du projet
  sf::st_transform(crs = projetCRS) |>
  # Nouvelle colonne d'étiquette
  dplyr::mutate(
    labs = sprintf(
      # Joindre le nom d'un point eBird et le nombre d'espèces.
      fmt = "%s — %s sp.",
      locName,
      numSpeciesAllTime
    ),
    ratio_sp_chkclst = (numSpeciesAllTime / numChecklistsAllTime * 100) |>
      round(digits = 2)
  )


sf::st_write(
  obj = ebird_hp_sf,
  driver = "parquet",
  dsn = file.path("data/mod", "ebird_hot_spots.parquet"),
  delete_dsn = TRUE,
  layer_options = c("COMPRESSION=ZSTD", "COMPRESSION_LEVEL=12")
)

# Obtenir les données les plus récentes des observations eBird
# path_ebird <- "data/raw/biodiv/ebird"
# files_ebird <- list.files(path = path_ebird)
# dates_str <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\..*", "\\1", files_ebird)
# latest_file <- files_ebird[which.max(as.Date(dates_str))]
# pts_chauds_ebird <- file.path(
#   path_ebird,
#   latest_file
# )
#
# # Mettre le fichier en mémoire
# ebird_hp <- read.csv(file = pts_chauds_ebird, header = FALSE)
# head(ebird_hp)
# # Ajouter un nom aux colonnef
# names(ebird_hp) <- c(
#   "locId",
#   "countryCode",
#   "subnational1Code",
#   "subnational2Code",
#   "lat",
#   "lng", # Données spatiales!
#   "locName", # Nom des sites
#   "latestObsDt", # Date de la dernière observation
#   "numSpeciesAllTime", # Nombre d'espèces
#   "numChecklistsAllTime"
# )
#
# # Préparer les données spatiales pour une cartographie
# ebird_hp_sf <- ebird_hp |>
#   # Mettre tableau en format spatial
#   sf::st_as_sf(coords = c('lng', 'lat')) |>
#   # Choisir le CRS
#   sf::st_set_crs(value = 4326) |>
#   # Formatter la colonne de date
#   dplyr::mutate(
#     date_obs_recent = as.POSIXct(
#       latestObsDt,
#       format = "%Y-%m-%d %H:%M",
#       tz = Sys.timezone()
#     )
#   ) |>
#   # Projection du jeu de données selon le CRS du projet
#   sf::st_transform(crs = projetCRS) |>
#   # Nouvelle colonne d'étiquette
#   dplyr::mutate(
#     labs = sprintf(
#       # Joindre le nom d'un point eBird et le nombre d'espèces.
#       fmt = "%s — %s sp.",
#       locName,
#       numSpeciesAllTime
#     ),
#     ratio_sp_chkclst = ( numSpeciesAllTime / numChecklistsAllTime * 100 ) |> round(digits = 2)
#   )
#
