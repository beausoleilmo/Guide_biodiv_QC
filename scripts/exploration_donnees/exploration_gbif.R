## Exploration des données GBIF
## Constats :
## NOTE :
##  -> filtrer pour enlever les lignes avec individualCount == 0
##  -> Extraction d'information supplémentaire des datasetKey en utilisant l'API de GBIF
##     Utile pour communiquer d'où viennent les données (musées, science citoyenne, etc.)
source("./scripts/99.pipeline/99.0_prep.R")


out_gbif_pq <- file.path(
  "data/raw/biodiv/gbif_data",
  "occurrence_trans.parquet"
)

out_gbif_pq <- file.path(
  "output/biodiv/gbif_data",
  sprintf("gbif_prep_h3_res_%s.parquet", res_fine)
)

# Charge les données en mémoire
con <- evolovr::setup_duckdb()
gbif_raw <- duckspatial::ddbs_open_dataset(path = out_gbif_pq, conn = con)
# Exploration des colonnes
colnames(gbif_raw)

res <- 10L
occStat <- c("PRESENT")
basisRec <- c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION", "PRESERVED_SPECIMEN")
taxRank <- c("SPECIES", "SUBSPECIES", "VARIETY")
kingdm <- c("Chromista", "Fungi", "Plantae", "Animalia")
coordUncertainM <- 200

out_admin_pq <- file.path(
  "output",
  "admin",
  sprintf("admin_mun_res_%s.parquet", res)
)
message("Lecture de la grille admin-H3")
admin_h3_idx_precalc <- dplyr::tbl(
  src = con,
  from = dbplyr::sql(
    glue::glue(
      "SELECT * FROM read_parquet('{out_admin_pq}')"
    )
  )
)

gbif_raw |>
  dplyr::filter(
    # is.na(species),
    basisOfRecord %in% basisRec,
    decimalLatitude >= 50,
    occurrenceStatus %in% occStat,
    taxonRank %in% taxRank,
    kingdom %in% kingdm,
    coordinateUncertaintyInMeters <= coordUncertainM |
      is.na(coordinateUncertaintyInMeters)
  ) |>
  # Création colonne H3
  dplyr::mutate(
    # Utilisation de dbplyr::sql pour forcer l'évaluation par DuckDB
    h3_cell = dbplyr::sql(
      glue::glue(
        # h3_latlng_to_cell necessite l'extension duckdb H3
        "h3_latlng_to_cell(
             ST_Y(geometry), ST_X(geometry),
             {as.integer(res)}
          )"
      )
    )
  ) |>

  # Ajout de l'information administrative
  # --> trouver l'intersection entre X et Y
  dplyr::inner_join(
    admin_h3_idx_precalc,
    by = "h3_cell"
  ) |>
  duckspatial::ddbs_write_dataset(
    path = "output/biodiv/gbif_data/gbif_test.parquet",
    overwrite = TRUE
  )
dplyr::collect()

gbif_raw |>
  filter(gbifID == "5054292101") |>
  dplyr::select(
    species,
    basisOfRecord,
    occurrenceStatus,
    taxonRank,
    kingdom,
    coordinateUncertaintyInMeters
  ) |>
  collect() |>
  t()

gbif_raw |>
  count(occurrenceStatus)

# NOTE : certains individualCount sont == 0, mais occurrenceStatus == PRESENT
# --> les retirer?
idzero <- gbif_raw |>
  filter(individualCount == 0) |>
  select(gbifID:recordNumber, kingdom:year) |>
  collect()


frq_summary <- gbif_raw |>
  count(individualCount) |>
  collect() |>
  arrange(individualCount) |>
  mutate(indc = as.factor(individualCount))
# filter(n > 1000)

# Plotting the precomputed frequencies
ggplot(frq_summary, aes(x = indc, y = log(n))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Frequency Chart",
    x = "Category",
    y = "Count"
  ) +
  theme_minimal()

# ligne log-log pour individualCount et nombre de fois dans les données
ggplot(frq_summary, aes(x = log(individualCount + 1), y = log(n + 1))) +
  geom_line() +
  labs(
    title = "Frequency Chart",
    x = "Category",
    y = "Count"
  ) +
  theme_minimal()


###
### Extraction des informations sur les bases de données de GBIF
### NOTE : Les données peuvent être utilisé pour faire un sommaire des données
### présente et communiquer la contribution citoyenne, muséale, etc.
### Permet aussi de joindre aux données GBIF pour obtenir toute l'information
### des bases de données.
# Compte nombre d'occurrence pour datasetkey
count_datasetkey <- gbif_raw |>
  count(datasetKey, institutionCode, basisOfRecord) |>
  collect() |>
  arrange(-n)

uuids <- count_datasetkey$datasetKey
message(
  sprintf("Appel API GBIF pour %s datasetKey", length(uuids))
)

tictoc::tic("Extraire les informations des datasetKey")
# Helper function to collapse lists/vectors into a single comma-separated string
collapse_field <- function(val) {
  if (is.null(val) || length(val) == 0) {
    return(NA_character_)
  }
  # Flatten nested lists or vectors and join with commas
  paste(unlist(val), collapse = ", ")
}

# Fetch details for each UUID and combine results
dataset_details <- imap(uuids, function(id, i) {
  if (i %% 50 == 0 || i == 1) {
    message(sprintf("Processing item %d of %d...", i, length(uuids)))
  }

  res <- dataset_get(uuid = id)

  tibble(
    title = collapse_field(res$title),
    type = collapse_field(res$type),
    category = collapse_field(res$category),
    key = collapse_field(res$key),
    publishingOrganizationKey = collapse_field(res$publishingOrganizationKey),
    doi = collapse_field(res$doi),
    license = collapse_field(res$license)
  )
})
tictoc::toc()

df_datasetkey <- dataset_details |>
  bind_rows()

df_sommaire_dk <- count_datasetkey |>
  left_join(
    y = df_datasetkey,
    by = join_by(
      datasetKey == key
    )
  )

write.csv(
  df_sommaire_dk |>
    dplyr::relocate(type, category, .after = n),
  file = file.path(
    "output/biodiv/gbif_data/datasetkey_info.csv"
  )
)
