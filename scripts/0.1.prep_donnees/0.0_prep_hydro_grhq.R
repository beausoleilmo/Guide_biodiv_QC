# Téléchargement des fichiers du GRHQ sous format GDB
library(httr)
library(jsonlite)
library(stringr)

# 1. Define output directory and target API URL
output_dir <- "/Volumes/g_magni/data/DonneeQuebec/hydro/donnees_qc_downloads"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data <- httr2::request(
  "https://www.donneesquebec.ca/recherche/api/3/action/datastore_search"
) |>
  httr2::req_url_query(
    resource_id = "4ef6703f-0ecb-46a0-bdb4-7074f00ff514",
    limit = 100
  ) |>
  httr2::req_perform() |>
  httr2::resp_body_json()

# Extraction des valeurs du schéma JSON
# Pluck the records array, bind into a data frame, and filter targeted column
download_urls <- data |>
  purrr::pluck("result", "records") |>
  purrr::map_df(as_tibble) |>
  dplyr::filter(grepl(".zip", x = FGDB)) |>
  dplyr::pull(FGDB) |>
  unique()

# 4. Download and unzip all files
# for (fichier_i in seq_along(download_urls)) {
#
# 15 : fichier 10_2.zip avait un problème...
# Error in unzip(dest_path, exdir = unzip_dir) :
#   Cannot extract entry `GRHQ_10BC.gdb/a00000001.freelist` from archive `/Volumes/g_magni/data/DonneeQuebec/hydro/do
# nnees_qc_downloads/10_2.zip`: unsupported feature @rzip.c:198 (R_zip_error_handler)
# BESOIN d'utiliser le terminal sur macOS
# --> unzip 10_2.zip
for (fichier_i in 16:length(download_urls)) {
  url <- download_urls[fichier_i]
  file_name <- basename(url)
  dest_path <- file.path(output_dir, file_name)
  options(timeout = 3600)
  # Download using httr2 file request (handles binary correctly)
  cat(sprintf(
    "[%d/%d] Téléchargement: %s ...\n",
    fichier_i,
    length(download_urls),
    file_name
  ))

  request(url) |>
    req_perform(path = dest_path)

  # Unzip to a dedicated subfolder per file
  unzip_dir <- file.path(output_dir, tools::file_path_sans_ext(file_name))
  cat(sprintf("      Unzipping vers: %s ...\n", unzip_dir))
  unzip(dest_path, exdir = unzip_dir)
}

cat("\nFait! Fichiers téléchargé et unzipped.\n")
