## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Téléchargement avec filtrer et envoyer requête avec l'API de GBIF
#   -->
#   --> Il est possible de lire les données directement d'un parquet sur AWS S3
#       https://data-blog.gbif.org/post/apache-arrow-and-parquet/
#   --> Finalement, prendre toutes les données de GBIF.
#       Il sera possible de faire des statistiques sommaires.
#       Beaucoup de données d'occurrences proviennent de collection
#       muséales. Les herbiers du Québec sont particulièrement intéréssants.
#       C'est plus de 1.5 million de données à analyser.

## ____________####
## Prépare l'environnement --------

# Mettre le fichier en mémoire avec la modification
readRenviron("~/.Renviron")

# Script simplification des régions administratives du Québec
# Polygon doit être en ordre 'sens-des-aiguilles-montre' ou CW
# source(file.path(
#   incub,
#   "scripts/partie_2/1.extract/01.gbif_polygon_filter_occurences.R"
# ))

# Visualisation du polyogne
# mapview(st_as_sfc(pol_wkt, crs = 4326))

### Construire requête

fields_parquet <- occ_download_describe(x = "simpleParquet")
fields_parquet$fields


### Requête envoyé GBIF -----------------------------------------------------

# Requête de téléchargement à GBIF avec filtres
## Voir aussi : occ_download_prep pour TESTER avant d'envoyerla requête!
## download_key est de class occ_download
download_key <- rgbif::occ_download(
  # body = request_body,
  # Organisme présent
  # rgbif::pred(key = "OCCURRENCE_STATUS", value = "present"),
  # Prendre ce qui est observé (et non des spécimens de musées)
  # rgbif::pred_in(
  #   key = "BASIS_OF_RECORD",
  #   value = c("MACHINE_OBSERVATION", "HUMAN_OBSERVATION")
  # ),
  # filtre Spatial du Québec
  rgbif::pred_within(
    value = pol_wkt
  ),
  # Demande d'avoir un fichier parquet
  format = "SIMPLE_PARQUET"
)

attributes(download_key)

# NOTE : SIMPLE_PARQUET permet d'obtenir un dossier parquet avec des partitions.
# Par contre, le pipeline d'exportation Spark de GBIF ('backend' qui filtre et
# génère les données) donne des fichiers vide (e.g., 000000). Une étape de
# post process gère ces fichiers

# Si vous avez besoin d'annuler une requête
# occ_download_cancel(key=download_key)

# Voir les données demandées sur GBIF avec un nom d'utilisateur
# list_donnees = occ_download_list(user="beausoleilmo")
# View(list_donnees$results)

# Metadonnées
# occ_download_meta(key=download_key)

# État de la requête
rgbif::occ_download_wait(download_key)

# attributes(download_key)$downloadLink

### Télécharger les données -------------------------------------------------
# --> NOTE : Téléchargement long

# Si on télécharge le fichier à partir du portail, le fichier sera
# nommé "0010290-260519110011954.zip" et extrait comme "occurrence.parquet"

# Télécharge les résultats nommés 'download_key.zip'
# Le téléchargement est long (plus de 4GB compressé)
# Mon internet est rapide, mais je n'avais pas plus de 1-3 MB/s
options("timeout") # Regarder le timeout, 1 min
options(timeout = 35 * 60) # augmenter à 35 min * 60 sec/min = 2100s

# Téléchargement despz données GBIF
# 900 sec, 15 min par le web et 2000 s (34 min) avec R
tictoc::tic(sprintf("Téléchargement GBIF vers %s", chemin_sortie))
download_path <- rgbif::occ_download_get(
  key = download_key[1],
  path = chemin_sortie,
  overwrite = TRUE
)
tictoc::toc()
# téléchargement d'un fichier .zip
# "0040587-260226173443078.zip"
# "0010290-260519110011954.zip"

# unzip le fichier dans un dossier
message("Unzip fichier GBIF")
tictoc::tic("Unzip le fichier") # 50 s - 112 s
zip::unzip(
  zipfile = download_path[1],
  # Mettre les données dans ce dossier
  exdir = file.path(
    chemin_sortie,
    download_key[1]
  )
)
tictoc::toc()


### Construire les métadonnées  ---------------------------------------------
message("Construire les métadonnées")

# Liste les données préparé sur GBIF
rgbif::occ_download_list()

# Extraire les métadonnées
# "0010290-260519110011954"
meta <- occ_download_meta(
  key = download_key[1]
)

meta_data_gbif <- data.frame(
  key = meta$key,
  created = meta$created,
  doi = meta$doi,
  license = meta$license,
  status = meta$status,
  nb_records = meta$totalRecords,
  nb_datasets = meta$numberDatasets,
  size = meta$size,
  format = attributes(meta)$format,
  user = attributes(download_key)$user,
  email = attributes(download_key)$email,
  downloadLink = attributes(download_key)$downloadLink,
  citation = attributes(download_key)$citation,
  download_path = download_path[1]
)

file_export <- file.path(
  path_out,
  "metadata_gbif.csv"
)
key_to_check <- "key" # Your specific column name

# Vérifier si le fichier existe
if (file.exists(file_export)) {
  # Lire 'key'
  existing_keys <- readr::read_csv2(
    file = file_export,
    col_select = all_of(key_to_check)
  ) |>
    pull(!!sym(key_to_check))

  # Filtrer pour inclure ce qui n'est PAS dans existing_keys
  data_to_append <- meta_data_gbif |>
    filter(!(!!sym(key_to_check) %in% existing_keys))

  # Si le nb de rangées est plus que 0, ajouter
  if (nrow(data_to_append) > 0) {
    readr::write_excel_csv2(
      x = data_to_append,
      file = file_export,
      append = TRUE,
      col_names = FALSE
    )
  }
} else {
  # Si le fichier n'existe pas, il faut le créer
  readr::write_excel_csv2(
    x = meta_data_gbif,
    file = file_export
  )
}


## Citation des données
# ajout de la citation
citation_en <- rgbif::gbif_citation(x = meta)$download
# rgbif::gbif_citation(x = download_path)

# Fonction de conversion English GBIF citation to French
translate_gbif_citation_fr <- function(citation) {
  citation |>
    stringr::str_replace(
      "GBIF Occurrence Download",
      "Téléchargement d'occurrences GBIF"
    ) |>
    stringr::str_replace("Accessed from", "Consulté depuis") |>
    stringr::str_replace(" via ", " via ") |>
    stringr::str_replace(" on ", " le ")
}

# Apply the translation
citation_fr <- translate_gbif_citation_fr(citation_en)

# Output the result
cat(citation_fr)

## Si on filtre les données et qu'on veut une nouvelle citation
## --> derived_dataset()

# Carte d'occurence complète dans le coin du Québec
map_fetch(z = 4, x = 8:11, y = 2:4) |> plot()
