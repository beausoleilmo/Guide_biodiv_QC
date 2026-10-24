source("./scripts/99.pipeline/99.0_prep.R")
source("./scripts/99.pipeline/99.1_obtenir_donnees_GBIF.R")
source("./scripts/99.pipeline/99.2_charge_donnees.R")
source(
  file = file.path(
    "./scripts/5.1.prep_data.R"
  )
)
colnames(gbif_prep_type_fr_h3)

ds_key <- gbif_prep_type_fr_h3 |>
  dplyr::filter(nom_sci == "Danaus plexippus") |>
  dplyr::count(datasetKey) |>
  dplyr::collect()


# Vecteur vide à remplir selon résultat de l'API de GBIF
getnames_dataset <- NULL
# Calcul nombre d'élément
len_dat <- length(ds_key$datasetKey)
# compteur
w <- 1

# Boucle pour chercher le nom des dataset
for (dataset_key_i in ds_key$datasetKey) {
  message(sprintf("%03d/%d, %s", w, len_dat, dataset_key_i))
  w <- w + 1

  # Extraire information du GBIF
  org_df <- rgbif::dataset_get(uuid = dataset_key_i)

  puborg <- datasetkey_df |>
    dplyr::filter(datasetKey == dataset_key_i) |>
    dplyr::pull(publishingOrgKey)
  # Organiser les données retournées par GBIF dans un tableau
  df_out <- data.frame(
    name = org_df$title,
    doi = ifelse(
      test = length(org_df$doi) == 0,
      yes = "",
      no = org_df$doi
    ),
    datasetKey = dataset_key_i,
    publishingorgkey = puborg
  )
  # Ajouter au tableau
  getnames_dataset <- rbind(getnames_dataset, df_out)
}

#
getnames_dataset |>
  # Ajouter le nombre d'occurrence par datasetKey
  dplyr::left_join(y = ds_key, by = join_by(datasetKey))
