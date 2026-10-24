## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Téléchargement des données
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> script de pipeline roule DANS L'ORDRE les différentes
#       manipulations nécessaires.
#

# Chemin d'accès pour télécharger les résultats brute de GBIF
chemin_sortie <- file.path('/Volumes/g_magni/gbif_data')

# Chemin d'accès pour exporter les métadonnées GBIF et le fichier GBIF
# à copier localement
path_out <- file.path("data/raw/biodiv/gbif_data")

message(sprintf(
  "Création polygones :%s
Téléchargement GBIF :%s
Transformation données GBIF : %s",
  polygone_qc_gbif,
  telecharge_gbif_donnee,
  transforme_gbif_donnee
))
if (polygone_qc_gbif) {
  # Préparation d'un polygone simplifié du Québec pour le téléchargement de GBIF
  message("--> Charger polygone pour extraire données GBIF")
  source(
    file.path(
      "scripts/0.2.charge_donn/0.1_charge_donnees_admin.R"
    )
  )

  # Création d'un polygone pour le Québec simplifié utilisé comme filtre pour
  # GBIF.
  source(
    file.path(
      "scripts/1.extract/01.gbif_polygon_filter_occurences.R"
    )
  )
}

# Requête de téléchargement des données GBIF
if (polygone_qc_gbif & telecharge_gbif_donnee) {
  # NOTE : vous devez mettre ces variables en mémoire
  # pour que cela fonctionne
  # usethis::edit_r_environ()
  # Mettre vos identifiant,
  # voir https://docs.ropensci.org/rgbif/articles/gbif_credentials.html
  # GBIF_USER="username"
  # GBIF_PWD="safe_fake_password_123"
  # GBIF_EMAIL="username@gbif.org"

  # Mettre le fichier en mémoire avec la modification
  readRenviron("~/.Renviron")

  message("--> Envoyer requête et téléchargement des données GBIF")

  # Permet de construire la requête GBIF et de démarrer le téléchargement
  # Voir le statut en ligne sur le compte GBIF_USER
  # browseURL(url = "https://www.gbif.org/user/download")
  # Prend du temps chez GBIF pour faire la requête
  source(
    file.path(
      "scripts/1.extract/02.commande_download_gbif.R"
    )
  )
}


if (transforme_gbif_donnee) {
  # NOTE :
  # Les données de GBIF doivent être préparés pour faire les analyses.
  #
  # Lire les métadonnées
  meta_data <- read.csv2(
    file = "data/raw/biodiv/gbif_data/metadata_gbif.csv"
  )
  # Extraire la clée la plus récente
  gbif_recent_key <- meta_data |>
    mutate(datetime = lubridate::ymd_hms(created)) |>
    slice_max(order_by = created, n = 1) |>
    pull(key)

  message("--> Transforme les données duckdb")
  # Exportation des données originales Apache Spark en
  # Parquet simple pour duckdb.

  # Code de téléchargement pour les données GBIF
  # "0010290-260519110011954"
  code_donnees_gbif <- gbif_recent_key
  # Voir les informations de ce téléchargement
  # browseURL(
  #   url = sprintf(
  #     "https://www.gbif.org/fr/occurrence/download/%s",
  #     code_donnees_gbif
  #   )
  # )
  # Le dossier est nommé en fonction de la clé GBIF
  data_path <- file.path(
    chemin_sortie,
    code_donnees_gbif
  )

  # Fichier de sortie
  fichier_sortie <- file.path(data_path, "occurrence_trans.parquet")

  # Si le fichier de sortie existe, ne pas rouler de nouveau
  if (!file.exists(fichier_sortie)) {
    message(sprintf("Données GBIF %s", code_donnees_gbif))
    message(sprintf("Exportation du fichier %s", fichier_sortie))
    # Préparation des données téléchargés en données standardisées
    # Spatiales
    # 20s-150s environ
    devtools::load_all("../evolovr")
    # evolovr::
    transforme_gbif(
      entree = file.path(data_path, "occurrence.parquet"),
      sortie = fichier_sortie,
      compression = "zstd",
      compression_level = 7
    )

    message(sprintf("Copie du fichier %s", path_out))
    # Copier les données transformées localement
    file.copy(
      from = fichier_sortie,
      to = path_out,
      overwrite = TRUE
    )
    # Vérification :
    # Lire tous les fichiers sauf ceux problématique
    # Compte le nombre de lignes
    # -- Extraction des fichiers du téléchagement de GBIF
    # SET variable gb_files = (
    #    SELECT list(file)
    #    FROM glob('/Volumes/g_magni/gbif_data/0016316-260903145123482/occurrence.parquet/*')
    #     WHERE file NOT LIKE '%/000000'
    #      AND file NOT LIKE '%/._%'
    #      );
    # -- lire les données
    # SELECT count(*) AS nombre_lignes
    # FROM read_parquet(getvariable('gb_files')) LIMIT 10;
    # -- comparaison avec les données sorties
    # SELECT count(*) AS nb
    # FROM read_parquet('/Volumes/g_magni/gbif_data/0016316-260903145123482/occurrence_trans.parquet');
    #
  }

  # Prépare la grille H3 + filtre données GBIF (jointure spatiale avec ADMIN)
  # La grille admin-H3 est préparé si pas existante
  # des fichiers log sont créé pour connaître le contenue des données de GBIF
  # Jointure des données de grille admin-H3 avec données GBIF
  # 140 sec environ
  source(
    file = file.path(
      "scripts/2.transf_donn/2.transform_gbif_data_admin_region.R"
    )
  )
}
