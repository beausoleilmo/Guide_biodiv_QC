# Préparation des données auxiliaires
message("--> Prépare les données de région pour carto")
source(
  file.path(
    "scripts/0.1.prep_donnees/0.0_prep_admin_parquet.R"
  )
)

message("--> Prépare les données des Points eBird")
source(
  file = file.path(
    "scripts/0.1.prep_donnees/0.0_prep_ebird_hotspots_parquet.R"
  )
)
