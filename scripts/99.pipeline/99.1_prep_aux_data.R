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


# TODO : SIMPLIFIER CETTE ÉTAPE PUIS ERREUR DANS UNZIP AVEC R
# En attendant, FALSE pour ne pas rouler cette partie
if (FALSE) {
  message("--> Prépare les données GRHQ")
  source(
    file = file.path(
      "scripts/0.1.prep_donnees/0.0_prep_hydro_grhq.R"
    )
  )

  # TODO : finaliser cette partie aussi (CLEAN UP)
  # Mettre les données en Parquet
  source(
    file = file.path(
      "scripts/0.1.prep_donnees/0.1_prep_grhq_parquet.R"
    )
  )
}
