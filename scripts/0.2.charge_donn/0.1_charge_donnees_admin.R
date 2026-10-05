## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Charge les données des régions du Québec avec le bon CRS

message("Charge : 'regqc' (MRC du Québec)")
# Charger les Régions du Québec

regqc <- duckspatial::ddbs_open_dataset(
  path = file.path("data/mod", "munic_s.parquet")
) |>
  st_as_sf() |>
  sf::st_transform(
    crs = projetCRS
  )
