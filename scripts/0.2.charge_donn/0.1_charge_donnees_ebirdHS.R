## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Charge les données points ebirds avec le bon CRS
#   NOTE : besoin de la EBIRD_KEY dans .Renviron
# @source :
message("Charge : 'ebird_hp_sf' (points chauds d'observations eBird)")
# Charger les Régions du Québec
ebird_hp_sf <- duckspatial::ddbs_open_dataset(
  path = file.path("data/mod", "ebird_hot_spots.parquet")
) |>
  st_as_sf() |>
  sf::st_transform(
    crs = projetCRS
  )
