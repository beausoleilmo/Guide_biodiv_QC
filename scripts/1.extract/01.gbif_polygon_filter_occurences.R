## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> En utilisant les données spatiales des régions du Québec,
#       faire un large polygone simplifié pour filtrer et
#       extraire les données de GBIF.
#       Il est pratique de tester ce polygone sur le site web de
#       [GBIF](https://www.gbif.org) pour
#       voir s'il est assez simple.

## ____________####
## Prépare l'environnement --------

# source(
#   file.path(
#     incub,
#     "scripts/partie_2/0.1.charge_donn/0.1_charge_donnees_regions.R"
#   )
# )

message("Union et simplifier POLYGON pour GBIF")

tictoc::tic() # 1.5 sec
reg_union <- regqc |>
  # Faire 1 seul polygone
  sf::st_union() |>
  # CRS en mètre
  sf::st_transform(crs = 32198) |>
  # Simplification avec tolérance 1km
  sf::st_simplify(dTolerance = 1e3) |>
  # Tampon pour englober le Québec au complet
  sf::st_buffer(dist = 75e3) |>
  # Simplification du tampon
  sf::st_simplify(dTolerance = 5e4) |>
  # Transformer sfc (seulement colonne géométrie) en sf
  sf::st_as_sf() |>
  # Remettre en CRS d'origine
  sf::st_transform(crs = sf::st_crs(regqc)) |>
  # Nommer la colonne géométrie
  sf::st_set_geometry(value = "geometry")
tictoc::toc()

# Validation que le polygone simplifié englobe le Québec
# mapview::mapview(reg_union)+
# mapview::mapview(regqc)

# Le polygone pour GBIF doit être dans un certain ordre
lwgeom::st_is_polygon_cw(reg_union)

pol_lim_qc_fixed <- reg_union |>
  sf::st_geometry() |>
  # Rendre valide si ce n'est pas le cas
  sf::st_make_valid() |>
  # Rendre les points du polygone 'CW' clockwise ou horaire.
  lwgeom::st_force_polygon_cw() |>
  st_reverse()

# Vérification
lwgeom::st_is_polygon_cw(pol_lim_qc_fixed)

# Extraction des coordonnées
# (pol_wkt <- pol_lim_qc_fixed |>
#     # GBIF requier CRS epsg:4326 (WGS 84, latitude + longitude, non-projeté)
#     st_transform(crs = 4326) |>
#     # extraire le texte seulement
#     sf::st_as_text())

message("Polygone en CRS epsg:4326 (pour GBIF):")
pol_wkt <- pol_lim_qc_fixed |>
  # GBIF requier CRS epsg:4326 (WGS 84, latitude + longitude, non-projeté)
  st_transform(crs = 4326) |>
  # extraire le texte seulement
  sf::st_as_text()

# Afficher le polygone dans la console
cat(pol_wkt, fill = TRUE)

# Sur macOS, possible d'écrire dans le presse papier
clip <- pipe("pbcopy", "w")
write(pol_wkt, file = clip)
close(clip)
