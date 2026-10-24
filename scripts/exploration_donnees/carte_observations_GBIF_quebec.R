# Carte des observations GBIF du Québec
# NOTE : voir dans le Nord, il y a moins d'observation.
# Est-ce que cela veut dire qu'on en connait moins? Pas nécessairement, il
# faudrait aussi inclure les localisations de populations autochtones,
# inuits, etc. et parler de connaissance traditionnelles.

# Compte le nombre de LIGNES dans les données GBIFs
# --> Pour ajouter dans le titre du graphique afin de communiquer
# des infos sur les données.
compte_pts <- gbif_prep_type_fr_h3 |> count() |> collect() |> pull()

# Compte sur une grille pour faire un raster
grid_resolution <- 0.1 # Smaller number = higher resolution grid
binned_data <- gbif_prep_type_fr_h3 |>
  mutate(
    grid_lon = round(decimalLongitude / grid_resolution) * grid_resolution,
    grid_lat = round(decimalLatitude / grid_resolution) * grid_resolution
  ) |>
  group_by(grid_lon, grid_lat) |>
  summarise(point_count = n(), .groups = "drop") |>
  collect()

# Construire un raster avec les données X, y et 'z' étant le compte
rast_binned <- terra::rast(
  x = binned_data,
  type = "xyz",
  crs = paste0("epsg:", 4326)
) |>
  terra::project(
    y = paste0("epsg:", projetCRS)
  )

### Le désavantage majeur de la carte sans information spatiale
### est que nous ne pouvons pas facilement changer la projection et
### combiner avec d'autres données spatiales
# message("Carte d'observations au Québec (sans info spatiale)")
# Carte du Québec avec les observations aggrégées
# carte_obs_gbif <- ggplot(
#   data = binned_data,
#   aes(x = grid_lon, y = grid_lat, fill = point_count)
# ) +
#   geom_tile() +
#   scale_fill_viridis_c(option = "magma", trans = "log10") +
#   labs(
#     title = "DuckDB-Aggregated 2D Density Map",
#     subtitle = sprintf("%s Points", compte_pts),
#     fill = "Count (Log Scale)"
#   ) +
#   theme_minimal()
#
# print(carte_obs_gbif)

message("Carte d'observations au Québec (avec info spatiale)")
# Avec les informations spatiales, on peut plus facilement ajouter
# des couches spatiales supplémentaire (comme le contour du Québec avec
# l'union de toutes les régions du Québec)

r_bin <- range(binned_data$point_count)
round(r_bin[2], digits = -6)
# Afficher la carte avec plot
# plot(
#   log10(rast_binned),
#   main = "DuckDB-Aggregated 2D Density Map",
#   sub = sprintf("%s Points", compte_pts)
# )
# plot(regqc_union, add = TRUE)

# Nouvelle carte avec tidyterra (pour le raster)
carte_obs_gbif <- ggplot2::ggplot() +
  # Ajout du contour du Québec
  ggplot2::geom_sf(
    data = regqc_union,
    # fill = NA,
    fill = "grey90",
    inherit.aes = FALSE,
    colour = "black",
    linewidth = .5
  ) +
  # Dessiner le raster
  tidyterra::geom_spatraster(
    data = rast_binned
  ) +
  # Changer l'affichage des couleurs et transformer les données en log
  ggplot2::scale_fill_viridis_c(
    option = "magma",
    trans = "log10",
    na.value = "transparent",
    # Modification de l'échelle pour avoir un visualisation log, mais une
    # échelle 'originale'
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::label_comma()
  ) +
  ggplot2::labs(
    title = "Aggrégation DuckDB carte densité-2D",
    subtitle = sprintf("%s Points", compte_pts),
    fill = "Compte (Échelle Log)"
  ) +
  ggplot2::theme_minimal()

print(carte_obs_gbif)
