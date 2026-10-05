## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-29
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> faire une carte du nombre d'observations avec une
#       sélection de Type_FR (et kingdom), autour d'une région sélectionné
#       en gardant toutes les données du Québec pour mettre en contexte la carte
#   -->

# Liste d'espèces avec Type_FR
# source("Incubateur/2025_05_24_Guide_biodiv_qc/scripts/partie_2/0.1_charge_donnees_sp_nm.R")

# Voir 5.1. Obtenir la liste des Type_FR pour en faire des facteurs
TYPE_FCT_LIST <- gbif_somme_query |>
  distinct(Type_FR) |>
  pull(Type_FR)

# Nom des régions
REGION_SEL_LIST <- unique(regqc$MUS_NM_REG)
MRC_SEL_LIST <- unique(regqc$MUS_NM_MRC)
# gbif_somme_query |> distinct(MUS_NM_REG) |> pull(MUS_NM_REG)
target_classes <- c(
  "Amphibiens",
  "Couleuvres",
  "Tortues",
  "Champignons",
  "Plantes",
  "Mammifères",
  "Poissons",
  "Bobittes",
  "Oiseaux"
)

message("Nb observation par h3_parent (mémoire)")
tictoc::tic() # 25 s
# NB observations par h3_parent des données GBIF (par kingdom et Type_FR)
gb_compte <- gbif_somme_query |>
  # Garde uniquement les niveaux taxonomiques désirés
  filter(reformat_taxo %in% target_classes) |>
  collect() |>
  # Colonne avec les type_FR en ordre
  mutate(
    tax_gr = factor(Type_FR, TYPE_FCT_LIST),
    reformat_taxo = factor(reformat_taxo, unique(reformat_taxo))
  ) |>
  st_as_sf(wkt = "wkt_geom", crs = 4326) |> # 4326 is standard WGS84 (Lat/Lon)
  # Retire z (données de profondeurs)
  st_zm()
tictoc::toc()

# Nombre de type (utiliser pour la cartographie et choix de couleurs)
nb_type <- length(unique(gb_compte$reformat_taxo))

message("Union région admin")
# union des régions administratives en pleine qualité  (limites régionales)
tictoc::tic() # 12 s
union_region <- regqc |>
  group_by(MUS_NM_REG) |>
  summarise(geometry = st_union(geometry)) |>
  mutate(area_m2 = st_area(geometry))

union_region <- union_region |>
  mutate(
    fact = units::drop_units(x = area_m2) / 130000,
    fact2 = sqrt(units::drop_units(x = area_m2)) / 10
  )
tictoc::toc()

# Union des Région ET MRC (limites régionales + MRC, pour carto)
tictoc::tic() # 12 s
union_MRC_region <- regqc |>
  group_by(MUS_NM_MRC, MUS_NM_REG) |>
  summarise(geometry = st_union(geometry))
tictoc::toc()

# Filtre région au besoin et simplification
reg_sel_spatial <- regqc |>
  dplyr::filter(MUS_NM_REG %in% REGION_SEL_LIST) |>
  st_simplify(dTolerance = 1e2)

# Filtre les points eBirds pour la région sélectionnée
ebird_hp_sf_reg <- ebird_hp_sf_admin |>
  st_filter(reg_sel_spatial)

## ____________####
## Gg_map --------

# Zoom sur région reg_idx
reg_idx <- 10 # 16 = Montréal, 10 = Lanaudière
(reg_sel <- REGION_SEL_LIST[reg_idx])

# Sélection d'une région pour en faire une bbox (pour zoomer)
reg_sel_map <- reg_sel_spatial |>
  filter(
    MUS_NM_REG == reg_sel
    # , MUS_NM_MRC == sort(MRC_SEL_LIST)[26]
  )

reg_bbox_original_crs <- reg_sel_map |>
  st_buffer(
    dist = union_region |> filter(MUS_NM_REG == reg_sel) |> pull(fact)
  ) |>
  st_bbox()

reg_bbox <- reg_sel_map |>
  st_buffer(
    dist = union_region |> filter(MUS_NM_REG == reg_sel) |> pull(fact2)
  ) |>
  st_transform(crs = 4326) |>
  st_bbox()

# Pour étiquette sur la carte (centroid du polygone)
pt_lab <- reg_sel_map |>
  st_union() |>
  st_centroid()

# Rogner les données spatiales pour un affichage plus rapide des résultats
cropped_gb_compte <- st_crop(
  gb_compte |> st_transform(crs = st_crs(reg_bbox_original_crs)),
  reg_bbox_original_crs
)
cropped_region <- st_crop(union_region, reg_bbox_original_crs)
cropped_MRC_region <- st_crop(union_MRC_region, reg_bbox_original_crs)
cropped_hpol <- st_crop(hpol, reg_bbox_original_crs)
cropped_reg_sel_spatial <- st_crop(reg_sel_spatial, reg_bbox_original_crs)
bbox_crs <- st_transform(reg_bbox, crs = st_crs(x = 32198))


message("Faire la carte : gg_carte_sommaire_obs")
# Afficher une carte pour une région seulement
gg_carte_sommaire_obs <- cropped_gb_compte |>
  filter(reformat_taxo == "Oiseaux") |>
  ggplot() +
  # Ajout des limites régionales (toutes les RÉGIONS)
  geom_sf(
    data = cropped_region,
    # fill = rep(
    #   viridis::inferno(
    #     n = length(
    #       cropped_region |>
    #         pull(MUS_NM_REG)
    #     ),
    #     alpha = .2
    #   ),
    #   nb_type # selon le nombre de 'type'
    # ),
    colour = scales::alpha('grey10', .5),
    # Lignes plus grosses
    linewidth = 0,
    inherit.aes = FALSE
  ) +
  # Séparation des données par groupe taxonomique de Type_FR
  facet_wrap(. ~ reformat_taxo, ncol = 4) +
  # Ajout des limites régionales (toutes les MRCs)
  geom_sf(
    data = cropped_MRC_region,
    fill = NA,
    colour = scales::alpha('grey10', .5),
    # Lignes plus petites que régions
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = cropped_hpol,
    fill = "lightblue",
    # Pas de ligne pour l'eau
    linewidth = 0,
    inherit.aes = FALSE
  ) +
  # Ajout des hexagones (voir gb_compte)
  geom_sf(
    mapping = aes(fill = log_n),
    colour = NA
  ) +
  # Ajout (lignes seulement) des limites régionales (toutes les RÉGIONS)
  # geom_sf(
  #   data = cropped_region,
  #   fill = NA,
  #   colour = scales::alpha('grey90', .5),
  #   # Lignes plus grosses
  #   linewidth = 2.5,
  #   inherit.aes = FALSE
  # ) +
  # Bordure (surbrillance) autour de la région sélectionnée
  geom_sf(
    # Données de la région sélectionné : union pour ne garder que le contour
    data = cropped_region |>
      filter(
        MUS_NM_REG == reg_sel
      ) |>
      st_union(),
    linewidth = 2,
    colour = scales::alpha("red", .5),
    fill = NA,
    inherit.aes = FALSE
  ) +
  # Bordure (surbrillance) autour de la MRC sélectionnée
  # geom_sf(
  #   # Données de la MRC sélectionné : union pour ne garder que le contour
  #   data = union_MRC_region |> filter(
  #     MUS_NM_REG == reg_sel
  #     , MUS_NM_MRC == sort(MRC_SEL_LIST)[26]
  #   ),
  #   linewidth = 2,
  #   colour = scales::alpha("orange", .5),
  #   fill = NA,
  #   inherit.aes = FALSE
  # ) +
  # Points eBirds
  # geom_sf(
  #   data = ebird_hp_sf_reg,
  #   mapping = aes(colour = numSpeciesAllTime),
  #   size = 2
  # ) +
  # Ajout des étiquettes
  # geom_sf_label(
  #   data = reg_sel_spatial,
  #   aes(label = MUS_NM_MRC),
  #   fun.geometry = sf::st_centroid, # Forces the label to the center of the polygon
  #   size = 2,
  #   color = "grey0",
  #   alpha = 0.8
  # ) +
  # Ajoute contour par dessus les hexagones pour toutes les municipalités
  geom_sf(
    data = cropped_reg_sel_spatial,
    fill = NA,
    colour = scales::alpha("grey50", alpha = .9),
    # Fine ligne
    linewidth = 0.2,
    inherit.aes = FALSE
  ) +
  # Ajout d'étiquette sur carte -----
  # geom_sf_label(
  #   data = pt_lab,
  #   aes(label = REGION_SEL_LIST[reg_idx]),
  #   inherit.aes = FALSE
  # ) +
  # Couleur des hexagones
  scale_fill_viridis_c(
    name = "Nombre \nd'observations",
    alpha = .95,
    labels = scales::trans_format(
      trans = "exp",
      format = scales::comma_format()
    )
  ) +
  scale_colour_viridis_c(option = "inferno", alpha = .6) +
  # Thème avec rien pour la carto
  theme_void() +
  # Ajout de quelques trucs pour la légende et l'apparence des titres de cartes
  theme(
    # legend.position = "bottom",
    plot.background = element_rect(fill = "white"),
    # Change background color of the strip
    strip.background = element_rect(
      fill = "white",
      color = "white",
      linewidth = 0.5,
      linetype = "solid"
    ),
    # Align text to the left within the strip box
    strip.text = element_text(
      hjust = 0,
      color = "black",
      face = "plain",
      size = 10
    )
  ) +
  labs(
    title = sprintf(
      "Région : %s \nRésolution H3 : %s",
      reg_sel, # "toutes", # region_sel,
      res_parent
    )
  ) +
  # Zoom un peu plus dans l'image
  coord_sf(
    xlim = c(bbox_crs["xmin"], bbox_crs["xmax"]),
    ylim = c(bbox_crs["ymin"], bbox_crs["ymax"]),
    expand = TRUE # Zoom avec espace autour
  )
gg_carte_sommaire_obs

ggsave(
  filename = file.path(
    "output",
    sprintf(
      "gg_carte_sommaire_obs_reg_%s.png",
      janitor::make_clean_names(reg_sel)
    )
  ),
  plot = gg_carte_sommaire_obs,
  dpi = 300,
  units = "cm",
  width = 25,
  height = 15
)


## ____________####
## Carte interactive --------

# Filtrer un groupe d'organismes
gb_h3pol_oi <- cropped_gb_compte |>
  filter(Type_FR == "Oiseaux")

which(duplicated(gb_h3pol_oi$h3_parent))

# Polygones biodiversité
mapview::mapview(gb_h3pol_oi, zcol = "log_n", label = "n") +
  # Ajout des régions
  mapview::mapview(
    cropped_reg_sel_spatial,
    zcol = "MUS_NM_MRC",
    col.regions = viridis::cividis(n = nrow(cropped_reg_sel_spatial)),
    legend = FALSE
  ) +
  # Points eBird pour interpréation :
  # Les hexagones qui ont BEAUCOUP d'observations sont souvent ceux de eBird!
  mapview::mapview(
    st_crop(ebird_hp_sf_admin, reg_bbox_original_crs),
    zcol = "numSpeciesAllTime",
    label = "labs",
    legend = FALSE,
    col.regions = viridis::inferno(n = nrow(ebird_hp_sf_reg))
  )
