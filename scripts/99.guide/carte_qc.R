# Lire les données des régions administratives
regqc <- duckspatial::ddbs_open_dataset(file.path(
  "data/mod",
  "munic_s.parquet"
))
# lire le GRHQ
grhq <- duckspatial::ddbs_open_dataset(file.path(
  "data/mod",
  "grhq_rhs_simple.parquet"
))
# Union les données admin
re <- regqc |> ddbs_union()
# préparation données grhq
tgrh <- ddbs_transform(grhq, y = re)
# intersection pour ne garder que ce qui recoupe admin
tictoc::tic()
filtered_streams_db <- duckspatial::ddbs_intersection(
  tgrh,
  (re)
)
tictoc::toc()

# Exportation des données pour un accès plus rapide
duckspatial::ddbs_write_dataset(
  data = filtered_streams_db,
  path = "data/mod/grhq_rhs_int.parquet",
  overwrite = TRUE
)


# Lire les régions
regqc <- duckspatial::ddbs_open_dataset(file.path(
  "data/mod",
  "munic_s.parquet"
)) |>
  sf::st_as_sf() |>
  # Carte plus simple et rapide à dessiner
  sf::st_simplify(dTolerance = 1e3)


runion_raw <- regqc |>
  group_by(MUS_NM_REG, MUS_CO_REG) |>
  summarise(geom = st_union(geometry)) |>
  mutate(
    # MRS_CO_REG = gsub("\\b0(\\d)", "\\1", as.numeric(MRS_CO_REG))|> as.character(),
    code_reg = paste(MUS_CO_REG, MUS_NM_REG, sep = " ")
  )

region_noms <- tibble::tribble(
  ~MUS_CO_REG , ~MUS_NM_REG                     , ~MUS_NM_REG_short ,
  "01"        , "Bas-Saint-Laurent"             , "Bas-St-Laurent"  ,
  "02"        , "Saguenay–Lac-Saint-Jean"       , "Saguenay–LSJ"    ,
  "03"        , "Capitale-Nationale"            , "C.-Nationale"    ,
  "04"        , "Mauricie"                      , "Mauricie"        ,
  "05"        , "Estrie"                        , "Estrie"          ,
  "06"        , "Montréal"                      , "Montréal"        ,
  "07"        , "Outaouais"                     , "Outaouais"       ,
  "08"        , "Abitibi-Témiscamingue"         , "Abitibi-T"       ,
  "09"        , "Côte-Nord"                     , "Côte-N."         ,
  "10"        , "Nord-du-Québec"                , "Nord-Qc"         ,
  "11"        , "Gaspésie–Îles-de-la-Madeleine" , "Gaspésie–ÎDLM"   ,
  "12"        , "Chaudière-Appalaches"          , "Chaudière-A."    ,
  "13"        , "Laval"                         , "Laval"           ,
  "14"        , "Lanaudière"                    , "Lanaudière"      ,
  "15"        , "Laurentides"                   , "Laurentides"     ,
  "16"        , "Montérégie"                    , "Montérégie"      ,
  "17"        , "Centre-du-Québec"              , "Centre-Qc"
) |>
  mutate(
    # MRS_CO_REG = gsub("\\b0(\\d)", "\\1", as.numeric(MRS_CO_REG)) |> as.character(),
    code_reg_small = paste(MUS_CO_REG, MUS_NM_REG_short, sep = " ")
  )


runion <- runion_raw |>
  left_join(
    y = region_noms,
    by = join_by(MUS_CO_REG, MUS_NM_REG)
  )


## Carte du Québec

# ```{r maps, echo=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=5, out.width="70%", fig.dpi=192}
# Faire la carte
carte_qc <- runion |>
  ggplot() +
  geom_sf(
    linewidth = 3,
    colour = "white"
  ) +
  geom_sf(
    # show.legend = FALSE,
    mapping = aes(
      fill = code_reg_small,
      colour = code_reg_small
    )
  ) +
  ggrepel::geom_label_repel(
    max.overlaps = Inf,
    segment.square = FALSE,
    segment.inflect = TRUE,
    min.segment.length = 0.5,
    label.size = 0,
    fill = scales::alpha("white", .5),
    aes(label = MUS_CO_REG, geometry = geom),
    stat = "sf_coordinates"
  ) +
  coord_sf(crs = 32198) +
  scale_fill_viridis_d(name = "Région") +
  scale_colour_viridis_d(name = "Région") +
  # guides(fill = guide_legend(ncol = 2)) +
  theme_void() +
  theme(
    legend.text = element_text(size = 8),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(
      fill = alpha("white", 0.5),
      color = alpha("white", 0.25),
      linewidth = 0.25
    )
  )

# carte_qc
# ggsave(plot = carte_qc, filename = "../img/carte_qc.png", width = 7, height = 5)
# ggsave(
#   plot = carte_qc,
#   filename = "img/carte_qc2.png",
#   width = 7,
#   height = 5
#   )
# ```
