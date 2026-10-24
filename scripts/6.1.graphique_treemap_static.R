## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-28
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Exploration des données selon le nombre d'observations ou d'espèces
#   --> Création des graphiques TreeMaps
#   --> treemapify permet de créer des graphique 'treemap' avec ggplot2.
#       Par contre, c'est suuuuuuuper long à afficher, donc treemap ça sera!

### duckdb cmd : extraire info sommaire  -------------
# Charge les données
# source(
#   file = file.path(
#     incub, "scripts/partie_2/6.0.load_compte_especes.R"
# ))

# Regarder les données
count_sp |> arrange(desc(n)) |> head()

# Enlever les taxonomies manquantes est important pour ggplot `treemapify`
especes_compte_taxo_sans_na <- count_sp |>
  filter_out(
    is.na(class) |
      is.na(order) |
      is.na(family)
  )
# Nombre de rangées retirés pour les taxonomies manquantes
nrow(count_sp) - nrow(especes_compte_taxo_sans_na)
# Compte le nombre d'éléments qui n'ont un NA dans class, order, family
# aned <- count_sp |>
# mutate(
#   order = dplyr::case_when(
#     family == "Ampharetidae" ~ "Terebellida",
#     .default = order
#   )
# )

#
# Exploration de la taxonomie de ce speciesKey
# taxinfo <- rcol::col_usage_info(id = "4HGZH")

# Exploration de la taxonomie de PLUSIEURS speciesKey
# keys <- c("4HGZH", "3KD5D")

# Query each key individually and bind into one data frame
# taxinfo_list <- lapply(keys, function(id) {
#   info <- rcol::col_usage_info(id = id)
#   return(info)
# })

# tax_summary <- lapply(taxinfo_list, function(res) {
#   # Note: Adjust fields depending on whether res contains a $usage or $classification element
#   data.frame(
#     # id = res$id %||% NA,
#     scientificName = res$names$scientificName[1] %||% NA,
#     class = (res$classification |> filter(rank == "class") |> pull(name)) %||%
#       NA,
#     order = (res$classification |> filter(rank == "order") |> pull(name)) %||%
#       NA,
#     family = (res$classification |> filter(rank == "family") |> pull(name)) %||%
#       NA
#   )
# })  |> dplyr::bind_rows()

# Treemap
# Observation : Montre le biais d'échantillonnage du nombre
# d'observations surtout pour les oiseaux !
treemap(
  dtf = especes_compte_taxo_sans_na,
  index = c(
    "class",
    "order",
    "family"
  ), # The hierarchy levels
  vSize = "n", # Area based on the count
  title = "Hiérarchie imbriquée: COFa",
  fontsize.labels = 10,
  bg.labels = 150,
  palette = "Set3"
)

# Similaire à treemap, mais beaucoup plus long à calculer
# library(treemapify)
# library(ggplot2)
# ggplot(
#   especes_compte_taxo_sans_na  |>  slice_sample(n = 100),
#   aes(
#     area = n,
#     fill = class,
#     subgroup = class,
#     subgroup2 = order,
#     subgroup3 = family
#   )
# ) +
#   # Surface principale des rectangles
#   treemapify::geom_treemap() +
#
#   # Frontières visuelles pour chaque niveau de hiérarchie
#   treemapify::geom_treemap_subgroup3_border(colour = "white", size = 0.5) +
#   treemapify::geom_treemap_subgroup2_border(colour = "white", size = 1.5) +
#   treemapify::geom_treemap_subgroup_border(colour = "black", size = 3) +
#   #
#   # Étiquettes de texte pour le dernier niveau (famille)
#   treemapify::geom_treemap_subgroup3_text(
#     aes(label = family),
#     place = "center",
#     grow = FALSE,
#     alpha = 0.7,
#     colour = "black"
#   ) +
#
#   # Palette de couleurs et titre
#   scale_fill_brewer(palette = "Set3") +
#   labs(
#     title = "Hiérarchie imbriquée: COFa",
#     fill = "Classe"
#   ) +
#   theme_minimal()

## Zoom sur certains groupes

# Montre seulement les mammifères
# Observation : des écureuils partout!
treemap(
  dtf = especes_compte_taxo_sans_na |>
    filter(class == "Mammalia"),
  index = c(
    "order",
    "family",
    "label"
  ), # The hierarchy levels
  vSize = "n", # Area based on the count
  title = "Hiérarchie imbriquée: Mammifères",
  palette = "Set3"
)

# Montre seulement les papillons
# Observation : Danaus plexippus très rapporté!
treemap(
  dtf = especes_compte_taxo_sans_na |> filter(order == "Lepidoptera"),
  index = c(
    "family",
    "sp2w"
  ), # The hierarchy levels
  vSize = "n", # Area based on the count
  title = "Hiérarchie imbriquée: Papillons",
  palette = "Set3"
)


# Montre seulement les oiseaux
# Observation : Certains oiseaux très présents
treemap(
  dtf = especes_compte_taxo_sans_na |> filter(order == "Passeriformes"),
  index = c(
    "family",
    "nom_fr"
  ), # The hierarchy levels
  vSize = "n", # Area based on the count
  title = "Hiérarchie imbriquée: Oiseaux, Passeriformes",
  palette = "Set3"
)

# Met emphase sur le NOMBRE *D'ESPÈCES* UNIQUE
# DANS FAMILLE selon observations faites
treemap(
  dtf = hierarchy_counts,
  # The hierarchy levels
  index = c(
    # "kingdom",
    "class",
    "order",
    "family"
  ),
  vSize = "n", # Area based on the count
  title = "Hiérarchie imbriquée: COFa",
  palette = "Set3"
)

# Simplification avec seulement
# certains groupes d'organismes
treemap(
  dtf = hierarchy_counts |>
    filter(
      kingdom == "Animalia",
      # phylum != "Chordata"

      class %in%
        c(
          "Amphibia",
          "Arachnida",
          "Aves",
          "Chilopoda",
          "Gastropoda",
          "Insecta",
          "Malacostraca",
          "Mammalia",
          "Squamata",
          "Testudines"
        )
    ),
  index = c(
    #"kingdom",
    # "phylum",
    "class",
    "order",
    "family"
  ), # The hierarchy levels
  vSize = "n", # Area based on the count
  title = "Hiérarchie imbriquée: COFa",
  palette = "Set3"
)

# Tree graph (visualisation similaire à précédente, mais beaucoup de texte!)
treegraph(
  dtf = hierarchy_counts |>
    filter(
      kingdom == "Animalia",
      class %in%
        c(
          "Amphibia",
          "Arachnida",
          "Aves",
          "Chilopoda",
          "Gastropoda",
          "Insecta",
          "Malacostraca",
          "Mammalia",
          "Pycnogonida",
          "Squamata",
          "Testudines"
        )
    ),
  directed = FALSE,
  index = c("class", "order", "family"),
  show.labels = T,
  vertex.label.dist = .3,
  vertex.label.cex = 0.5
)
