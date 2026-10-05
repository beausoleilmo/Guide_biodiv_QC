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


### duckdb cmd : extraire info sommaire  -------------
# Charge les données 
# source(
#   file = file.path(
#     incub, "scripts/partie_2/6.0.load_compte_especes.R"
# ))

# Regarder les données 
count_sp |> arrange(desc(n)) |> head()


# Treemap 
# Observation : Montre le biais d'échantillonnage du nombre 
# d'observations surtout pour les oiseaux ! 
treemap(
  dtf = count_sp,
  index = c(
    "class", 
    "order", 
    "family"), # The hierarchy levels
  vSize = "n",                     # Area based on the count
  title = "Hiérarchie imbriquée: COFa",
  palette = "Set3")

## Zoom sur certains groupes 

# Montre seulement les mammifères 
# Observation : des écureuils partout! 
treemap(
  dtf = count_sp |> 
    filter(class == "Mammalia"),
  index = c(
    "order",
    "family", 
    "species"), # The hierarchy levels
  vSize = "n",                     # Area based on the count
  title = "Hiérarchie imbriquée: Mammifères",
  palette = "Set3")

# Montre seulement les papillons 
# Observation : Danaus plexippus très rapporté! 
treemap(
  dtf = count_sp |> filter(order == "Lepidoptera"),
  index = c(
    "family", 
    "species"), # The hierarchy levels
  vSize = "n",                     # Area based on the count
  title = "Hiérarchie imbriquée: Papillons",
  palette = "Set3")


# Montre seulement les oiseaux 
# Observation : Certains oiseaux très présents 
treemap(
  dtf = count_sp |> filter(order == "Passeriformes"),
  index = c(
    "family", 
    "species"), # The hierarchy levels
  vSize = "n",                     # Area based on the count
  title = "Hiérarchie imbriquée: Oiseaux",
  palette = "Set3")

# Met emphase sur le NOMBRE *D'ESPÈCES* UNIQUE 
# DANS FAMILLE selon observations faites 
treemap(
  dtf = hierarchy_counts,
  # The hierarchy levels
  index = c(
    # "kingdom",
    "class"
    , "order"
    , "family"
    ), 
  vSize = "n",                     # Area based on the count
  title = "Hiérarchie imbriquée: COFa",
  palette = "Set3")

# Simplification avec seulement 
# certains groupes d'organismes
treemap(
  dtf = hierarchy_counts |> 
    filter(
      kingdom == "Animalia"
      ,
      # phylum != "Chordata"
      
      class %in% c("Amphibia", "Arachnida", "Aves", "Chilopoda",
                   "Gastropoda", "Insecta", "Malacostraca",
                   "Mammalia", "Squamata", "Testudines")
    ),
  index = c(#"kingdom", 
    # "phylum",
            "class", "order", "family"), # The hierarchy levels
  vSize = "n",                     # Area based on the count
  title = "Hiérarchie imbriquée: COFa",
  palette = "Set3")

# Tree graph (visualisation similaire à précédente, mais beaucoup de texte!)
treegraph(
  dtf = hierarchy_counts |> 
    filter(
      kingdom == "Animalia",
      class %in%  c("Amphibia", "Arachnida", "Aves", "Chilopoda",
                    "Gastropoda", "Insecta", "Malacostraca",
                    "Mammalia", "Pycnogonida", "Squamata", "Testudines")
    ),
  directed = FALSE,
  index = c("class", "order", "family"), 
  show.labels = T, 
  vertex.label.dist = .3, 
  vertex.label.cex = 0.5
)
