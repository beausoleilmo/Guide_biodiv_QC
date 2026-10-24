## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-14
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Charge données et Correction de données manquantes dans les Type_FR

## ____________####
## Prépare l'environnement --------

suppressMessages(
  {
    require(dplyr)
    require(sf)
  }
)

####
# Données de noms d'espèces
message("Chargement des noms d'espèces")
sp_nm_path_raw <- "output/esp_noms_gbif.csv"

# Lire les données
spnm <- read.csv(file = sp_nm_path_raw)


head(spnm)

spnm |>
  count(Type_FR) |>
  arrange(Type_FR) # |> View()


# Liste des Types Anglais - Français
# Certaines lignes avec Type_EN n'avaient pas le Type_FR,
# alors que d'autres lignes avaient ces indications
list_copmlete <- spnm |>
  filter(!is.na(Type_EN)) |>
  distinct(Type_EN, Type_FR) |>
  dplyr::select(
    Type_EN,
    Type_FR_2 = Type_FR
  ) |>
  arrange(Type_FR_2)

# list_copmlete |> pull(Type_FR_2) |> cat()

# Noms des Types possible
spnm |>
  # gb_dat |>
  st_drop_geometry() |>
  count(Type_FR) |>
  arrange(Type_FR) |>
  pull(Type_FR) |>
  cat()

# spnm |>
#   count(Type_FR) |>
#   pull(Type_FR) |>
#   cat()

# Importation de correction de noms
# nom_corr_ia <- read.csv(file.path(
#   "Incubateur/2025_05_24_Guide_biodiv_qc/",
#   "data/partie_2/",
#   "nom_esp_ia_wild_sp.csv"
# ))
#
# nom_corr_ia_order <- read.csv(file.path(
#   "Incubateur/2025_05_24_Guide_biodiv_qc/",
#   "data/partie_2/",
#   "nom_esp_ia_wild_order.csv"
# ))

##################
##################
#
# sp_null <- st_read(
#   dsn = file.path(
#     "Incubateur/2025_05_24_Guide_biodiv_qc/",
#     "data/partie_2/biodiv/gbif_data/gbif_type_null.parquet"
#   )
# )
#
# sp_type_fr <- sp_null |>
#   left_join(
#     y = nom_corr_ia_order,
#     by = join_by(
#       order
#     )
#   ) |>
#   left_join(
#     y = nom_corr_ia,
#     by = join_by(
#       scientificName
#     )
#   ) |>
#   # This was used before getting a list of species small eough to put in AI
#   mutate(Type_FR = case_when(
#     class == "Aves" ~ "Oiseaux",
#     order == "Lepidoptera" ~ "Papillons",
#     class == "Mammalia" ~ "Mammifères",
#     class == "Magnoliopsida" ~ "Plantes vasculaires",
#     class == "Liliopsida" ~ "Plantes vasculaires",
#     class == "Polypodiopsida" ~ "Plantes vasculaires",
#     order == "Hymenoptera" ~ "Abeilles",
#     order == "Diptera" ~ "Certaines mouches",
#     kingdom == "Fungi" ~ "Macrochampignons",
#     order == "Coleoptera" ~ "Coléoptères",
#     class == "Squamata" ~ "Reptiles",
#     order == "Anura" ~ "Amphibiens",
#     order == "Caudata" ~ "Amphibiens",
#     class == "Testudines" ~ "Reptiles",
#     order == "Megaloptera" ~ "Neuroptera",
#     order == "Isopoda" ~ "Isopode",
#     order == "Hemiptera" ~ "Punaises",
#     class == "Arachnida" ~ "Araignées"
#     # ,
#     # kingdom == "Plantae" ~ "Plantes vasculaires",
#   )) |>
#   mutate(
#     Type_FR = coalesce(Type_FR_cat, Type_FR, Category_from_your_list),
#   ) |>
#   dplyr::select(-c(Type_FR_cat, Category_from_your_list)) |>
#   # View()
#   # filter(is.na(Type_FR)) |>
#   as_tibble() # |>
# # distinct(order) |>
# # pull(scientificName) |>
# # cat()

##################
##################
