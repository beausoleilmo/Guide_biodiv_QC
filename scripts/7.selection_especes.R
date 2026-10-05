## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-28
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> 
#   -->

### duckdb cmd : extraire info sommaire  -------------

# Regarder les données 
hierarchy_counts |> 
  count(class) |> 
  arrange(-n)

hierarchy_counts |> 
  filter(
    class %in% c(
      "Aves", 
      "Mammalia",
      # "Insecta",
      "Testudines",
      "Squamata"))

select_taxa = hierarchy_counts |> 
  filter(
    class %in% c(
      "Aves", 
      "Mammalia",
      "Amphibia",
      # "Insecta",
      # "Arachnida",
      # "Chilopoda",
      # "Diplopoda",
      # "Arthropoda
      # "Malacostraca", order == Isopoda
      "Testudines",
      "Squamata"), 
    order != "Cetacea" | is.na(order)
)

# Voir le nombre d'espèce par groupe 
select_taxa |> 
  group_by(class) |> 
  summarise(sum = sum(n))
# Le nombre d'organismes pour oiseaux et mammifère est très grand.
# Il faut en prendre moins 

min_obs = 25 # Minimum d'observations qui doivent être fait pour être inclue

count_sp |> 
  filter(
    class == "Mammalia", 
    order != "Cetacea",
    n > min_obs
         ) |> 
  arrange(desc(n))

select_sp_noAves = count_sp |>
  filter(
    class %in% c(
      # "Aves", 
      "Mammalia",
      "Amphibia",
      # "Insecta",
      # "Arachnida",
      # "Chilopoda",
      # "Diplopoda",
      # "Arthropoda
      # "Malacostraca", order == Isopoda
      "Testudines",
      "Squamata"), 
    order != "Cetacea" | is.na(order), 
    n > min_obs
  )

# Champignons
select_sp_fungi = count_sp |>
  filter(
    kingdom %in% c(
      "Fungi"),
    n > min_obs
  )

# plantes
select_sp_plantes = count_sp |>
  filter(
    kingdom %in% c(
      "Plantae"),
    n > min_obs
  )

select_sp_noAves |> 
  mutate(pa = 1 ) |> 
  group_by(class) |> 
  summarise(sum = sum(pa))

top_proportion = 0.4
# Continuer la sélection pour les oiseaux seulement 
select_sp_Aves = count_sp |> 
  filter(
    class == "Aves", 
    n > min_obs * 100 # x fois plus exigeant pour les oiseaux!
    ) |> 
  group_by(family) |> 
  # Garde les données si c'est le top "top_proportion" ou minimum 1 
  filter(min_rank(desc(n)) <= pmax(1, n() * top_proportion)) |>
  arrange(desc(n))

# Oiseaux
treemap(
  dtf = select_sp_Aves ,
  index = c(
    "order",
    "family", 
    "species"), # The hierarchy levels
  vSize = "n",                     # Area based on the count
  title = "Hiérarchie imbriquée: Oiseaux",
  palette = "Set3")

# Champgnions
treemap(
  dtf = select_sp_fungi,
  index = c(
    "order",
    "family", 
    "species"), # The hierarchy levels
  vSize = "n",                     # Area based on the count
  title = "Hiérarchie imbriquée: Champignon",
  palette = "Set3")

# reste 
treemap(
  dtf = select_sp_noAves,
  index = c(
    "class",
    "order",
    "family"), # The hierarchy levels
  vSize = "n",                     # Area based on the count
  title = "Hiérarchie imbriquée: reste",
  palette = "Set3")

# combiner les espèces choisies
select_sp_tout = select_sp_noAves |> 
  bind_rows(select_sp_Aves) |>
  bind_rows(select_sp_fungi) |>
  bind_rows(select_sp_plantes) |>
  mutate(
pa = 1,
    total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  )  

message("Tableau large du nombre d'espèces de chaque Classes par région")
select_sp_tout |>
  group_by(
    # MUS_NM_REG,
    # MRS_NM_MRC,
    class
  ) |>
  summarise(sum = sum(pa)) |>
  pivot_wider(
    names_from = class,
    values_from = sum, names_sort = TRUE
  ) 
