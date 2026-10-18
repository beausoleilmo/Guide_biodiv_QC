## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-28
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Sélection d'espèces par région avec filtres taxonomique
#   --> Un nombre d'observations minimales est requis pour être gardé
#   --> Pour les oiseaux :
#         nombre d'observation minimales requis
#         sélection par proportion de chaque famille (avec boucle while)
#   --> Combiner les données

# Nb observations Espèce par région
# count_sp_reg <- get_sp_reg_counts(file.raw) |>
#   # Correction de classe
#   mutate(
#     # Reclassification nécessaire pour éviter d'avoir
#     # des duplicats dans class et ordre
#     class = if_else(
#       class == "Diplura",
#       true = "Entognatha",
#       false = class
#     ),
#     pa = 1
#   )

message("Requiert internet pour API GBIF")
count_sp_reg |>

  count(kingdom, phylum)

count_sp_reg |>
  filter(
    class %in%
      c(
        # "Insecta", "Arachnida", "Chilopoda", "Diplopoda", "Arthropoda, "Malacostraca", # order == Isopoda
        # "Aves",
        "Mammalia",
        "Amphibia",
        "Reptilia"
      ),
  ) |>
  count(class)


# Minimum d'observations
# qui doivent être fait pour être inclue
# Cela aide à éviter d'avoir trop d'observations erronées
min_obs <- 10

# Extraire compte observations (avec minimum) pour
# les organismes qui ne sont pas des oiseaux
select_sp_noAves <- count_sp_reg |>
  filter(
    class %in%
      c(
        # "Insecta", "Arachnida", "Chilopoda", "Diplopoda", "Arthropoda, "Malacostraca", # order == Isopoda
        # "Aves",
        "Mammalia",
        "Amphibia",
        "Reptilia"
      ),
    order != "Cetacea" | is.na(order),
    n >= min_obs
  )

select_sp_noAves |>
  mutate(pa = 1) |>
  group_by(
    MUS_NM_REG,
    class
  ) |>
  summarise(sum = sum(pa)) |>
  pivot_wider(names_from = class, values_from = sum)

# Arthropodes
select_sp_arthrop <- count_sp_reg |>
  filter(
    class %in%
      c(
        "Insecta",
        "Arachnida",
        "Chilopoda",
        "Diplopoda",
        "Arthropoda",
        "Malacostraca"
      ),
    n >= 20
  )

# Poissons
select_sp_pouessons <- count_sp_reg |>
  filter(
    Type_FR %in%
      c(
        "Poissons"
      )
  )

# select_sp_pouessons |> count(MUS_NM_REG)

nrow(select_sp_pouessons)
#
# Recherche taxonomique poissons
tictoc::tic("match poissons") # 10 s
df_matches <- rgbif::name_backbone_checklist(
  select_sp_pouessons |>
    dplyr::select(scientificName, kingdom, family) |>
    distinct(),
  verbose = TRUE # Montre les alternatifs
)
tictoc::toc()

select_sp_pouessons <- select_sp_pouessons |>
  # Ajouter les données taxonomique de GBIF
  left_join(
    y = df_matches |>
      filter(
        matchType == "EXACT",
        rank == "SPECIES"
      ) |>
      dplyr::select(verbatim_name, order),
    by = join_by(species == verbatim_name)
  ) |>
  mutate(order = coalesce(order.x, order.y)) |>
  dplyr::select(-c(order.x, order.y)) |>
  relocate(order, .after = class)


# Champignons
select_sp_fungi <- count_sp_reg |>
  filter(
    kingdom %in%
      c(
        "Fungi"
      )
  )

# plantes
select_sp_plantes <- count_sp_reg |>
  filter(
    kingdom %in%
      c(
        "Plantae"
      )
  )


message(
  "\nSélection programatique des espèces avec groupe taxonomique:
"
)

region_lvl <- "MUS_NM_REG"
# Préfiltre
nb_min_obs <- min_obs_aves <- 100 # Minimum d'observations nécessaire pour gader une espèce

select_sp_Aves <- especes_proportion_famille(
  # Sélectionne un nombre D'OBSERVATION minimale pour les oiseaux pour
  # être considérer dans le calcul
  donnees = count_sp_reg |> filter(class == "Aves", n > nb_min_obs),
  nb_max_esp = 140,
  taxo_lvl = "family",
  type = "m1",
  region_lvl = region_lvl,
  maxiter = 100
)

select_sp_plantes_filt <- especes_proportion_famille(
  donnees = select_sp_plantes,
  nb_max_esp = 100,
  taxo_lvl = "order",
  type = "m1",
  region_lvl = region_lvl,
  maxiter = 100
)

select_sp_arthrop_filt <- especes_proportion_famille(
  donnees = select_sp_arthrop,
  nb_max_esp = 100,
  taxo_lvl = "order",
  type = "m1",
  region_lvl = region_lvl,
  maxiter = 200
)
select_sp_pouessons_filt <- especes_proportion_famille(
  donnees = select_sp_pouessons,
  nb_max_esp = 100,
  type = "m1",
  region_lvl = region_lvl,
  maxiter = 200
)
select_sp_fungi_filt <- especes_proportion_famille(
  donnees = select_sp_fungi,
  nb_max_esp = 100,
  type = "m1",
  region_lvl = region_lvl,
  maxiter = 200
)
# matplot(t(select_sp_arthrop_filt[[2]][,-c(1), drop = FALSE]));abline(h = 140)

select_sp_Aves$table_especes_filtre

# Treemaps ----------------------------------------------------------------

message("Treemaps pour une région en particulier")
# Extraire région
reg_lst <- unique(select_sp_Aves$table_especes_filtre$MUS_NM_REG)

# 5 == estrie
(reg_sel <- reg_lst[5])
message(sprintf("Sélection de la région %s", reg_sel))

treemap(
  dtf = select_sp_Aves$table_especes_filtre |>
    filter(MUS_NM_REG %in% reg_sel),
  index = c(
    "order",
    "family",
    "nom_fr"
  ),
  vSize = "n",
  title = sprintf("Hiérarchie imbriquée: Oiseaux. Reg : %s", reg_sel),
  palette = "Set3"
)

treemap(
  dtf = select_sp_arthrop_filt$table_especes_filtre |>
    filter(MUS_NM_REG %in% reg_sel),
  index = c(
    "order",
    "family",
    "label"
  ),
  vSize = "n",
  title = sprintf("Hiérarchie imbriquée: Bebittes Reg : %s", reg_sel),
  palette = "Set3"
)


treemap(
  dtf = select_sp_noAves |>
    filter(MUS_NM_REG %in% reg_sel),
  index = c(
    "class",
    "order",
    "family"
  ),
  vSize = "n",
  title = sprintf("Hiérarchie imbriquée: rest Reg : %s", reg_sel),
  palette = "Set3"
)


# Compte final d'espèce ---------------------------------------------------
# Voir le nombre d'espèce par groupe
count_sp_reg |>
  filter(
    kingdom %in% c("Animalia", "Plantae", "Fungi"),
    # class %in%
    #   c(
    #     "Aves",
    #     "Mammalia",
    #     "Amphibia",
    #     "Reptilia"
    #   ),
    order != "Cetacea" | is.na(order)
  ) |>
  group_by(
    MUS_NM_REG,
    # MRS_NM_MRC,
    kingdom,
    class
  ) |>
  summarise(sum = sum(pa)) |>
  pivot_wider(
    names_from = c(kingdom, class),
    values_from = sum,
    names_sort = TRUE
  )
# Certaines régions n'ont pas de tortues!
select_sp_noAves |>
  count(order)


message(
  "Combiner les tables d'espèces sélectionnées selon niveau taxonomique"
)
select_sp_tout_reg <- select_sp_noAves |>
  bind_rows(select_sp_Aves$table_especes_filtre) |>
  bind_rows(select_sp_fungi_filt$table_especes_filtre) |>
  bind_rows(select_sp_plantes_filt$table_especes_filtre) |>
  bind_rows(select_sp_arthrop_filt$table_especes_filtre) |>
  bind_rows(select_sp_pouessons_filt$table_especes_filtre) |>
  mutate(
    total = rowSums(across(where(is.numeric)), na.rm = TRUE),
    # treemap ne fonctionne pas s'il y a des NA dans 'order'
    # order = ifelse(class == "Squamata", yes = "Couleuvres", no = order),
    # order = ifelse(class == "Testudines", yes = "Tortues", no = order),
    # Espèce renommé en 2025!
    species = recode(sp2w, "Setophaga petechia" = "Setophaga aestiva"),
    # Création d'une colonne de taxonomie 'vernaculaire'
    taxo_lvl_gr = case_when(
      kingdom == "Plantae" ~ "Plantes",
      kingdom == "Fungi" ~ "Champigons",
      class == "Aves" ~ "Oiseaux",
      class == "Mammalia" ~ "Mammifères",
      class == "Amphibia" ~ "Amphibiens",
      # class == "Reptilia" ~ "Reptiles",
      class == "Insecta" ~ "Insectes",
      class == "Malacostraca" ~ "Crustacés",
      class == "Arachnida" ~ "Araignées",
      class %in%
        c(
          "Teleostei",
          "Chondrostei",
          "Elasmobranchii",
          "Holostei"
        ) ~ "Poissons",
      order == "Cetacea" ~ "Mammifères marins",
      order == "Testudines" ~ "Tortues",
      order == "Squamata" ~ "Couleuvres",
      .default = class
    )
  )

# Exploration : compte des espèces par groupe d'organismes par région
# Donne le NOMBRE d'ESPÈCE PAR RÉGION ET GROUPE D'ORGANISME
select_sp_tout_reg |>
  group_by(
    MUS_NM_REG,
    taxo_lvl_gr
  ) |>
  summarise(sum = sum(pa)) |>
  pivot_wider(
    names_from = c(taxo_lvl_gr),
    values_from = sum,
    names_sort = TRUE
  )
# Trouve les espèces très fréquentes dans les régions
sp_bcp_present <- select_sp_tout_reg |>
  arrange(desc(n)) |>
  select(
    species,
    nom_sci,
    sp2w,
    nom_fr,
    Type_FR,
    MUS_NM_REG,
    n
  ) |>
  head(n = 20) |>
  pull(species) |>
  unique()
select_sp_tout_reg |>
  filter(species %in% c(sp_bcp_present)) |>
  select(
    nom_fr,
    MUS_NM_REG,
    n
  ) |>
  pivot_wider(
    names_from = c(nom_fr),
    values_from = n,
    names_sort = TRUE
  )


message(sprintf("Nb de rangées : %s", nrow(select_sp_tout_reg)))

## Exporter données --------
## Ce sont les données utilisées par le GUIDE DE BIODIVERSITÉ
readr::write_excel_csv2(
  x = select_sp_tout_reg,
  file = file.path(
    "output",
    "select_sp_tout_reg.csv"
  )
)

# Régions
reg_lst <- unique(select_sp_Aves$MUS_NM_REG)

# for (reg_sel in reg_lst) {
#   for (class_sel in list("Aves", c("Mammalia", "Amphibia", "Reptilia"))
#   ) {
#     if (all(class_sel %in% "Aves")) {
#       class_name <- "oiseaux"
#       idx <- c(
#         # "class",
#         "order",
#         "family",
#         "species"
#       )
#     } else {
#       class_name <- "reste"
#       idx <- c(
#         "class",
#         "order",
#         "family",
#         "species"
#       )
#     }
#     png(
#       filename = sprintf(
#         "InCubateur/2025_05_24_Guide_biodiv_qc/output/partie_2/plot_%s_%s.png",
#         class_name,
#         reg_sel
#       ),
#       width = 10, height = 8, units = "in", res = 300
#     )
#     treemap(
#       dtf = select_sp_tout |>
#         filter(
#           MUS_NM_REG %in% reg_sel,
#           class %in% class_sel
#         ),
#       index = idx,
#       vSize = "n",
#       title = sprintf("Hiérarchie imbriquée: %s Reg : %s", class_name, reg_sel),
#       palette = "Set3"
#     )
#     dev.off()
#   }
# }
