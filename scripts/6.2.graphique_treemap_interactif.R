## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-28
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Exploration des données selon le nombre d'observations ou d'espèces 
#   -->


### duckdb cmd : extraire info sommaire  -------------
# Charge les données 
# source(
#   file = file.path(
#     incub, "scripts/partie_2/6.0.load_compte_especes.R"
#   ))

# Regarder les données 
count_sp |> arrange(desc(n)) |> head()

### Diagramme interactif ---------------------

# Sélectionner les données
df <- count_sp |>
  ungroup() |> 
  filter(
    # kingdom == "Animalia",
    !is.na(kingdom),
    !is.na(class),
    !is.na(order),
    !is.na(family)
  ) |> 
  mutate(
    pa = 1 # pour tester la visualisation 
    # n = pa
    # n = log10(n)
  )

### Structure hierarchique des données ---------
# Niveau 1: Kingdom (Parent is empty)
lvl1 <- df |>
  group_by(labels = kingdom) |>
  summarise(n = sum(n), .groups = 'drop') |>
  mutate(ids = labels, parents = "")

# Niveau 2: Class (Parent == Kingdom)
lvl2 <- df |>
  group_by(labels = class, parents = kingdom) |>
  summarise(n = sum(n), .groups = 'drop') |>
  mutate(ids = paste0(parents, "-", labels)) |> arrange(desc(n))

# Niveau 3: Order (Parent == Class)
lvl3 <- df |>
  group_by(labels = order, parents = class) |>
  summarise(n = sum(n), .groups = 'drop') |>
  # Use parent-child IDs to ensure uniqueness if names repeat across levels
  mutate(ids = paste0(parents, "-", labels))|> arrange(desc(n))

# Niveau 4: Family (Parent == Order)
lvl4 <- df |>
  group_by(labels = family, parents = order) |>
  summarise(n = sum(n), .groups = 'drop') |>
  mutate(ids = paste0(parents, "-", labels))|> arrange(desc(n))

# Niveau 5: Species (Parent == family)
lvl5 <- df |>
  group_by(labels = species, parents = family) |>
  summarise(n = sum(n), .groups = 'drop') |>
  mutate(ids = paste0(parents, "-", labels))|> arrange(desc(n))

### Combiner tous les niveaux
plot_data_tm <- bind_rows(
  lvl1, lvl2, lvl3, lvl4
  , lvl5
) |> 
  arrange(desc(n)) |>
  mutate(n = (n), 
         log_n = round(log10(n), 2), 
         hover_text = paste0(#"<b>Category:</b> ", labels, "<br>",
           "<b>Actual Value:</b> ", format(n, big.mark = ","), "<br>"
           # ,
           # "<b>Log10 Value:</b> ", log_n
         ))

# Ne doit PAS y avoir de duplicat
plot_data_tm$labels[which(duplicated(plot_data_tm$labels))]

### Graphique interactif ----------

treemap_inter = plot_ly(
  data = plot_data_tm,
  type = "treemap",
  labels = ~labels,
  parents = ~parents,
  values = ~n, branchvalues = "total"#,
  # marker = list(
  #   colors = ~n, # LOG determines the color intensity
  #   colorscale = "Viridis",
  #   showscale = TRUE
  # )
  # values = ~log_n, branchvalues = "remainder",
  # text = ~hover_text,
  # hovertemplate = "%{text}<extra></extra>"
) |> 
  layout(title = "Treemap du nombre d'observations pour chaque espèce")

# Montre le graphique interactif 
treemap_inter
