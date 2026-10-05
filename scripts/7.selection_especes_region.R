## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-28
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Sélection d'espèces par région
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

# Minimum d'observations
# qui doivent être fait pour être inclue
# Cela aide à éviter d'avoir trop d'observations erronées
min_obs <- 10

count_sp_reg |>
  count(phylum)

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
        "Testudines",
        "Squamata"
      ),
    order != "Cetacea" | is.na(order),
    n >= min_obs
  )


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

select_sp_pouessons <- count_sp_reg |>
  filter(
    Type_FR %in%
      c(
        "Poissons"
      )
  )

# select_sp_pouessons |> count(MUS_NM_REG)

# Recherche taxonomique poissons
df_matches <- rgbif::name_backbone_checklist(
  select_sp_pouessons |>
    dplyr::select(name = species, kingdom, family) |>
    distinct(),
  verbose = TRUE # Montre les alternatifs
)

select_sp_pouessons <- select_sp_pouessons |>
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


select_sp_noAves |>
  mutate(pa = 1) |>
  group_by(
    MUS_NM_REG,
    # MRS_NM_MRC,
    class
  ) |>
  summarise(sum = sum(pa)) |>
  pivot_wider(names_from = class, values_from = sum)


# Préfiltre
nb_min_obs <- min_obs_aves <- 100 # Minimum d'observations nécessaire pour gader une espèce
nb_min_esp <- nb_esp_niveau <- 140 # Nombre d'espèce cible à garder

#' Filtrer le nombre d'espèces itérativement
#'
#' @description
#' Les données d'occurrences d'espèces sont parfois trop volumineuses pour être
#' présenté au complet dans le guide de biodiversité. Un stratégie est de
#' synthétiser les observations en mettant une limite du nombre d'organismes
#' à montrer. Cette fonction permet de filtrer les données en gardant
#' un niveau taxonomique d'espèces (choisir les espèces dans chaque famille).
#' Les espèces finales sélectionnées sont alors échantillonnées en priorisant
#' les occurrences les plus fréquences.
#'
#' @param donnees data.frame. Tableau de données à filtrer pour un groupe
#' d'organismes
#' @param nb_min_esp Integer. Nombre d'espèce cible final pour chaque région
#' @param type Modèle de la fonctoin [more_prec()] c("m1", "m2", "m3",)
#' @param taxo_lvl Nom de colonne du niveau taxonomique à
#' grouper (`group_by()`). Nécessaire pour certain groupe taxonomique : grouper
#' par famille pour les plantes est 'trop précis' et on se retrouve avec trop
#' de famille avec 1 espèce dedans.
#'
#' @returns
#' @export
#'
#' @examples
#'
especes_proportion_famille <- function(
  donnees,
  nb_min_esp,
  type = "m1",
  taxo_lvl = "family",
  maxiter = 100
) {
  # Pour enregistrer le processus itératif
  iter_rec <- NULL

  w <- 1 # Compte pour la boucle while

  # Au début, chaque région a une proportion de 100% = 1
  # Cette proportion sera ajusté en fonction d'un algorithme
  # de Décroissance exponentielle
  reg_prop <- count_sp_reg |>
    distinct(MUS_NM_REG) |>
    # Débute avec proportion == 1 (votre chat peut tout prendre...)
    mutate(prop = 1)

  if (w == maxiter) {
    message("Itération maximale atteinte")
  }
  # Boucle pour filtrer de plus en plus finement les données
  # afin d'avoir un nombre d'oiseaux par région
  while (w != maxiter) {
    # Continuer la sélection pour les oiseaux seulement
    select_sp_Aves <- donnees |>
      left_join(
        y = reg_prop,
        by = join_by(MUS_NM_REG)
      ) |>
      # Garder un nombre d'espèce par niveau taxonomique : famille
      group_by(MUS_NM_REG, {{ taxo_lvl }}) |>
      # Garde les données si c'est le top "top_proportion" ou minimum 1
      filter(
        # S'assure de garder au moins 1 rangée!
        # desc(n): Place valeurs plus élevés en premier.
        # min_rank(...): Assigne un rang aux rangées. Plus grosse valeur = 1
        # n(): nb rangées dans groupe.
        # prop: fraction décimale à garder
        # n() * prop: Nombre de rangers à garder
        # pmax(1, ...): Compare 1 et la valeur calculé et choisi le + gros.
        min_rank(desc(n)) <= pmax(1, n() * prop)
      ) |>
      arrange(desc(n))

    # Compte, par région, le nb d'espèces restant après filtration
    aves_count <- select_sp_Aves |>
      group_by(MUS_NM_REG) |>
      summarise(sum = sum(pa))

    # enregistrer le processus de filtration
    if (w == 1) {
      # Si c'est 1re itération
      iter_rec <- iter_rec |>
        bind_cols(aves_count)
    } else {
      iter_rec <- iter_rec |>
        bind_cols(
          aves_count |>
            select(-MUS_NM_REG) |>
            # Rennomer les colonnes séquentiellement
            rename_with(~ paste0("sum_", w), everything())
        )
    }

    # Informe sur la précision actuelle selon le compteur 'w'
    # Extraire pour la valeur 'm1' = 1/x ou x^(-1).
    if (w %% 10 == 0) {
      message(
        sprintf(
          fmt = "Précision (%s) : %s/1000",
          w, # Itération courante
          # Arrondir pour avoir un chiffre assez précis (voir aussi format())
          round(more_prec(w)[[type]], 4) * 1000
        )
      )
    }
    ## VALIDATION --
    # Doit tester si certaines régions ont atteint leur objectif suite :
    # Extraire région qui n'ont pas atteint le nombre d'espèces espéré
    reg_to_update <- aves_count |>
      # touver les régions qui ont plus de 'nb_min_esp' (Booléen)
      mutate(prop_update = sum >= nb_min_esp) |>
      # Filtrer les régions qui ont trop d'espèces
      filter(prop_update) |>
      pull(MUS_NM_REG)

    # S'il reste rien, quitter la boucle
    if (length(reg_to_update) == 0) {
      break
    }

    # print(reg_prop)

    # Mettre à jour le tableau de proportion avec la décroissance exponentielle
    reg_prop <- reg_prop |>
      mutate(
        prop = ifelse(
          test = MUS_NM_REG %in% reg_to_update,
          # Remplacer la proportion initiale
          # Diminution de la précision à chaque coup
          # pour filtrer plus précisément
          yes = prop - more_prec(w)[[type]],
          no = prop
        )
      )

    # Augmente le compteur de 1
    w <- w + 1
  }

  return(list(select_sp_Aves, iter_rec))
}

select_sp_Aves <- especes_proportion_famille(
  donnees = count_sp_reg |> filter(class == "Aves", n > nb_min_obs),
  nb_min_esp = 140,
  type = "m1"
)[[1]]

select_sp_plantes_filt <- especes_proportion_famille(
  donnees = select_sp_plantes,
  nb_min_esp = 80,
  taxo_lvl = "order",
  type = "m1"
)[[1]]

select_sp_arthrop_filt <- especes_proportion_famille(
  donnees = select_sp_arthrop,
  nb_min_esp = 80,
  taxo_lvl = "order",
  type = "m1",
  maxiter = 200
)[[1]]
select_sp_pouessons_filt <- especes_proportion_famille(
  donnees = select_sp_pouessons,
  nb_min_esp = 100,
  type = "m1",
  maxiter = 200
)[[1]]
select_sp_fungi_filt <- especes_proportion_famille(
  donnees = select_sp_fungi,
  nb_min_esp = 100,
  type = "m1",
  maxiter = 200
)[[1]]
# matplot(t(select_sp_arthrop_filt[[2]][,-c(1), drop = FALSE]));abline(h = 140)

select_sp_Aves

# Treemaps ----------------------------------------------------------------

message("Treemaps pour une région en particulier")
# Extraire région
reg_lst <- unique(select_sp_Aves$MUS_NM_REG)

# 5 == estrie
(reg_sel <- reg_lst[5])

treemap(
  dtf = select_sp_Aves |>
    filter(MUS_NM_REG %in% reg_sel),
  index = c(
    "order",
    "family",
    "species"
  ),
  vSize = "n",
  title = sprintf("Hiérarchie imbriquée: Oiseaux. Reg : %s", reg_sel),
  palette = "Set3"
)

treemap(
  dtf = select_sp_arthrop_filt |>
    filter(MUS_NM_REG %in% reg_sel),
  index = c(
    "order",
    "family",
    "species"
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
    class %in% c("Aves", "Mammalia", "Amphibia", "Testudines", "Squamata"),
    order != "Cetacea" | is.na(order)
  ) |>
  group_by(
    MUS_NM_REG,
    # MRS_NM_MRC,
    class
  ) |>
  summarise(sum = sum(pa)) |>
  pivot_wider(
    names_from = class,
    values_from = sum,
    names_sort = TRUE
  )
# Certaines régions n'ont pas de tortues!

message("combiner les espèces choisies")
select_sp_tout_reg <- select_sp_noAves |>
  bind_rows(select_sp_Aves) |>
  bind_rows(select_sp_fungi_filt) |>
  bind_rows(select_sp_plantes_filt) |>
  bind_rows(select_sp_arthrop_filt) |>
  bind_rows(select_sp_pouessons_filt) |>
  mutate(
    total = rowSums(across(where(is.numeric)), na.rm = TRUE),
    # treemap ne fonctionne pas s'il y a des NA dans 'order'
    order = ifelse(class == "Squamata", yes = "Couleuvres", no = order),
    order = ifelse(class == "Testudines", yes = "Tortues", no = order),
    # Espèce renommé en 2025!
    species = recode(species, "Setophaga petechia" = "Setophaga aestiva")
  )

message(sprintf("Nb de rangées : %s", nrow(select_sp_tout_reg)))

## Exporter données --------
readr::write_excel_csv2(
  x = select_sp_tout_reg,
  file = file.path(
    "output/select_sp_tout_reg.csv"
  )
)

# Régions
reg_lst <- unique(select_sp_Aves$MUS_NM_REG)

# for (reg_sel in reg_lst) {
#   for (class_sel in list("Aves", c("Mammalia", "Amphibia", "Testudines", "Squamata"))
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

