## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Définition de fonction supplémentaire pour exécuter les scripts
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-28
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Extraire NOMS en français des espèces provenant d'iNaturalist pour le guide
#   --> À FAIRE : étape manuelle d'ajouter les espèces du fichier sp_nm_df_MOD2.csv -> sp_nm_df_MOD.csv

## ____________####
## Prépare l'environnement --------

# source(file = file.path(
#   "~/Github_proj/evologie/posts/guide_biodiv_qc/2025_05_24_Guide_biodiv_qc",
#   "scripts/00_init/functions.R"
# ))

# Pour chaque région
# Requis : 5.1.prep_data.R
source(
  file = file.path(
    "scripts/7.selection_especes_region.R"
  )
)


# Lire la liste d'espèces et de photos iNaturalist
# obtenue avec getTaxonInfo (evolovr::get_taxon_info())
sp_rec2 <- read.csv2(
  file = file.path(
    "output/sp_list_fil_iNat.csv"
  )
)

# ===========================
# inclure les données de wild species canada?
# Importer les noms d'espèces en français
esp_sc_gbif <- read.csv(
  file = "output/esp_noms_gbif.csv"
)

esp_sc_gbif |>
  count(rank)

esp_filt <- esp_sc_gbif |>
  filter(
    rank == "SPECIES",
    is.na(nom_commun_en_francais)
  ) |>
  dplyr::select(
    scientificName,
    canonicalName,
    nom_commun_en_francais,
    Type_FR
  )


esp_filt |> count(Type_FR)
nrow(esp_filt)
nrow(sp_rec2)

# esp_filt |>
#   filter(canonicalName %in% sp_rec2$nom_sp)
#
# esp_filt |>
#   select(canonicalName, nom_commun_en_francais, Type_FR) |>
#   left_join(
#     y = sp_rec2,
#     by = join_by(canonicalName == nom_sp)
#   )
#

sp_rec2 |>
  # Retirer les espèces déjà extraitent
  filter(!(nom_sp %in% esp_filt$canonicalName)) |>
  nrow()
# ===========================

### Téléchargement des images 'Typiques' de iNaturalist ---------------------

# Fichier
fichier_sp_nom <- file.path(
  "output/sp_nm_df_MOD.csv"
)

# Si le fichier n'existe pas ne pas exécuter
if (file.exists(fichier_sp_nom)) {
  message("Filtrer les espèces pour extraire ce qui manque")

  # Importer le fichier
  sp_nm_df_mod <- read.csv(fichier_sp_nom)

  ## Vérification si espèces à ajouter
  sp_tab_a_faire <- sp_rec2 |>
    # Retirer les espèces déjà extraitent
    filter(!(nom_sp %in% sp_nm_df_mod$species)) |>
    distinct()

  # Test s'il reste des rangées
  if (nrow(sp_tab_a_faire) == 0) {
    warning(
      "Pu de données dans le tableau... bravo Sylvain! Le filtre a tout enlevé!"
    )
  }
} else {
  message("Utiliser sp_rec2")
  sp_tab_a_faire <- sp_rec2
}


# Liste des noms français de noms d'espèces provenant d'iNaturalist
sp_nm_df <- NULL
attente_i <- 10
nb_requete <- 10 # Chaque nb_requete requête, attendre attente_i secondes

for (sp_nm_idx in 1:nrow(sp_tab_a_faire)) {
  if (nrow(sp_tab_a_faire) == 0) {
    warning(
      "Pu de données dans le tableau... bravo Sylvain! 
       Je peux rien faire... 
       donne moi des espèces en latinnn 
       Siouplaittt!
       Oueilldont!"
    )
    break
  }
  message(
    sprintf(
      "%03d/%03d (%02d %%)",
      sp_nm_idx,
      nrow(sp_tab_a_faire),
      round(sp_nm_idx / nrow(sp_tab_a_faire) * 100, 0)
    )
  )

  sp_nm <- sp_tab_a_faire$nom_sp[sp_nm_idx]

  # Attente pour ne pas surcharger le serveur
  Sys.sleep(time = 0.1)

  # Fonction personnelle voir ?readTaxonFR
  fr_nm <- evolovr::get_try_taxon_name(
    taxon_id = sp_tab_a_faire$id[sp_nm_idx],
    lang = "french",
    # Si problème dans CETTE fonction, va attendre 'attente_i'
    attend = attente_i
  )
  #readTaxonFR(
  # taxon = sp_tab_a_faire$id[sp_nm_idx],
  # wait = attente_i)

  # Corriger la case des noms d'organismes ---
  # -- Mettre la première lettre en majuscule
  # -- Pattern: Find a lowercase letter ([a-z]) that is preceded by
  # -- either the start of the string (^) OR a semicolon (;)
  fr_nm <- gsub(
    x = fr_nm,
    pattern = "(^|;)([a-z])",
    replacement = "\\1\\U\\2",
    perl = TRUE
  )

  # Ajouter l'espèces recherché et le résultat
  sp_nm_df <- rbind(
    sp_nm_df,
    data.frame(
      species = sp_nm,
      fr_nm = fr_nm
    )
  )

  # Tous les 'nb_requete' requêtes, attendre 'attente_i' secondes
  # Pour ne pas trop en demander au serveur de iNaturalist
  if (sp_nm_idx %% nb_requete == 0) {
    message(sprintf(
      fmt = "Fait %s. Attend %s s",
      sp_nm_idx,
      attente_i
    ))
    Sys.sleep(time = attente_i)
  }
}


# =======================++++=========
# =======================++++=========
# =======================++++=========
message("Lire les données VASCAN")
vascanqc <- readr::read_delim(
  file = file.path(
    "data/raw/biodiv/more/vascan_qc_2026_06_20.txt"
  ),
  delim = "\t",
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  mutate(
    vernacular_fr = gsub(
      x = vernacular_fr,
      pattern = "(^)([a-z])",
      replacement = "\\1\\U\\2",
      perl = TRUE
    )
  ) |>
  dplyr::select(scientific_name, vernacular_fr)

mycoliste <- readxl::read_xlsx(
  path = "data/raw/biodiv/more/Mycoliste.xlsx",
  skip = 3
) |>
  janitor::clean_names()
# =======================++++=========
# =======================++++=========
# =======================++++=========
# =======================++++=========

# Si le fichier n'existe pas, ne pas exécuter
if (file.exists(fichier_sp_nom) & !is.null(sp_nm_df)) {
  message(
    "Exporter les données (sp_nm_df_MOD2.csv) et 
mettre à jour 'manuellement' sp_nm_df_MOD.csv choisir les noms du Québec"
  )

  # À partir des noms de iNaturalist (species, fr_nm), compléter pour avoir
  # les noms français du Québec avec des listes d'espèces contenant des noms
  # Français
  sp_nm_qc <- sp_nm_df |>
    # Ajout de la colonne de nom d'espèces en français (Québec), s'il n'y a
    # qu'un seul nom
    mutate(
      # Création de nom_commun_qc : s'il y a plusieurs noms d'espèces, mette ""
      nom_commun_qc = ifelse(
        test = grepl(pattern = ';', x = fr_nm),
        yes = "",
        no = fr_nm
      ),
      # Si pas de nom d'espèce, on doit le trouver à la main (note = 1)
      find.tect = ifelse(
        test = nom_commun_qc == "" | is.na(nom_commun_qc),
        yes = 1,
        no = 0
      )
    ) |>
    # Ajout des données vascan ------------------------
    left_join(
      y = vascanqc,
      by = join_by(species == scientific_name)
    ) |>
    # combiner les noms français
    mutate(
      nom_commun_qc = coalesce(na_if(nom_commun_qc, ""), vernacular_fr)
    ) |>
    # Retirer les colonnes coalesce
    dplyr::select(
      -c(vernacular_fr)
    ) |>
    # Noms standardisés ----------------------------------------
    # Voir script 8.1.iNaturalist_...
    # Ajout des données avec les noms préparé GBIF, puis les noms français.
    left_join(
      y = especes_tout_gbif |>
        dplyr::select(nom_sci, nom_fr),
      by = join_by(species == nom_sci)
    ) |>
    mutate(
      nom_commun_qc = coalesce(na_if(nom_commun_qc, ""), nom_fr)
    ) |>
    dplyr::select(-nom_fr) |>
    # Ajoute le nom des champginons ---------------------------
    left_join(
      y = mycoliste |>
        select(nom_latin, nom_francais),
      by = join_by(species == nom_latin)
    ) |>
    mutate(nom_commun_qc = dplyr::coalesce(nom_commun_qc, nom_francais)) |>
    select(-nom_francais) |>
    # ---------------------------------------------------------
    # Ajouter à la liste existante (voir fichier_sp_nom)
    bind_rows(sp_nm_df_mod) |>
    arrange(find.tect) |>
    mutate(
      nom_commun_qc = nom_commun_qc[order(is.na(nom_commun_qc))],
      is_duplicate = duplicated(species) | duplicated(species, fromLast = TRUE)
    )

  # Trouver les espèces avec des noms dupliqué
  especes_dupliquees <- sp_nm_qc |>
    # distinct(species) |>
    group_by(species) |>
    count() |>
    filter(n > 1)

  # Exporter cette nouvelle liste préremplie avec les noms d'espèce
  # Par contre, certains noms d'espèces sont encore NA ou "". Il faut
  # donc compléter à la main
  readr::write_excel_csv(
    x = sp_nm_qc |>
      filter(!is.na(nom_commun_qc), !is_duplicate) |>
      select(-is_duplicate),
    file = file.path(
      "output",
      # à modifier manuellement et remplacer "sp_nm_df_MOD.csv"
      # (optionel) Noter les noms cherché à la main
      "sp_nm_df_MOD2.csv"
      # -> Champignon https://www.mycoquebec.org
      # -> Lichen https://lichens.quebec/especes/
      # -> Bryophytes https://www.societequebecoisedebryologie.org/Bryoquel_presentation.html
      # -> Tout https://explorer.natureserve.org
      # -> Algues : https://fourchettebleue.ca/produits/main-de-mer-palmee/
      # -> Poissons : https://www.fishbase.se/summary/Upeneus_parvus.html
    ),
    quote = "needed"
  )
}


# Si le fichier n'existe pas ne pas exécuter
if (file.exists(fichier_sp_nom) & !is.null(sp_nm_df)) {
  message("Filtrer les espèces pour extraire ce qui manque")
  # Importer le fichier
  sp_nm_df_mod <- read.csv(fichier_sp_nom)
  sp_a_ajouter <- sp_nm_qc |>
    filter(!(species %in% sp_nm_df_mod$species))

  sp_a_ajouter_coal <- sp_a_ajouter |>
    mutate(
      nom_commun_qc = na_if(nom_commun_qc, "NA"),
      nom_commun_qc = na_if(nom_commun_qc, "<NA>")
    )

  readr::write_excel_csv(
    x = sp_a_ajouter_coal,
    file = file.path(
      "output",
      # à modifier manuellement et remplacer "sp_nm_df_MOD.csv"
      # (optionel) Noter les noms cherché à la main
      "sp_nm_df_MOD3_simple.csv"
    ),
    quote = "needed"
  )
}


atomiser_colonne_nom <- FALSE
if (atomiser_colonne_nom) {
  ## --> finalement, je ne pense pas fonctionner comme cela.
  ## --> Je crois que de mettre dans des fichiers MOD et ajouter manuellement est le plus rapide
  ## --> sinon, connecter avec les liens de recherche de nom d'espèces (voir si API existe)
  # Tableau de noms d'espèces :
  #  séparation de la colonne fr_nm pour que chaque élément séparé par ';'
  # se retrouve sur une nouvelle ligne.
  sp_nm_df_long <- sp_nm_df |>
    separate_longer_delim(
      cols = fr_nm,
      delim = ";"
    ) |>
    # groupe par espèce, mais pas par nom fr (comptte doublons)
    group_by(species) |>
    mutate(
      # compte le nombre de duplicat de noms d'espèces
      n = n(),
      # Préférence d'utilisation d'un nom
      preference = ifelse(
        # Si le nom est 1 seule fois présent,
        test = n == 1,
        # mettre 1, sinon 0 et doit être choisi manuellement
        yes = 1,
        no = 0
      )
    ) |>
    ungroup()
}

# Si null, joindre avec les données déjà exportés auparavant "sp_nm_df_mod"
if (!is.null(sp_nm_df)) {
  nom_esp_qc_taxon_info <- sp_nm_df |>
    bind_rows(sp_nm_df_mod) |>
    left_join(
      y = sp_rec2,
      by = join_by(species == nom_sp)
    )
} else {
  nom_esp_qc_taxon_info <- sp_nm_df_mod |>
    left_join(
      y = sp_rec2,
      by = join_by(species == nom_sp)
    )
}

# Joindre la liste d'espèces et les noms français des espèces
# sp_list_nom_fr = select_sp_tout |>
sp_list_nom_fr <- gbif_prep_type_fr_h3 |>
  # Obtenir la taxonomie
  distinct(
    kingdom,
    phylum,
    class,
    `order`,
    family,
    species
  ) |>
  # Mettre les données en mémoire
  collect() |>
  # Pas vraiemnt besoin de cela
  # left_join(
  #   y = sp_rec2,
  #   by =  join_by(species == nom_sp),
  #   relationship = "many-to-many"
  # ) |>
  # Noms en français! (par contre, des noms multiples sont aussi intégrés)
  left_join(
    y = nom_esp_qc_taxon_info,
    by = join_by(species),
    relationship = "many-to-many"
  )


## ____________####
## Exporter données --------
readr::write_excel_csv2(
  x = sp_list_nom_fr,
  file = file.path(
    "output/sp_list_fil_iNat_nomFR.csv"
  )
)

# Exporter le tableau des noms Latin + noms français d'iNaturalist
readr::write_excel_csv2(
  x = sp_nm_df,
  file = file.path(
    "output/sp_nm_df.csv"
  )
)
#
# readr::write_excel_csv2(
#   x = sp_nm_df_long,
#   file = file.path(
#     "output/sp_nm_df_long.csv"
#   )
# )
