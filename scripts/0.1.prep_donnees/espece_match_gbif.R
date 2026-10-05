## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## #
## Préparation des données d'occurence de biodiversté GBIF
#
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Date cération : 2026-02-14
# Auteur: Marc-Olivier Beausoleil

## __________####
## LISEZMOI ####

## Problématique
#  --> Il n'existe pas de liste *complète* de noms d'espèces standardisés
#      et réputé pour le Canada et pour le Québec. Certains noms d'espèces ne
#      sont pas à jour selon la liste (certains oiseaux ont changés de noms,
#      etc.). Il faut corriger construire sa propre liste de nom d'organismes
#      et faire les corrections.
#      Au départ, la liste 'wild species Canada' apparaissait être complète,
#      mais il manque beaucoup d'organismes de certains groupes taxonomique.
#      Les listes additionneles et spécifiques à un groupe d'organismes
#      permet de mettre à jour les données taxonomiques et surtout, d'avoir
#      la liste la plus à jour de tous les noms communs en français du Québec.
#  --> Utilisation de plusieurs sources de données de base pour compléter une
#      liste d'espèces pour le Québec.
#      -> utilisation de `left_join` pour *ajouter* les noms communs
#      -> utilisation de `bind_rows` pour *ajouter* de nouvelles espèces.
#      Les diverses sources permettent de compléter les noms communs manquants
#      ET de trouver les noms d'espèces manquants entre les listes. E.g.,
#      Wild species Canada est énorme, mais il manque certaines espèces ou
#      les noms ont changés...
#  --> Fichier source :
#       Pour suivre l'origine des noms des espèces,
#       une colonne est ajoutée à tous les jeux de données pour
#       indiquer le fichier source.
#  --> Groupe taxonomique :
#      Le groupe taxonomique pour chaque liste doit être ajouté afin de
#      créer la colonne Type_fr (type d'organisme). C'est une indication
#      taxonomique vernaculaire.
#  --> API GBIF :
#      La liste est ensuite passé dans l'API de GBIF pour extraire les noms
#      standardisés pour toutes les espèces. Cela permettra de faire une
#      jointure des données.
#   --> L'outil de GBIF 'Species-lookup'
#       https://www.gbif.org/tools/species-lookup
#       ne permet pas plus de 6000 espèces et recommande d'utiliser
#       l'API pour faire une requête avec beaucoup d'espèces.
#

## Objectif :
# GBIF : Système mondial d'information sur la biodiversité
# Match la liste d'espèces du Canada avec la liste du 'backbone'
# taxonomique de GBIF (SMIB)

# Données
#  --> Liste taxonomique standard :
#      Obtenir une liste d'espèces avec les noms commun selon la taxonomie GBIF
#      Permet de joindre les données GBIF avec les données d'autres sources
#  --> Type_FR :
#      Création du Type_FR basé sur les données de wild species Canada

# Progiciels
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(rgbif)
library(taxize) # https://www.r-bloggers.com/2011/11/use-case-combining-taxize-and-rgbif/

out_liste <- "output/biodiv/liste_especes"
dir.create(
  path = file.path(out_liste),
  showWarnings = FALS,
  recursive = TRUE
)

#' Vérification de l'intersection de tableaux de noms d'espèces
#'
#' @description
#' Nombre de lignes suite au recoupement de noms d'espèces d'un tableau de
#' données (dat1) et d'un deuxième (dat2) pour voir combien de lignes sont
#' ajoutés ou pas suite à un left_join
#'
#' @param dat1 données wild species canada
#' @param dat2 données à tester
#' @param colsp Nom de la colonne d'espèces
#'
#' @return
#'Le nombre de lignes qui n'ont pas de correspondances entre dat1 et dat2
#'
#' @export
verif_noms_especes <- function(dat1, dat2, colsp) {
  dat2_name <- deparse(substitute(dat2))
  cat("\nDonnées comparaison avec", dat2_name, fill = TRUE)

  datfilt <- dat1 |>
    dplyr::filter(
      nom_scientifique %in% (dat2 |> dplyr::pull({{ colsp }}))
    )

  datfiltnot <- dat1 |>
    dplyr::filter(
      !(nom_scientifique %in% (dat2 |> dplyr::pull({{ colsp }})))
    )

  a_metric <- nrow(datfilt)
  b_metric <- nrow(datfiltnot)
  c_metric <- nrow(dat1)
  d_metric <- nrow(dat2)
  e_metric <- d_metric - a_metric
  message(sprintf(
    "A: données dat2 qui se retrouvent dans dat1 : %s",
    a_metric
  ))
  message(sprintf("B: données dat2 excluent de dat1 : %s", b_metric))
  message(sprintf("C: nb rangées de dat1 : %s", c_metric))
  message(sprintf("D: nb rangées de dat2 : %s", d_metric))
  message(sprintf("E: D-A : %s", e_metric))

  return(e_metric)
}

#  --> @source : noms communs
# Dossier source des données
dossier_liste_especes <- "data/raw/biodiv/listes_especes"

## --> Liste des fichiers de noms d'espèces
##     Le nom du fichier est ajouté aux données finale
##     pour retracer leur origines (particulièrement en utilisant 'bind_rows')
##
#      - Espèces sauvage Canada
liste_nom_esp_sauvage_ca <- '2024_10_wild_sp_common_names_Especes_sauvages_noms_communs.xlsx'
#          - https://www.wildspecies.ca ne contiennent pas tous les noms.
#          - https://www.wildspecies.ca/common-names
liste_nom_esp_sauvage_ca_2 <- 'Wild_Species_2020_Data_Especes_sauvages_2020_Donnees.xlsx'
#          - https://www.wildspecies.ca/reports
#      - Liste de la faune vertébrée du Québec (LFVQ)
liste_faune_vert_qc <- 'LFVQ_17_07_2025.csv'
#        - https://www.donneesquebec.ca/recherche/dataset/liste-de-la-faune-vertebree-du-quebec
#      - VASCAN
#        - https://data.canadensys.net/vascan/checklist?lang=en&habit=all&taxon=0&combination=anyof&province=QC&status=native&status=introduced&status=ephemeral&rank=class&rank=subclass&rank=superorder&rank=order&rank=family&rank=subfamily&rank=tribe&rank=subtribe&rank=genus&rank=subgenus&rank=section&rank=subsection&rank=series&rank=species&rank=subspecies&rank=variety&hybrids=true&limitResults=true&nolimit=false&sort=taxonomically&criteria_panel=selection
liste_nom_plantes <- "vascan_qc_2026_06_20.txt"
#      - Mycoliste
#        - https://www.mycoquebec.org/liste-especes.php
liste_nom_champignons <- "Mycoliste.xlsx"
#      - Bryoquel
#        - https://societequebecoisedebryologie.org/Bryoquel.html
liste_nom_bryophytes <- "BRYOQUEL_Liste_des_Bryophytes_Qc-Labr.xlsx"
#      - Lichen Québec
#        - https://lichens.quebec
liste_nom_lichen <- "lichen_quebec_corr.csv"


################################
# Charger les listes d'espèces --------------------------------------------
# L'idée est la fusion de liste d'espèces
# pour obtenir les noms français et TRype_FR

################################
# Charge liste d'espèces ---------------------------------------------------
especes_sauvage_canada_noms_communs <- readxl::read_xlsx(
  path = file.path(
    dossier_liste_especes,
    liste_nom_esp_sauvage_ca
  ),
  sheet = "Common names - Noms communs",
  # Scan les rangées avant de trouver le type de données dans les colonnes
  guess_max = 2e6
) |>
  janitor::clean_names() |>
  dplyr::select(
    code = code_code,
    numero = number_numero,
    groupe_taxonomique = taxonomic_group_groupe_taxonomique,
    ordre = order_ordre,
    famille = family_famille,
    nom_scientifique = scientific_name_nom_scientifique,
    nom_francais = nom_commun_en_francais,
    nom_anglais = english_common_name,
    fr_justification_fr
  ) |>
  dplyr::mutate(
    nom_liste = liste_nom_esp_sauvage_ca
  )


################################
# Wild species Canada ----------------------------------------
wild_sp_canada <- readxl::read_xlsx(
  path = file.path(
    dossier_liste_especes,
    liste_nom_esp_sauvage_ca_2
  ),
  sheet = "Ranks - Rangs",
  guess_max = 2e6
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    nom_liste = liste_nom_esp_sauvage_ca_2
  )


# Obtenir la liste la plus complète (même si pas tous les noms français)
# Wild species Canada
wild_species_canada_list <- wild_sp_canada |>
  # tidyr::separate(
  #   taxonomic_group_groupe_taxonomique,
  #   into = c("Type_EN", "Type_FR"),
  #   sep = " - "
  # ) |>
  # Retire des noms d'espèces dupliqués
  dplyr::distinct(
    scientific_name_nom_scientifique,
    .keep_all = TRUE
  ) |>
  # Retirer les noms scientifiques qui se
  # retouve dans la liste des noms communs
  dplyr::filter(
    !(scientific_name_nom_scientifique %in%
      especes_sauvage_canada_noms_communs$nom_scientifique)
  ) |>
  dplyr::select(
    code = code_code,
    numero = number_numero,
    groupe_taxonomique = taxonomic_group_groupe_taxonomique,
    ordre = order_ordre,
    famille = family_famille,
    nom_scientifique = scientific_name_nom_scientifique,
    nom_francais = nom_commun_en_francais,
    # qc,
    # qc_org,
    nom_liste
  )

################################
# Faune vertébrée : Liste de la faune vertébrée du Québec (LFVQ) en CSV ------
# Utilisation des noms communs pour cette liste
#
gr_lfvq <- tribble(
  ~classe          , ~groupe_taxonomique       ,
  "Actinopterygii" , "Fishes - Poissons"       ,
  "Amphibia"       , "Amphibians - Amphibiens" ,
  "Aves"           , "Birds - Oiseaux"         ,
  "Chondrichthyes" , "Fishes - Poissons"       ,
  "Mammalia"       , "Mammals - Mammifères"    ,
  "Myxini"         , "Fishes - Poissons"       ,
  "Petromyzontida" , "Fishes - Poissons"       ,
  "Reptilia"       , "Reptiles - Reptiles"
)

list_faune_vertebree_qc <- read.csv(
  file = file.path(
    dossier_liste_especes,
    liste_faune_vert_qc
  )
) |>
  janitor::clean_names() |>
  dplyr::select(
    nom_scientifique,
    classe,
    ordre,
    famille,
    nom_anglais,
    nom_commun_en_francais = nom_francais
  ) |>
  dplyr::mutate(
    nom_liste = liste_faune_vert_qc
  ) |>
  dplyr::left_join(y = gr_lfvq, by = join_by(classe))


################################
# PLANTES --------------------------------------------------------------------
# Liste de VASCAN et joindre info de GBIF
vascanqc <- readr::read_delim(
  file = file.path(
    dossier_liste_especes,
    liste_nom_plantes
  ),
  delim = "\t",
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  # Mettre 1re lettre de nom vernaculaire en majuscule
  dplyr::mutate(
    nom_liste = liste_nom_plantes,
    # Mettre 1re lettre en majuscule
    nom_commun_en_francais = gsub(
      x = vernacular_fr,
      pattern = "(^)([a-z])",
      replacement = "\\1\\U\\2",
      perl = TRUE
    ),
    groupe_taxonomique = "Vascular plants - Plantes vasculaires"
  ) |>
  # Sélectionne et renomme colonnes
  dplyr::select(
    rank,
    scientificName = scientific_name,
    nom_commun_en_francais,
    english_common_name = vernacular_en,
    nom_liste,
    groupe_taxonomique
  ) |>
  # Espèces et plus bas dans la taxonomie
  dplyr::filter(
    !is.na(nom_commun_en_francais),
    rank %in% c("Species", "Subspecies", "Variety")
  ) |>
  dplyr::select(
    -rank
  )

################################
# Bryophytes ----------------------------------------------------------------
# TODO :
# pas ajouté encore à la liste des données
bryoquel <- readxl::read_xlsx(
  path = file.path(
    dossier_liste_especes,
    liste_nom_bryophytes
  ),
  # Exclure les premières lignes du fichier Excel
  skip = 7,
  sheet = "Liste"
) |>
  janitor::clean_names() |>
  # Janitor n'a pas bien renommer cette colonne.
  dplyr::rename("id_taxon" = i_dtaxon) |>
  # Retirer les lignes dont l'ID == NA (puisque c'est des lignes de 'groupe')
  dplyr::filter(!is.na(id_taxon)) |>
  # Mettre 1re lettre de nom vernaculaire en majuscule
  dplyr::mutate(
    nom_liste = liste_nom_bryophytes,
    # Mettre 1re lettre en majuscule
    nom_commun_en_francais = gsub(
      x = noms_francais_acceptes,
      pattern = "(^)([a-z])",
      replacement = "\\1\\U\\2",
      perl = TRUE
    ),
    # Ajout du nom de groupe_taxonomique
    groupe_taxonomique = "Bryophytes - Bryophytes"
  ) |>
  dplyr::rename(
    "nom_scientifique" = noms_latins_acceptes,
    "nom_commun_en_anglais" = noms_anglais_acceptes
  ) |>
  dplyr::select(
    -c(id_taxon, gg, qc, l)
  )


################################
# Lichen ----------------------------------------------------------------
lichens <- read.csv2(
  file = file.path(
    dossier_liste_especes,
    liste_nom_lichen
  )
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    nom_liste = liste_nom_lichen,
    groupe_taxonomique = "Lichens - Lichens"
  ) |>
  dplyr::select(
    nom_scientifique,
    nom_commun_en_francais = nom_francais,
    # full_species_name,
    classe = class,
    ordre = order,
    famille = family,
    nom_liste,
    groupe_taxonomique
  )


colnames(lichens)

lichens |>
  head()


################################
# Champignons ----------------------------------------------------------------
mycoliste <- readxl::read_xlsx(
  path = file.path(
    dossier_liste_especes,
    liste_nom_champignons
  ),
  skip = 3
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    nom_liste = liste_nom_champignons,
    nom_francais = ifelse(
      test = nom_francais == "Nom français indéterminé",
      yes = "",
      no = nom_francais
    ),
    classe = ifelse(
      test = classe == "n. d.",
      yes = "",
      no = classe
    ),
    ordre = ifelse(
      test = ordre == "n. d.",
      yes = "",
      no = ordre
    ),
    famille = ifelse(
      test = famille == "n. d.",
      yes = "",
      no = famille
    ),
    # Probablement pas exact pour tous mais 'approximation...'
    groupe_taxonomique = "Macrofungi - Macrochampignons"
  ) |>
  dplyr::select(
    nom_latin,
    nom_francais,
    # ancien_s_nom,
    classe,
    ordre,
    famille,
    groupe_taxonomique,
    nom_liste
  )


#######################################
### Jointure des listes
# espèces de species Canada
wsc <- especes_sauvage_canada_noms_communs |>
  # Ajouter Wild species canada ET LFVQ
  dplyr::bind_rows(
    wild_species_canada_list
  )

# Rapport du nombre de noms d'espèces qui sont présents
# dans les 2 jeux de données
#' Exploration rapide de statistiques de multiples données
#'
#' @param master_dat
#'
#' @return
#' @export
nb_rangees_reste <- function(master_dat) {
  met_lvq <- verif_noms_especes(
    dat1 = master_dat,
    dat2 = list_faune_vertebree_qc,
    colsp = "nom_scientifique"
  )
  met_vascan <- verif_noms_especes(
    dat1 = master_dat,
    dat2 = vascanqc,
    colsp = "scientificName"
  )
  met_mycol <- verif_noms_especes(
    dat1 = master_dat,
    dat2 = mycoliste,
    colsp = "nom_latin"
  )
  met_lichens <- verif_noms_especes(
    dat1 = master_dat,
    dat2 = lichens,
    colsp = "nom_scientifique"
  )
  met_bryo <- verif_noms_especes(
    dat1 = master_dat,
    dat2 = bryoquel,
    colsp = "nom_scientifique"
  )

  # Calcul d'un nombre de lignes à ajouter à wsc
  # NOTE : Si on fait seulement des `left_join`, c'est le nombre
  # de lignes qu'on va perdre des autres tableaux d'espèces.
  var_metriques <- ls(pattern = "^met_")
  total_metriques <- Reduce(`+`, mget(var_metriques))
  return(total_metriques)
}
nb_rangees_reste(master_dat = wsc)
nb_rangees_reste(master_dat = liste_esp_joint)

liste_esp_joint <- wsc |>

  dplyr::select(
    -c(code, numero)
  ) |>
  # Ajouter la LFVQ --------------
  dplyr::full_join(
    y = list_faune_vertebree_qc,
    by = dplyr::join_by(
      nom_scientifique == nom_scientifique
    )
  ) |>
  # Ajouter VASCAN
  dplyr::full_join(
    y = vascanqc,
    by = dplyr::join_by(
      nom_scientifique == scientificName
    )
  ) |>
  # Ajouter Bryoquel
  dplyr::full_join(
    y = bryoquel,
    by = dplyr::join_by(
      nom_scientifique == nom_scientifique
    )
  ) |>
  # Mycoliste
  full_join(
    y = mycoliste,
    by = join_by(
      nom_scientifique == nom_latin
    )
  ) |>
  # Ajouter lichen
  dplyr::full_join(
    y = lichens,
    by = dplyr::join_by(
      nom_scientifique == nom_scientifique
    )
  ) |>
  # Combiner les colonnes pour finaliser la colonne des noms communs
  tidyr::unite(
    col = "nom_liste",
    starts_with("nom_liste"),
    sep = ", ",
    na.rm = TRUE
  ) |>
  # Dynamic coalesce across column patterns
  dplyr::mutate(
    classe = do.call(
      dplyr::coalesce,
      dplyr::pick(dplyr::starts_with("classe"))
    ),
    ordre = do.call(dplyr::coalesce, dplyr::pick(dplyr::starts_with("ordre"))),
    famille = do.call(
      dplyr::coalesce,
      dplyr::pick(dplyr::starts_with("famille"))
    ),
    groupe_taxonomique = do.call(
      dplyr::coalesce,
      dplyr::pick(dplyr::starts_with("groupe_taxonomique"))
    ),

    nom_commun_en_francais = do.call(
      dplyr::coalesce,
      dplyr::pick(
        dplyr::starts_with("nom_commun_en_francais"),
        dplyr::starts_with("nom_francais"),
        dplyr::any_of("noms_francais_acceptes")
      )
    ),

    english_common_name = do.call(
      dplyr::coalesce,
      dplyr::pick(
        dplyr::starts_with("english_common_name"),
        dplyr::starts_with("nom_anglais"),
        "nom_commun_en_anglais"
      )
    )
  ) |>

  # Drop leftover suffixed raw columns (.x, .y, .x.x, etc.)
  dplyr::select(
    -dplyr::matches(
      "^(classe|ordre|famille|groupe_taxonomique|nom_francais|nom_anglais)\\."
    ),
    -dplyr::matches("^nom_commun_en_francais\\."),
    -c(nom_commun_en_anglais, noms_francais_acceptes)
  )


#   dplyr::mutate(
#     # classe             = coalesce(pick(starts_with("classe"))),
# classe = do.call(dplyr::coalesce, dplyr::pick(dplyr::starts_with("classe"))),
#     # classe = dplyr::coalesce(
#     #   classe.x,
#     #   classe.y
#     # ),
#     ordre = dplyr::coalesce(
#       ordre.x,
#       ordre.y,
#       ordre.x.x,
#       ordre.y.y
#     ),
#     famille = dplyr::coalesce(
#       famille.x,
#       famille.x.x,
#       famille.y,
#       famille.y.y
#     ),
#     groupe_taxonomique = dplyr::coalesce(
#       groupe_taxonomique.x,
#       groupe_taxonomique.x.x,
#       groupe_taxonomique.x.x.x,
#       groupe_taxonomique.y,
#       groupe_taxonomique.y.y,
#       groupe_taxonomique.y.y.y
#     ),
#     nom_commun_en_francais = dplyr::coalesce(
#       nom_commun_en_francais.x,
#       nom_commun_en_francais.x.x,
#       nom_commun_en_francais.y,
#       nom_commun_en_francais.y.y,
#       noms_francais_acceptes,
#       nom_francais.x,
#       nom_francais.y
#     ),
#     english_common_name = dplyr::coalesce(
#       english_common_name,
#       nom_anglais.x,
#       nom_anglais.y
#     )
#   ) |>
#   # Retirer les colonnes coalesce
#   dplyr::select(
#     -c(
#       groupe_taxonomique.x,
#       groupe_taxonomique.x.x,
#       groupe_taxonomique.x.x.x,
#       groupe_taxonomique.y,
#       groupe_taxonomique.y.y,
#       groupe_taxonomique.y.y.y,
#       ordre.x,
#       ordre.y,
#       ordre.x.x,
#       ordre.y.y,
#       famille.x,
#       famille.x.x,
#       famille.y,
#       famille.y.y,
#       classe.x,
#       classe.y,
#       nom_commun_en_francais.x,
#       nom_commun_en_francais.x.x,
#       nom_commun_en_francais.y,
#       nom_commun_en_francais.y.y,
#       noms_francais_acceptes,
#       nom_francais.x,
#       nom_francais.y,
#       # english_common_name,
#       nom_anglais.x,
#       nom_anglais.y
#     )
#   )
colnames(liste_esp_joint)
writexl::write_xlsx(
  x = liste_esp_joint,
  path = file.path(
    out_liste,
    "liste_esp_combinee.xlsx"
  )
)


# # Trouve les champignons manquant de la liste
# champi_manquant <- mycoliste |>
#   dplyr::filter(
#     !nom_latin %in% liste_esp_joint$nom_scientifique
#   ) |>
#   dplyr::select(
#     "nom_scientifique" = nom_latin,
#     ordre,
#     nom_francais,
#     ancien_s_nom,
#     division,
#     sous_division,
#     classe
#   )
#
# # Trouve les groupes de champignons (selon le niveau taxonomique de l'ordre )
# group_champi <- liste_esp_joint |>
#   dplyr::filter(
#     groupe_taxonomique %in%
#       c(
#         "Lichens - Lichens",
#         "Macrofungi - Macrochampignons",
#         "Slime moulds - Myxomycètes"
#       )
#   ) |>
#   # compte le nombre de fois que l'ordre est trouvé
#   count(groupe_taxonomique, ordre) |>
#   # Trouve si l'ordre est dupliqué (si oui, il y a 2 groupes pour un même ordre)
#   # Assume que si pas dupliqué, c'est donc le groupe taxonomique unique.
#   mutate(dup_grp = duplicated(ordre)) |>
#   # Garde seulement les éléments NON-dupliqués
#   filter(!dup_grp) |>
#   # Enlève les colonnes intermédiaires
#   dplyr::select(-c(n, dup_grp))
#
# # jointure des données complètes
# liste_esp_complete_joint <-
#   liste_esp_joint |>
#   bind_rows(
#     champi_manquant |>
#       left_join(y = group_champi, by = join_by(ordre))
#   )
#
#
# # Sp dans VASCAN qui ne sont pas dans les autres listes
# nasp <- vascanqc |>
#   dplyr::filter(
#     !(scientificName %in%
#       liste_esp_complete_joint$nom_scientifique),
#     # Retire les hybrides
#     !grepl(pattern = "×", x = scientificName)
#   ) |>
#   dplyr::rename(nom_scientifique = scientificName) |>
#   dplyr::mutate(sp2w = stringr::word(nom_scientifique, 1, 2))
#
# # La majorité de ce qui manque comme nom d'espèce est "Subspecies" et "Variety"
# nasp |> dplyr::count(rank)
#
# liste_esp_complete <- liste_esp_complete_joint |>
#   # Joindre seulement les espèces restantes (ne pas prendre Subspecies et Variety)
#   dplyr::bind_rows(nasp |> dplyr::filter(rank == "Species"))
#

################################
# Extraire nom des espèces API GBIF ------------------------------------------
# --> Tableau de données pour passer à l'API de GBIF via le progiciel `rgbif`
# --> Ajout de 'order' et 'family' pour éviter les faux positifs

# Était liste_esp_complete
listes_especes_api_gbif <- liste_esp_joint |>
  # Sélectionne des colonnes pour rgbif
  dplyr::select(
    # Noms scientifiques à chercher
    scientificName = nom_scientifique,
    # Raffiner la recherche taxonomique avec ordre et famille
    # Probablement pour éviter les faux positifs
    order = ordre,
    family = famille
  ) |>
  dplyr::mutate(
    # Index de 1 au total du nombre de noms d'espèces
    id = dplyr::row_number()
  )

message(sprintf("Nb de rangées : %s", nrow(listes_especes_api_gbif)))


################################
# rgbif : Extraction noms espèces avec taxonomique de GBIF --------------------

message("Extraire les noms avec l'API de GBIF (rgbif)")
# rgbif: Match names from the data frame
# Cette fonction tente de trouver les noms fournis
# dans la banque de données de
# 190 sec (pour 50591) # 3 min-ish
# 1630 sec pour 54174 noms d'espèces
tictoc::tic()
df_matches <- rgbif::name_backbone_checklist(
  listes_especes_api_gbif,
  verbose = TRUE # Montre les alternatifs
)
tictoc::toc()


writexl::write_xlsx(
  x = df_matches,
  path = file.path(
    out_liste,
    "liste_esp_GBIF_match_API.xlsx"
  )
)
# Exploration des noms -------------

# Extraction des noms d'espèces seulement
df_matches_simple <- df_matches |>
  dplyr::filter(
    # Retirer les noms alternatifs
    !is_alternative,
    rank == "SPECIES"
  ) |>
  dplyr::mutate(
    # Vérification que les noms canoniques sont les mêmes que les noms verbatim
    check = canonicalName == verbatim_name
  )

# Vérification des noms différents de la taxonomie de GBIF
df_matches_simple |>
  dplyr::select(
    canonicalName,
    verbatim_name,
    check
  ) |>
  dplyr::filter(!check)

# Voir les types de rangs
df_matches_simple |>
  dplyr::count(rank)

# Vérification si TOUS les IDs originaux sont dedans le tableau final
# Si oui, == integer(0)
setdiff((listes_especes_api_gbif$id), (df_matches$verbatim_id)) # Données originales
setdiff((listes_especes_api_gbif$id), (df_matches_simple$verbatim_id)) # Après filtres...

# combiner avec données GBIF
#
especes_sauvage_canada_noms_communs_gbif <- liste_esp_joint |> # liste_esp_complete |>
  # Ajout des noms GBIF
  dplyr::left_join(
    df_matches_simple,
    by = dplyr::join_by(nom_scientifique == verbatim_name)
  ) |>
  # séparation colonnes de wild species et les informations ajoutés pour les
  # listes d'espèces
  tidyr::separate(
    groupe_taxonomique,
    into = c("Type_EN", "Type_FR"),
    sep = " - "
  ) |>
  # Extraire ce qui se trouve entre \"TEXTE\"
  dplyr::mutate(
    desc_fr = stringr::str_extract(
      string = fr_justification_fr,
      # Match tout ".*" ce qui se retrouve entre " et "
      pattern = "(?<=\").*(?=\")"
    )
    # rank = dplyr::coalesce(rank.x, rank.y)
  ) #|>
# Retirer les colonnes coalesce
# dplyr::select(
#   -c(rank.x, rank.y)
# )

names(especes_sauvage_canada_noms_communs_gbif)
head(especes_sauvage_canada_noms_communs_gbif)


# Exportationdes noms extraient de rgbif
write.csv(
  x = especes_sauvage_canada_noms_communs_gbif,
  row.names = FALSE,
  file = file.path(
    "output",
    "esp_noms_gbif_maj.csv"
  )
)

writexl::write_xlsx(
  x = especes_sauvage_canada_noms_communs_gbif,
  path = file.path(
    out_liste,
    "esp_noms_gbif_maj.xlsx"
  )
)
