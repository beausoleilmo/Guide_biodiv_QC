#################################################################
# TODO : clean cette partie
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
# Suite à l'obtention et l'extraction des données de GBIF,
# les noms d'espèces sont regardés pour voir les correspondances entre la
# liste de GBIF et la liste d'espèces obtenue de diverse source.
# NOTE : les données d'espèces (taxonomiques) sont un réel défis.
# Certaines espèces ont des noms différents, mais c'est peut être une erreur :
# voir Amanita amerivirosa et Amanita virosa. Les deux sont des amanites
# vireuses, mais la première est d'Amérique du Nord alors que la 2e est
# d'Europe. Il est probable que l'identification de l'amanite avant une époque
# ait été A. virosa, mais qu'il a été reconnu plus tard au Québec qu'il y
# avait une autre espèce soit A. amerivirosa.
#
# NOTE : speciesKey :
# speciesKey est un code qui résout un nom d'espèce même si plusieurs synonymes
# sont présents.

# Extraction des noms d'espèces des données GBIF après les différents filtres
con <- evolovr::setup_duckdb()
gb_tbl <- tbl(
  src = con,
  from = sprintf("read_parquet('%s')", paths$out_gbif_pq)
)


# Lecture des données de la liste des espèces combinées
out_liste <- "output/biodiv/liste_especes"

especes_sauvage_canada_noms_communs_gbif <- arrow::read_parquet(
  file = file.path(
    out_liste,
    "esp_noms_gbif_maj.parquet"
  )
)


# Exploration de l'extraction des nom d'espèces de GBIF.

# Vérifier noms d'amphibiens (Rana vs lithobates)
gb_amphibien <- gb_tbl |>
  dplyr::filter(
    grepl(pattern = "Rana", x = scientificName),
    class == "Amphibia"
  ) |>
  dplyr::distinct() |>
  dplyr::collect() |>
  # Convert any list-columns into character strings joined by semicolon
  mutate(across(
    where(is.list),
    ~ map_chr(.x, ~ paste(unlist(.x), collapse = "; "))
  ))


gb_dat <- gb_tbl |>
  distinct(scientificName, .keep_all = TRUE) |>
  # count(scientificName, verbatimScientificName, order, class, family) |>
  collect() |>
  # arrange(desc(n)) |>
  # mutate(sp2w = stringr::word(scientificName, 1, 2))
  mutate(
    sp2w = scientificName |>
      str_remove_all("\\s*\\([^)]+\\)") |> # Remove parentheticals like (Danaus) or (Linnaeus, 1758)
      word(1, 2)
  ) |>
  # Convert any list-columns into character strings joined by semicolon
  mutate(across(
    where(is.list),
    ~ map_chr(.x, ~ paste(unlist(.x), collapse = "; "))
  ))
nrow(gb_dat)


# Valeurs dupliquées dans les données d'espèces
especes_sauvage_canada_noms_communs_gbif |>
  count(speciesKey) |>
  filter(n > 1)

esp_sauv_unique_spkey <- especes_sauvage_canada_noms_communs_gbif |>
  # Retrait des données dupliquées dans speciesKey
  dplyr::select(
    -c(
      "scientificName",
      "kingdom",
      "phylum",
      "class",
      "order",
      "family",
      "genus",
      "species",
      "infraspecificEpithet"
    )
  ) |>
  distinct(
    speciesKey,
    .keep_all = TRUE
  )

# Joindre les données GBIF et la liste d'espèces
gb_sp_match <- gb_tbl |>
  # Données d'espèces avec nom commun
  dplyr::left_join(
    y = esp_sauv_unique_spkey,
    # speciesKey est ce qui est commun entre les synonymes
    by = join_by(
      speciesKey
    ),
    # mettre dans la table d'espèces dans la même connection des données GBIF
    copy = "temp-table"
  )

# Vérification des noms de colonnes qui sont dupliquées avec left_join
# devrait être character(0) si toutes les colonnes aux noms similaires
# sont retirées
coln <- colnames(gb_sp_match) |>
  grep(pattern = "\\.y", value = TRUE) |>
  gsub(pattern = "\\.y", replacement = "")

# trouver les
gb_nm_cnt <- gb_sp_match |>
  distinct(speciesKey, nom_commun_en_francais, .keep_all = TRUE) |>
  group_by(nom_commun_en_francais) |>
  count(nom_commun_en_francais) |>
  filter(n > 1) |>
  collect() |>
  arrange(desc(n))


sp_check <- gb_nm_cnt |>
  filter(!is.na(nom_commun_en_francais)) |>
  pull(nom_commun_en_francais)

check_gb <- gb_sp_match |>
  filter(nom_commun_en_francais %in% sp_check) |>
  distinct(speciesKey, nom_commun_en_francais, .keep_all = TRUE) |>
  collect() |>
  # Convert any list-columns into character strings joined by semicolon
  mutate(across(
    where(is.list),
    ~ map_chr(.x, ~ paste(unlist(.x), collapse = "; "))
  )) |>
  select(speciesKey, canonicalName, nom_commun_en_francais) |>
  arrange(nom_commun_en_francais)


# Requête IA pour comprendre pourquoi certaines espèces ont le même nom
# français.
# De cette liste d'espèces pourquoi le même nom français pour plusieurs
# nom latin. expliquer chaque paire de nom sous forme d'un tableau.
# les noms viennent de gbif. par exemple pour l'amanite, ce serait à cause
# que l'espèce aurait été assigné après que des recherches auraient
# découverte l'espèce de l'Amérique du nord. check_gb
# # A tibble: 20 × 3
#    speciesKey canonicalName             nom_commun_en_francais
#    <chr>      <chr>                     <chr>
#  1 9YMZF      Amanita amerivirosa       Amanite vireuse
#  2 5TZ3B      Amanita virosa            Amanite vireuse
#  3 NNDH       Chrismofulvea dialyta     Buellie dispersée
#  4 MD3T6      Buellia dispersa          Buellie dispersée
#  5 6ZFFN      Squamulea subsoluta       Caloplaque crénelée
#  6 68MDH      Blastenia crenularia      Caloplaque crénelée
#  7 VLNTH      Carex gynocrates          Carex à côtes
#  8 R7XT       Carex alascana            Carex à côtes
#  9 6NGXM      Juncus tweedyi            Jonc brévicaudé
# 10 VK6W5      Juncus brevicaudatus      Jonc brévicaudé
# 11 C873Z      Mutarda arvensis          Moutarde des champs
# 12 VJT6H      Sinapis arvensis          Moutarde des champs
# 13 C8744      Mutarda nigra             Moutarde noire
# 14 VL8FM      Brassica nigra            Moutarde noire
# 15 35KNB      Dichanthelium lanuginosum Panic laineux
# 16 35KLQ      Dichanthelium acuminatum  Panic laineux
# 17 TWXVJ      Usnea strigosa            Usnée touffue
# 18 TWXZS      Usnea subfusca            Usnée touffue
# 19 5BH3V      Viola macloskeyi          Violette pâle
# 20 9KY8V      Viola minuscula           Violette pâle
#
#Voici l'explication détaillée pour chaque paire de votre tableau :
# Paire de noms scientifiques	Nom commun	Raison explicative de la dualité des noms latins
# Amanita amerivirosa
# Amanita virosa
# Amanite vireuse	Séparation géographique et spéciation : Amanita virosa est une espèce strictement européenne. Les spécimens nord-américains (comme ceux du Québec) longtemps identifiés sous ce nom ont été reconnus comme une espèce distincte et renommés Amanita amerivirosa. Le nom français s'est maintenu pour les deux.
#
# Chrismofulvea dialyta
# Buellia dispersa
# Buellie dispersée	Reclassement générique / Changement de genre : Le lichen Buellia dialyta a été reclassé dans le genre Chrismofulvea (Chrismofulvea dialyta). Buellia dispersa est une autre espèce du même genre originel. GBIF associe la "Buellie dispersée" à ces entités de la même lignée.
#
# Squamulea subsoluta
# Blastenia crenularia
# Caloplaque crénelée	Révision taxonomique de la famille des Teloschistaceae : Les genres de lichens de l'ancien grand genre Caloplaca ont été morcelés en plusieurs nouveaux genres (Squamulea, Blastenia). L'attribution du nom vernaculaire "Caloplaque" persiste pour ces taxons reclassés.
#
# Carex gynocrates
# Carex alascana
# Carex à côtes	Typification et synonymie taxonomique : Des études récentes sur la flore nord-américaine (notamment au Canada/VASCAN) ont établi que Carex alascana est le nom valide/accepté pour ce taxon, dont Carex gynocrates est un synonyme couramment employé dans la littérature plus ancienne.
#
# Juncus tweedyi
# Juncus brevicaudatus
# Jonc brévicaudé	Règle de priorité du nom le plus ancien : Une étude de Knapp et al. (2022) a démontré que Juncus tweedyi (publié en 1900) et Juncus brevicaudatus (publié en 1904) désignent la même espèce. Selon les règles nomenclaturales, Juncus tweedyi priorise et remplace J. brevicaudatus.
#
# Mutarda arvensis
# Sinapis arvensis
# Moutarde des champs	Francisation de genre / Variante orthographique : Sinapis arvensis est le nom botanique valide. "Mutarda" est une variante/francisation nomenclaturale historique parfois encodée dans les bases de données informatiques.
#
# Mutarda nigra
# Brassica nigra
# Moutarde noire	Francisation de genre / Synonyme nomenclatural : Tout comme pour la moutarde des champs, Brassica nigra est le nom scientifique valide accepté. L'entrée Mutarda nigra reflète un doublon ou un synonyme sous-jacent issu de répertoires historiques.
#
# Dichanthelium lanuginosum
# Dichanthelium acuminatum
# Panic laineux	Fusion taxonomique (Synonymie) : Dichanthelium lanuginosum est aujourd'hui généralement considéré comme une variété ou un synonyme de Dichanthelium acuminatum, une espèce complexe très variable.
#
# Usnea strigosa
# Usnea subfusca
# Usnée touffue	Nom vernaculaire paraphyLétique ou généraliste : En lichénologie, le nom commun "Usnée touffue" est appliqué de manière large à plusieurs espèces morphologiquement très proches du genre Usnea présentes dans la même région.
#
# Viola macloskeyi
# Viola minuscula
# Violette pâle	Statut d'espèce vs sous-espèce : Viola minuscula est souvent traitée soit comme une espèce distincte, soit comme une sous-espèce de Viola macloskeyi (Viola macloskeyi subsp. pallens). Le même nom vernaculaire leur est assigné.
#

colnames(gb_sp_match)
gb_sp_match |>
  count(Type_FR) |>
  collect() |>
  arrange(Type_FR) |>
  as.data.frame()
# Exploration des noms manquants en français.
# pour les speciesKey uniques
sp_na_fr <- gb_sp_match |>
  filter(is.na(nom_commun_en_francais)) |>
  distinct(speciesKey, .keep_all = TRUE) |>
  count(
    nom_commun_en_francais,
    desc_fr,
    Type_FR,
    kingdom,
    phylum,
    class,
    order,
    family,
    scientificName,
    basisOfRecord
  ) |>
  collect() |>
  arrange(class, scientificName)

# Compte le nombre de noms manquants en français.
compte_sp_na_fr <- gb_sp_match |>
  filter(is.na(nom_commun_en_francais)) |>
  count(
    nom_commun_en_francais,
    desc_fr,
    Type_FR,
    kingdom,
    phylum,
    class,
    order,
    family,
    scientificName,
    basisOfRecord
  ) |>
  collect() |>
  arrange(desc(n), class, scientificName)


compte_sp_class <- compte_sp_na_fr |>
  filter(is.na(Type_FR)) |>
  count(class) |>
  arrange(desc(n))

compte_sp_fam <- compte_sp_na_fr |>
  filter(is.na(Type_FR)) |>
  count(class, family) |>
  arrange(desc(n))

# Vérification des Type_FR manquant pour compléter la liste manuellement
# construite.
type_fr_path <- file.path(
  'data/raw/biodiv/more',
  "corresp_table_class_orderType_FR.csv"
)
mapping_tbl <- read.csv(type_fr_path)
m_famil <- mapping_tbl |>
  filter(Rank == "Family") |>
  select(Value, family_match = Type_FR_Mapped)
m_class <- mapping_tbl |>
  filter(Rank == "Class") |>
  select(Value, class_match = Type_FR_Mapped)
m_order <- mapping_tbl |>
  filter(Rank == "Order") |>
  select(Value, order_match = Type_FR_Mapped)
m_kingdom <- mapping_tbl |>
  filter(Rank == "Kingdom") |>
  select(Value, kingdom_match = Type_FR_Mapped)
m_phylum <- mapping_tbl |>
  filter(Rank == "Phylum") |>
  select(Value, phylum_match = Type_FR_Mapped)

tab_type <- compte_sp_na_fr |>
  ### Correction des Type_FR
  # Joindre pour ajouter la colonne de Famille modifiée
  left_join(m_famil, by = c("family" = "Value")) |>
  # Joindre pour ajouter la colonne de Classe modifiée
  left_join(m_class, by = c("class" = "Value")) |>
  # Joindre pour ajouter la colonne de Ordre modifiée
  left_join(m_order, by = c("order" = "Value")) |>
  # Joindre pour ajouter la colonne de Règne modifiée
  left_join(m_kingdom, by = c("kingdom" = "Value")) |>
  # Joindre pour ajouter la colonne de Embramchement modifiée
  left_join(m_phylum, by = c("phylum" = "Value")) |>
  # 5. Coalesce logic (Priority: Species > Order > Class > Kingdom)
  mutate(
    # Coalesce dans l'ordre pour garder le plus précis possible, sinon prendre 'spnm_type' si rien d'autre
    Type_FR = coalesce(
      order_match,
      family_match,
      class_match,
      phylum_match,
      kingdom_match
    )
  )

missing_type <- tab_type |>
  filter(is.na(Type_FR))
missing_type |> select(kingdom, phylum, class, order, family) |> as.data.frame()


head(gb_nm_cnt, n = 15)

gb_sp_match |>
  filter(
    # is.na(nom_commun_en_francais),
    nom_commun_en_francais %in% c("Amanite vireuse", "Carex à côtes")
  ) |>
  select(species, nom_commun_en_francais, speciesKey) |>
  collect() |>
  as.data.frame()

miss_sp <- especes_sauvage_canada_noms_communs_gbif |>
  filter(!(speciesKey %in% gb_dat$speciesKey))
especes_sauvage_canada_noms_communs_gbif |>
  filter(speciesKey %in% gb_dat$speciesKey)

sp_miss <- gb_dat |>
  # dplyr::select(EncodeElement) |>
  filter(
    !(speciesKey %in% especes_sauvage_canada_noms_communs_gbif$speciesKey)
  ) |>
  # dplyr::select(sp2w, order, verbatimScientificName, class, family) |>
  arrange(class)
