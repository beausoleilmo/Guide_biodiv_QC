## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-04-2?
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
# --> Extraction des publishing organisation key pour trouver leur noms 
#     avec l'API de GBIF 
# --> Extraction des dataset key pour trouver leur noms (moins utile ici)

## ____________####

# Create a connection to an in-memory DuckDB instance
con <- DBI::dbConnect(duckdb::duckdb())

gbif_data = file.path(
  "InCubateur/2025_05_24_Guide_biodiv_qc/", 
  "data/partie_2/biodiv/gbif_data/gbif_raw_new.parquet") # Était gbif_prep_h3.parquet

# Nom unique des organismes de publication de données 
requete = sprintf(
  "SELECT DISTINCT publishingorgkey
  FROM '%s';",
  gbif_data
)
# Exécuter la commande avec duckdb 
# obtenir une liste des organismes de publication du 
# tableau de données d'origine
porgk = DBI::dbGetQuery(con, statement = requete)

### Extraction datasetkey ---------------------------------------------
requete_datasets = sprintf(
  "SELECT DISTINCT publishingOrgKey, datasetKey, COUNT (*) as n
  FROM '%s'
  GROUP BY ALL
  ORDER BY n DESC;",
  gbif_data
)

datasetkey_df = DBI::dbGetQuery(
  conn = con, statement = requete_datasets)

length(datasetkey_df$datasetKey)

# Vecteur vide à remplir selon résultat de l'API de GBIF 
getnames_dataset = NULL
# Calcul nombre d'élément
len_dat = length(datasetkey_df$datasetKey)
# compteur 
w = 1

# Boucle pour chercher le nom des dataset 
for (dataset_key_i in datasetkey_df$datasetKey) {
  message(sprintf("%03d/%d, %s",w, len_dat, dataset_key_i ))
  w = w+1
  
  # Extraire information du GBIF 
  org_df = rgbif::dataset_get(uuid = dataset_key_i)
  
  puborg = datasetkey_df |> 
    dplyr::filter(datasetKey == dataset_key_i) |> 
    dplyr::pull(publishingOrgKey)
  # Organiser les données retournées par GBIF dans un tableau
  df_out = data.frame(
    name = org_df$title, 
    doi = ifelse(
      test = length(org_df$doi) == 0, 
      yes = "", 
      no = org_df$doi), 
    datasetKey = dataset_key_i,
    publishingorgkey = puborg
  )
  # Ajouter au tableau 
  getnames_dataset = rbind(getnames_dataset, df_out)
  
}

### Extraction publishingOrgKey ---------------------------------------------

# Extraction des clés seulement
getorgs = porgk$publishingOrgKey

# Combien de clés à trouver leur titre? 
length(getorgs)

# Vecteur vide à remplir selon résultat de l'API de GBIF 
getnames_orgs = NULL

# Faire toutes les clé d'organisme de publication de données 
for (pub_org_key_i in getorgs) {
  print(pub_org_key_i)
  
  # Extraire information du GBIF 
  org_df = rgbif::organizations(uuid = pub_org_key_i)$data
  
  # Organiser les données retournées par GBIF dans un tableau
  df_out = data.frame(
    org_publication = org_df$title, 
    homepage = ifelse(
      test = length(org_df$homepage) == 0, 
      yes = "", 
      no = org_df$homepage),
    description = ifelse(
      test = length(org_df$description) == 0, 
      yes = "", 
      no = org_df$description), 
    publishingOrgKey = pub_org_key_i
  )
  
  # Ajouter au tableau 
  getnames_orgs = rbind(getnames_orgs, df_out)
  
}

# Exportation des données extraites de GBIF 
write.csv(
  x = getnames_orgs, 
  file = file.path(
    incub, 
    "data/partie_2/biodiv/gbif_data/gbif_pubOrg_key.csv"
  ), 
  row.names = FALSE
)


### Compter nb observation par org_pub --------------------------------------
# Prendre les données exportés et les joindre aux noms d'organisations 
# Faire un compte d'observation par organisation 
# Les données sont groupées selon les catégories des organismes de publications 
# count_orgs = DBI::dbGetQuery(
#   conn = con,
#   statement = "
# SELECT
# gbifdata.institutionCode,
# COUNT(*) AS n_inst,
# pubkey.*
# -- DONNÉES ORIGINALES -- gbifdata
# FROM  'InCubateur/2025_05_24_Guide_biodiv_qc/data/partie_2/biodiv/gbif_data/gbif_prep_type_fr_h3.parquet' AS gbifdata
# -- DONNÉES EXTRAITES DE L'API DE GBIF -- pubkey
# LEFT JOIN read_csv('InCubateur/2025_05_24_Guide_biodiv_qc/data/partie_2/biodiv/gbif_data/gbif_pubOrg_key.csv', header=True) AS pubkey
#   ON gbifdata.publishingorgkey = pubkey.publishingorgkey
# GROUP BY ALL
# ORDER BY n_inst;
# ")

# Disconnect
# DBI::dbDisconnect(con, shutdown = TRUE)

duckdb_register(conn = con, name = "puborgkey", df = getnames_orgs, overwrite = TRUE)

# Point dbplyr to that virtual view
getnames_orgs_duck <- tbl(con, "puborgkey")

# Joindre les 2 tables
count_orgs = gbif_prep_type_fr_h3 |> 
  dplyr::select(institutionCode, publishingOrgKey) |> 
  left_join(
    y = getnames_orgs_duck, 
    by = join_by(publishingOrgKey)
  ) |> 
  # Group by all joined fields and count occurrences
  group_by(across(everything())) |> 
  summarise(n_inst = n(), .groups = "drop") |> 
  collect() |> 
  # Order by the resulting count
  arrange(n_inst)


# Sommaire groupé en noms des organismes seulement 
count_orgs |> 
  dplyr::group_by(publishingOrgKey, org_publication, homepage) |> 
  # Somme des 
  dplyr::summarise(sum_org = sum(n_inst, na.rm = TRUE)) |> 
  dplyr::arrange(-sum_org) |>
  View()
