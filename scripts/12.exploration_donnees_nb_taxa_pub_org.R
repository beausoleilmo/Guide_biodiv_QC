# Nombre de taxa par organisme de publication 
nb_obs_type_pub_inst = dbGetQuery(conn = con, statement = "
    SELECT 
gbifdata.Type_FR, 
count(*) as n_inst,
pubkey.org_publication
  FROM 'InCubateur/2025_05_24_Guide_biodiv_qc/data/partie_2/biodiv/gbif_data/gbif_prep_type_fr_h3.parquet' AS gbifdata
-- DONNÉES EXTRAITES DE L'API DE GBIF -- pubkey
    LEFT JOIN read_csv('InCubateur/2025_05_24_Guide_biodiv_qc/data/partie_2/biodiv/gbif_data/gbif_pubOrg_key.csv', header=True) AS pubkey
      ON gbifdata.publishingorgkey = pubkey.publishingorgkey 
    GROUP BY ALL
    ORDER BY n_inst DESC;
")

nb_taxa_org = nb_obs_type_pub_inst |> 
  summarise(nb = n(),.by = org_publication) |> 
  arrange(desc(nb))

nb_obs_type_pub_inst |> 
  pivot_wider(names_from = Type_FR,values_from = n_inst)

orgs_pub = nb_obs_type_pub_inst |> 
  summarise(tot = sum(n_inst),.by = org_publication) |> 
  filter(tot >= 1000) |> pull(org_publication)
nb_obs_type_pub_inst |> 
  filter(org_publication %in% orgs_pub) |> 
  ggplot() + 
  geom_bar(aes(x = Type_FR, 
               y = log(n_inst), 
               fill = Type_FR), stat = 'identity') + 
  facet_wrap(.~org_publication, scales = "free_y") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
