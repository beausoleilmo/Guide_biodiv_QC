message("Importer : données hydrologiques (déjà préparé...)")
# données hydrologiques du Québec
# hpol <- sf::st_read(
#   dsn = file.path(
#     "~/Github_proj/evologie",
#     "posts/guide_biodiv_qc/2025_05_24_Guide_biodiv_qc/",
#     "data/partie_1/hydro/grhq_sud_qc.gpkg"
#   ),
#   quiet = TRUE
# )
#
hpol <- sf::st_read(
  dsn = "data/mod/grhq_rhs_simple.parquet",
  quiet = TRUE
)
