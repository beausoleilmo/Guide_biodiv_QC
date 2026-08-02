library(rmarkdown)
library(dplyr)

# ! Note !
# Le nombre d'observation par espèce n'est pas le même que iNat et est dans certain cas 
# beaucoup plus bas. C'est à cause des filtres et en particulier celui 
# des incertitudes au niveau des coordonnées. De plus, les données iNaturalist peuvent compter 
# les observations qui ne sont PAS de "qualité recherche" et donc ne pas se retrouver dans GBIF.
# Les données exposées ici sont SEULEMENT celles de GBIF. 

# Note QMD
# tout doit être fait RELATIF (chemin d'accès) au .qmd 

# Change entre 3 et 7 colonnes
column_count <- 6

base_path = file.path(
  "~/Github_proj/evologie/InCubateur/2025_05_24_Guide_biodiv_qc/",
  "output/partie_2"
  )

# params = list(
#     num_cols = column_count
#   )
# 11 s
tictoc::tic()
# Faire le rapport 
rmarkdown::render(
  # input = "~/Github_proj/Guide_biodiv_QC/scripts/guide.Rmd",
  input = "scripts/guide_regs.qmd",
  params = list(
    num_cols = column_count
  ),
  output_file = file.path(
    "..", # Mettre la sortie à la racine du dossier 
    paste0("index.html")
    # paste0("Report_", target_region, ".html")
  )
)
tictoc::toc()

# logo <- base64enc::base64encode("img/IMG_1160.jpg")
# nchar(logo)
# À inclure dans le fichier meta-head.html
# <link rel="apple-touch-icon" sizes="180x180" href="data:image/png;base64,">

