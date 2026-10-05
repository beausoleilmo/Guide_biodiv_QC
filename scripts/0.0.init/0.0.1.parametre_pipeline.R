# Variables pour rouler ou non certaines parties du pipeline
polygone_qc_gbif <- FALSE # Polygone pour filtrer les données GBIF
telecharge_gbif_donnee <- FALSE # Prend du temps à exécuter
transforme_gbif_donnee <- FALSE # ajout des régions admin, H3

cat(
  glue::glue(
    "Modules chargés (voir 99.1) :
Admin; union polygones du Québec : {polygone_qc_gbif}
GBIF ; Téléchargement données GBIF : {telecharge_gbif_donnee}
Transformation des données GBIF : {transforme_gbif_donnee}
"
  ),
  fill = TRUE
)
# Résolution de la grille H3 à prendre
res_fine <- 10L

# Résolution parent par rapport à la résolution généré initialement
# Résolution avec laquelle nous allons faire les analyses.
# Nous n'avons pas vraiment besoin d'aller plus précis que H3-niveau-10
# Donc, nous allons avoir une résolution plus grossière si on diminue
# res_parent
res_parent <- 9L
if (res_fine < res_parent) {
  stop(
    "res_parent est plus élevé que res_fine. 
Nécessaire : res_parent <= res_fine"
  )
}

message(
  sprintf(
    "res_fin = %s\nres_parent = %s",
    res_fine,
    res_parent
  )
)
