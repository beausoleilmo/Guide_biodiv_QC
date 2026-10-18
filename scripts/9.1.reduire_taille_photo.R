# Rendre le script exécutable
system2(
  command = "chmod",
  args = c("+x", normalizePath("scripts/utils/reduce_image_size.sh"))
)

# Afficher l'aide et description de la fonction
system2(
  command = "bash",
  args = c(
    "scripts/utils/reduce_image_size.sh",
    "--help"
  )
)
# Exécution de la réduction de la taille des photos
system2(
  command = "bash",
  args = c(
    "scripts/utils/reduce_image_size.sh",
    "output/images/main_page_image",
    "output/images/main_page_image2/"
  )
)
