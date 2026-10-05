## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Définition de fonction supplémentaire pour exécuter les scripts
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2026-03-28
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Extraire fichiers photos des espèces d'iNaturalist pour le guide

## ____________####
## Prépare l'environnement --------

# Test si fonction marchent du progiciel evolovr 
# source(file = file.path(
#   "~/Github_proj/evologie/posts/guide_biodiv_qc/2025_05_24_Guide_biodiv_qc",
#   "scripts/00_init/functions.R"
# ))


### Téléchargement des images 'Typiques' de iNaturalist ---------------------

# Lire la liste d'espèces et de photos iNaturalist
sp_list_fil <- read.csv2(
  file = file.path(
    incub,
    "output/partie_2/sp_list_fil_iNat_nomFR.csv"
  )
)

# Dossier pour télécharger les photos (originales provenant d'iNaturalist)
out_down <- file.path(
  incub,
  "output/images/main_page_image"
)

# Liste de noms d'espèces 
sp_nm_list = sp_list_fil |> 
  dplyr::distinct() 
# dplyr::distinct(
#   species, 
#   kingdom,
#   phylum,
#   class,
#   order,
#   family, 
#   default_photo_medium_url, # photo_url
#   default_photo_license_code) 


# Noter la licence des photos.
# Il n'est pas possible de toutes les utiliser.
# Il faut donc utiliser des photos alternatives 
sp_list_fil |>
  dplyr::distinct(species, 
                  default_photo_license_code) |> 
  dplyr::count(default_photo_license_code)

sp_list = sp_list_fil |>
  dplyr::distinct(species)

# Garde le nom d'espèce et le fichier de photo
# Cela sert à connaître la position de la photo sur l'ordinateur 
# pour faire référence à celle-ci au besoin 
sp_photo = NULL


progres_message = 25
# Téléchargement pour chaque espèce
# --> MAJ tous les progres_message
for (sp_dwln_idx in 1:nrow(sp_nm_list)) {
  
  if (sp_dwln_idx %% progres_message == 0 ) {
    message(sprintf(fmt = "%d/%03d",sp_dwln_idx, nrow(sp_nm_list)))
  }
  
  sp_tmp <- sp_nm_list[sp_dwln_idx, ]

 
  # Dans le dossier de téléchargement, faire 
  # une hiérarchie de dossier jusqu'à la famille 
  out_down_file <- file.path(
    out_down,
    sp_tmp$kingdom,
    sp_tmp$phylum,
    sp_tmp$class,
    sp_tmp$order,
    sp_tmp$family,
    paste0(
      make.names(sp_tmp$species),
      ".", 
      tools::file_ext(sp_tmp$default_photo_medium_url))
  )
  
  
  # Si photo url == NA, noter cela dans les données et passer au suivant
  if (is.na(sp_tmp$default_photo_medium_url)) {
    # message("URL == NA")
    # Données temporaire pour ajouter au tableau à chaque itération 
    sp_photo_tmp = sp_tmp |> 
      dplyr::select(species) |> 
      # Colonne du nom du fichier
      dplyr::bind_cols(fichier_photo = NA)
    
    next
  }  else {
    # Données temporaire pour ajouter au tableau à chaque itération 
    sp_photo_tmp = sp_tmp |> 
      dplyr::select(species) |> 
      # Colonne du nom du fichier
      dplyr::bind_cols(
        fichier_photo = out_down_file
        )
  }
  
  # Ajouter les données par incrément 
  sp_photo = dplyr::bind_rows(
    sp_photo, 
    sp_photo_tmp
    )
  
  # Passe au prochain si le fichier existe déjà
  if (file.exists(out_down_file)) {
    message("Passe au prochain (fichier existe déjà)")
    
    next
  }
  
  # Créer la hierarchie des dossiers 
  dir.create(
    path = dirname(out_down_file), 
    showWarnings = FALSE, 
    recursive = TRUE
  )
  
  message(sprintf("Téléchargement ici : \n%s", out_down_file))
  # Téléchargement
  utils::download.file(
    url = sp_tmp$default_photo_medium_url,
    destfile = out_down_file
  )
  
  # Rallentir pour ne pas surcharger iNaturalist
  Sys.sleep(1)
  
}

sp_list_fil_all = sp_list_fil |> 
  dplyr::left_join(
    y = sp_photo, 
    by = join_by(species), 
    relationship = "many-to-many"
  )

readr::write_excel_csv2(
  x = sp_list_fil_all, 
  file = file.path(
    incub,
    "output", 
    "partie_2",
    "sp_list_fil_all.csv"
  ))


### Téléchargement de photos alternatives -----------------------------------
cc0_extract = FALSE

if (cc0_extract) {
  
  # Ces photos sont choisies pour leur licence ouverte et donc l'utilisation dans le produit 
  sp_list_no_res <- NULL
  year_select <- 2025
  max_res <- 50
  photo_lic <- "CC0" #"CC-BY"# "CC0" ou "CC-BY"
  place_id <- 6712 # Canada
  
  fold_out <- file.path(
    incub,
    "output/images", 
    sprintf("iNat_%s", make.names(photo_lic))
  )
  
  
  # Ajouter dossier si manquant 
  dir.create(
    path = fold_out, 
    showWarnings = FALSE)
  
  
  # sp_check_i <- 1 # Pour tester 
  for (sp_check_i in 1:nrow(sp_list)) {
    species_i <- sp_list[sp_check_i, , drop=FALSE] |> pull(species)
    
    message(sprintf(
      "%s  -  %s/%s  (%s %%)",
      species_i,
      sp_check_i, nrow(sp_list),
      round(sp_check_i / nrow(sp_list) * 100, 2)
    ))
    
    
    # Tous les X requête, prend pause!
    if (sp_check_i %% 20 == 0) {
      message(sprintf(
        fmt = "Done %s/%s", sp_check_i, nrow(sp_list)
      ))
      Sys.sleep(time = 10)
    }
    
    # Données iNaturalist avec photo. Voir ?iNatTry
    sp.out <- iNatTry(
      sp_check = species_i,
      year = year_select,
      photo_license = photo_lic,
      place_id = place_id,
      maxresults = max_res
    )
    
    
    # Ajout Dryophytes versicolor manuellement
    # sp.out = get_inat_obs(taxon_id = "1668923",
    #                               year = year_select,
    #                               photo_license = 'CC0',
    #                               maxresults = max_res)
    
    # Si sp.out n'est qu'une suite de caractère, passe au prochain
    if (class(sp.out) == "character") {
      next
    }
    
    # Prendre en note les informations
    sp_list_no_res <- c(
      sp_list_no_res,
      if (class(sp.out) == "character") {
        sp.out
      }
    )
    
    sp.out <- sp.out |>
      dplyr::filter(license == photo_lic)
    
    # Si nombre de rangés = 0, passe au prochain
    if (nrow(sp.out) == 0) {
      next
    }
    
    # Trouver les fichiers déjà téléchargés
    fichiers_local <- list.files(
      path = fold_out,
      pattern = ".jpg",
      recursive = TRUE, 
      full.names = TRUE
    )
    
    # Extraire l'ID iNaturalist de la photo
    inat_ID <- gsub(
      pattern = ".*ID\\.(.*?)\\.jpg", 
      replacement = "\\1", 
      x = basename(fichiers_local))
    
    # Enlever les fichiers déjà télécharger
    sp.out_a_faire <- sp.out |>
      dplyr::filter(!(id %in% inat_ID))
    
    # Si nombre de rangés = 0, passe au prochain
    if (nrow(sp.out_a_faire) == 0) {
      next
    }
    
    
    # url_image_i = 1 # Pour test
    message(sprintf("Nb rows: %s", nrow(sp.out_a_faire)))
    for (url_image_i in 1:nrow(sp.out_a_faire)) {
      df_tmp_row <- sp.out_a_faire[url_image_i, ]
      
      # Fabrication du chemin d'accès pour télécharger les photos
      order_name <- unique(sp_list[sp_check_i, ] |> pull(order))
      if (is.na(order_name)) {
        order_name <- unique(sp_list[sp_check_i, ] |> pull(class))
      }
      
      dir_sp <- file.path(
        fold_out,
        sprintf(
          "%s/%s_%s/%s_%s",
          
          # iNat iconic name
          make.names(unique(df_tmp_row$iconic_taxon_name)),
          # Ordre
          make.names(order_name),
          # Famille
          make.names(unique(sp_list[sp_check_i, ] |> pull(family))),
          # nom latin
          make.names(unique(df_tmp_row$scientific_name)),
          # Nom commun
          make.names(unique(df_tmp_row$common_name))
        )
      )
      
      dir.create(
        path = dir_sp,
        recursive = TRUE,
        showWarnings = FALSE
      )
      
      title <- sprintf(
        "%s_%s_%s_licence.%s_ID.%s",
        df_tmp_row$iconic_taxon_name,
        df_tmp_row$common_name,
        df_tmp_row$scientific_name,
        df_tmp_row$license,
        df_tmp_row$id
      )
      
      # Nom du fichier
      name_file <- sprintf("iNat_%s", make.names(title))
      tmp_f <- file.path(
        dir_sp,
        sprintf("%s.jpg", name_file)
      )
      
      # Téléchargement
      utils::download.file(
        url = df_tmp_row$image_url,
        destfile = tmp_f
      )
      
      # Imprime message de suivi d'exportation
      mess <- sprintf("fichier au %s", tmp_f)
      message(mess)
      # plot_jpeg(path = df_tmp_row$image_url)
      # mtext(text = title)
      # invisible(readline(prompt="Press [enter] to continue"))
    }
  }
}
