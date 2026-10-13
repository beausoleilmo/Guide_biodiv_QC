# TODO : à mettre dans evolovr

#' Filtrer le nombre d'espèces itérativement
#'
#' @description
#' Les données d'occurrences d'espèces sont parfois trop volumineuses pour être
#' présenté au complet dans le guide de biodiversité. Un stratégie est de
#' synthétiser les observations en mettant une limite du nombre d'organismes
#' à montrer. Cette fonction permet de filtrer les données en gardant
#' un niveau taxonomique d'espèces (choisir les espèces dans chaque famille).
#' Les espèces finales sélectionnées sont alors échantillonnées en priorisant
#' les occurrences les plus fréquences.
#'
#' @param donnees data.frame. Tableau de données à filtrer pour un groupe
#' d'organismes
#' @param nb_min_esp Integer. Nombre d'espèce cible final pour chaque région
#' @param type Modèle de la fonctoin [more_prec()] c("m1", "m2", "m3",)
#' @param taxo_lvl Nom de colonne du niveau taxonomique à
#' grouper (`group_by()`). Nécessaire pour certain groupe taxonomique : grouper
#' par famille pour les plantes est 'trop précis' et on se retrouve avec trop
#' de famille avec 1 espèce dedans.
#'
#' @returns
#' @export
#'
#' @examples
#'
especes_proportion_famille <- function(
  donnees,
  nb_min_esp,
  type = "m1",
  taxo_lvl = "family",
  region_lvl = "MUS_NM_REG",
  maxiter = 100
) {
  message(sprintf("Niveau taxonomique : %s", taxo_lvl))
  # Pour enregistrer le processus itératif
  iter_rec <- NULL

  w <- 1 # Compte pour la boucle while

  # Au début, chaque région a une proportion de 100% = 1
  # Cette proportion sera ajusté en fonction d'un algorithme
  # de Décroissance exponentielle
  reg_prop <- donnees |>
    dplyr::distinct(!!rlang::sym(region_lvl)) |>
    # Débute avec proportion == 1 (votre chat peut tout prendre...)
    dplyr::mutate(prop = 1)

  if (w == maxiter) {
    message("Itération maximale atteinte")
  }
  # Boucle pour filtrer de plus en plus finement les données
  # afin d'avoir un nombre d'oiseaux par région
  while (w != maxiter) {
    # Continuer la sélection pour les oiseaux seulement
    select_sp_Aves <- donnees |>
      dplyr::left_join(
        y = reg_prop,
        by = dplyr::join_by(!!rlang::sym(region_lvl))
      ) |>
      dplyr::mutate(taxon_lvl_gr = {{ taxo_lvl }}) |>
      # Garder un nombre d'espèce par niveau taxonomique : famille
      dplyr::group_by(!!rlang::sym(region_lvl), taxon_lvl_gr) |>
      # Garde les données si c'est le top "top_proportion" ou minimum 1
      dplyr::filter(
        # S'assure de garder au moins 1 rangée!
        # desc(n): Place valeurs plus élevés en premier.
        # min_rank(...): Assigne un rang aux rangées. Plus grosse valeur = 1
        # n(): nb rangées dans groupe.
        # prop: fraction décimale à garder
        # n() * prop: Nombre de rangers à garder
        # pmax(1, ...): Compare 1 et la valeur calculé et choisi le + gros.
        dplyr::min_rank(
          dplyr::desc(n)
        ) <=
          base::pmax(1, dplyr::n() * prop)
      ) |>
      dplyr::arrange(dplyr::desc(n))

    # Compte, par région, le nb d'espèces restant après filtration
    aves_count <- select_sp_Aves |>
      dplyr::group_by(!!rlang::sym(region_lvl)) |>
      dplyr::summarise(sum = sum(pa))

    # enregistrer le processus de filtration
    if (w == 1) {
      # Si c'est 1re itération
      iter_rec <- iter_rec |>
        dplyr::bind_cols(aves_count)
    } else {
      iter_rec <- iter_rec |>
        dplyr::bind_cols(
          aves_count |>
            dplyr::select(-!!rlang::sym(region_lvl)) |>
            # Rennomer les colonnes séquentiellement
            dplyr::rename_with(
              ~ paste0("sum_", w),
              tidyselect::everything()
            )
        )
    }

    # Informe sur la précision actuelle selon le compteur 'w'
    # Extraire pour la valeur 'm1' = 1/x ou x^(-1).
    if (w %% 10 == 0) {
      message(
        sprintf(
          fmt = "Précision (%s) : %s/1000",
          w, # Itération courante
          # Arrondir pour avoir un chiffre assez précis (voir aussi format())
          round(
            more_prec(w)[[type]],
            4
          ) *
            1000
        )
      )
    }
    ## VALIDATION --
    # Doit tester si certaines régions ont atteint leur objectif suite :
    # Extraire région qui n'ont pas atteint le nombre d'espèces espéré
    reg_to_update <- aves_count |>
      # touver les régions qui ont plus de 'nb_min_esp' (Booléen)
      dplyr::mutate(prop_update = sum >= nb_min_esp) |>
      # Filtrer les régions qui ont trop d'espèces
      dplyr::filter(prop_update) |>
      dplyr::pull(!!rlang::sym(region_lvl))

    # S'il reste rien, quitter la boucle
    if (length(reg_to_update) == 0) {
      break
    }

    # print(reg_prop)

    # Mettre à jour le tableau de proportion avec la décroissance exponentielle
    reg_prop <- reg_prop |>
      dplyr::mutate(
        prop = ifelse(
          test = !!rlang::sym(region_lvl) %in% reg_to_update,
          # Remplacer la proportion initiale
          # Diminution de la précision à chaque coup
          # pour filtrer plus précisément
          yes = prop - more_prec(w)[[type]],
          no = prop
        )
      )

    # Augmente le compteur de 1
    w <- w + 1
  }

  return(list(select_sp_Aves, iter_rec))
}


# Certain fichiers de iNaturalist n'ont PAS d'Extensions
# On peut TESTER quelle extension les fichiers ont avec des 'bytes' signatures.
detect_image_types_fast <- function(image_urls) {
  # Track which values are NA
  is_na_url <- is.na(image_urls)

  # Initialize results vector with NA for those positions
  results <- character(length(image_urls))
  results[is_na_url] <- NA_character_

  # Extract only the valid, non-NA URLs for parallel processing
  valid_indices <- which(!is_na_url)
  valid_urls <- image_urls[valid_indices]

  # If all URLs are NA, return immediately
  if (length(valid_urls) == 0) {
    return(results)
  }

  # Requête pour URLs valides seulements
  reqs <- lapply(valid_urls, function(u) {
    httr2::request(u) |>
      httr2::req_headers(Range = "bytes=0-11") |>
      httr2::req_error(is_error = function(resp) FALSE)
  })

  #  Exécution en parallèle
  resps <- httr2::req_perform_parallel(reqs, on_error = "continue")

  # 4. Parse the results
  valid_results <- sapply(
    resps,
    function(resp) {
      if (inherits(resp, "error") || httr2::resp_status(resp) >= 400) {
        return("error")
      }

      header <- httr2::resp_body_raw(resp)
      # Vérification du type d'image
      if (
        length(header) >= 3 && all(header[1:3] == as.raw(c(0xFF, 0xD8, 0xFF)))
      ) {
        return("jpeg")
      } else if (
        length(header) >= 4 &&
          all(header[1:4] == as.raw(c(0x89, 0x50, 0x4E, 0x47)))
      ) {
        return("png")
      } else if (
        length(header) >= 4 &&
          all(header[1:4] == as.raw(c(0x47, 0x49, 0x46, 0x38)))
      ) {
        return("gif")
      } else if (
        length(header) >= 12 &&
          all(header[9:12] == as.raw(c(0x57, 0x45, 0x42, 0x50)))
      ) {
        return("webp")
      } else {
        return("unknown")
      }
    },
    USE.NAMES = FALSE
  )

  # Put the processed results back into their original positions
  results[valid_indices] <- valid_results
  return(results)
}
