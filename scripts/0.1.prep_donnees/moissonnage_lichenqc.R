## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Extraction de liste de lichens du Québec
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Date cération : 2026-09-26
# Auteur: Marc-Olivier Beausoleil

## __________####
## LISEZMOI ####

## Objectif :

library(httr2)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(xml2)

# 1. Build and execute the request
url <- "https://lichens.quebec/especes/"

resp <- request(url) |>
  req_user_agent(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) R lichen-scraper"
  ) |>
  req_perform()

# 2. Parse HTML content
page <- resp |>
  resp_body_html()

# 3. Extract the elements matching class 'speciesItem'
# (Note: Targets .speciesItem .speciesName to get both href and species name)
species_nodes <- page |>
  html_elements(".speciesItem a.speciesName")

# 4. Construct the data frame
df_species <- data.frame(
  link = html_attr(species_nodes, "href"),
  species = html_text2(species_nodes),
  stringsAsFactors = FALSE
)

# Preview results
head(df_species)

# Function to scrape detailed metadata from a single species page

# Helper function to extract and validate vernacular names
extract_vernacular <- function(page, latin_name) {
  nodes <- html_elements(
    page,
    ".elementor-widget-shortcode .elementor-shortcode"
  )
  if (length(nodes) == 0) {
    return(NA_character_)
  }

  # Strip inline style/script tags
  xml_find_all(nodes, ".//style | .//script") |> xml_remove()
  texts <- html_text2(nodes) |> str_trim()

  # Filter out CSS blocks, empty text, and plugin placeholder text
  valid_texts <- texts[
    texts != "" &
      !str_detect(
        texts,
        "\\{|font-family|background-color|Exemple PHP avec CSS"
      )
  ]

  if (length(valid_texts) == 0) {
    return(NA_character_)
  }

  result <- valid_texts[1]

  # If the string simply repeats the scientific name, consider vernacular as missing (NA)
  if (!is.na(latin_name) && str_detect(result, fixed(latin_name))) {
    return(NA_character_)
  }

  return(result)
}

scrape_species_detail <- function(url) {
  tryCatch(
    {
      resp <- request(url) |>
        req_user_agent(
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) R lichen-scraper"
        ) |>
        req_perform()

      page <- resp_body_html(resp)

      # Helper to extract clean text safely
      get_text <- function(css_selector) {
        node <- html_element(page, css_selector)
        if (is.na(node)) {
          return(NA_character_)
        }
        xml_find_all(node, ".//style | .//script") |> xml_remove()
        html_text2(node) |> str_trim()
      }

      latin_name <- get_text("h1.query-result")

      # Helper to extract values following specific header/title labels
      get_field_by_label <- function(label_regex) {
        xpath <- sprintf(
          "//*[self::h2 or self::strong][contains(text(), '%s')]",
          label_regex
        )
        label_node <- html_element(page, xpath = xpath)

        if (is.na(label_node)) {
          return(NA_character_)
        }

        val_node <- html_element(
          label_node,
          xpath = "following-sibling::*[contains(@class, 'query-result') or contains(@class, 'specimen-text')][1]"
        )

        if (!is.na(val_node)) {
          xml_find_all(val_node, ".//style | .//script") |> xml_remove()
          return(html_text2(val_node) |> str_trim())
        } else {
          raw_text <- html_text2(html_element(
            label_node,
            xpath = "following-sibling::text()[1]"
          ))
          return(str_trim(raw_text))
        }
      }

      data.frame(
        link = url,
        full_species_name = latin_name,
        vernacular_name = extract_vernacular(page, latin_name),
        classification = get_text(".classification"),
        type = get_field_by_label("Type"),
        photobionte = get_field_by_label("Photobionte"),
        reproduction = get_field_by_label("Reproduction"),
        substrat = get_field_by_label("Substrat"),
        rang_conservation_mondial = get_field_by_label(
          "Rang de conservation mondial"
        ),
        rang_conservation_canada = get_field_by_label(
          "Rang de conservation au Canada"
        ),
        rang_conservation_quebec = get_field_by_label(
          "Rang de conservation au Québec"
        ),
        specimen_reference = get_field_by_label("Spécimen de référence"),
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      message(paste("Error scraping:", url, "-", e$message))
      return(data.frame(link = url, stringsAsFactors = FALSE))
    }
  )
}


# Extraction des données de Lichen Québec directement
# Le script permet de limiter la surcharge du serveur de Lichen Québec
# 1. Processus en petits lots des liens pour faire le moissonnage
# 2. Inclue une attente lors du moissonnage
# 3. Après le chargement des lots, attente aléatoire entre 10 et 20 secondes
fichier_lichen_qc <- file.path(
  "data/raw/biodiv/listes_especes",
  "lichen_quebec.csv"
)
while_indicateur <- nrow(df_species)
while (while_indicateur != 0) {
  # Si le fichier de sortie existe déjà,
  # pas besoin d'extraire les données de nouveau
  if (
    file.exists(
      fichier_lichen_qc
    )
  ) {
    message(
      "Le fichier de sortie de lichen existe déjà
Extraction des liens manquants seulement"
    )
    lichen_qc_out <- read.csv2(file = fichier_lichen_qc)

    data_process <- df_species |>
      dplyr::filter(
        !(link %in% lichen_qc_out$link)
      )

    while_indicateur <- nrow(data_process)
    message(
      sprintf(
        "Reste %s rangées...",
        while_indicateur
      )
    )
  } else {
    data_process <- df_species
  }
  # Iterate over df_species$link using purrr::map_df
  # NOTE : Adding a small delay (1 second) between requests
  # is good web-scraping practice
  liens_a_extraire <- data_process$link[1:sample(5:10, size = 1)]
  nrow_datap <- length(liens_a_extraire)
  message(
    sprintf(
      "Extraction de %s items",
      nrow_datap
    )
  )

  # Temps pour 10 : 22.681 à 24.772 sec.
  # Temps estimé pour 1472 (2.2681*1472/60 = 55.64 min, environ 1h)
  tictoc::tic("Extraction données lichen Québec")
  df_species_details <- imap_dfr(liens_a_extraire, function(url, idx) {
    # Print progress every 10 iterations
    if (idx %% 10 == 0 || idx == 1) {
      message(
        sprintf(
          "Obtenir item %d de %d (%.0f%%)...",
          idx,
          nrow_datap,
          (idx / nrow_datap) * 100
        )
      )
    }

    Sys.sleep(1) # Ne pas surcharger le serveur
    scrape_species_detail(url)
  })
  tictoc::toc()

  # Preview the combined detailed dataset
  head(df_species_details)

  # Exportation d'un csv2
  write.table(
    x = df_species_details,
    file = fichier_lichen_qc,
    sep = ";",
    dec = ",",
    row.names = FALSE,
    # Adds headers only if the file doesn't exist yet
    col.names = !file.exists(fichier_lichen_qc),
    append = TRUE
  )
  # Attente aléatoire
  temps_attente <- sample(10:20, size = 1)
  message(
    sprintf(
      "Attends : %s secondes",
      temps_attente
    )
  )
  Sys.sleep(temps_attente)
}


# Charge les données de lichen Québec
lqc <- read.csv2(fichier_lichen_qc)


# Normalise les noms latin et retire les noms communs manquants
lqc_corr <- lqc |>
  mutate(
    # Nom latin en 2 mots
    nom_scientifique = stringr::word(full_species_name, 1, 2),
    # Certains noms français de la base de données pas présentes.
    nom_francais = ifelse(
      test = grepl(
        # Trouve les lignes qui ont reçu nom commun qui contient "Liens ext..."
        pattern = "Liens externes",
        x = vernacular_name
      ),
      yes = "",
      no = vernacular_name
    )
  ) |>
  # Séparation des données taxonomiques provenant du site web
  separate_wider_delim(
    cols = classification,
    delim = " > ",
    names = c(
      "phylum",
      "subphylum",
      "class",
      "subclass",
      "order",
      "family"
    )
  )
# Exportation des données de noms d'espèces de lichen corrigés
fichier_lichen_qc <- file.path(
  "data/raw/biodiv/listes_especes",
  "lichen_quebec_corr.csv"
)

# Exportation de type csv2
write.table(
  x = lqc_corr,
  file = fichier_lichen_qc,
  sep = ";",
  dec = ",",
  row.names = FALSE
)
