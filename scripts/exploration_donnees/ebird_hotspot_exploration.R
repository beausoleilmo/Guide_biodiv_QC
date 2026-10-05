# Ajouter les municipalités aux points chauds de eBird
ebird_hp_sf_admin <- ebird_hp_sf |>
  sf::st_intersection(
    y = regqc |>
      dplyr::select(
        "MUS_NM_MUN",
        "MUS_NM_MRC",
        "MUS_NM_REG"
      )
  )

# Top 3 hotspots eBird par région
ebird_hp_leader <- ebird_hp_sf_admin |>
  dplyr::group_by(MUS_NM_MRC) |>
  dplyr::slice_max(order_by = numSpeciesAllTime, n = 3) |>
  dplyr::select(
    MUS_NM_MRC,
    MUS_NM_MUN,
    locName,
    numSpeciesAllTime,
    latestObsDt,
    numChecklistsAllTime,
    date_obs_recent
  )

# Carte des points importants
mapView(x = ebird_hp_leader, zcol = "locName")
