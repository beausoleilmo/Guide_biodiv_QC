# Charge les données auxiliaires
#
message("--> Charger région pour carto")
source(
  file.path(
    "scripts/0.2.charge_donn/0.1_charge_donnees_admin.R"
  )
)

message("--> Charger les Points eBird")
source(
  file = file.path(
    "scripts/0.2.charge_donn/0.1_charge_donnees_ebirdHS.R"
  )
)

message("--> Charge les données hydrologiques")
source(
  file = file.path(
    "scripts/0.2.charge_donn/0.1_charge_donnees_hydrologique.R"
  )
)
