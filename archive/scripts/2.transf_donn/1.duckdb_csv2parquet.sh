## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## # 
## Préparation des données d'occurence de biodiversté GBIF
#
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Date cération : 2026-02-14
# Auteur: Marc-Olivier Beausoleil

##__________####
## LISEZMOI ####

## Description : 
#  --> Après le téléchargement des données GBIF (>20Gb), il faut préparer pour 
#      être capable de mettre en mémoire de R.
#  --> Puisque les données n'entre pas en mémoire (un ordinateur avec 16GB RAM)
#      duckDB est une solution pragmatique pour lire ces données 
## Fonctionnement : 
#  --> L'installation de duckDB (https://duckdb.org/install) est requise 
#  --> En utilisant le terminal, on navigue au dossier pour 
#      faire les opérations voulues. 

# Go to correct location 
cd ~/Github_proj/evologie/Incubateur/2025_05_24_Guide_biodiv_qc/data/partie_2/biodiv/gbif_data

# Open duckdb instance 
duckdb

-- Syntaxe duckdb (SQLish) à partir de maintenant 

-- Paramètres duckdb
.timer on 
.maxrows 50

-- Installation et charge librairies 
INSTALL spatial; 
LOAD spatial;

-- Définir le 'sentier' vers les données GBIF brutes
SET VARIABLE gb_path_raw = '/Volumes/g_magni/gbif_data/0047252-250827131500795.csv';

-- Montre toutes les colonnes
-- SELECT UNNEST(Columns)  FROM sniff_csv(getvariable('gb_path_raw'));

-- Exportation des données CSV (>23GB) en PARQUET (2.8GB) en 32 secondes
COPY (
  SELECT *
    FROM
  read_csv(getvariable('gb_path_raw'), 
           sep = '\t', 
           types={'catalogNumber': 'VARCHAR'})
) TO '0047252-250827131500795.parquet' (FORMAT parquet);

-- quit duckdb
.exit
