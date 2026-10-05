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
#  -->  Faire un fichier parquet (et non un dossier) avec les données 
#.      Ce fichier servira pour le reste des analyses 

## Fonctionnement : 
#  --> 

cd "Incubateur/2025_05_24_Guide_biodiv_qc/data/partie_2/biodiv/gbif_data"

duckdb
.timer on 
INSTALL spatial; LOAD spatial; 
SET VARIABLE outpath = "gbif_raw_new.parquet";

-- Données sans post-processing (élimine le fichier 000000 automatiquement)
SET VARIABLE gb_path_pq = "/Volumes/g_magni/gbif_data/0010290-260519110011954/occurrence.parquet";
-- SELECT count(*) FROM read_parquet(getvariable('gb_path_pq') || '/[!.]*[!0]*');

-- Filtrer les chemins (paths) et mettre dans une variable
-- Gère les fichiers avec 0-bytes 
SET variable gb_files = (
    SELECT list(file) 
    FROM glob(getvariable('gb_path_pq') || '/*') 
    WHERE file NOT LIKE '%/000000' 
      AND file NOT LIKE '%/._%'
);

-- Combien de fichiers lu
SELECT length(getvariable('gb_files'));

-- Lire les fichiers et compte combien de lignes (à comparer avec GBIF)
-- SELECT count(*) FROM read_parquet(getvariable('gb_files'));



-- Copier les résultats dans un fichier parquet portable 

-- Prépare la requête en remplaçant le chemin par un paramètre (?)
PREPARE copy_spatial_data AS 
COPY (
  SELECT
    *,
    ST_Point(decimalLongitude, decimalLatitude) AS geometry
  FROM
    read_parquet(getvariable('gb_files'))
) TO ? (FORMAT parquet);

-- Exécuter la requête (passer la variable)
-- Temps : 1 min 
EXECUTE copy_spatial_data(getvariable('outpath'));

-- Nettoyer la requête préparée
DEALLOCATE copy_spatial_data;



---
---
-- ancienne version 
---
---


SET VARIABLE gb_path_pq = "/Volumes/g_magni/gbif_data/0040587-260226173443078.parquet/*";
SET VARIABLE outpath = "Incubateur/2025_05_24_Guide_biodiv_qc/data/partie_2/biodiv/gbif_data/gbif_prep_all";

-- Copier les résultats dans un fichier parquet portable 
COPY (
  SELECT
    *,
    -- Création de la colonne de géométrie spatiale 
    ST_Point(decimalLongitude, decimalLatitude) AS geometry
  FROM
    read_parquet(getvariable('gb_path_pq'))
  -- Gère les fichiers avec 0-bytes 
  WHERE 
    filesize > 0
) TO getvariable('outpath') (FORMAT parquet);