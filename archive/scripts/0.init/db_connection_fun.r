message("-> setup_duckdb()")
setup_duckdb <- function() {
  #' Initialisation du 'pilote' ou 'moteur' duckdb
  #'
  #' @description
  #' Démarrer une session duckdb en mémoire avec un environnement avec 
  #' des extensions spatiales (spatial + h3)
  #' @returns Connection duckdb 
  #' @examples
  #' \dontrun{
  #' con = setup_duckdb() # Exporte con dbdir=':memory:' 
  #' dbGetQuery( 
  #'   conn = con, 
  #'   statement = "SELECT extension_name, installed, loaded, description 
  #'   FROM duckdb_extensions() 
  #'   WHERE installed = true;" # Filtre ce qui est installé 
  #' ) 
  #' }
  # Connection avec duckdb (mémoire)
  con <- DBI::dbConnect(drv = duckdb::duckdb())
  
  # Install et charge les extensions duckdb
  DBI::dbExecute(conn = con, statement = "INSTALL spatial; LOAD spatial;")
  DBI::dbExecute(conn = con, statement = "INSTALL h3 FROM community; LOAD h3;")
  DBI::dbExecute(conn = con, statement = "SET geometry_always_xy = true;")
  # Retourne la connection pour réutiliser 
  return(con)
}

message("-> discocon()")
discocon <- function(con) {
  #' Ferme le 'pilote' ou 'moteur' duckdb
  #'
  #' @description
  #' Ferme une session duckdb en mémoire.
  #'  
  #' @returns NULL 
  #' @examples
  #' \dontrun{
  #' con = setup_duckdb() # Exporte con dbdir=':memory:' 
  #' discocon(con)
  #' }
  # Ferme duckdb (mémoire)
  DBI::dbDisconnect(con)
}
