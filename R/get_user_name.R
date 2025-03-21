get_user_name <- function(conn) {
  # Sicherstellen, dass es sich um eine DBI-Verbindung handelt
  if (!inherits(conn, "DBIConnection")) {
    stop("Das Argument ist keine gültige DBI-Datenbankverbindung.")
  }

  # Datenbanktyp bestimmen
  db_type <- get_database_type(conn)

  # Nutzername abrufen basierend auf dem Datenbanktyp
  tryCatch({
    query <- NULL

    if (db_type == "PostgreSQL") {
      # PostgreSQL: CURRENT_USER gibt den Benutzernamen zurück
      query <- "SELECT CURRENT_USER;"
    } else if (db_type == "MySQL" || db_type == "MariaDB") {
      # MySQL/MariaDB: USER() liefert den aktuellen Nutzer
      query <- "SELECT USER();"
    } else if (db_type == "SQLite") {
      # SQLite: Keine spezifische Nutzerverwaltung, Standardnutzer zurückgeben
      return("default_user")
    } else if (db_type == "MS SQL Server") {
      # MS SQL Server: SUSER_NAME() gibt den Benutzernamen zurück
      query <- "SELECT SUSER_NAME();"
    } else if (db_type == "Oracle") {
      # Oracle: USER liefert den aktuellen Benutzernamen
      query <- "SELECT USER FROM DUAL"
    } else {
      stop("Nutzerabfrage für diese Datenbank wird nicht unterstützt.")
    }

    # Query ausführen und den Nutzernamen zurückgeben
    result <- dbGetQuery(conn, query)
    return(as.character(result[[1]]))

  }, error = function(e) {
    warning("Fehler beim Abrufen des Nutzernamens: ", e$message)
    return(NA) # Rückgabe von NA bei Fehler
  })
}
