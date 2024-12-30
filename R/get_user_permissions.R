library(DBI)

get_user_permissions <- function(conn) {
  # Sicherstellen, dass es sich um eine DBI-Verbindung handelt
  if (!inherits(conn, "DBIConnection")) {
    stop("Das Argument ist keine gültige DBI-Datenbankverbindung.")
  }

  # Datenbanktyp bestimmen
  db_type <- get_db_type(conn)

  # Rechte prüfen basierend auf dem Datenbanktyp
  tryCatch({
    query <- NULL

    if (db_type == "PostgreSQL") {
      # PostgreSQL: Überprüfe die Rollen- und Tabellenberechtigungen
      query <- "SELECT DISTINCT privilege_type
                FROM information_schema.role_table_grants
                WHERE grantee = CURRENT_USER;"
    } else if (db_type == "MySQL" || db_type == "MariaDB") {
      # MySQL/MariaDB: Rechte aus der user_privileges Tabelle
      query <- "SELECT DISTINCT privilege_type
                FROM information_schema.user_privileges
                WHERE grantee = CURRENT_USER();"
    } else if (db_type == "SQLite") {
      # SQLite: Da es keine granularen Rechte gibt, vereinfachen wir
      return(c("read_write")) # Alle SQLite-Nutzer haben in der Regel vollständige Rechte
    } else if (db_type == "MS SQL Server") {
      # MS SQL Server: Rechte aus sys.database_permissions
      query <- "SELECT DISTINCT dp.permission_name
                FROM sys.database_permissions dp
                WHERE USER_NAME(dp.grantee_principal_id) = USER_NAME();"
    } else if (db_type == "Oracle") {
      # Oracle: Rechte aus USER_TAB_PRIVS
      query <- "SELECT DISTINCT privilege
                FROM user_tab_privs
                WHERE grantee = USER"
    } else {
      stop("Rechteprüfung für diese Datenbank wird nicht unterstützt.")
    }

    # Query ausführen
    result <- dbGetQuery(conn, query)

    # Ergebnis in einen Vektor umwandeln und zurückgeben
    return(unique(as.character(result[[1]])))

  }, error = function(e) {
    warning("Fehler beim Abrufen der Nutzerrechte: ", e$message)
    return(character(0)) # Leerer Vektor bei Fehler
  })
}
