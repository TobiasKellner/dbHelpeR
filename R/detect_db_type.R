library(DBI)

get_db_type <- function(conn) {
  if (!inherits(conn, "DBIConnection")) {
    stop("Das Argument ist keine gültige DBI-Datenbankverbindung.")
  }

  # Extrahiere die Klassen der Verbindung
  conn_class <- class(conn)
  conn_class_string <- paste(conn_class, collapse = " ")

  # Definiere Muster für verschiedene Datenbanktypen
  if (grepl("PqConnection|PostgreSQL", conn_class_string, ignore.case = TRUE)) {
    return("PostgreSQL")
  } else if (grepl("MySQLConnection", conn_class_string, ignore.case = TRUE)) {
    return("MySQL")
  } else if (grepl("MariaDBConnection", conn_class_string, ignore.case = TRUE)) {
    return("MariaDB")
  } else if (grepl("SQLiteConnection", conn_class_string, ignore.case = TRUE)) {
    return("SQLite")
  } else if (grepl("SQLServerConnection|Microsoft SQL Server", conn_class_string, ignore.case = TRUE)) {
    return("MS SQL Server")
  } else if (grepl("OraConnection", conn_class_string, ignore.case = TRUE)) {
    return("Oracle")
  } else {
    return("Unbekannter Typ")
  }
}
