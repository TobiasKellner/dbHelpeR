library(DBI)

# Funktion zum Abrufen des Tabellenkommentars
get_table_comment <- function(con, schema, table_name) {
  # Datenbanktyp automatisch erkennen
  db_type <- get_db_type(con)
  db_type <- tolower(db_type)

  if (grepl("sql server", db_type)) {
    db_type <- "mssql"
  } else if (grepl("postgresql", db_type)) {
    db_type <- "postgres"
  } else {
    stop("Datenbanktyp wird nicht unterstützt. Unterstützte Typen: MSSQL, PostgreSQL.")
  }

  # Query basierend auf dem Datenbanktyp erstellen
  query <- switch(
    db_type,
    "mssql" = paste0(
      "SELECT ep.value AS table_comment\n",
      "FROM sys.tables t\n",
      "INNER JOIN sys.schemas s ON t.schema_id = s.schema_id\n",
      "LEFT JOIN sys.extended_properties ep\n",
      "  ON t.object_id = ep.major_id AND ep.minor_id = 0 AND ep.name = 'MS_Description'\n",
      "WHERE s.name = '", schema, "' AND t.name = '", table_name, "';"
    ),
    "postgres" = paste0(
      "SELECT
      d.description AS table_comment
      FROM
      pg_class c
      JOIN pg_namespace n ON c.relnamespace = n.oid
      LEFT JOIN pg_description d ON c.oid = d.objoid
      WHERE
      c.relname = '",table_name, "' -- Name der Tabelle
      AND n.nspname = '", schema, "'; -- Name des Schemas
      "
    )
  )

  # Query ausführen und Ergebnis zurückgeben
  result <- dbGetQuery(con, query)
  if (nrow(result) > 0) {
    return(result$table_comment[1])
  } else {
    warning("Kein Kommentar für die angegebene Tabelle gefunden.")
    return(NA)
  }
}

# Beispielaufruf:
# Verbindung zu einer MSSQL- oder PostgreSQL-Datenbank herstellen
# library(odbc)
# con <- dbConnect(odbc::odbc(), dsn = "Deine_DSN")
# Kommentar abrufen
# mssql_comment <- table_comment(con, "dbo", "deine_tabelle")
# postgres_comment <- table_comment(con, "public", "deine_tabelle")
