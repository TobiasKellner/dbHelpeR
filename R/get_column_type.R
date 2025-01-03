get_column_type <- function(conn, schema = NULL, table_name, column_name) {
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

  # Handle schema prefix if provided
  schema_prefix <- if (!is.null(schema)) paste0(schema, ".") else ""

  # Define a switch for supported database types
  query <- switch(
    db_type,
    "postgres" = paste0(
      "SELECT data_type FROM information_schema.columns ",
      "WHERE table_schema = '", ifelse(is.null(schema), "public", schema), "' ",
      "AND table_name = '", table_name, "' AND column_name = '", column_name, "';"
    ),
    "mssql" = paste0(
      "SELECT DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS ",
      "WHERE TABLE_SCHEMA = '", ifelse(is.null(schema), "dbo", schema), "' ",
      "AND TABLE_NAME = '", table_name, "' AND COLUMN_NAME = '", column_name, "';"
    ),
    stop("Unsupported database type: ", db_type)
  )

  # Query ausführen
  result <- dbGetQuery(con, query)

  return(result[[1]])
}
