delete_column_comment <- function(conn, schema = NULL, table_name, column_name) {

  # detect database type
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
      "COMMENT ON COLUMN ",
      schema_prefix, "\"", table_name,"\"", ".", "\"", column_name, "\"",
      " IS NULL;"
    ),
    "mssql" = paste0(
      "EXEC sys.sp_dropextendedproperty ",
      "@name = N'MS_Description', ",
      "@level0type = N'SCHEMA', @level0name = '", ifelse(is.null(schema), "SCHEMA_NAME()", schema), "', ",
      "@level1type = N'TABLE', @level1name = '", table_name, "', ",
      "@level2type = N'COLUMN', @level2name = '", column_name, "';"
    ),
    stop("Unsupported database type: ", db_type)
  )

  # Query ausführen
  dbExecute(con, query)
}
