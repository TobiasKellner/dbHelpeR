set_table_comment <- function(con, schema, table_name, comment) {
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

  # Query basierend auf dem Datenbanktyp erstellen
  query <- switch(
    db_type,
    "mssql" = paste0(
      "EXEC sp_addextendedproperty \n",
      "  @name = 'MS_Description',\n",
      "  @value = '", comment, "',\n",
      "  @level0type = 'SCHEMA', @level0name = '", schema, "',\n",
      "  @level1type = 'TABLE', @level1name = '", table_name, "';"
    ),
    "postgres" = paste0(
      "COMMENT ON TABLE ", schema, ".\"", table_name, "\" IS '", comment, "';"
    )
  )

  # Query ausführen
  dbExecute(con, query)
}
