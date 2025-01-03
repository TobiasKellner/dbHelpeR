get_column_comment <- function(con, schema = NULL, table_name, column_name) {
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

      "select
      c.table_schema,
      c.table_name,
      c.column_name,
      pgd.description
      from pg_catalog.pg_statio_all_tables as st
      inner join pg_catalog.pg_description pgd on (pgd.objoid = st.relid)
      inner join information_schema.columns c on (
        pgd.objsubid   = c.ordinal_position and
        c.table_schema = st.schemaname and
        c.table_name   = st.relname
      )
      WHERE c.table_schema = '", schema, "'
      AND c.table_name = '", table_name, "'
      AND c.column_name = '", column_name, "';
      "

      # "SELECT col_description((SELECT oid FROM pg_class WHERE relname = '", table_name, "'), ",
      # "(SELECT attnum FROM pg_attribute WHERE attname = '", column_name, "' AND attrelid = (SELECT oid FROM pg_class WHERE relname = '", table_name, "')));"

      )
    ,
    "mssql" = paste0(
      "SELECT value FROM sys.extended_properties ",
      "WHERE name = 'MS_Description' AND ",
      "major_id = OBJECT_ID('", ifelse(is.null(schema), table_name, paste0(schema, ".", table_name)), "') AND ",
      "minor_id = (SELECT column_id FROM sys.columns WHERE name = '", column_name, "' AND object_id = OBJECT_ID('", ifelse(is.null(schema), table_name, paste0(schema, ".", table_name)), "'));")
    ,
    stop("Unsupported database type: ", db_type)
  )

  result <- dbGetQuery(con, query)
  if (nrow(result) > 0) {
    return(result$description[1])
  } else {
    warning("Kein Kommentar für die angegebene Tabelle gefunden.")
    return(NA)
  }
}
