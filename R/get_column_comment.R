#' Get column comment
#'
#' This function returns the column comment of a database table.
#'
#' @param conn A DBIConnection object.
#' @param schema The name of the parent schema of the database table.
#'  If no schema is specified, the default schema of the database is used
#'  (e.g. 'dbo' for Microsoft SQL Server).
#' @param table The name of the database table.
#' @param column The column name
#'
#' @return
#' @export
#'
#' @examples
#' get_column_comment(conn = con, schema = "schema", table = "table", column = "column")
#'
get_column_comment <- function(conn, schema = NULL, table, column) {

  # detect database type
  db_type <- get_database_type(con)
  db_type <- tolower(db_type)

  if (grepl("sql server", db_type)) {
    db_type <- "mssql"
  } else if (grepl("postgresql", db_type)) {
    db_type <- "postgres"
  } else {
    stop("Database type is not supported. Supported types are: Microsoft SQL Server, PostgreSQL.")
  }

  # handle schema prefix if provided
  schema_prefix <- if (!is.null(schema)) schema else "public"

  # define a switch for supported database types
  query <- switch(
    db_type,
    "postgres" = paste0(
      "SELECT d.description AS description
      FROM pg_catalog.pg_attribute a
      JOIN pg_catalog.pg_class c ON a.attrelid = c.oid
      JOIN pg_catalog.pg_namespace n ON c.relnamespace = n.oid
      LEFT JOIN pg_catalog.pg_description d ON a.attrelid = d.objoid AND a.attnum = d.objsubid
      WHERE n.nspname = '", schema_prefix, "'
      AND c.relname = '", table, "'
      AND a.attname = '", column, "';
      "
      )
    ,
    "mssql" = paste0(
      "SELECT ep.value AS description
      FROM sys.extended_properties ep
      JOIN sys.columns c ON ep.major_id = c.object_id AND ep.minor_id = c.column_id
      JOIN sys.tables t ON c.object_id = t.object_id
      JOIN sys.schemas s ON t.schema_id = s.schema_id
      WHERE ep.name = 'MS_Description'
      AND s.name = '", ifelse(is.null(schema), "dbo", schema), "'
      AND t.name = '", table, "'
      AND c.name = '", column, "';
      ")
    ,
    stop("Unsupported database type: ", db_type)
  )

  result <- dbGetQuery(con, query)
  if (nrow(result) > 0) {
    return(result$description[1])
  } else {
    warning("No comment found for the specified table column.")
    return(NA)
  }

}
