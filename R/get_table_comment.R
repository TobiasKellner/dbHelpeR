#' Get table comment
#'
#' This function the comment of the selected database table.
#'
#' @param conn A DBIConnection object.
#' @param schema The name of the parent schema of the database table.
#'  If no schema is specified, the default schema of the database is used
#'  (e.g. 'dbo' for Microsoft SQL Server).
#' @param table The name of the database table.
#'
#' @return A character string representing the table comment.
#' @export
#'
#' @examples
#' get_table_comment(conn = con, schema = "schema", table = "table")
#'
get_table_comment <- function(con, schema, table) {

  # detect database type
  db_type <- get_db_type(con)
  db_type <- tolower(db_type)

  if (grepl("sql server", db_type)) {
    db_type <- "mssql"
  } else if (grepl("postgresql", db_type)) {
    db_type <- "postgres"
  } else {
    stop("Database type is not supported. Supported types are: Microsoft SQL Server, PostgreSQL.")
  }

  # Create query based on the database type
  query <- switch(
    db_type,
    "mssql" = paste0(
      "SELECT ep.value AS table_comment\n",
      "FROM sys.tables t\n",
      "INNER JOIN sys.schemas s ON t.schema_id = s.schema_id\n",
      "LEFT JOIN sys.extended_properties ep\n",
      "  ON t.object_id = ep.major_id AND ep.minor_id = 0 AND ep.name = 'MS_Description'\n",
      "WHERE s.name = '", schema, "' AND t.name = '", table, "';"
    ),
    "postgres" = paste0(
      "SELECT
      d.description AS table_comment
      FROM
      pg_class c
      JOIN pg_namespace n ON c.relnamespace = n.oid
      LEFT JOIN pg_description d ON c.oid = d.objoid
      WHERE
      c.relname = '",table, "' -- Name der Tabelle
      AND n.nspname = '", schema, "'; -- Name des Schemas
      "
    )
  )

  # Execute query and return result
  result <- dbGetQuery(con, query)
  if (nrow(result) > 0) {
    return(result$table_comment[1])
  } else {
    warning("No comment found for the specified table.")
    return(NA)
  }

}
