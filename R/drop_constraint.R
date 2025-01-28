#' Drop constraint
#'
#' This function removes a specific constraint (e.g. Primary Key, Foreign Key,
#'  Unique, Check) from the specified database table.
#'
#' @param conn A DBIConnection object.
#' @param schema The name of the parent schema of the database table.
#'  If no schema is specified, the default schema of the database is used
#'  (e.g. 'dbo' for Microsoft SQL Server).
#' @param table The name of the database table.
#' @param constraint The name of the constraint.
#'
#' @return
#' @export
#'
#' @examples
#'
drop_constraint <- function(conn, schema = NULL, table, constraint) {

  # detect database type
  db_type <- get_database_type(conn)
  db_type <- tolower(db_type)

  if (grepl("sql server", db_type)) {
    db_type <- "mssql"
  } else if (grepl("postgresql", db_type)) {
    db_type <- "postgres"
  } else {
    stop("Database type is not supported. Supported types are: Microsoft SQL Server, PostgreSQL.")
  }

  # Handle schema prefix if provided
  schema_prefix <- if (!is.null(schema)) paste0(schema, ".") else ""

  # Define a switch for supported database types
  sql_command <- switch(
    db_type,
    "postgres" = paste0(
      "ALTER TABLE ", schema_prefix, "\"", table, "\"",
      " DROP CONSTRAINT ", constraint, ";"
    ),
    "mssql" = paste0(
      "ALTER TABLE ", schema_prefix, table,
      " DROP CONSTRAINT ", constraint, ";"
    ),
    stop("Unsupported database type: ", db_type)
  )

  # execute SQLcommand
  dbExecute(con, sql_command)

  # Message about successful dropping the constraint
  cat(
    "Constraint ", "\"", constraint, "\"" ," successfully droped of the table ",
    schema_prefix, table
    , "\n", sep = ""
  )

}
