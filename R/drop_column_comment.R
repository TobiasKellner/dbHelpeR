#' Drop column comment
#'
#' This function deletes an existing column comment of a database table.
#'
#' @param conn A DBIConnection object.
#' @param schema The name of the parent schema of the database table.
#'  If no schema is specified, the default schema of the database is used
#'  (e.g. 'dbo' for Microsoft SQL Server).
#' @param table The name of the database table.
#' @param column The name of the column.
#'
#' @return
#' @export
#'
#' @examples
#' delete_column_comment(conn = con, schema = "schema", table = "table", column = "column")
#'
drop_column_comment <- function(conn, schema = NULL, table, column) {

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
  schema_prefix <- if (!is.null(schema)) paste0(schema, ".") else ""

  # define a switch for supported database types
  sql_command <- switch(
    db_type,
    "postgres" = paste0(
      "COMMENT ON COLUMN ",
      schema_prefix, "\"", table,"\"", ".", "\"", column, "\"",
      " IS NULL;"
    ),
    "mssql" = paste0(
      "EXEC sys.sp_dropextendedproperty ",
      "@name = N'MS_Description', ",
      "@level0type = N'SCHEMA', @level0name = '", ifelse(is.null(schema), "dbo", schema), "', ",
      "@level1type = N'TABLE', @level1name = '", table, "', ",
      "@level2type = N'COLUMN', @level2name = '", column, "';"
    ),
    stop("Unsupported database type: ", db_type)
  )

  # execute SQLcommand
  dbExecute(con, sql_command)

}
