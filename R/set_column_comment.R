#' Set column comment
#'
#' This function sets a comment for the selected column of a database table.
#'
#' @param conn A DBIConnection object.
#' @param schema The name of the parent schema of the database table.
#'  If no schema is specified, the default schema of the database is used
#'  (e.g. 'dbo' for Microsoft SQL Server).
#' @param table The name of the database table
#' @param column The column name
#' @param comment The comment you want to set for this column
#'
#' @return
#' @export
#'
#' @examples
#' set_column_comment(conn = con, schema = "schema", table = "table", column = "column", comment = "Your column comment")
#'
set_column_comment <- function(conn, schema = NULL, table, column, comment) {

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

  # handle schema prefix if provided
  schema_prefix <- if (!is.null(schema)) paste0(schema, ".") else ""

  # define a switch for supported database types
  sql_command <- switch(
    db_type,
    "postgres" = paste0(
      "COMMENT ON COLUMN ",
      schema_prefix, "\"", table,  "\"", ".", "\"",column,"\"",
      " IS '", comment, "';"
    ),
    "mssql" = paste0(
      "EXEC sys.sp_addextendedproperty ",
      "@name = N'MS_Description', ",
      "@value = '", comment, "', ",
      "@level0type = N'SCHEMA', @level0name = '", ifelse(is.null(schema), "dbo", schema), "', ",
      "@level1type = N'TABLE', @level1name = '", table, "', ",
      "@level2type = N'COLUMN', @level2name = '", column, "';"
    ),
    stop("Unsupported database type: ", db_type)
  )

  dbExecute(con, sql_command)

}
