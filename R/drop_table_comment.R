#' Drop table comment
#'
#' This function deletes the existing comment of the selected database table.
#'
#' @param conn A DBIConnection object.
#' @param schema The name of the parent schema of the database table.
#'  If no schema is specified, the default schema of the database is used
#'  (e.g. 'dbo' for Microsoft SQL Server).
#' @param table The name of the database table.
#'
#' @return
#' @export
#'
#' @examples
#' drop_table_comment(conn = con, schema = "schema", table = "table")
#'
drop_table_comment <- function(conn, schema = NULL, table) {

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
    "mssql" = paste0(
      "EXEC sp_dropextendedproperty \n",
      "  @name = 'MS_Description',\n",
      "  @level0type = 'SCHEMA', @level0name = '", ifelse(is.null(schema), "dbo", schema), "',\n",
      "  @level1type = 'TABLE', @level1name = '", table, "';"
    ),
    "postgres" = paste0(
      "COMMENT ON TABLE ", schema_prefix, "\"", table, "\" IS NULL;"
    )
  )

  # execute SQLcommand
  dbExecute(conn, sql_command)

}
