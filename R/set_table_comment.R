#' Create a comment for a database table
#'
#' This function sets a comment for the selected database table.
#'
#' @param conn A DBIConnection object
#' @param schema The name of the parent schema of the database table
#' @param table The name of the database table
#' @param comment The comment you want to use for this table
#'
#' @return
#' @export
#'
#' @examples set_table_comment(conn = con, schema = "test_schema", table = "test_table", comment = "This is my test comment.")
#'
set_table_comment <- function(conn, schema, table, comment) {

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

  # Create query based on the database type
  query <- switch(
    db_type,
    "postgres" = paste0(
      "COMMENT ON TABLE ", schema, ".\"", table, "\" IS '", comment, "';"
    ),
    "mssql" = paste0(
      "EXEC sp_addextendedproperty \n",
      "  @name = 'MS_Description',\n",
      "  @value = '", comment, "',\n",
      "  @level0type = 'SCHEMA', @level0name = '", schema, "',\n",
      "  @level1type = 'TABLE', @level1name = '", table, "';"
    )
  )

  # execute SQL statement
  dbExecute(con, query)

}
