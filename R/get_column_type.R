#' Get column type
#'
#' This function returns the column type of a selected column respectively a
#' table of the type assignment of all columns if the 'dataframe' option is set to TRUE.
#'
#' @param conn A DBIConnection object.
#' @param schema The name of the parent schema of the database table.
#'  If no schema is specified, the default schema of the database is used
#'  (e.g. 'dbo' for Microsoft SQL Server).
#' @param table The name of the database table
#' @param column The name of the column \cr
#'  Note: If the 'dataframe' is set to TRUE, the 'column' option can be omitted to get a table of all columns with column type mapping
#' @param dataframe A boolean value for obtaining a character string or dataframe
#'
#' @return A character string representing the column type respectively a dataframe object representing the column type if 'dataframe' is set to TRUE
#' @export
#'
#' @examples
#' get_column_type(conn = con, schema = "schema", table = "table", column = "column", dataframe = FALSE)
#' get_column_type(conn = con, schema = "schema", table = "table", dataframe = TRUE)
#'
get_column_type <- function(conn, schema = NULL, table, column = NULL, dataframe = FALSE) {

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

  # define a switch for supported database types
  sql_command <- switch(
    db_type,
    "postgres" = paste0(
      "SELECT column_name AS \"COLUMN_NAME\", data_type AS \"DATA_TYPE\" FROM information_schema.columns ",
      "WHERE table_schema = '", ifelse(is.null(schema), "public", schema), "' ",
      "AND table_name = '", table,
      if(!is.null(column)) {paste0("' AND column_name = '", column)},
      "';"
    ),
    "mssql" = paste0(
      "SELECT COLUMN_NAME, DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS ",
      "WHERE TABLE_SCHEMA = '", ifelse(is.null(schema), "dbo", schema), "' ",
      "AND TABLE_NAME = '", table,
      if(!is.null(column)) {paste0("' AND COLUMN_NAME = '", column)},
      "';"
    ),
    stop("Unsupported database type: ", db_type)
  )

  # execute SQLcommand
  result <- dbGetQuery(con, sql_command)

  if(dataframe == FALSE) {
    return(result[[1, 2]])
  } else if(dataframe == TRUE) {
    return(result)
  } else {
    stop("Incorrect input in the 'dataframe' option")
  }

}
