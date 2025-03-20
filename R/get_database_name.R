#' Get database name
#'
#' This function returns the database name under the used database connection.
#'
#' @param conn A DBIConnection object.
#'
#' @return A character string representing the database name.
#' @export
#'
#' @examples
#' get_database_name(conn = con)
#'
get_database_name <- function(conn) {

  # detect database type
  db_type <- get_database_type(con)
  db_type <- tolower(db_type)

  if (grepl("sql server", db_type)) {
    db_type <- "mssql"
  } else if (grepl("postgresql", db_type)) {
    db_type <- "postgres"
  } else {
    stop("Database type is not supported. Supported types: MSSQL, PostgreSQL.")
  }

  # Define a switch for supported database types
  query <- switch(
    db_type,
    "postgres" = "SELECT current_database();",
    "mssql" = "SELECT DB_NAME();",
    stop("Unsupported database type: ", db_type)
  )

  result <- dbGetQuery(con, query)

  return(result[[1, 1]])

}
