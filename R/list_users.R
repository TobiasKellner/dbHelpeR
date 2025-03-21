#' List database users
#'
#' This function lists all database users and returns them as a dataframe object.
#'
#' @param conn A DBIConnection object.
#'
#' @return A character vector representing the users who have access to the database.
#' @export
#'
#' @examples
#' list_users(conn = con)
#'
list_users <- function(conn) {

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

  # define a switch for supported database types
  sql_command <- switch(
    db_type,
    "postgres" = "
      SELECT
        usename AS username
      FROM pg_user;
      ",
    "mssql" = "
    SELECT
      name AS username
    FROM sys.sql_logins;
    ",
    stop("Unsupported database type: ", db_type)
  )

  result <- dbGetQuery(conn, sql_command)

  return(result[, 1])

}
