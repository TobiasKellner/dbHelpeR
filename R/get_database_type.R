#' Detect database type
#'
#' This function returns the detected type of the database connection.
#'
#' @param conn A DBIConnection object.
#'
#' @return A character string representing the type of the database.
#' @export
#'
#' @examples
#' get_database_type(conn = con)
#'
get_database_type <- function(conn) {

  if (!inherits(conn, "DBIConnection")) {
    stop("This argument is not a valid DBI database connection.")
  }

  # extract the classes of the database connection
  conn_class <- class(conn)
  conn_class_string <- paste(conn_class, collapse = " ")

  # define pattern for distinct database types
  if (grepl("PqConnection|PostgreSQL", conn_class_string, ignore.case = TRUE)) {
    return("PostgreSQL")
  } else if (grepl("MySQLConnection", conn_class_string, ignore.case = TRUE)) {
    return("MySQL")
  } else if (grepl("MariaDBConnection", conn_class_string, ignore.case = TRUE)) {
    return("MariaDB")
  } else if (grepl("SQLiteConnection", conn_class_string, ignore.case = TRUE)) {
    return("SQLite")
  } else if (grepl("SQLServerConnection|Microsoft SQL Server", conn_class_string, ignore.case = TRUE)) {
    return("MS SQL Server")
  } else if (grepl("OraConnection", conn_class_string, ignore.case = TRUE)) {
    return("Oracle")
  } else {
    return("Unknown type")
  }

}
