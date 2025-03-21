#' Get permissions of database user
#'
#' This function returns the permissions of the database user.
#'
#' @param conn A DBIConnection object.
#'
#' @return A character vector representing the permissions of the database user.
#' @export
#'
#' @examples
#' get_user_permissions(conn = con)
#'
get_user_permissions <- function(conn) {

  if (!inherits(conn, "DBIConnection")) {
    stop("The argument is not a valid DBI database connection.")
  }

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

  # check rights based on the database type
  tryCatch({
    sql_command <- NULL

    if (db_type == "postgres") {
      sql_command <- "SELECT DISTINCT privilege_type
                FROM information_schema.role_table_grants
                WHERE grantee = CURRENT_USER;"
    } else if (db_type == "mssql") {
      sql_command <- "SELECT DISTINCT dp.permission_name
                FROM sys.database_permissions dp
                WHERE USER_NAME(dp.grantee_principal_id) = USER_NAME();"
    } else {
      stop("Permission check for this database type is not supported.")
    }

    # execute query
    result <- dbGetQuery(conn, sql_command)

    # convert result into a vector and return it
    return(unique(as.character(result[[1]])))

  }, error = function(e) {
    warning("Error retrieving user rights: ", e$message)
    return(character(0))
  })

}
