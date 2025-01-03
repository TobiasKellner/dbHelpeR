get_database_name <- function(conn) {
  # detect database type
  db_type <- get_db_type(con)
  db_type <- tolower(db_type)

  if (grepl("sql server", db_type)) {
    db_type <- "mssql"
  } else if (grepl("postgresql", db_type)) {
    db_type <- "postgres"
  } else {
    stop("Datenbanktyp wird nicht unterstützt. Unterstützte Typen: MSSQL, PostgreSQL.")
  }

  # Define a switch for supported database types
  query <- switch(
    db_type,
    "postgres" = "SELECT current_database();",
    "mssql" = "SELECT DB_NAME();",
    stop("Unsupported database type: ", db_type)
  )

  result <- dbGetQuery(con, query)

  if (nrow(result) > 0) {
    return(result[[1, 1]])
  } else {
    warning("-")
    return(NA)
  }
}
