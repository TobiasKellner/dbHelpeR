#' Set primary key
#'
#' This function sets a primary key for a selected database table. \cr
#' Note: For Microsoft SQL Server databases, please ensure that the primary key
#' columns are defined as \code{NOT NULL}.
#'
#' @param conn A DBIConnection object.
#' @param schema The name of the parent schema of the database table.
#'  If no schema is specified, the default schema of the database is used
#'  (e.g. 'dbo' for Microsoft SQL Server).
#' @param table The name of the database table.
#' @param columns Character vector. The column(s) to set as the primary key.
#' @param pk_name The optional name of the primary key. If no name is specified,
#'  the primary key name is generated as follows: \cr
#'  \code{pk_name <- paste0("PK_", table)}
#'
#' @return
#' @export
#'
#' @examples
#'
set_primary_key <- function(conn, schema = NULL, table, columns, pk_name = NULL) {

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

  # validate input
  if (length(columns) < 1) {
    stop("At least one column must be specified for the primary key.")
  }

  # handle schema prefix if provided
  schema_prefix <- if (!is.null(schema)) paste0(schema, ".") else ""

  # define name of primary key
  pk_name <- ifelse(is.null(pk_name), paste0("PK_", table), pk_name)

  # join columns with commas for composite primary keys
  columns_list <- paste(columns, collapse = ", ")
  columns_list_postgres <- paste(shQuote(columns, type = "cmd"), collapse = ", ")

  # define a switch for supported database types
  sql_command <- switch(
    db_type,
    "postgres" = paste0(
      "ALTER TABLE ", schema_prefix, "\"", table, "\"",
      " ADD CONSTRAINT ", pk_name, " PRIMARY KEY (", columns_list_postgres, ");"
    ),
    "mssql" = paste0(
      "ALTER TABLE ", schema_prefix, table,
      " ADD CONSTRAINT ", pk_name, " PRIMARY KEY (", columns_list, ");"
    ),
    stop("Unsupported database type: ", db_type)
  )

  # execute SQLcommand
  dbExecute(conn, sql_command)

  # message about successful setting of the primary key
  cat(
    "Primary key ", "\"", pk_name, "\"" ," successfully set on table '",
    schema_prefix, table,
    "' for column(s): ", paste(columns, collapse = ", "), "\n", sep = ""
    )

}
