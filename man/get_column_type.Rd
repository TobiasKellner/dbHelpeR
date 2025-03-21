% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_column_type.R
\name{get_column_type}
\alias{get_column_type}
\title{Get column type}
\usage{
get_column_type(conn, schema = NULL, table, column = NULL, dataframe = FALSE)
}
\arguments{
\item{conn}{A DBIConnection object.}

\item{schema}{The name of the parent schema of the database table.
If no schema is specified, the default schema of the database is used
(e.g. 'dbo' for Microsoft SQL Server).}

\item{table}{The name of the database table}

\item{column}{The name of the column \cr
Note: If the 'dataframe' is set to TRUE, the 'column' option can be omitted to get a table of all columns with column type mapping}

\item{dataframe}{A boolean value for obtaining a character string or dataframe}
}
\value{
A character string representing the column type respectively a dataframe object representing the column type if 'dataframe' is set to TRUE
}
\description{
This function returns the column type of a selected column respectively a
table of the type assignment of all columns if the 'dataframe' option is set to TRUE.
}
\examples{
get_column_type(conn = con, schema = "schema", table = "table", column = "column", dataframe = FALSE)
get_column_type(conn = con, schema = "schema", table = "table", dataframe = TRUE)

}
