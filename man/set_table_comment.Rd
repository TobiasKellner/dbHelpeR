% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_table_comment.R
\name{set_table_comment}
\alias{set_table_comment}
\title{Create a comment for a database table}
\usage{
set_table_comment(conn, schema = NULL, table, comment)
}
\arguments{
\item{conn}{A DBIConnection object}

\item{schema}{The name of the parent schema of the database table}

\item{table}{The name of the database table}

\item{comment}{The comment you want to set for this table}
}
\description{
This function sets a comment for the selected database table.
}
\examples{
set_table_comment(conn = con, schema = "test_schema", table = "test_table", comment = "This is my test comment.")

}
