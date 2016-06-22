#' Function to get/fetch data from a database with a statement. 
#' 
#' \code{db_get} is a wrapper for \code{DBI::dbGetQuery}. 
#' 
#' @param con Database connection. 
#' @param statement Statement to send to \code{con}. 
#' 
#' @export
db_get <- function(con, statement) DBI::dbGetQuery(con, statement)


#' Function to read an entire database table. 
#' 
#' \code{db_read_table} is a wrapper for \code{DBI::dbReadTable}. 
#' 
#' @param con Database connection. 
#' @param table Table to read. 
#' 
#' @export
db_read_table <- function(con, table) DBI::dbReadTable(con, table)


#' Function to list all variables/columns/fields in a database table. 
#' 
#' \code{db_list_variables} is a wrapper for \code{DBI::dbListFields}. 
#' 
#' @param con Database connection. 
#' @param table Table to list variables/columns/fields. 
#' 
#' @export
db_list_variables <- function(con, table) DBI::dbListFields(con, table)


#' Function to list results for a database connection. 
#' 
#' @param con Database connection.
#' 
#' @export
db_list_results <- function(con) DBI::dbListResults(con)[[1]]


#' Function to clear results for a database connection. 
#' 
#' @param con Database connection.
#' 
#' @export
db_clear_results <- function(con) DBI::dbClearResult(db_list_results(con))


#' Function to commit transactions for a database connection. 
#' 
#' @param con Database connection.
#' 
db_commit <- function(con) DBI::dbCommit(con)
