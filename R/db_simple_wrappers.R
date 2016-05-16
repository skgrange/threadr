#' Function to send a statement to a database. 
#' 
#' \code{db_send} is a wrapper for \code{DBI::dbSendQuery}. 
#' 
#' @param con Database connection. 
#' @param statement Statement to send to \code{con}. 
#' 
#' @export
db_send <- function(con, statement) DBI::dbSendQuery(con, statement)


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
