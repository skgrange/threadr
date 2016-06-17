#' Function to arrange variables in a data frame to match those found in a 
#' database table. 
#' 
#' \code{db_arrange_variables} is useful for preparing data for insert and when
#' look-up tables are being created. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' @param table Table name in database. 
#' @param df Data frame. 
#' 
#' @export
db_arrange_variables <- function(con, table, df) 
  plyr::rbind.fill(db_table_names(con, table), df)
