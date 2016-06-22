#' Function to send a statements to a database. 
#' 
#' \code{db_send} is a wrapper for \code{DBI::dbSendQuery} but is vectorised 
#' over \code{statement}
#' 
#' @param con Database connection. 
#' @param statement Statement to send to \code{con}. 
#' @param progress Type of progress bar to display. Default is \code{"none"}. 
#' 
#' @export
db_send <- function(con, statement, progress = "none") {
  
  # Vectorise the function
  plyr::l_ply(statement, function(x) DBI::dbSendQuery(con, x), 
              .progress = progress)
  
  # No return
  
}
