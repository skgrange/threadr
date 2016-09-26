#' Function to quickly use \code{jsonlite::toJSON}.
#' 
#' @param x An R object.
#' 
#' @param pretty Should the JSON object be pretty printed? Default is 
#' \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
to_json <- function(x, pretty = TRUE) 
  jsonlite::toJSON(x, pretty = pretty, digits = NA, auto_unbox = TRUE)
