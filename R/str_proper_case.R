#' Function to capitalise the first letter of every word in a string. 
#' 
#' A simple wrapper for stringi::stri_trans_totitle. 
#' 
#' @author Stuart K. Grange
#'
#' @examples
#' 
#' \dontrun{
#' # Some strings
#' string <- c('auckland', 'wellington', 'berlin')
#' 
#' # Make proper case
#' str_proper_case(string)
#' "Auckland"   "Wellington" "Berlin"
#'}
#' 
#' @export
#'
str_proper_case <- function (x) stringi::stri_trans_totitle(x)
