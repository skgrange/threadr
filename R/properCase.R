#' Function to capitalise the first letter of every word in a string
#' 
#' Wrapper for stringi::stri_trans_totitle
#' 
#' @author Stuart k. Grange
#'
#' @examples
#' 
#' \dontrun{
#' # Some strings
#' string <- c('auckland', 'wellington', 'berlin')
#' 
#' # Make proper case
#' properCase(string)
#' "Auckland"   "Wellington" "Berlin"
#'}
#' 
#' @export
#'
properCase <- function(x) stringi::stri_trans_totitle(x)
