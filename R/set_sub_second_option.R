#' Function to set R's options to print fractional seconds. 
#' 
#' @param digits Number of digits to show. 
#' 
#' @export
set_sub_second_option <- function(digits = 5) options(digits.secs = digits)
