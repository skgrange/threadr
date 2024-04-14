#' Function to evaluate and execute an R expression stored as text (a character
#' vector). 
#' 
#' \code{evaluate_text} uses \code{eval(parse(text = x))}.
#' 
#' @author Stuart K. Grange
#' 
#' @param text Text that is valid R code. 
#' 
#' @examples
#' 
#' # Evaluate a simple expression
#' evaluate_text("5 + 5")
#' 
#' @export 
evaluate_text <- function(text) {
  eval(parse(text = text))
}
