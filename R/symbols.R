#' Functions for commonly used unicode symbols. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
symbol_degree <- function() "\u00b0"


#' @rdname symbol_degree
#' @export
symbol_mu_lower <- function() "\u00b5"


#' @rdname symbol_degree
#' @export
symbol_subscript_1 <- function() "\u2081"


#' @rdname symbol_degree
#' @export
symbol_subscript_2 <- function() "\u2082"


#' @rdname symbol_degree
#' @export
symbol_subscript_4 <- function() "\u2084"


#' @rdname symbol_degree
#' @export
symbol_subscript_10 <- function() stringr::str_c("\u2081", "\u2080")


#' @rdname symbol_degree
#' @export
symbol_subscript_x <- function() "\u2093"


#' @rdname symbol_degree
#' @export
symbol_subscript_2.5 <- function() stringr::str_c("\u2082", "\u2024", "\u2085")


#' @rdname symbol_degree
#' @export
symbol_subscript_3 <- function() "\u2083"


#' @rdname symbol_degree
#' @export
symbol_superscript_3 <- function() "\u00B3"


#' @rdname symbol_degree
#' @export
symbol_superscript_minus <- function() "\u207B"
