#' Function to chunk a vector into pieces of \emph{n} length. 
#' 
#' @author Harlan
#' 
#' @param x Vector or list to be chunked into pieces. 
#' 
#' @param n Length of chunk elements. 
#' 
#' @return List. 
#' 
#' @seealso \url{https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r}
#' 
#' @export
chunk <- function(x, n) split(x, ceiling(seq_along(x) / n))


#' Function to chunk a vector into \emph{n} pieces. 
#' 
#' @author Joachim Schork
#' 
#' @param x Vector to be chunked. 
#' 
#' @param n Number of chunks to split \code{x} into. 
#' 
#' @export
chunk_n <- function(x, n) {
  split(x, cut(seq_along(x), n, labels = FALSE))
}
