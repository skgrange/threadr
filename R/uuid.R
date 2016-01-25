#' Function to generate randomly generated universally unique identifiers 
#' (UUIDs). 
#'  
#' A UUID is usually considered 32 lowercase hexadecimal digits displayed in 
#' five groups separated by hyphens (a total of 36 characters). The number of 
#' characters in the five groups are 8-4-4-4-12. There are about 3.4 x 10 ^ 38 
#' possible UUID combinations, therefore it is unlikely that two UUIDs will be 
#' identical. 
#' 
#' \code{base::sample} is used as the random number generator. Random-base 
#' UUIDs are sometimes called version 4 UUIDs.
#' 
#' The \code{uuid_rowwise} function uses \code{replicate} so a variable/column
#' in a data frame with unique uuids can be generated conveniently. 
#'  
#' @author Christopher Bare, modified by Stuart K. Grange
#' Originally from: \url{http://gist.github.com/cbare/5979354}
#' 
#' @param uppercase Should the letters be returned as uppercase? Default
#' is \code{FALSE}. 
#' 
#' @param sep What character should separate the five groups of characters? 
#' Default is hyphens (-).
#' 
#' @param df Input data frame for row-wise transformation.
#' 
#' @seealso \code{\link{sample}}, \code{\link{replicate}}
#' 
#' @examples
#' \dontrun{
#' # Get a single identifier
#' identifier <- uuid()
#' 
#' # Print
#' identifier
#' "8e7675d5-4a87-4e0b-ae9b-9c97819aace5"
#' 
#' # Generate unique uuids for every row in a data frame
#' data_ozone$uuid <- uuid_rowwise(data_ozone)
#' 
#' }
#'
#' @export
uuid <- function(uppercase = FALSE, sep = "-") {
  
  # Generate hexadecimal symbols
  hex_digits <- c(as.character(0:9), letters[1:6])
  hex_digits <- if (uppercase) toupper(hex_digits) else hex_digits
  
  # 
  y_digits <- hex_digits[9:12]
  
  string <- stringr::str_c(
    stringr::str_c(sample(hex_digits, 8, replace = TRUE), collapse = ""),
    stringr::str_c(sample(hex_digits, 4, replace = TRUE), collapse = ""),
    stringr::str_c("4", stringr::str_c(sample(hex_digits, 3, replace = TRUE), 
                     collapse = ""), collapse = ""),
    stringr::str_c(sample(y_digits, 1), stringr::str_c(sample(hex_digits, 3, replace = TRUE), 
                                     collapse = ""), collapse = ""),
    stringr::str_c(sample(hex_digits, 12, replace = TRUE), collapse = ""),
    sep = sep)
  
  # Return
  string
  
}


#' @rdname uuid
#' 
#' @export
uuid_rowwise <- function (df) replicate(nrow(df), uuid())
