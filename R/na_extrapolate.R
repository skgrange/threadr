#' Function to extrapolate leading and trailing \code{NA}s. 
#' 
#' \code{na_extrapolate} interpolates leading and trailing \code{NA}s with a 
#' linear function. 
#' 
#' @param x Numeric or integer vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector. 
#' 
#' @seealso \code{\link{na_interpolate}}
#' 
#' @examples 
#' 
#' na_extrapolate(c(NA, 1:5, rep(NA, 5)))
#' 
#' @export
na_extrapolate <- function(x) {
  
  # Check input
  stopifnot(class(x) %in% c("integer", "numeric"))
  
  # Get indices
  index_na <- which(is.na(x))
  index_value <- which(!is.na(x))
  x_complete <- x[index_value]
  
  # 
  index_trailing <- max(index_value):length(x)
  index_leading <- 1:min(index_value)
  
  # Model these points with lm, a simple delta in this case
  fit_trailing <- build_lm_from_index_and_values(
    index = tail(index_value, 2),
    values = tail(x_complete, 2)
  )

  fit_leading <-  build_lm_from_index_and_values(
    index = head(index_value, 2),
    values = head(x_complete, 2)
  )
  
  # Use models to predict
  x_trailing <- predict(fit_trailing, newdata = tibble(x = index_trailing))
  x_leading <- predict(fit_leading, newdata = tibble(x = index_leading))
  
  # Drop duplicated values, already present in complete vector
  x_trailing <- unname(x_trailing[-1])
  x_leading <- unname(x_leading[-length(x_leading)])
  
  # Bind the complete vectors together
  x <- c(x_leading, x_complete, x_trailing)
  
  return(x)
  
}


build_lm_from_index_and_values <- function(index, values) {
  
  stopifnot(length(index) == 2)
  stopifnot(length(values) == 2)
  
  # Create data frame
  df <- tibble(x = index, y = values)
  
  # Model these points with lm, a simple delta in this case
  fit <- lm(y ~ x, data = df)
  
  return(fit)
  
}
