#' Function to generate a custom colour palettes from Lancet figures.
#' 
#' @author Stuart K. Grange
#' 
#' @param palette Palette number. 
#' 
#' @return Character vector.
#' 
#' @examples 
#' 
#' colours_lancet()
#' colours_lancet(palette = 2)
#'
#' @export 
colours_lancet <- function(palette = c(1, 2)) {
  
  if (palette[1] == 1) {
    x <- c(
      "#b30738", "#e0c100", "#6cb33f", "#00703c", "#91bda6", "#009fc3", "#00549e", 
      "#936fb1", "#850c70"
    )
  } else if (palette[1] == 2) {
    x <- c(
      "#c5167d", "#8e0051", "#d83026", "#f26d43", "#fbae61", "#ffffbe", "#acd9e9", 
      "#75acd2", "#4476b4", "#323695", "#03665e", "#33978f" 
    )
  }
  
  x <- stringr::str_to_lower(x)
  
  return(x)
  
}


#' @rdname colours_lancet
#' @export 
colours_icct <- function() {
  c(
    "#007d93", "#6c953e", "#f1ae1d", "#d54929", "#9d053b", "#652566", "#4f3227", 
    "#414d56"
  )
}
