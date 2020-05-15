#' Function to generate a vector of colours from ColorBrewer palettes. 
#' 
#' @param palette ColorBrewer palette name.
#' 
#' @return Character vector.
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link[RColorBrewer]{brewer.pal}}, \url{http://colorbrewer.org}
#' 
#' @export
colours_brewer <- function(palette = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Dark2")) {
  
  # Get first element
  palette <- palette[1]
  
  # Check
  stopifnot(palette %in% c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Dark2"))
  
  if (palette == "Set1") {
    x <- RColorBrewer::brewer.pal(9, palette)[-6]
  } else if (palette == "Set2") {
    x <- RColorBrewer::brewer.pal(8, palette)
  } else if (palette == "Set3") {
    x <- RColorBrewer::brewer.pal(12, palette)
  } else if (palette == "Pastel1") {
    x <- RColorBrewer::brewer.pal(9, palette)
  } else if (palette == "Pastel2") {
    x <- RColorBrewer::brewer.pal(8, palette)
  } else if (palette == "Dark2") {
    x <- RColorBrewer::brewer.pal(8, palette)
  }
  
  return(x)
  
}
