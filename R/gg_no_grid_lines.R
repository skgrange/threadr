#' Function to remove all grid lines from a ggplot2 plot. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, a ggplot2 plot. 
#' 
#' @export 
gg_no_grid_lines <- function() {
  
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank()
  )
  
}
