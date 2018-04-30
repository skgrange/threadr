#' Function for "tufte-like" \strong{ggplot2} appearance. 
#' 
#' @param grid_lines Should the plot have grid lines? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, a ggplot2 plot. 
#' 
#' @export
ggtheme_threadr <- function(grid_lines = FALSE) {
  
  # Tufte styling, maximise data ink
  plot <- ggthemes::theme_tufte(base_family = "GillSans")
  
  # Add grid lines too
  if (grid_lines) plot <- plot + theme(panel.grid.major = element_line())
  
  return(plot)
  
}
