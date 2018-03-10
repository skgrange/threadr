#' @export
ggtheme_threadr <- function(grid_lines = FALSE) {
  
  # Tufte styling, maximise data ink
  plot <- ggthemes::theme_tufte(base_family = "GillSans")
  
  # Add grid lines too
  if (grid_lines) plot <- plot + theme(panel.grid.major = element_line())
  
  return(plot)
  
}
