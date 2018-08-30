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
  if (grid_lines) 
    plot <- plot + ggplot2::theme(panel.grid.major = ggplot2::element_line())
  
  return(plot)
  
}


#' @rdname ggtheme_threadr
#' @export
theme_super_minimal <- function(grid_lines = FALSE) {
  
  # Tufte styling, maximise data ink but use a common pdf font
  plot <- ggthemes::theme_tufte(base_family = "sans")
  
  # Add grid lines too
  if (grid_lines) 
    plot <- plot + ggplot2::theme(panel.grid.major = ggplot2::element_line())
  
  return(plot)
  
}


#' @rdname ggtheme_threadr
#' @export
colour_byzantine_blue <- function() "#3457D5"


#' @rdname ggtheme_threadr
#' @export
colour_crimson <- function() "#DC143C"


#' @rdname ggtheme_threadr
#' @export
colour_inferno_purple <- function() "#6B186E"


#' @rdname ggtheme_threadr
#' @export
colour_inferno_orange <- function() "#FCA50A"
