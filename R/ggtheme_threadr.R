#' Function for "tufte-like" \strong{ggplot2} appearance. 
#' 
#' @param grid_lines Should the plot have grid lines? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, modification to a \strong{ggplot2} plot. 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Load package
#' library(ggplot2)
#' 
#' # Create data
#' data_example <- tibble(
#'   x = seq(1:3), 
#'   y = sample(1:10, 3)
#' )
#' 
#' # Plot with a tufte theme
#' ggplot(data_example, aes(x, y)) + 
#'   geom_point() + 
#'   ggtheme_threadr()
#'   
#' # Plot with a super minimal
#' ggplot(data_example, aes(x, y)) + 
#'   geom_point() + 
#'   theme_super_minimal()
#'   
#' }
#' 
#' @export
ggtheme_threadr <- function(grid_lines = FALSE) {
  
  # Tufte styling, maximise data ink
  plot <- ggthemes::theme_tufte(base_family = "GillSans")
  
  # Add subtle grid lines too
  if (grid_lines) {
    plot <- plot + 
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(size = 0.05, linetype = "dashed")
      )
  }
  
  return(plot)
  
}


#' @rdname ggtheme_threadr
#' @export
theme_super_minimal <- function(grid_lines = FALSE) {
  
  # Tufte styling, maximise data ink but use a common pdf font
  plot <- ggthemes::theme_tufte(base_family = "sans")
  
  # Add subtle grid lines too
  if (grid_lines) {
    plot <- plot + 
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(size = 0.05, linetype = "dashed")
      )
  }
  
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


#' @rdname ggtheme_threadr
#' @export
colour_inferno_peach <- function() "#E65C30"


#' @rdname ggtheme_threadr
#' @export
colours_ggpubr <- function() c("#00AFBB", "#E7B800", "#FC4E07", "#868686FF")


#' @rdname ggtheme_threadr
#' @export
colours_plotly <- function() {
  c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2",
    "#7f7f7f", "#bcbd22"
  )
}
