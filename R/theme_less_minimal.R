#' Function for a minimal \strong{ggplot2} appearance but with bounding lines 
#' for plot area. 
#' 
#' @param base_size Base font size.
#' 
#' @param base_family Base font family. 
#' 
#' @param base_line_size Base size for line elements. 
#' 
#' @param base_rect_size Base size for rectangle elements.
#' 
#' @param x_strip_margin,y_strip_margin Margins for strip headings.
#' 
#' @param angle Angle of text. 
#' 
#' @param narrow_strips Should the plot have narrow strips for facet headings? 
#' 
#' @param x_label_rotate Angle for x-axes labels. 
#' 
#' @param legend_position The position of the legend ("none", "left", "right", 
#' "bottom", "top", or two-element numeric vector).
#' 
#' @param location,size Specific arguments for \code{ggplot_caption_format}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, modification to a \strong{ggplot2} plot. 
#' 
#' @examples
#' 
#' # Load package
#' library(ggplot2)
#' 
#' # Create data
#' data_example <- tibble::tibble(
#'   x = seq(1:3), 
#'   y = sample(1:10, 3),
#'   facet_heading = "The plot"
#' )
#' 
#' # Plot with the theme
#' ggplot(data_example, aes(x, y)) + 
#'   geom_point() + 
#'   facet_wrap("facet_heading") + 
#'   theme_less_minimal()
#'   
#' # Plot with the theme with some options
#' ggplot(data_example, aes(x, y)) + 
#'   geom_point() + 
#'   facet_wrap("facet_heading") + 
#'   theme_less_minimal(narrow_strips = TRUE, x_label_rotate = 45)
#'   
#' # Plot and move legend
#' ggplot(data_example, aes(x, y, colour = facet_heading)) + 
#'   geom_point() + 
#'   theme_less_minimal(legend_position = "bottom")
#'   
#' # Plot and move legend
#' ggplot(data_example, aes(x, y, colour = facet_heading)) + 
#'   geom_point() + 
#'   theme_less_minimal() + 
#'   labs(caption = "A caption") + 
#'   ggplot_caption_format(size = 10)
#' 
#' @export
theme_less_minimal <- function(base_size = 11, base_family = "", 
                               base_line_size = base_size / 22, 
                               base_rect_size = base_size / 22,
                               narrow_strips = TRUE, x_label_rotate = 0,
                               legend_position = NA) {
  
  plot <- ggplot2::theme_bw(
    base_size = base_size, 
    base_family = base_family, 
    base_line_size = base_line_size, 
    base_rect_size = base_rect_size) %+replace% 
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(), 
      legend.background = ggplot2::element_blank(), 
      legend.key = ggplot2::element_blank(), 
      panel.background = ggplot2::element_blank(), 
      panel.border = ggplot2::element_rect(fill = NA, linetype = 1, size = 0.1),
      strip.background = ggplot2::element_rect(linetype = 1, size = 0.1),
      plot.background = ggplot2::element_blank(),
      complete = TRUE
    )
  
  # Do a couple of extra things
  if (narrow_strips) plot <- plot + theme_narrow_strips()
  
  # x label work
  # Switch for old default use
  if (is.logical(x_label_rotate) && x_label_rotate) {
    x_label_rotate <- 45
  }
  
  if (x_label_rotate != 0) {
    plot <- plot + theme_x_label_rotate(angle = x_label_rotate)
  }
  
  if (!is.na(legend_position[1])) {
    plot <- plot + ggplot2::theme(legend.position = legend_position)
  }
  
  return(plot)
  
}


#' @rdname theme_less_minimal
#' 
#' @export
theme_narrow_strips <- function(x_strip_margin = 2.2, y_strip_margin = 2.5) {
  
  ggplot2::theme(
    strip.text.x = ggplot2::element_text(
      margin = ggplot2::margin(b = x_strip_margin, t = x_strip_margin),
    ),
    strip.text.y = ggplot2::element_text(
      margin = ggplot2::margin(b = y_strip_margin, t = y_strip_margin),
      angle = -90
    )
  )
  
}


#' @rdname theme_less_minimal
#' 
#' @export
theme_x_label_rotate <- function(angle = 45) {
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = 1))
}


#' @rdname theme_less_minimal
#' 
#' @export
ggplot_caption_format <- function(location = "left", size = NULL) {
  
  # Switch
  location <- if_else(location == "left", 0L, 1L)
  
  # Do
  ggplot2::theme(
    plot.caption = ggplot2::element_text(hjust = location, size = size)
  )
  
}
