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
#' @param strip_margin Margins for strip headings. 
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
#' }
#' 
#' @export
theme_less_minimal <- function(base_size = 11, base_family = "", 
                               base_line_size = base_size / 22, 
                               base_rect_size = base_size / 22,
                               strip_margin = 2) {
  
  ggplot2::theme_bw(
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
      complete = TRUE,
      strip.text.x = ggplot2::element_text(
        margin = ggplot2::margin(b = strip_margin, t = strip_margin)
      ),
      strip.text.y = ggplot2::element_text(
        margin = ggplot2::margin(b = strip_margin, t = strip_margin)
      )
    )
  
}
