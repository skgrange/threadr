#' Function to plot time series data. 
#' 
#' @param df Input data frame. 
#' 
#' @param colour Colour of line geometry or which variable in \code{df} to code
#' for colour. 
#' 
#' @param facet_variable Variable in \code{df} to facet the plot. 
#' 
#' @param size Size/width of line geometry. 
#' 
#' @param ylim Limits for y-axes. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible \strong{ggplot2} plot. 
#' 
#' @examples 
#' 
#' # Build data frame with correct variables
#' df <- data.frame(
#'   date = lubridate::ymd(c("2019-01-01", "2019-01-25"), tz = "UTC"),
#'   value = 1:2
#' )
#' 
#' # Plot
#' plot_time_series(df)
#' 
#' @export
plot_time_series <- function(df, colour = "#FC4E07", facet_variable = NA, 
                             size = 0.2, ylim = c(NA, NA)) {
  
  # Check input
  stopifnot("value" %in% names(df) && is.numeric(df$value))
  stopifnot("date" %in% names(df) && lubridate::is.POSIXct(df$date))
  
  if (colour %in% names(df)) {
    
    # Requires the use of aes_string
    plot <- ggplot2::ggplot(
      data = df, 
      ggplot2::aes_string("date", "value", colour = colour)
    ) +
      ggplot2::geom_line(size = size, na.rm = TRUE)
    
  } else {
    
    # A bit simpler
    plot <- ggplot2::ggplot(data = df, ggplot2::aes(date, value)) +
      ggplot2::geom_line(colour = colour, size = size, na.rm = TRUE)
    
  }
  
  # Add extras to the plot
  plot <- plot +
    theme_less_minimal(narrow_strips = TRUE) + 
    ggplot2::xlab("Date") +
    ggplot2::coord_cartesian(ylim = ylim)
  
  # Facet plot
  if (!is.na(facet_variable[1])) {
    plot <- plot + ggplot2::facet_wrap(facet_variable)
  }
  
  # Make sure the plot is printed
  print(plot)
  
  return(invisible(plot))
  
}
