#' Function for plotting interactive time-series plots in a similar way to
#' \code{openair::timePlot}. 
#' 
#' @param df Data frame with a variable/column named \code{"date"}.
#' 
#' @param variable Variable in \code{df} to plot. 
#' 
#' @param color Color of plotted geometry. 
#' 
#' @param range Should a range selector be plotted? 
#' 
#' @param step Should the plot take a stepped look? 
#' 
#' @param points Should the plot include points where observations are recorded? 
#' 
#' @param fill Should the plot fill between the x-axis and the geometry? 
#' 
#' @param ylab String for y-axis label. 
#' 
#' @param legend_width Width of legend in pixels. Default is 400. 
#' 
#' @param mouse_label String for legend label. If not used, label will be 
#' \code{variable}. 
#' 
#' @param tz Override for timezone. 
#'
#' @seealso \link{openair::timePlot}
#' 
#' @author Stuart K. Grange
#' 
#' @export
time_dygraph <- function (df, variable = "no2", colour = "red", 
                          range = TRUE, step = FALSE, points = FALSE, fill = FALSE,
                          color = colour, ylab = NA, legend_width = 400,
                          mouse_label = NA, tz = "UTC") {
  
  # Catch dplyr's table data frame
  df <- base_df(df)
  
  if (is.na(mouse_label)) mouse_label <- variable
  
  # Create timeseries object
  time_series <- xts::xts(df[, variable], df[, "date"], order.by = df[, "date"], 
                          tzone = tz)
  
  # Plot
  plot <- dygraphs::dygraph(time_series) %>%  
    dygraphs::dyOptions(colors = color, stepPlot = step, fillGraph = fill, 
                        useDataTimezone = TRUE) %>% 
    dygraphs::dySeries(label = mouse_label, drawPoints = points)  %>% 
    dygraphs::dyLegend(width = legend_width)
  
  # Add range selector
  if (range) {
    
    plot <- plot %>% 
      dygraphs::dyRangeSelector()
    
  }
  
  if (!is.na(ylab)) {
    
    plot <- plot %>% 
      dygraphs::dyAxis("y", label = ylab)
    
  }
  
  # Return
  plot
  
}
