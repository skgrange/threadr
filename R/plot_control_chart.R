#' Function to plot a control chart. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Input data frame containing \code{value} and \code{date} variables.
#' 
#' @param by A character vector indicating the grouping variables within 
#' \code{df} to calculate control limits for and plot as facets. 
#' 
#' @param scales A \strong{ggplot2} argument for setting the scales of the 
#' facets.
#' 
#' @param control_constant Constant used to transform the mean delta to 
#' sequential deviation. Through simulations, this is usually set as 1.128
#' 
#' @param control_multiplier Multiplier for sequential deviation to get the 
#' lower and upper control limits, typically 3.
#' 
#' @seealso \code{\link{calculate_control_limits}}
#' 
#' @return ggplot2 plot. 
#' 
#' @export
plot_control_chart <- function(df, by = as.character(), 
                               control_constant = 1.128, control_multiplier = 3, 
                               scales = "fixed") {
  
  # Check inputs
  # Only two groups can be plotted
  stopifnot(length(by) <= 2)
  
  # The needed variables
  stopifnot(all(c("date", "value", by) %in% names(df)))
  
  # Calculate control limits
  df_limits <- df %>% 
    dplyr::group_by_at(by) %>% 
    dplyr::group_modify(
      ~calculate_control_limits(
        .$value, 
        control_constant = control_constant, 
        control_multiplier = control_multiplier
      )
    ) %>% 
    ungroup()
  
  # Make longer for plotting
  df_limits_long <- df_limits %>% 
    tidyr::pivot_longer(-1:-sequential_deviation, names_to = "limit")
  
  # Add outlier variable to input
  df_join <- df %>% 
    left_join(df_limits, by = by) %>% 
    mutate(outlier = if_else(value >= upper | value <= lower, TRUE, FALSE))
  
  plot <- ggplot2::ggplot() + 
    ggplot2::geom_hline(
      data = df_limits_long, ggplot2::aes(yintercept = value), linetype = "dashed"
    ) + 
    ggplot2::geom_line(
      data = df_join, ggplot2::aes(date, value), colour = "black"
    ) + 
    ggplot2::geom_point(
      data = df_join, ggplot2::aes(date, value, colour = outlier), size = 3
    ) + 
    theme_less_minimal(legend_position = "none") + 
    ggplot2::scale_colour_manual(values = colours_ggpubr()[c(1, 3)]) + 
    ggplot2::labs(
      x = "Date",
      caption = "Dashed lines show mean, lower, and upper control limits"
    )
  
  # Facet plots
  if (!length(by) == 0L) {
    plot <- plot + ggplot2::facet_wrap(facets = by, scales = scales)
  }
  
  return(plot)
  
}
