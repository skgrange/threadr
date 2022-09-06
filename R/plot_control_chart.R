#' Function to plot a control chart. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Input data frame containing \code{value} and \code{date} variables.
#' 
#' @param by A character vector indicating the grouping variables within 
#' \code{df} to calculate control limits for and plot as facets. 
#' 
#' @param size Size of the plots' points.
#' 
#' @param control_constant Constant used to transform the mean delta to 
#' sequential deviation. Through simulations, this is usually set as 1.128
#' 
#' @param control_multiplier Multiplier for sequential deviation to get the 
#' lower and upper control limits, typically 3. 
#' 
#' @param add_reference Should a reference value be added to the plot? This value
#' must be called \code{value_reference} in \code{df}. 
#' 
#' @param scales A \strong{ggplot2} argument for setting the scales of the 
#' facets.
#' 
#' @seealso \code{\link{calculate_control_limits}}
#' 
#' @return ggplot2 plot. 
#' 
#' @export
plot_control_chart <- function(df, by = as.character(), size = 2,
                               control_constant = 1.128, control_multiplier = 3, 
                               add_reference = FALSE, scales = "fixed") {
  
  # Check inputs
  # Only two groups can be plotted
  stopifnot(length(by) <= 2)
  
  # The needed variables
  stopifnot(all(c("date", "value", by) %in% names(df)))
  
  if (add_reference && !"value_reference" %in% names(df)) {
    stop("`value_reference`", call. = FALSE)
  }
  
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
  
  # # Make longer for plotting with lines
  # df_limits_long <- df_limits %>% 
  #   tidyr::pivot_longer(-1:-sequential_deviation, names_to = "limit")
  
  # ggplot2::geom_hline(
  #   data = df_limits_long, 
  #   ggplot2::aes(yintercept = value), 
  #   linetype = "dashed",
  #   colour = "grey50"
  # ) 
  
  # Add outlier variable to input
  df_join <- df %>% 
    left_join(df_limits, by = by) %>% 
    mutate(outlier = value >= upper | value <= lower, 
           outlier = logical_to_yes_no(outlier))
  
  # Build most of the plot
  plot <- ggplot2::ggplot() + 
    ggplot2::geom_rect(
      data = df_limits,
      ggplot2::aes(
        ymin = lower, 
        ymax = upper, 
        xmin = as.POSIXct(-Inf),
        xmax = as.POSIXct(Inf)
      ),
      alpha = 0.15
    ) + 
    ggplot2::geom_line(
      data = df_join, ggplot2::aes(date, value), colour = "black"
    ) + 
    ggplot2::geom_point(
      data = df_join, ggplot2::aes(date, value, colour = outlier), size = size
    ) + 
    theme_less_minimal(legend_position = "none") + 
    ggplot2::scale_colour_manual(values = colours_ggpubr()[c(3, 1)], drop = FALSE) + 
    ggplot2::labs(x = "Date")
  
  # Add reference values to plot too
  if (add_reference) {
    
    # Get distinct reference values
    df_reference <- df_join %>% 
      distinct(across(dplyr::all_of(c(by, "value_reference"))))
    
    # Add reference values to plot as another horizontal line
    plot <- plot + 
      ggplot2::geom_hline(
        data = df_reference, 
        ggplot2::aes(yintercept = value_reference),
        linetype = "dashed",
        colour = "black"
      )
    
  }
  
  # For caption label
  if (add_reference) {
    caption <- "Shaded zone shows the control limit's range & dashed line shows reference value"
  } else {
    caption <- "Shaded zone shows the control limit's range"
  }
  
  # Add caption to plot
  plot <- plot + ggplot2::labs(caption = caption)

  # Facet plots
  if (!length(by) == 0L) {
    plot <- plot + ggplot2::facet_wrap(facets = by, scales = scales)
  }
  
  return(plot)
  
}
