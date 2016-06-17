#' @export
dplyr_progress <- function() options()$dplyr.show_progress


#' @export
dplyr_quiet <- function() options(dplyr.show_progress = FALSE)


#' @export
dplyr_progress_revert <- function(x = TRUE)  options(dplyr.show_progress = x)
