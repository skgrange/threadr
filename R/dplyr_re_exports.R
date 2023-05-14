#' Pseudo-function to re-export \strong{dplyr}'s common functions. 
#'
#' @name dplyr functions
#'
#' @importFrom dplyr select rename mutate filter arrange distinct summarise 
#'     do group_by ungroup rowwise do left_join inner_join everything bind_rows 
#'     pull as_tibble tibble if_else slice across relocate sym join_by between
#' 
NULL


#' Pseudo-function to re-export \strong{magrittr}'s pipe. 
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL


#' Pseudo-function to re-export \strong{stringr}'s common functions. 
#' 
#' @name stringr functions
#' 
#' @importFrom stringr str_c str_trim str_split str_split_fixed str_replace 
#'   str_replace_all str_to_lower str_to_upper str_remove str_remove_all
#'
NULL


#' Pseudo-function to re-export \strong{rlang}'s walrus operator.
#'
#' @importFrom rlang :=
#' @name :=
#' @rdname walrus
#' @export
NULL
