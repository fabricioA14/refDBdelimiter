#' Remove Trailing Numbers from Names
#'
#' This function removes trailing numbers from names.
#'
#' @param name A character string.
#' @return A character string with trailing numbers removed.
#' @examples
#' remove_trailing_numbers("apple 1")
#' @export
remove_trailing_numbers <- function(name) {
  gsub(" [0-9]+$", "", name)
}