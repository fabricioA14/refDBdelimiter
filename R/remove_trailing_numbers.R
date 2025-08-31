#' Remove Trailing Numbers from Names
#'
#' This function removes trailing numbers from names.
#'
#' @param name A character vector of names from which trailing numbers should be removed.
#' @return A character vector with trailing numbers removed.
#' @examples
#' names <- c("Homo sapiens 1", "Pan troglodytes 2", "Gorilla gorilla 3")
#' remove_trailing_numbers(names)
#' @import readr
#' @export
remove_trailing_numbers <- function(name) {
  gsub(" [0-9]+$", "", name)
}