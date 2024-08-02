#' Capitalize the First Letter of Each String
#'
#' This function takes a vector of strings and capitalizes the first letter of each string.
#'
#' @param x A character vector.
#' @return A character vector with each string having its first letter capitalized.
#' @examples
#' capitalize(c("apple", "banana", "cherry"))
#' @export
capitalize <- function(x) {
  sapply(x, function(y) paste0(toupper(substring(y, 1, 1)), substring(y, 2)), USE.NAMES = FALSE)
}