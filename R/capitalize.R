#' Capitalize the First Letter of Each Word
#'
#' This function capitalizes the first letter of each word in a string.
#'
#' @param x A character vector of strings to be capitalized.
#' @return A character vector with the first letter of each word capitalized.
#' @details This function takes a character vector and returns the same vector with the first letter of each word capitalized.
#' @examples
#' capitalize("this is a test")
#' capitalize(c("hello world", "goodbye world"))
#' @export
capitalize <- function(x) {
  sapply(x, function(y) paste0(toupper(substring(y, 1, 1)), substring(y, 2)), USE.NAMES = FALSE)
}
