#' Make Unique Names
#'
#' This function ensures that a vector of names are unique, appending numbers to duplicates if necessary.
#'
#' @param names A character vector of names.
#' @return A character vector of unique names.
#' @examples
#' make_unique_names(c("apple", "banana", "apple"))
#' @export
make_unique_names <- function(names) {
  make.unique(names, sep = " ")
}