#' Make Unique Names
#'
#' This function ensures that names in a character vector are unique by appending numbers where necessary.
#'
#' @param names A character vector of names to be made unique.
#' @return A character vector with unique names.
#' @examples
#' make_unique_names(c("Panthera", "Panthera", "Felis", "Canis", "Canis"))
#' @import readr
#' @export
make_unique_names <- function(names) {
  make.unique(names, sep = " ")
}