#' Remove Trailing Coordinates from Names
#'
#' This function removes trailing coordinates of the form `_lon_lat` 
#' (numbers separated by underscores) from taxon names.
#'
#' @param name A character vector of names from which trailing coordinates should be removed.
#' @return A character vector with trailing coordinates removed.
#' @examples
#' names <- c("Pyrenula nitida_18.515806_54.396611",
#'            "Evernia prunastri_-12.009611_54.537694",
#'            "Scoliciosporum umbrinum_18.455_54.303611")
#' remove_trailing_numbers(names)
#' # [1] "Pyrenula nitida"        "Evernia prunastri"      
#' # [3] "Scoliciosporum umbrinum"
#' @import readr
#' @export
remove_trailing_numbers <- function(name) {
  sub("_[0-9\\.-]+_[0-9\\.-]+$", "", name)
}