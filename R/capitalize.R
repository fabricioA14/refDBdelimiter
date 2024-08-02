# Function to capitalize the first letter of each string in a vector
capitalize <- function(x) {
  sapply(x, function(y) paste0(toupper(substring(y, 1, 1)), substring(y, 2)), USE.NAMES = FALSE)
}