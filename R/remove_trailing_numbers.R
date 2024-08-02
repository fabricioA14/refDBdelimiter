# Function to remove trailing numbers from names
remove_trailing_numbers <- function(name) {
  gsub(" [0-9]+$", "", name)
}