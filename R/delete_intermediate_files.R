#' Delete Intermediate Files
#'
#' This function deletes intermediate files specified in the input vector.
#'
#' @param files A character vector of file paths to be deleted.
#' @return The function returns a logical vector indicating whether the files were successfully deleted.
#' @examples
#' # Create example files
#' file.create("file1.txt")
#' file.create("file2.txt")
#'
#' # Check if files exist
#' file.exists("file1.txt")
#' file.exists("file2.txt")
#' delete_intermediate_files(c("file1.txt", "file2.txt"))
#' # Check if files exist
#' file.exists("file1.txt")
#' file.exists("file2.txt")
#' @import readr
#' @export
delete_intermediate_files <- function(files) {
  sapply(files, function(file) {
    if (file.exists(file)) {
      file.remove(file)
    }
  })
}
