#' Delete Intermediate Files
#'
#' This function deletes intermediate files.
#'
#' @param files A character vector specifying the files to delete.
#' @return None
#' @examples
#' delete_intermediate_files(c("file1.txt", "file2.txt"))
#' @export
delete_intermediate_files <- function(files) {
  sapply(files, function(file) {
    if (file.exists(file)) {
      file.remove(file)
    }
  })
}