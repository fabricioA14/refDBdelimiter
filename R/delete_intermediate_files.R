# Function to delete intermediate files
delete_intermediate_files <- function(files) {
  sapply(files, function(file) {
    if (file.exists(file)) {
      file.remove(file)
    }
  })
}