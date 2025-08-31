\name{delete_intermediate_files}
\alias{delete_intermediate_files}
\title{Delete Intermediate Files}
\description{
  This function deletes intermediate files specified in the input vector.
}
\usage{
delete_intermediate_files(files)
}
\arguments{
  \item{files}{A character vector of file paths to be deleted.}
}
\value{
  The function returns a logical vector indicating whether the files were successfully deleted.
}
\examples{
# Create example files
file.create("file1.txt")
file.create("file2.txt")

# Check if files exist
file.exists("file1.txt")
file.exists("file2.txt")

# Delete the files
delete_intermediate_files(c("file1.txt", "file2.txt"))

# Check if files exist
file.exists("file1.txt")
file.exists("file2.txt")
}