#' Process NCBI Database for use with makeblastdb
#'
#' This function processes an NCBI database by appending unique numbered sequences to the headers and replacing spaces with underscores, preparing it for use with the makeblastdb tool.
#'
#' @param cleaned_ncbi_database Path to the cleaned NCBI database file.
#' @param ncbi_database_based_on_gbif Path to the output NCBI database file based on the GBIF database.
#' @return A processed NCBI database file ready for use with makeblastdb.
#' @examples{
#' \dontrun{
#' # Example usage of refDB_ncbiToMakeblastdb
#' cleaned_ncbi_database <- "path/to/cleaned_ncbi_database.fasta"
#' ncbi_database_based_on_gbif <- "path/to/ncbi_database_based_on_gbif.fasta"
#'
#' refDB_ncbiToMakeblastdb(cleaned_ncbi_database, ncbi_database_based_on_gbif)
#' )}
#' }
#' @import readr
#' @export
refDB_ncbiToMakeblastdb <- function(cleaned_ncbi_database, ncbi_database_based_on_gbif) {
  
  file.create(ncbi_database_based_on_gbif, showWarnings = FALSE)
  fileConn <- file(ncbi_database_based_on_gbif, open = "wt")
  close(fileConn)
  
  system(paste0("
    wsl 

    # Function to replace spaces with underscores
    replace_spaces_with_underscores() {
        sed 's/ /_/g'
    }

    # Function to append unique numbered sequences based on the entire header string
    append_unique_numbered_sequences() {
        awk '
            /^>/ {
                header = substr($0, 2)  # Remove the initial >
                if (!(header in seen)) {
                    seen[header] = 1
                } else {
                    seen[header]++
                }
                print \">\" seen[header] \"_\" header
                getline
                print
            }' ", cleaned_ncbi_database ," | replace_spaces_with_underscores >> ", ncbi_database_based_on_gbif ,"
    }

    # Append unique numbered sequences
    append_unique_numbered_sequences
  "))
  
  # Add the sed command to remove trailing underscores
  system(paste0("sed -i '/^>.*_$/s/_$//' ", ncbi_database_based_on_gbif))
}
