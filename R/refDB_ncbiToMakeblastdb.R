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

    # Copy and clean the original database, replacing spaces with underscores
    replace_spaces_with_underscores < ", cleaned_ncbi_database ," > temp_file && mv temp_file ", ncbi_database_based_on_gbif ,"
  "))
  
  # Add the sed command to remove trailing underscores
  system(paste0("sed -i '/^>.*_$/s/_$//' ", ncbi_database_based_on_gbif))
  
  # Path to the file where you want to save the excluded parts
  excluded_parts_file <- "SelectedSequences.txt"
  
  # Extract and save the excluded parts
  #system(paste0("wsl sed -n 's/^[^_]*_\\([^_]*\\)_.*/\\1/p' ", ncbi_database_based_on_gbif, " | paste -sd '|' - > ", excluded_parts_file))
  #system(paste0("wsl sed -n 's/^[^_]*_\\([^_]*\\)_.*/\\1/p' ", ncbi_database_based_on_gbif, " | paste -sd '|' - | sed 's/$/|/' > ", excluded_parts_file))
  
  system(paste0("wsl sed -n 's/^>\\([^_]*\\)_.*/\\1/p' ", ncbi_database_based_on_gbif, " | paste -sd '|' - > ", excluded_parts_file))
  system(paste0("wsl sed -n 's/^>\\([^_]*\\)_.*/\\1/p' ", ncbi_database_based_on_gbif, " | paste -sd '|' - | sed 's/$/|/' > ", excluded_parts_file))

  # Remove the first underscore and everything before the second underscore
  #system(paste0("wsl sed -i 's/_[^_]*_/_/' ", ncbi_database_based_on_gbif))
  system(paste0("wsl sed -i 's/^>[^_]*_/>/' ", ncbi_database_based_on_gbif))
  
  # Insert sequential numbers at the beginning of each header
  system(paste0("
    wsl awk '
    BEGIN { seq_num=1 }
    /^>/ {
      print \">\" seq_num++ \"_\" substr($0, 2)
    }
    !/^>/ {
      print
    }' ", ncbi_database_based_on_gbif, " > temp_file && mv temp_file ", ncbi_database_based_on_gbif))
}
