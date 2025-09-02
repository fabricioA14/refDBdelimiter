#' Subset NCBI Database Based on GBIF Database
#'
#' This function subsets an NCBI database based on a GBIF database by searching for and appending sequences that match names from the GBIF database. The matching can be based on exact names or specific conditions.
#'
#' @param gbif_database Path to the GBIF database file containing names.
#' @param cleaned_ncbi_database Path to the cleaned NCBI database file.
#' @param ncbi_database_based_on_gbif Path to the output NCBI database file based on the GBIF database.
#' @param genus_flexibility Logical condition to determine the matching strategy.
#' @return An NCBI database file subsetted based on the GBIF database.
#' @examples{
#' \dontrun{
#' # Example usage of refDB_SubsetNcbiGbif
#' gbif_database <- "path/to/gbif_database.txt"
#' cleaned_ncbi_database <- "path/to/cleaned_ncbi_database.fasta"
#' ncbi_database_based_on_gbif <- "path/to/ncbi_database_based_on_gbif.fasta"
#' genus_flexibility <- TRUE
#'
#' refDB_SubsetNcbiGbif(gbif_database, cleaned_ncbi_database, ncbi_database_based_on_gbif, genus_flexibility)
#' )}
#' }
#' @import readr
#' @importFrom utils read.table write.table
#' @export
refDB_SubsetNcbiGbif <- function(gbif_database, cleaned_ncbi_database, ncbi_database_based_on_gbif, genus_flexibility) {
  
  # Always create an empty output file to start
  file.create(ncbi_database_based_on_gbif, showWarnings = FALSE)
  fileConn <- file(ncbi_database_based_on_gbif, open = "wt")
  close(fileConn)
  
  # If gbif_database exists and is provided
  if (!is.null(gbif_database) && file.exists(gbif_database)) {
    
    if (genus_flexibility == TRUE) {
      # -------------- (1) GENUS_FLEXIBILITY == TRUE CODE --------------
      system(paste0("
        wsl names=$(<", gbif_database ,")

        # Function to split names into chunks
        split_names_into_chunks() {
            local chunk_size=$1
            local num_chunks=$(( (${#names} + $chunk_size - 1) / $chunk_size ))
            local chunks=()
            for (( i = 0; i < $num_chunks; i++ )); do
                local start=$(( $i * $chunk_size ))
                chunks+=( \"$(echo \"${names:$start:$chunk_size}\")\" )
            done
            echo \"${chunks[@]}\"
        }

        # Function to replace spaces with underscores
        replace_spaces_with_underscores() {
            sed 's/ /_/g'
        }

        # Function to search and append sequences with exact matching
        search_and_append() {
            local chunk=\"$1\"
            awk -v pattern=\"$chunk\" '
                BEGIN {
                    split(pattern, pat, \"|\")
                }
                /^>/ {
                    header = substr($0, 2)  # Remove >
                    split(header, parts, \"_\")
                    if (parts[2] == pat[1] || parts[1] == pat[1]) {
                        print \">\" parts[1]   # <-- apenas accession
                        getline
                        print
                    }
                }' ", cleaned_ncbi_database ," | replace_spaces_with_underscores >> ", ncbi_database_based_on_gbif ,"
        }

        # Split names into chunks of 1000
        chunk_size=1000
        IFS='|' read -ra chunks <<< \"$(split_names_into_chunks $chunk_size)\"

        # Search and append sequences for each chunk
        for chunk in \"${chunks[@]}\"
        do
            search_and_append \"$chunk\"
        done
      "))

      # Create a file with  selected accession numbers
      excluded_parts_file <- "SelectedSequences.txt"
      system(paste0("wsl sed -n 's/^>\\([^ ]*\\).*/\\1/p' ",
                    ncbi_database_based_on_gbif, 
                    " | paste -sd '|' - | sed 's/$/|/' > ", 
                    excluded_parts_file))

    } else {
      # -------------- (2) GENUS_FLEXIBILITY == FALSE CODE --------------
      system(paste0("
        wsl names=$(<", gbif_database ,")

        split_names_into_chunks() {
            local chunk_size=$1
            local num_chunks=$(( (${#names} + $chunk_size - 1) / $chunk_size ))
            local chunks=()
            for (( i = 0; i < $num_chunks; i++ )); do
                local start=$(( $i * $chunk_size ))
                chunks+=( \"$(echo \"${names:$start:$chunk_size}\")\" )
            done
            echo \"${chunks[@]}\"
        }

        search_and_append() {
            local chunk=\"$1\"
            awk '/^>/{f=0} 
                 /^>.*'\"$chunk\"'/{f=1} 
                 f' ", cleaned_ncbi_database ," >> ", ncbi_database_based_on_gbif ,"
        }

        modified_names=$(echo \"$names\" | sed 's/[[:space:]]/_/g')

        chunk_size=1000
        IFS='|' read -ra chunks <<< \"$(split_names_into_chunks $chunk_size)\"

        for chunk in \"${chunks[@]}\"
        do
            search_and_append \"$chunk\"
        done
      "))

      # Only accession in the SelectedSequences
      excluded_parts_file <- "SelectedSequences.txt"
      system(paste0("wsl sed -n 's/^>\\([^_]*\\).*/\\1/p' ",
                    ncbi_database_based_on_gbif, 
                    " | paste -sd '|' - | sed 's/$/|/' > ", 
                    excluded_parts_file))

      # Only accession in the headers
      system(paste0("wsl sed -i 's/^>\\([^_]*\\)_.*/>\\1/' ", ncbi_database_based_on_gbif))
    }

  } else {
    # -------------- (3) NO gbif_database PROVIDED --------------
    excluded_parts_file <- "SelectedSequences.txt"

    # Extract and save accession numbers
    system(paste0("wsl sed -n 's/^>\\([^ ]*\\).*/\\1/p' ",
              cleaned_ncbi_database, 
              " | paste -sd '|' - | sed 's/$/|/' > ", 
              excluded_parts_file))
    
    # Accession number in the headers
    system(paste0(
      "wsl sed 's/^>\\([^_]*\\)_.*/>\\1/' ",
      cleaned_ncbi_database,
      " > ",
      ncbi_database_based_on_gbif
    ))
  }
}
