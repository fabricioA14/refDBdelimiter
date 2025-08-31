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

        # Function to replace spaces with underscores except the last one
        replace_spaces_with_underscores() {
            sed 's/ /_/g'
        }

        # Function to search and append sequences with exact matching
        # based on the part after the first underscore
        search_and_append() {
            local chunk=\"$1\"
            awk -v pattern=\"$chunk\" '
                BEGIN {
                    split(pattern, pat, \"|\")
                    for (p in pat) {
                        counts[pat[p]] = 0
                    }
                }
                /^>/ {
                    header = substr($0, 2)  # Remove >
                    split(header, parts, \"_\")
                    if (parts[2] == pat[1] || parts[1] == pat[1]) {
                        counts[pat[1]]++
                        print \">\" counts[pat[1]] \"_\" header
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

      # Final cleanup steps for genus_flexibility == TRUE
      system(paste0("wsl sed -i '/^>.*_$/s/_$//' ", ncbi_database_based_on_gbif))
      excluded_parts_file <- "SelectedSequences.txt"
      system(paste0("wsl sed -n 's/^[^_]*_\\([^_]*\\)_.*/\\1/p' ",
                    ncbi_database_based_on_gbif, 
                    " | paste -sd '|' - | sed 's/$/|/' > ", 
                    excluded_parts_file))
      system(paste0("wsl sed -i 's/_[^_]*_/_/' ", ncbi_database_based_on_gbif))

    } else {
      # -------------- (2) GENUS_FLEXIBILITY == FALSE CODE --------------
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

        # Function to search and append sequences
        search_and_append() {
            local chunk=\"$1\"
            awk '/^>/{f=0} 
                 /^>.*'\"$chunk\"'/{f=1} 
                 f' ", cleaned_ncbi_database ," >> ", ncbi_database_based_on_gbif ,"
        }

        modified_names=$(echo \"$names\" | sed 's/[[:space:]]/_/g')

        # Split modified names into chunks of 1000
        chunk_size=1000
        IFS='|' read -ra chunks <<< \"$(split_names_into_chunks $chunk_size)\"

        # Search and append sequences for each chunk
        for chunk in \"${chunks[@]}\"
        do
            c=0
            search_and_append \"$chunk\"
        done
      "))

      # Final cleanup steps for genus_flexibility == FALSE
      system(paste0("wsl sed -i '/^>.*_$/s/_$//' ", ncbi_database_based_on_gbif))
      excluded_parts_file <- "SelectedSequences.txt"
      system(paste0("wsl sed -n 's/^>\\([^_]*\\)_.*/\\1/p' ",
                    ncbi_database_based_on_gbif, 
                    " | paste -sd '|' - | sed 's/$/|/' > ", 
                    excluded_parts_file))
      system(paste0("wsl sed -i 's/^>[^_]*_\\([^_]*_[^_]*\\).*/>\\1/' ", ncbi_database_based_on_gbif))

      # Optional final pass to rename duplicates
      system(paste0("
        wsl awk '
          /^>/ {
            name = substr($0, 2)
            if (!seen[name]++) {sequence[name]=1}
            print \">\" sequence[name] \"_\" name
            sequence[name]++
          } 
          !/^>/ {print $0}
        ' ", ncbi_database_based_on_gbif, " > temp_file && mv temp_file ", ncbi_database_based_on_gbif))
    }

  } else {
    # -------------- (3) NO gbif_database PROVIDED --------------
    # Run only your final commands (or any minimal editing you desire)
    # For example, if you only want to do the cleanup steps on 
    # the existing 'ncbi_database_based_on_gbif' file without using gbif_database:

    #system(paste0("wsl sed -i '/^>.*_$/s/_$//' ", cleaned_ncbi_database))
    
    # Path to the file where you want to save the excluded parts
    excluded_parts_file <- "SelectedSequences.txt"

    # Extract and save the excluded parts
    system(paste0("wsl sed -n 's/^>\\([^_]*\\)_.*/\\1/p' ",
              cleaned_ncbi_database, 
              " | paste -sd '|' - | sed 's/$/|/' > ", 
              excluded_parts_file))
    
    #
    #system(paste0(
    #"wsl sed 's/^>\\([^_]*\\)_.*/>\\1/' ",
    #cleaned_ncbi_database, 
    #" > ",
    #ncbi_database_based_on_gbif
    #))

    system(paste0(
    "wsl sed 's/^>\\(.*[0-9]\\)[^_]*_.*$/>\\1/' ",
    cleaned_ncbi_database,
    " > ",
    ncbi_database_based_on_gbif
    ))
    

    #Remove the first underscore and everything before the second underscore
    #system(paste0("wsl sed -i 's/_[^_]*_/_/' ", cleaned_ncbi_database))
  }
}
