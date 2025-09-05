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
refDB_SubsetNcbiGbif_ <- function(gbif_database,
                                  cleaned_ncbi_database,
                                  ncbi_database_based_on_gbif,
                                  genus_flexibility) {
  
  # Always create an empty output file to start
  file.create(ncbi_database_based_on_gbif, showWarnings = FALSE)
  fileConn <- file(ncbi_database_based_on_gbif, open = "wt")
  close(fileConn)
  
  # Helper: write SelectedSequences.txt from FASTA
  write_selected_sequences <- function(fasta_file, output_file = "SelectedSequences.txt") {
    system(paste0(
      "wsl sed -n 's/^>\\(.*\\)/\\1/p' ",
      fasta_file,
      " | paste -sd '|' - | sed 's/$/|/' > ",
      output_file
    ))
  }
  
  if (!is.null(gbif_database) && file.exists(gbif_database)) {
    
    # Read GBIF names in chunks
    chunk_code <- "
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
    "
    
    if (genus_flexibility) {
      # GENUS_FLEXIBILITY TRUE
      search_code <- paste0("
        wsl names=$(<", gbif_database, ")
        ", chunk_code, "
        
        search_and_append() {
          local chunk=\"$1\"
          awk -v pattern=\"$chunk\" '
            BEGIN { split(pattern, pat, \"|\") }
            /^>/ {
              header = substr($0, 2)
              split(header, parts, \"-\")      # <- Use '-' as separator
              if (parts[2] == pat[1] || parts[1] == pat[1]) {
                print \">\" parts[1]          # Only accession after selection
                getline
                print
              }
            }' ", cleaned_ncbi_database, " >> ", ncbi_database_based_on_gbif, "
        }
        
        chunk_size=1000
        IFS='|' read -ra chunks <<< \"$(split_names_into_chunks $chunk_size)\"
        for chunk in \"${chunks[@]}\"
        do
          search_and_append \"$chunk\"
        done
      ")
      system(search_code)
      
} else {
  # GENUS_FLEXIBILITY FALSE
  # Convert underscores to dashes in GBIF dataset at the start
  #temp_gbif <- tempfile(fileext = ".txt")
  #system(paste0("wsl sed 's/_/-/g' ", gbif_database, " > ", temp_gbif))
  #gbif_database <- temp_gbif
  
  formatted_gbif <- "FormattedGbifSpecies.txt"
  system(paste0("wsl sed 's/_/-/g' ", gbif_database, " > ", formatted_gbif))
  gbif_database <- formatted_gbif
  on.exit(unlink(formatted_gbif), add = TRUE)
  
  search_code <- paste0("
    wsl awk -v RS='>' -v ORS='' '
      NR==FNR {
        # Read GBIF species list (split by |)
        n = split($0, gbif_species, \"|\")
        next
      }
      NR>FNR {
        split($0, lines, \"\\n\")
        header = lines[1]
        sequence = \"\"
        for (i=2; i<=length(lines); i++) sequence = sequence lines[i]
        gsub(/\\n/, \"\", sequence)
        
        # Extract genus-species after first dash
        dash = index(header, \"-\")
        if (dash > 0) {
          header_species = substr(header, dash+1)
        } else {
          header_species = header
        }

        # Compare with all GBIF species
        for (i=1; i<=n; i++) {
          if (header_species == gbif_species[i]) {
            print \">\" header \"\\n\" sequence \"\\n\"
            break
          }
        }
      }
    ' ", gbif_database, " ", cleaned_ncbi_database, " > ", ncbi_database_based_on_gbif
  )
  system(search_code)}
    
    # Truncate FASTA headers at first '-' (only accession)
    system(paste0("wsl sed -i 's/^>\\([^\\-]*\\).*/>\\1/' ", ncbi_database_based_on_gbif))
    
    # Write SelectedSequences.txt with '|' separator
    write_selected_sequences(ncbi_database_based_on_gbif)
    
  } else {
    # No GBIF provided: use cleaned NCBI directly
    file.copy(cleaned_ncbi_database, ncbi_database_based_on_gbif, overwrite = TRUE)
    
    # Truncate FASTA headers at first '-' (only accession)
    system(paste0("wsl sed -i 's/^>\\([^\\-]*\\).*/>\\1/' ", ncbi_database_based_on_gbif))
    
    # Write SelectedSequences.txt with '|' separator
    write_selected_sequences(ncbi_database_based_on_gbif)
  }
  
}
