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
  
  file.create(ncbi_database_based_on_gbif, showWarnings = FALSE)
  fileConn <- file(ncbi_database_based_on_gbif, open = "wt")
  close(fileConn)
  
  if (genus_flexibility) {
    # If genus_flexibility is TRUE:
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

      # Function to search and append sequences with exact matching based on the first part before the underscore
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
                  header = substr($0, 2)  # Remove the initial >
                  split(header, parts, \"_\")
                  for (p in pat) {
                      if (parts[1] == pat[p]) {
                          counts[parts[1]]++
                          print \">\" counts[parts[1]] \"_\" header
                          getline
                          print
                          break
                      }
                  }
              }' ", cleaned_ncbi_database ," | replace_spaces_with_underscores >> ", ncbi_database_based_on_gbif ,"
      }

      # Split names into chunks of 1000 names each
      chunk_size=1000
      IFS='|' read -ra chunks <<< \"$(split_names_into_chunks $chunk_size)\"

      # Search and append sequences for each chunk
      for chunk in \"${chunks[@]}\"
      do
          search_and_append \"$chunk\"
      done
    "))
    
  } else {
    # if FALSE:
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
          sed -n -E '/^>('\"$chunk\"')$/ {:a;N;/^>/!ba;s/^>[[:space:]]*/>/p}' ", cleaned_ncbi_database ," | awk '/^>/ {sub(/>/, \">\" ++c \"_\")} 1' >> ", ncbi_database_based_on_gbif ,"
      }

      # Replace spaces with underscores in names
      modified_names=$(echo \"$names\" | sed 's/[[:space:]]/_/g')

      # Split modified names into chunks of 1000 names each
      chunk_size=1000
      IFS='|' read -ra chunks <<< \"$(split_names_into_chunks $chunk_size)\"

      # Search and append sequences for each chunk
      for chunk in \"${chunks[@]}\"
      do
          c=0 # Reset counter for each chunk
          search_and_append \"$chunk\"
      done
    "))
  }
  
  # Add the sed command to remove trailing underscores
  system(paste0("sed -i '/^>.*_$/s/_$//' ", ncbi_database_based_on_gbif))
}
