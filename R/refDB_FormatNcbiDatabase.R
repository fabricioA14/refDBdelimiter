#' Format NCBI Database
#'
#' This function formats an NCBI database by removing sequences below a specified minimum length, special characters from all the fasta headers and UNVERIFIED fasta sequences.
#'
#' @param raw_database Path to the raw NCBI database file.
#' @param database_cleaned Path to the cleaned NCBI database file.
#' @param min_sequence_length Minimum length for sequences to be retained in the cleaned database.
#' @param pattern Pattern to filter sequences in the raw database (default is "UNVERIFIED").
#' @return A cleaned and formatted NCBI database file.
#' @examples{
#' \dontrun{
#' # Example usage of refDB_FormatNcbiDatabase
#' raw_database <- "path/to/raw_database.fasta"
#' database_cleaned <- "path/to/database_cleaned.fasta"
#' min_sequence_length <- 200
#' pattern <- "UNVERIFIED"
#'
#' refDB_FormatNcbiDatabase(raw_database, database_cleaned, min_sequence_length, pattern)
#' )}
#' }
#' @import readr
#' @export
refDB_FormatNcbiDatabase <- function(raw_database, database_cleaned, min_sequence_length, pattern = "") {
  
  # Filter sequences based on the given pattern
  if (nzchar(pattern)) {
    filter_command <- paste0(
      "wsl awk",
      " -v pattern='" , pattern, "'",
      " -v output_file='" , database_cleaned, "'",
      " '/^>/ { if ($0 !~ pattern) { if (header) { print header; print sequence } header = $0; sequence = \"\" } next } { sequence = sequence $0 } END { if (header) { print header; print sequence } }' ",
      raw_database,
      ">",
      database_cleaned
    )
  } else {
    filter_command <- paste0("wsl cp ", raw_database, " ", database_cleaned)
  }
  
  system(filter_command)
  
  # Remove sequences below the minimum length
  command <- paste0('wsl temp_database_cleaned=$(mktemp)', 
                    ' && awk -v min_length=', min_sequence_length, 
                    ' -v RS=">" -v ORS="" \'{',
                    ' if (NR > 1) {',
                    '     header = ">" substr($0, 1, index($0, "\\n"));',
                    '     sequence = substr($0, index($0, "\\n")+1);',
                    '     gsub(/\\n/, "", sequence);',
                    '     if (length(sequence) >= min_length) {',
                    '         print header;',
                    '         print sequence;',
                    '         print "\\n";',
                    '     }',
                    ' }',
                    '}\' "', database_cleaned, '" > $temp_database_cleaned && ',
                    'mv $temp_database_cleaned "', database_cleaned, '"')
  system(command)
  
  # Remove special characters
  system(paste0("wsl tr -d '[]\"' < ", database_cleaned, " > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned))
  
  # Select specific taxonomy labels
  system(paste0("wsl awk '/^>/ {
      if ($4 == \"aff\" || $4 == \"aff.\" || $4 == \"cf\" || $4 == \"cf.\" || $4 == \"cv\" || $4 == \"cv.\" || $4 == \"f\" || $4 == \"f.\" || $4 == \"ined\" || $4 == \"ined.\" || $4 == \"p\" || $4 == \"p.\" || $4 == \"sect\" || $4 == \"sect.\" || $4 == \"lat\" || $4 == \"lat.\" || $4 == \"str\" || $4 == \"str.\" || $4 == \"nov\" || $4 == \"nov.\" || $4 == \"syn\" || $4 == \"syn.\" || $4 == \"var\" || $4 == \"var.\") {
          print $1 \" \" $2 \" \" $3 \" \" $4 \" \"
      }  else {
          print $1 \" \" $2 \" \" $3 \" \"
      }
  } 
  !/^>/ {print}' ", database_cleaned ," > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned))
  
  system(paste0(
    "wsl awk '",
    "/^>/ { ",
    "gsub(/ aff\\./, \"\"); gsub(/ cf\\./, \"\"); gsub(/ cv\\./, \"\"); gsub(/ f\\./, \"\"); gsub(/ ined\\./, \"\"); gsub(/ p\\./, \"\"); gsub(/ sect\\./, \"\"); gsub(/ lat\\./, \"\"); gsub(/ str\\./, \"\"); gsub(/ nov\\./, \"\"); gsub(/ syn\\./, \"\"); gsub(/ var\\./, \"\"); gsub(/ sp\\./, \"\"); ",
    "gsub(/ aff /, \"\"); gsub(/ cf /, \"\"); gsub(/ cv /, \"\"); gsub(/ f /, \"\"); gsub(/ ined /, \"\"); gsub(/ p /, \"\"); gsub(/ sect /, \"\"); gsub(/ lat /, \"\"); gsub(/ str /, \"\"); gsub(/ nov /, \"\"); gsub(/ syn /, \"\"); gsub(/ var /, \"\"); gsub(/ sp /, \"\"); ",
    "} { print }' ", database_cleaned, " > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned
  ))
  
  system(paste0("sed -i '/^>/ s/[[:space:]]/_/g' ", database_cleaned))
  
  system(paste0("sed -i '/^>.*_$/s/_$//' ", database_cleaned))
  
}