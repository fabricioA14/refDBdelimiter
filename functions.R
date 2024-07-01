
cleaning_ncbi_database <- function(raw_database, min_sequence_length) {
  
  # Create the output filename by appending "_cleaned" to the input filename
  database_cleaned <- sub("(.*)\\..*", "\\1_cleaned.fasta", raw_database)
  
  pattern <- "UNVERIFIED"
  
  filter_command <- paste0(
    "wsl awk",
    " -v pattern='" , pattern, "'",
    " -v output_file='" , database_cleaned, "'",
    " '/^>/ { if ($0 !~ pattern) { if (header) { print header; print sequence } header = $0; sequence = \"\" } next } { sequence = sequence $0 } END { if (header) { print header; print sequence } }' ",
    raw_database,
    ">",
    database_cleaned
  )
  
  system(filter_command)
  
  awk_command <- paste0("wsl awk '/^>/ {print substr($0, 1, 50); next} 1' ", database_cleaned, " > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned)
  
  system(awk_command)
  
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
  
  system(paste0("wsl tr -d '[]\"' < ", database_cleaned, " > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned))
}