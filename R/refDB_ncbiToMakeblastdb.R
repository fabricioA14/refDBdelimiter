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