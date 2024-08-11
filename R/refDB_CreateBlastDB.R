#' Create a BLAST Database
#'
#' This function creates a BLAST database using the `makeblastdb` command from NCBI BLAST+.
#'
#' @param database Path to the input database file.
#' @param parse_seqids Logical indicating whether to parse sequence IDs. Default is TRUE.
#' @param database_type Type of the database: "nucl" for nucleotide, "prot" for protein. Default is "nucl".
#' @param title Title for the database. Default is "local_database".
#' @param out Name of the output database files. Default is NULL.
#' @param hash_index Logical indicating whether to create a hash index. Default is FALSE.
#' @param mask_data Path to the mask data file. Default is NULL.
#' @param mask_id ID of the mask. Default is NULL.
#' @param mask_desc Description of the mask. Default is NULL.
#' @param gi_mask Logical indicating whether to use GI mask. Default is FALSE.
#' @param gi_mask_name Name of the GI mask. Default is NULL.
#' @param max_file_sz Maximum file size. Default is NULL.
#' @param logfile Path to the logfile. Default is NULL.
#' @param taxid Taxonomic ID. Default is NULL.
#' @param taxid_map Path to the taxonomic ID map file. Default is NULL.
#' @return A BLAST database created from the input database file.
#' @examples{
#' \dontrun{
#' # Example usage of refDB_CreateBlastDB
#' database <- "path/to/database.fasta"
#' out <- ""Brazil_Chordata_Database""
#' refDB_CreateBlastDB(database, out = out, title = "My Local Database")
#' )}
#' }
#' @import readr
#' @export
refDB_CreateBlastDB <- function(database, parse_seqids = TRUE, database_type = "nucl", title = "local_database", out = NULL, hash_index = FALSE, mask_data = NULL,  mask_id = NULL, mask_desc = NULL, gi_mask = FALSE,
                                 gi_mask_name = NULL, max_file_sz = NULL, logfile = refDB.log, taxid = NULL, taxid_map = NULL) {
  # Ensure that the 'out' parameter is set
  if (is.null(out)) {
    stop("The 'out' parameter must be specified.")
  }
  
  # Clear the log file if it already exists
  if (!is.null(logfile) && file.exists(logfile)) {
    writeLines(character(), con = logfile)  # This clears the log file
  }
  
  # Build the command string
  command <- paste0(
    "wsl makeblastdb -in ", shQuote(database),
    if (parse_seqids) " -parse_seqids" else "",
    " -title ", shQuote(title),
    " -dbtype ", shQuote(database_type),
    " -out ", shQuote(out),
    if (hash_index) " -hash_index" else "",
    if (!is.null(mask_data) && mask_data != "") paste0(" -mask_data ", shQuote(mask_data)) else "",
    if (!is.null(mask_id) && mask_id != "") paste0(" -mask_id ", shQuote(mask_id)) else "",
    if (!is.null(mask_desc) && mask_desc != "") paste0(" -mask_desc ", shQuote(mask_desc)) else "",
    if (gi_mask) " -gi_mask" else "",
    if (!is.null(gi_mask_name) && gi_mask_name != "") paste0(" -gi_mask_name ", shQuote(gi_mask_name)) else "",
    if (!is.null(max_file_sz) && max_file_sz != "") paste0(" -max_file_sz ", shQuote(max_file_sz)) else "",
    if (!is.null(logfile) && logfile != "") paste0(" -logfile ", shQuote(logfile)) else "",
    if (!is.null(taxid) && taxid != "") paste0(" -taxid ", shQuote(taxid)) else "",
    if (!is.null(taxid_map) && taxid_map != "") paste0(" -taxid_map ", shQuote(taxid_map)) else ""
  )
  
  # Run the command and capture the output
  system(command, intern = FALSE)
  
  # Process the log file to preserve critical information and remove unnecessary blank lines
  if (!is.null(logfile) && file.exists(logfile)) {
    # Read the content of the log file
    log_content <- readLines(logfile)
    
    # Ensure critical lines are preserved and remove only unnecessary blank lines
    critical_lines <- c("Building a new DB", "New DB name:", "New DB title:", "Sequence type:", "Adding sequences from FASTA;")
    cleaned_log_content <- log_content[log_content != "" | grepl(paste(critical_lines, collapse = "|"), log_content)]
    
    # Modify the line starting with "New DB name:"
    cleaned_log_content <- sapply(cleaned_log_content, function(line) {
      if (grepl("^New DB name:", line)) {
        # Remove content after the first ":" and before the last "/"
        return(sub("^New DB name:.*\\/", "New DB name:", line))
      } else {
        return(line)
      }
    })
    
    # Exclude the line starting with "Deleted existing Nucleotide"
    cleaned_log_content <- cleaned_log_content[!grepl("^Deleted existing Nucleotide", cleaned_log_content)]
    
    # Capture the parameters used and append to the log
    params_used <- paste(
      "Parameters used:",
      paste0("database = ", database),
      paste0("parse_seqids = ", parse_seqids),
      paste0("database_type = ", database_type),
      paste0("title = ", title),
      paste0("out = ", out),
      paste0("hash_index = ", hash_index),
      paste0("mask_data = ", ifelse(is.null(mask_data), "NULL", mask_data)),
      paste0("mask_id = ", ifelse(is.null(mask_id), "NULL", mask_id)),
      paste0("mask_desc = ", ifelse(is.null(mask_desc), "NULL", mask_desc)),
      paste0("gi_mask = ", gi_mask),
      paste0("gi_mask_name = ", ifelse(is.null(gi_mask_name), "NULL", gi_mask_name)),
      paste0("max_file_sz = ", ifelse(is.null(max_file_sz), "NULL", max_file_sz)),
      paste0("logfile = ", ifelse(is.null(logfile), "NULL", logfile)),
      paste0("taxid = ", ifelse(is.null(taxid), "NULL", taxid)),
      paste0("taxid_map = ", ifelse(is.null(taxid_map), "NULL", taxid_map)),
      sep = "\n"
    )
    
    # Append the parameters to the cleaned log content
    cleaned_log_content <- c(cleaned_log_content, params_used)
    
    # Append the selected sequences from the SelectedSequences.txt file
    if (file.exists("SelectedSequences.txt")) {
      selected_sequences <- readLines("SelectedSequences.txt")
      cleaned_log_content <- c(cleaned_log_content, paste0("Sequences:\n", paste(selected_sequences, collapse = "")))
    }
    
    # Write the cleaned content back to the log file
    writeLines(cleaned_log_content, con = logfile)
    
    # Remove all .perf files if they exist
    perf_files <- list.files(pattern = "\\.perf$")
    if (length(perf_files) > 0) {
      file.remove(perf_files)
    }
    
  }
}
