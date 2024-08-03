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
refDB_CreateBlastDB <- function(database, parse_seqids = TRUE, database_type = "nucl", title = "local_database", out = NULL, hash_index = FALSE, mask_data = NULL, mask_id = NULL, mask_desc = NULL, gi_mask = FALSE,
                                gi_mask_name = NULL, max_file_sz = NULL, logfile = NULL, taxid = NULL, taxid_map = NULL) {
  system(paste0("wsl makeblastdb -in ", database, 
                if (parse_seqids) " -parse_seqids" else "",
                " -title ", title, 
                " -dbtype ", database_type, 
                if (!is.null(out)) paste0(" -out ", out) else "",
                if (hash_index) " -hash_index" else "",
                if (!is.null(mask_data) && mask_data != "") paste0(" -mask_data ", mask_data) else "",
                if (!is.null(mask_id) && mask_id != "") paste0(" -mask_id ", mask_id) else "",
                if (!is.null(mask_desc) && mask_desc != "") paste0(" -mask_desc ", mask_desc) else "",
                if (gi_mask) " -gi_mask" else "",
                if (!is.null(gi_mask_name) && gi_mask_name != "") paste0(" -gi_mask_name ", gi_mask_name) else "",
                if (!is.null(max_file_sz) && max_file_sz != "") paste0(" -max_file_sz ", max_file_sz) else "",
                if (!is.null(logfile) && logfile != "") paste0(" -logfile ", logfile) else "",
                if (!is.null(taxid) && taxid != "") paste0(" -taxid ", taxid) else "",
                if (!is.null(taxid_map) && taxid_map != "") paste0(" -taxid_map ", taxid_map) else ""))
}
