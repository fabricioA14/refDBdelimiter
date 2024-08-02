#' Create BLAST Database
#'
#' This function creates a BLAST database from a given database file.
#'
#' @param database Path to the input database file.
#' @param parse_seqids Boolean indicating whether to parse SeqIDs (default: TRUE).
#' @param database_type Type of the database (default: "nucl").
#' @param title Title for the BLAST database.
#' @param out Name of the output BLAST database file.
#' @param hash_index Boolean indicating whether to create a hash index (default: FALSE).
#' @param mask_data Path to mask data (optional).
#' @param mask_id Mask ID (optional).
#' @param mask_desc Mask description (optional).
#' @param gi_mask Boolean indicating whether to use GI mask (default: FALSE).
#' @param gi_mask_name Name of the GI mask (optional).
#' @param max_file_sz Maximum file size (optional).
#' @param logfile Path to the log file (optional).
#' @param taxid TaxID (optional).
#' @param taxid_map Path to the TaxID map file (optional).
#' @return None.
#' @examples
#' refDB_CreateBlastDB("Chordata_Ncbi_Gbif.fasta", TRUE, "nucl", "local_database", "blast_db")
#' @export
refDB_CreateBlastDB <- function(database, parse_seqids = TRUE, database_type = "nucl", title = "local_database", out = NULL, hash_index = FALSE, mask_data = NULL, mask_id = NULL, mask_desc = NULL, gi_mask = FALSE, gi_mask_name = NULL, max_file_sz = NULL, logfile = NULL, taxid = NULL, taxid_map = NULL) {
  system(paste0("wsl makeblastdb -in " , database, " ", if (parse_seqids) paste0("-parse_seqids"), " -title ", title, " -dbtype ", database_type, " -out ", out, if (hash_index) "-hash_index ",
                if (!is.null(mask_data) && mask_data != "") paste0("-mask_data ", mask_data, " "), if (!is.null(mask_id) && mask_id != "") paste0("-mask_id ", mask_id, " "),
                if (!is.null(mask_desc) && mask_desc != "") paste0("-mask_desc ", mask_desc, " "), if (gi_mask) "-gi_mask ", if (!is.null(gi_mask_name) && gi_mask_name != "") paste0("-gi_mask_name ", gi_mask_name, " "),
                if (!is.null(max_file_sz) && max_file_sz != "") paste0("-max_file_sz ", max_file_sz, " "), if (!is.null(logfile) && logfile != "") paste0("-logfile ", logfile, " "),
                if (!is.null(taxid) && taxid != "") paste0("-taxid ", taxid, " "), if (!is.null(taxid_map) && taxid_map != "") paste0("-taxid_map ", taxid_map, " ")))
}