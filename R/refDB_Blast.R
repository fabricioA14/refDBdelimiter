#' Run BLAST and Process Results
#'
#' This function runs a BLAST search and processes the results to assign taxonomic information to OTUs.
#'
#' @param Directory The directory where the BLAST database and query files are located.
#' @param Database_File The BLAST database file.
#' @param otu_table The OTU table file. Default is "otu_table.txt".
#' @param query The query file. Default is "otus.fasta".
#' @param task The BLAST task. Default is "megablast".
#' @param out The output file. Default is "blast.txt".
#' @param max_target_seqs The maximum number of target sequences. Default is 50.
#' @param perc_identity The percentage identity threshold. Default is 95.
#' @param qcov_hsp_perc The query coverage HSP percentage. Default is 95.
#' @param num_threads The number of threads to use. Default is 6.
#' @param Specie_Threshold The species threshold. Default is 99.
#' @param Genus_Threshold The genus threshold. Default is 97.
#' @param Family_Threshold The family threshold. Default is 95.
#' @param penalty Optional BLAST parameter.
#' @param reward Optional BLAST parameter.
#' @param evalue Optional BLAST parameter.
#' @param word_size Optional BLAST parameter.
#' @param gapopen Optional BLAST parameter.
#' @param gapextend Optional BLAST parameter.
#' @param max_hsps Optional BLAST parameter.
#' @param xdrop_ungap Optional BLAST parameter.
#' @param xdrop_gap Optional BLAST parameter.
#' @param xdrop_gap_final Optional BLAST parameter.
#' @param searchsp Optional BLAST parameter.
#' @param sum_stats Optional BLAST parameter.
#' @param no_greedy Optional BLAST parameter.
#' @param min_raw_gapped_score Optional BLAST parameter.
#' @param template_type Optional BLAST parameter.
#' @param template_length Optional BLAST parameter.
#' @param dust Optional BLAST parameter.
#' @param filtering_db Optional BLAST parameter.
#' @param window_masker_taxid Optional BLAST parameter.
#' @param window_masker_db Optional BLAST parameter.
#' @param soft_masking Optional BLAST parameter.
#' @param ungapped Optional BLAST parameter.
#' @param culling_limit Optional BLAST parameter.
#' @param best_hit_overhang Optional BLAST parameter.
#' @param best_hit_score_edge Optional BLAST parameter.
#' @param subject_besthit Optional BLAST parameter.
#' @param window_size Optional BLAST parameter.
#' @param off_diagonal_range Optional BLAST parameter.
#' @param use_index Optional BLAST parameter.
#' @param index_name Optional BLAST parameter.
#' @param lcase_masking Optional BLAST parameter.
#' @param query_loc Optional BLAST parameter.
#' @param strand Optional BLAST parameter.
#' @param parse_deflines Optional BLAST parameter.
#' @param outfmt Output format. Default is "6".
#' @param show_gis Optional BLAST parameter.
#' @param num_descriptions Optional BLAST parameter.
#' @param num_alignments Optional BLAST parameter.
#' @param line_length Optional BLAST parameter.
#' @param html Optional BLAST parameter.
#' @param sorthits Optional BLAST parameter.
#' @param sorthsps Optional BLAST parameter.
#' @param mt_mode Optional BLAST parameter.
#' @param remote Optional BLAST parameter.
#' @return None
#' @examples{
#' \dontrun{
#' refDB_Blast(
#'   Directory = "/path/to/directory",
#'   Database_File = "database_file",
#'   otu_table = "otu_table.txt",
#'   query = "otus.fasta",
#'   out = "blast.txt"
#' )}
#' }
#' @import readr
#' @import Biostrings readDNAStringSet
#' @importFrom dplyr group_by top_n rename distinct filter mutate select ungroup case_when left_join slice_max relocate
#' @importFrom plyr match_df 
#' @importFrom readr read_table
#' @importFrom stats complete.cases
#' @importFrom stringr str_split
#' @importFrom tidyr pivot_wider
#' @importFrom utils read.table write.table
#' @importFrom rentrez entrez_summary
#' @importFrom taxizedb classification db_download_ncbi name2taxid src_ncbi
#' @export
refDB_Blast <- function(Directory, Database_File, otu_table = "otu_table.txt", query = "otus.fasta", task = "megablast", out = "blast.txt", 
                        max_target_seqs = 50, perc_identity = 95, qcov_hsp_perc = 95, num_threads = 6, 
                        Specie_Threshold = 99, Genus_Threshold = 97, Family_Threshold = 95, 
                        penalty = NULL, reward = NULL, evalue = NULL, word_size = NULL, gapopen = NULL, 
                        gapextend = NULL, max_hsps = NULL, xdrop_ungap = NULL, xdrop_gap = NULL, 
                        xdrop_gap_final = NULL, searchsp = NULL, sum_stats = NULL, no_greedy = NULL, 
                        min_raw_gapped_score = NULL, template_type = NULL, template_length = NULL, 
                        dust = NULL, filtering_db = NULL, window_masker_taxid = NULL, window_masker_db = NULL, 
                        soft_masking = NULL, ungapped = NULL, culling_limit = NULL, 
                        best_hit_overhang = NULL, best_hit_score_edge = NULL, subject_besthit = NULL, 
                        window_size = NULL, off_diagonal_range = NULL, use_index = NULL, index_name = NULL, 
                        lcase_masking = NULL, query_loc = NULL, strand = NULL, parse_deflines = NULL, 
                        outfmt = "6", show_gis = NULL, num_descriptions = NULL, num_alignments = NULL, 
                        line_length = NULL, html = NULL, sorthits = NULL, sorthsps = NULL, 
                        mt_mode = NULL, remote = NULL) {

  # Convert Windows directory to WSL/Linux-compatible paths
  linux_dir <- gsub("C:/", "/mnt/c/", Directory)
  wsl_dir <- gsub("C:\\\\", "/mnt/c/", gsub("\\\\", "/", Directory))
  windows_dir <- gsub("\\\\", "/", Directory)
  
  # Build optional BLAST parameters (only include non-NULL values)
  optional_params <- ""
  add_param <- function(param, value) {
    if (!is.null(value)) return(paste0(" -", param, " ", value))
    return("")
  }
  
  # Collect all optional parameters
  optional_params <- paste0(optional_params, 
                            add_param("penalty", penalty), 
                            add_param("reward", reward), 
                            add_param("evalue", evalue),
                            add_param("word_size", word_size),
                            add_param("gapopen", gapopen),
                            add_param("gapextend", gapextend),
                            add_param("max_hsps", max_hsps),
                            add_param("xdrop_ungap", xdrop_ungap),
                            add_param("xdrop_gap", xdrop_gap),
                            add_param("xdrop_gap_final", xdrop_gap_final),
                            add_param("searchsp", searchsp),
                            add_param("sum_stats", sum_stats),
                            add_param("no_greedy", no_greedy),
                            add_param("min_raw_gapped_score", min_raw_gapped_score),
                            add_param("template_type", template_type),
                            add_param("template_length", template_length),
                            add_param("dust", dust),
                            add_param("filtering_db", filtering_db),
                            add_param("window_masker_taxid", window_masker_taxid),
                            add_param("window_masker_db", window_masker_db),
                            add_param("soft_masking", soft_masking),
                            add_param("ungapped", ungapped),
                            add_param("culling_limit", culling_limit),
                            add_param("best_hit_overhang", best_hit_overhang),
                            add_param("best_hit_score_edge", best_hit_score_edge),
                            add_param("subject_besthit", subject_besthit),
                            add_param("window_size", window_size),
                            add_param("off_diagonal_range", off_diagonal_range),
                            add_param("use_index", use_index),
                            add_param("index_name", index_name),
                            add_param("lcase_masking", lcase_masking),
                            add_param("query_loc", query_loc),
                            add_param("strand", strand),
                            add_param("parse_deflines", parse_deflines),
                            add_param("outfmt", outfmt),
                            add_param("show_gis", show_gis),
                            add_param("num_descriptions", num_descriptions),
                            add_param("num_alignments", num_alignments),
                            add_param("line_length", line_length),
                            add_param("html", html),
                            add_param("sorthits", sorthits),
                            add_param("sorthsps", sorthsps),
                            add_param("mt_mode", mt_mode),
                            add_param("remote", remote)
  )
  
  # Define query and database paths for WSL
  query_file <- file.path(linux_dir, query)
  database_file <- file.path(linux_dir, Database_File)
  
  # Run BLASTn inside WSL
  system(paste0("wsl blastn -query ", query_file, 
                " -task ", task, 
                " -db ", database_file,
                " -out ", out, 
                " -max_target_seqs ", max_target_seqs, 
                " -perc_identity ", perc_identity, 
                " -qcov_hsp_perc ", qcov_hsp_perc, 
                " -num_threads ", num_threads, 
                optional_params))
  
  # Clean OTU table by removing '#' characters
  system(paste0("wsl tr -d '#' < ", otu_table, " > temp_raw_database && mv temp_raw_database ", otu_table))
  
  # Load BLAST results, OTU table, and query sequences
  blast_results <- read.table(file.path(windows_dir, out), sep = "", header = FALSE)
  sample_table <- read.table(file.path(windows_dir, otu_table), sep = "\t", header = TRUE)
  query_sequences <- Biostrings::readDNAStringSet(file.path(windows_dir, query))
  
  # Assign column names to BLAST output
  colnames(blast_results) <- c("qseqid", "seqid", "pident", "length", "mismatch", "gapopen", 
                               "qstart", "qend", "sstart", "send", "evalue", "bitscore")
  
  # Keep only the best hit (highest % identity) per query
  best_hits <- blast_results %>% group_by(qseqid) %>% dplyr::slice_max(pident, n = 1, with_ties = FALSE)
  
  # Retrieve NCBI taxonomy
  db_download_ncbi()
  src <- src_ncbi()
  
  get_taxid <- function(accession) {
    summary <- entrez_summary(db = "nuccore", id = accession)
    return(summary$taxid)
  }
  
  accession_numbers <- best_hits$seqid
  taxid_list <- sapply(accession_numbers, get_taxid)
  tax_classification <- taxizedb::classification(taxid_list, db = "ncbi")
  
  # Map accession numbers to tax IDs
  accession_tax_map <- data.frame(Acession = accession_numbers, tax_ids = names(tax_classification))
  
  # Ensure consistent data frame structure for missing results
  is_dataframe <- function(obj) inherits(obj, "data.frame")
  for (i in seq_along(tax_classification)) {
    if (!is_dataframe(tax_classification[[i]])) {
      tax_classification[[i]] <- data.frame(name = NA, rank = NA, id = NA)
    }
  }
  
  # Combine all taxonomic classifications into a single data frame
  taxa_long <- do.call(rbind, lapply(names(tax_classification), function(name) {
    df <- tax_classification[[name]]
    df$taxid <- name
    return(df)
  }))
  
  taxa_long <- taxa_long[, c("taxid", names(taxa_long)[1:(ncol(taxa_long) - 1)])]
  taxa_long <- taxa_long[, -ncol(taxa_long)]
  
  # Keep only standard taxonomic ranks
  taxa_long <- taxa_long %>%
    filter(rank %in% c("kingdom", "phylum", "class", "order", "family", "genus", "species", NA)) %>%
    group_by(taxid) %>%
    distinct()
  
  # Convert taxonomy to wide format
  taxonomy_wide <- taxa_long %>%
    tidyr::pivot_wider(names_from = rank, values_from = name)
  
  # Merge BLAST hits with taxonomy
  merged_hits <- best_hits %>%
    left_join(accession_tax_map, by = c("seqid" = "Acession"), relationship ="many-to-many") %>%
    dplyr::relocate(tax_ids, .after = 2) %>%  
    distinct() %>%
    left_join(
      taxonomy_wide %>% select(taxid, 2:8),  
      by = c("tax_ids" = "taxid")             
    ) %>% dplyr::relocate(names(taxonomy_wide)[2:8], .after = 3) %>% 
    select(1:12) 
  
  # Merge taxonomy with sample abundance table
  colnames(sample_table)[1] <- "qseqid"
  sample_table <- sample_table %>%
    dplyr::inner_join(merged_hits %>% distinct(qseqid), by = "qseqid") %>%
    dplyr::arrange(match(qseqid, merged_hits$qseqid)) %>%
    left_join(merged_hits %>% select(qseqid, 2:12), by = "qseqid") %>% 
    dplyr::relocate(names(merged_hits)[2:12], .after = 1)
  
  # Prepare raw sequences table
  raw_sequences <- data.frame(
    qseqid = names(query_sequences),
    Fasta = as.character(query_sequences),
    stringsAsFactors = FALSE
  )
  
  # Final merge: samples + taxonomy + sequences
  final_blast <- merge(sample_table, raw_sequences, by = "qseqid")
  final_blast <- as.data.frame(final_blast, stringsAsFactors = FALSE)
  
  # Apply identity thresholds to filter species/genus assignments
  final_blast$species <- ifelse(final_blast$pident >= Genus_Threshold & final_blast$pident < Specie_Threshold, "" ,final_blast$species)
  final_blast$species <- ifelse(final_blast$pident >= Family_Threshold & final_blast$pident < Genus_Threshold, "" ,final_blast$species)
  final_blast$genus <- ifelse(final_blast$pident >= Family_Threshold & final_blast$pident < Genus_Threshold, "" ,final_blast$genus)
  
  # Save final taxonomic assignment
  write.table(final_blast, file = file.path(windows_dir, "taxonomic_assignment.txt"), sep = "\t", row.names = F, col.names = T)
  
  print("End of Run")
}