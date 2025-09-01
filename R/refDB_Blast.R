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
#' @importFrom dplyr group_by top_n rename distinct filter mutate select ungroup case_when left_join
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
  
  linux_path <- gsub("C:/", "/mnt/c/", Directory)
  
  # Optional Parameters
  optional_params <- ""
  add_param <- function(param, value) {
    if (!is.null(value)) {
      return(paste0(" -", param, " ", value))
    }
    return("")
  }
  
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
  
  # blastn
query_path <- file.path(linux_path, query)
db_path    <- file.path(linux_path, Database_File)

system(paste0("wsl blastn -query ", query_path, 
              " -task ", task, 
              " -db ", db_path,
              " -out ", out, 
              " -max_target_seqs ", max_target_seqs, 
              " -perc_identity ", perc_identity, 
              " -qcov_hsp_perc ", qcov_hsp_perc, 
              " -num_threads ", num_threads, 
              optional_params))
  
  #system(paste0("wsl blastn -query ", paste0(linux_path, query), 
  #              " -task ", task, 
  #              " -db ", paste0(linux_path, Database_File), 
  #              " -out ", out, 
  #              " -outfmt 6",  # Set output format to tabular
  #              " -max_target_seqs 50", 
  #              " -perc_identity 95", 
  #              " -qcov_hsp_perc 95", 
  #              " -num_threads 6"))
  
  system(paste0("wsl tr -d '#' < ", otu_table, " > temp_raw_database && mv temp_raw_database ", otu_table))
  
  csv1 <- read.table(paste0(Directory, out), sep = "", header = FALSE)
  
  Samples <- read.table(paste0(Directory, otu_table), sep = "\t", header = TRUE)
  
  fasta_seq <- Biostrings::readDNAStringSet(paste0(Directory, query))
  
  ########## Files = csv + ID taxa ##########
  colnames(csv1) <- c("qseqid", "seqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
  
  library(dplyr)
  
  # Separate only the max value of match per ZOTU
  cl.max_ <- csv1 %>% group_by(qseqid) %>% slice_max(pident, n = 1, with_ties = FALSE)
  
  library(rentrez)
  library(taxizedb)
  
  # Validação do NCBI
  db_download_ncbi()
  src <- src_ncbi()
  
  get_taxid <- function(accession) {
  summary <- entrez_summary(db = "nuccore", id = accession)
  return(summary$taxid)}
  
  accession_numbers <- cl.max_$seqid

  # Aplicar a função a cada accession number
  taxids <- sapply(accession_numbers, get_taxid)
  
  #accession_numbers <- taxizedb::classification(accession_numbers, db = "ncbi")
  
  #taxids <- sapply(accession_numbers[1:5], get_taxid)
  #taxids
  taxids <- taxizedb::classification(taxids, db = "ncbi")
  taxids_ <- taxids
  
  taxa_names <- taxids
  
  accession_id <- data.frame(Acession = accession_numbers, tax_ids = names(taxa_names))
  
  # To test if we have a dataframe after extract each ID
  is_dataframe <- function(obj) {
    inherits(obj, "data.frame")
  }
  
  #If a df is not generated, we create an empty one (to standardize the "main" output object)
  for (i in seq_along(taxa_names)) {
    if (!is_dataframe(taxa_names[[i]])) {
      taxa_names[[i]] <- data.frame(name = NA, rank = NA, id = NA)
    }
  }
  
  # Here we unify all dfs. Each one has a ASV/OTU name associated (for further validations)
  taxa_names_df <- do.call(rbind, lapply(names(taxa_names), function(name) {
    df <- taxa_names[[name]]
    df$taxid <- name
    return(df)
  }))
  
  taxa_names_df <- taxa_names_df[, c("taxid", names(taxa_names_df)[1:(ncol(taxa_names_df) - 1)])]
  taxa_names_df <- taxa_names_df[, -ncol(taxa_names_df)]
  
  taxa_names_df <- taxa_names_df %>%
    filter(rank %in% c("kingdom", "phylum", "class", "order", "family", "genus", "species", NA)) %>%
    group_by(taxid) %>%
    distinct()
  
  identified_otus <- taxa_names_df %>%
  tidyr::pivot_wider(names_from = rank, values_from = name)

  csv1_merged <- cl.max_ %>%
  left_join(accession_id, by = c("seqid" = "Acession"), relationship ="many-to-many") %>%
  relocate(tax_ids, .after = 2) %>%  
  distinct()           
  
  #write.table(csv1_merged, file = paste0("identified_blast.txt"), sep = "\t", row.names = F, col.names = T)
  
# Passo 1: Fazer o left_join mantendo a estrutura de csv1_merged
csv1_merged <- csv1_merged %>%
  left_join(
    identified_otus %>% select(taxid, 2:8),  
    by = c("tax_ids" = "taxid")             
  ) %>% relocate(names(identified_otus)[2:8], .after = 3) %>% 
  select(1:12) 
  
  ########## File = Samples ##########
  
  colnames(Samples)[1] <- "qseqid"
  
  Samples <- Samples %>%
  inner_join(csv1_merged %>% distinct(qseqid), by = "qseqid")
  
  #csv1_merged <- csv1_merged[order(match(csv1_merged$qseqid, Samples$qseqid)), ]
  
  Samples <- Samples[order(match(Samples$qseqid, csv1_merged$qseqid)), ]
  
  Samples <- Samples %>%
  left_join(
    csv1_merged %>% select(qseqid, 2:12),  
    by = c("qseqid" = "qseqid")             
  ) %>% 
  relocate(names(csv1_merged)[2:12], .after = 1)
  
  raw_otus <- data.frame(
  qseqid = names(fasta_seq),
  Fasta = as.character(fasta_seq),
  stringsAsFactors = FALSE)
  
  Blast <- merge(Samples, raw_otus, by = "qseqid")
  
  Blast <- as.data.frame(Blast, stringsAsFactors = FALSE)

  write.table(Blast, file = paste0(Directory, "taxonomic_assignment.txt"), sep = "\t", row.names = F, col.names = T)
    
  print("End of Run")
}