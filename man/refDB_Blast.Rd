\name{refDB_Blast}
\alias{refDB_Blast}
\title{Run BLAST and Process the Results}
\description{
  This function runs a BLAST search and processes the results to assign taxonomic information to OTUs.
}
\usage{
refDB_Blast(Directory, Database_File, otu_table = "otu_table.txt", 
            query = "otus.fasta", task = "megablast", out = "blast.txt", 
            max_target_seqs = 50, perc_identity = 95, qcov_hsp_perc = 95, 
            num_threads = 6, Specie_Threshold = 99, Genus_Threshold = 97, 
            Family_Threshold = 95, penalty = NULL, reward = NULL, evalue = NULL, 
            word_size = NULL, gapopen = NULL, gapextend = NULL, max_hsps = NULL, 
            xdrop_ungap = NULL, xdrop_gap = NULL, xdrop_gap_final = NULL, 
            searchsp = NULL, sum_stats = NULL, no_greedy = NULL, 
            min_raw_gapped_score = NULL, template_type = NULL, 
            template_length = NULL, dust = NULL, filtering_db = NULL, 
            window_masker_taxid = NULL, window_masker_db = NULL, 
            soft_masking = NULL, ungapped = NULL, culling_limit = NULL, 
            best_hit_overhang = NULL, best_hit_score_edge = NULL, 
            subject_besthit = NULL, window_size = NULL, off_diagonal_range = NULL, 
            use_index = NULL, index_name = NULL, lcase_masking = NULL, 
            query_loc = NULL, strand = NULL, parse_deflines = NULL, outfmt = "6", 
            show_gis = NULL, num_descriptions = NULL, num_alignments = NULL, 
            line_length = NULL, html = NULL, sorthits = NULL, sorthsps = NULL, 
            mt_mode = NULL, remote = NULL)
}
\arguments{
  \item{Directory}{Directory where the files are located.}
  \item{Database_File}{Path to the BLAST database file.}
  \item{otu_table}{Path to the OTU table. Default is "otu_table.txt".}
  \item{query}{Path to the query file. Default is "otus.fasta".}
  \item{task}{BLAST task to perform. Default is "megablast".}
  \item{out}{Path to the output file. Default is "blast.txt".}
  \item{max_target_seqs}{Maximum number of aligned sequences to keep. Default is 50.}
  \item{perc_identity}{Percent identity threshold for BLAST. Default is 95.}
  \item{qcov_hsp_perc}{Query coverage per high-scoring pair. Default is 95.}
  \item{num_threads}{Number of threads to use. Default is 6.}
  \item{Specie_Threshold}{Threshold for species identification. Default is 99.}
  \item{Genus_Threshold}{Threshold for genus identification. Default is 97.}
  \item{Family_Threshold}{Threshold for family identification. Default is 95.}
  \item{penalty}{Penalty for a mismatch. Default is NULL.}
  \item{reward}{Reward for a match. Default is NULL.}
  \item{evalue}{Expectation value (E) threshold for saving hits. Default is NULL.}
  \item{word_size}{Word size for wordfinder algorithm. Default is NULL.}
  \item{gapopen}{Cost to open a gap. Default is NULL.}
  \item{gapextend}{Cost to extend a gap. Default is NULL.}
  \item{max_hsps}{Maximum number of HSPs (high-scoring pairs) to save for each query-subject pair. Default is NULL.}
  \item{xdrop_ungap}{X-dropoff value for ungapped extensions. Default is NULL.}
  \item{xdrop_gap}{X-dropoff value for gapped extensions. Default is NULL.}
  \item{xdrop_gap_final}{X-dropoff value for final gapped alignment. Default is NULL.}
  \item{searchsp}{Effective length of the search space. Default is NULL.}
  \item{sum_stats}{Show sum statistics. Default is NULL.}
  \item{no_greedy}{Use non-greedy dynamic programming algorithm. Default is NULL.}
  \item{min_raw_gapped_score}{Minimum raw gapped score to keep an alignment. Default is NULL.}
  \item{template_type}{Type of the template. Default is NULL.}
  \item{template_length}{Length of the template. Default is NULL.}
  \item{dust}{Dust filtering. Default is NULL.}
  \item{filtering_db}{Database to use for filtering. Default is NULL.}
  \item{window_masker_taxid}{Taxonomic ID to use with windowmasker. Default is NULL.}
  \item{window_masker_db}{Database to use with windowmasker. Default is NULL.}
  \item{soft_masking}{Enable soft masking. Default is NULL.}
  \item{ungapped}{Perform ungapped alignment only. Default is NULL.}
  \item{culling_limit}{Limit the number of HSPs. Default is NULL.}
  \item{best_hit_overhang}{Best hit overhang value. Default is NULL.}
  \item{best_hit_score_edge}{Best hit score edge value. Default is NULL.}
  \item{subject_besthit}{Subject best hit. Default is NULL.}
  \item{window_size}{Window size for multiple hits. Default is NULL.}
  \item{off_diagonal_range}{Off-diagonal range for multiple hits. Default is NULL.}
  \item{use_index}{Use index files. Default is NULL.}
  \item{index_name}{Name of the index file. Default is NULL.}
  \item{lcase_masking}{Use lower case masking. Default is NULL.}
  \item{query_loc}{Location on the query to search. Default is NULL.}
  \item{strand}{Strand of the query to search. Default is NULL.}
  \item{parse_deflines}{Parse deflines in the query and subject. Default is NULL.}
  \item{outfmt}{Output format. Default is "6".}
  \item{show_gis}{Show GIS in the output. Default is NULL.}
  \item{num_descriptions}{Number of descriptions to show. Default is NULL.}
  \item{num_alignments}{Number of alignments to show. Default is NULL.}
  \item{line_length}{Line length for formatting alignments. Default is NULL.}
  \item{html}{Output in HTML format. Default is NULL.}
  \item{sorthits}{Sort hits by a given criterion. Default is NULL.}
  \item{sorthsps}{Sort HSPs by a given criterion. Default is NULL.}
  \item{mt_mode}{Enable multi-threading mode. Default is NULL.}
  \item{remote}{Execute search remotely. Default is NULL.}
}
\value{
  A table with taxonomic assignments for OTUs.
}
\examples{
\dontrun{
Directory <- "C:/path/to/data/"
Database_File <- "database.fasta"
otu_table <- "otu_table.txt"
query <- "otus.fasta"
out <- "blast.txt"
refDB_Blast(Directory, Database_File, otu_table, query, out)
}
}
