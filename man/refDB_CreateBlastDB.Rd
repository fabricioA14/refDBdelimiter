\name{refDB_CreateBlastDB}
\alias{refDB_CreateBlastDB}
\title{Create a BLAST Database}
\description{
  This function creates a BLAST database using the `makeblastdb` command from NCBI BLAST+.
}
\usage{
refDB_CreateBlastDB(database, parse_seqids = TRUE, database_type = "nucl", 
                    title = "local_database", out = NULL, hash_index = FALSE, 
                    mask_data = NULL, mask_id = NULL, mask_desc = NULL, 
                    gi_mask = FALSE, gi_mask_name = NULL, max_file_sz = NULL, 
                    logfile = NULL, taxid = NULL, taxid_map = NULL)
}
\arguments{
  \item{database}{Path to the input database file.}
  \item{parse_seqids}{Logical indicating whether to parse sequence IDs. Default is TRUE.}
  \item{database_type}{Type of the database: "nucl" for nucleotide, "prot" for protein. Default is "nucl".}
  \item{title}{Title for the database. Default is "local_database".}
  \item{out}{Name of the output database files. Default is NULL.}
  \item{hash_index}{Logical indicating whether to create a hash index. Default is FALSE.}
  \item{mask_data}{Path to the mask data file. Default is NULL.}
  \item{mask_id}{ID of the mask. Default is NULL.}
  \item{mask_desc}{Description of the mask. Default is NULL.}
  \item{gi_mask}{Logical indicating whether to use GI mask. Default is FALSE.}
  \item{gi_mask_name}{Name of the GI mask. Default is NULL.}
  \item{max_file_sz}{Maximum file size. Default is NULL.}
  \item{logfile}{Path to the logfile. Default is NULL.}
  \item{taxid}{Taxonomic ID. Default is NULL.}
  \item{taxid_map}{Path to the taxonomic ID map file. Default is NULL.}
}
\value{
  A BLAST database created from the input database file.
}
\examples{
\dontrun{
# Example usage of refDB_CreateBlastDB
database <- "path/to/database.fasta"
out <- "Brazil_Chordata_Database"
refDB_CreateBlastDB(database, out = out, title = "My Local Database")
}
}