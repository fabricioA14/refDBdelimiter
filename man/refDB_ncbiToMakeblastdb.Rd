\name{refDB_ncbiToMakeblastdb}
\alias{refDB_ncbiToMakeblastdb}
\title{Process NCBI Database for use with makeblastdb}
\description{
  This function processes an NCBI database by appending unique numbered sequences to the headers and replacing spaces with underscores, preparing it for use with the makeblastdb tool.
}
\usage{
refDB_ncbiToMakeblastdb(cleaned_ncbi_database, ncbi_database_based_on_gbif)
}
\arguments{
  \item{cleaned_ncbi_database}{Path to the cleaned NCBI database file.}
  \item{ncbi_database_based_on_gbif}{Path to the output NCBI database file based on the GBIF database.}
}
\value{
  A processed NCBI database file ready for use with makeblastdb.
}
\examples{
\dontrun{
# Example usage of refDB_ncbiToMakeblastdb
cleaned_ncbi_database <- "path/to/cleaned_ncbi_database.fasta"
ncbi_database_based_on_gbif <- "path/to/ncbi_database_based_on_gbif.fasta"

refDB_ncbiToMakeblastdb(cleaned_ncbi_database, ncbi_database_based_on_gbif)
}
}