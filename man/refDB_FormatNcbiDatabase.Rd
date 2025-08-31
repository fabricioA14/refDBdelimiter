\name{refDB_FormatNcbiDatabase}
\alias{refDB_FormatNcbiDatabase}
\title{Format NCBI Database}
\description{
  This function formats an NCBI database by removing sequences below a specified minimum length, special characters from all the fasta headers and UNVERIFIED fasta sequences.
}
\usage{
refDB_FormatNcbiDatabase(
  raw_database, 
  database_cleaned, 
  min_sequence_length, 
  pattern = "UNVERIFIED"
)
}
\arguments{
  \item{raw_database}{Path to the raw NCBI database file.}
  \item{database_cleaned}{Path to the cleaned NCBI database file.}
  \item{min_sequence_length}{Minimum length for sequences to be retained in the cleaned database.}
  \item{pattern}{Pattern to filter sequences in the raw database (default is "UNVERIFIED").}
}
\value{
  A cleaned and formatted NCBI database file.
}
\examples{
\dontrun{
# Example usage of refDB_FormatNcbiDatabase
raw_database <- "path/to/raw_database.fasta"
database_cleaned <- "path/to/database_cleaned.fasta"
min_sequence_length <- 200
pattern <- "UNVERIFIED"

refDB_FormatNcbiDatabase(raw_database, database_cleaned, min_sequence_length, pattern)
}
}