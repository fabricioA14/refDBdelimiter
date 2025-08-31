\name{refDB_SubsetNcbiGbif}
\alias{refDB_SubsetNcbiGbif}
\title{Subset NCBI Database Based on GBIF Database}
\description{
  This function subsets an NCBI database based on a GBIF database by searching for and appending sequences that match names from the GBIF database. The matching can be based on exact names or specific conditions.
}
\usage{
refDB_SubsetNcbiGbif(
  gbif_database, 
  cleaned_ncbi_database, 
  ncbi_database_based_on_gbif, 
  genus_flexibility
)
}
\arguments{
  \item{gbif_database}{Path to the GBIF database file containing names.}
  \item{cleaned_ncbi_database}{Path to the cleaned NCBI database file.}
  \item{ncbi_database_based_on_gbif}{Path to the output NCBI database file based on the GBIF database.}
  \item{genus_flexibility}{Logical condition to determine the matching strategy.}
}
\value{
  An NCBI database file subsetted based on the GBIF database.
}
\examples{
\dontrun{
# Example usage of refDB_SubsetNcbiGbif
gbif_database <- "path/to/gbif_database.txt"
cleaned_ncbi_database <- "path/to/cleaned_ncbi_database.fasta"
ncbi_database_based_on_gbif <- "path/to/ncbi_database_based_on_gbif.fasta"
genus_flexibility <- TRUE

refDB_SubsetNcbiGbif(
  gbif_database, 
  cleaned_ncbi_database, 
  ncbi_database_based_on_gbif, 
  genus_flexibility
)
}
}