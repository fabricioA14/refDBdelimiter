\name{refDB_ExtractData}
\alias{refDB_ExtractData}
\title{Extract Data and Create a Data Frame}
\description{
  This function extracts data from a list of spatial features and creates a data frame.
}
\usage{
refDB_ExtractData(data)
}
\arguments{
  \item{data}{A list of spatial features. Each feature should have a \code{taxa} vector, a \code{layer} list with \code{properties} and \code{geometry}.}
}
\value{
  An \code{sf} object (data frame) with extracted and modified data, including unique taxa names.
}
\examples{
\dontrun{
data <- list(
  list(
    taxa = c("Panthera leo, Africa", "Panthera onca, America"), 
    layer = list(
      properties = list(`_leaflet_id` = 1, feature_type = "polygon"), 
      geometry = list(
        coordinates = list(
          matrix(c(1, 2, 3, 4, 5, 6, 1, 2), ncol = 2, byrow = TRUE)
        )
      )
    )
  ),
  list(
    taxa = c("Felis catus, Europe", "Canis lupus, North America"), 
    layer = list(
      properties = list(`_leaflet_id` = 2, feature_type = "polygon"), 
      geometry = list(
        coordinates = list(
          matrix(c(2, 3, 4, 5, 6, 7, 2, 3), ncol = 2, byrow = TRUE)
        )
      )
    )
  )
)
refDB_ExtractData(data)
}
}