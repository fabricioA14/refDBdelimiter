\name{refDB_SaveOccurrenceData}
\alias{refDB_SaveOccurrenceData}
\title{Save Occurrence Data in Specified Formats}
\description{
  This function saves occurrence data in specified formats, including Shapefile, GeoJSON, GPKG, KML, and CSV.
}
\usage{
refDB_SaveOccurrenceData(
  data, 
  base_filename, 
  formats = c("shp", "geojson", "gpkg", "kml", "csv")
)
}
\arguments{
  \item{data}{An sf object containing the occurrence data.}
  \item{base_filename}{A character string specifying the base filename for the output files.}
  \item{formats}{A character vector specifying the formats in which to save the data. Supported formats include "shp", "geojson", "gpkg", "kml", and "csv".}
}
\value{
  The function does not return a value but saves the occurrence data in the specified formats.
}
\examples{
# Create an example sf object
library(sf)
example_data <- st_sf(
  gbifID = c("1", "2", "3"),
  species = c("Panthera leo", "Panthera tigris", "Panthera onca"),
  decimalLongitude = c(34.5, 78.0, -58.3),
  decimalLatitude = c(-1.3, 23.5, -13.4),
  geometry = st_sfc(st_point(c(34.5, -1.3)), st_point(c(78.0, 23.5)), st_point(c(-58.3, -13.4))),
  crs = 4326
)

# Save the data in multiple formats
refDB_SaveOccurrenceData(example_data, "example_output", formats = c("shp", "geojson", "csv"))
}