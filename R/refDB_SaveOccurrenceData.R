#' Save Occurrence Data in Specified Formats
#'
#' This function saves occurrence data in specified formats, including Shapefile, GeoJSON, GPKG, KML, and CSV.
#'
#' @param data An sf object containing the occurrence data.
#' @param base_filename A character string specifying the base filename for the output files.
#' @param formats A character vector specifying the formats in which to save the data. Supported formats include "shp", "geojson", "gpkg", "kml", and "csv".
#' @return The function does not return a value but saves the occurrence data in the specified formats.
#' @examples{
#' \dontrun{
#' # Create an example sf object
#' library(sf)
#' example_data <- st_sf(
#'   gbifID = c("1", "2", "3"),
#'   species = c("Panthera leo", "Panthera tigris", "Panthera onca"),
#'   decimalLongitude = c(34.5, 78.0, -58.3),
#'   decimalLatitude = c(-1.3, 23.5, -13.4),
#'   geometry = st_sfc(st_point(c(34.5, -1.3)), st_point(c(78.0, 23.5)), st_point(c(-58.3, -13.4))),
#'   crs = 4326
#' refDB_SaveOccurrenceData(example_data, "example_output", formats = c("shp", "geojson", "csv"))
#' )}
#' }
#' @import readr
#' @importFrom sf st_write st_drop_geometry
#' @importFrom utils write.table
#' @export
refDB_SaveOccurrenceData <- function(data, base_filename, formats = c("shp", "geojson", "gpkg", "kml", "csv")) {
  # Define the output directory
  current_directory <- getwd()
  output_directory <- file.path(current_directory, "SHP")
  
  # Define output paths based on desired formats
  output_paths <- list(
    shp = file.path(output_directory, paste0(base_filename, ".shp")),
    geojson = paste0(base_filename, ".geojson"),
    gpkg = paste0(base_filename, ".gpkg"),
    kml = paste0(base_filename, ".kml"),
    csv = paste0(base_filename, ".csv")
  )
  
  # Save in specified formats
  if ("shp" %in% formats) {
    if (!dir.exists(output_directory)) {
      dir.create(output_directory)
    }
    # Convert gbifID to character to avoid field width problems
    data$gbifID <- as.character(data$gbifID)
    st_write(data, output_paths$shp, delete_layer = TRUE)
  }
  if ("geojson" %in% formats) {
    st_write(data, output_paths$geojson, delete_layer = TRUE)
  }
  if ("gpkg" %in% formats) {
    st_write(data, output_paths$gpkg, delete_layer = TRUE)
  }
  if ("kml" %in% formats) {
    st_write(data, output_paths$kml, delete_layer = TRUE)
  }
  if ("csv" %in% formats) {
    output_csv <- st_drop_geometry(data)
    write.table(output_csv, output_paths$csv, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  }
  
  cat("Data saved in formats:", paste(formats, collapse = ", "), "\n")
}