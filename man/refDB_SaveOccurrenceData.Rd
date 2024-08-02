#' Save Occurrence Data
#'
#' This function saves occurrence data in specified formats.
#'
#' @param data An sf object containing the data to save.
#' @param base_filename A character string specifying the base filename for the output.
#' @param formats A character vector specifying the formats to save the data in.
#' @return None
#' @examples
#' refDB_SaveOccurrenceData(data, "output", c("shp", "geojson"))
#' @export
refDB_SaveOccurrenceData <- function(data, base_filename, formats = c("shp", "geojson", "gpkg", "kml", "csv")) {
  current_directory <- getwd()
  output_directory <- file.path(current_directory, "SHP")
  
  output_paths <- list(
    shp = file.path(output_directory, paste0(base_filename, ".shp")),
    geojson = paste0(base_filename, ".geojson"),
    gpkg = paste0(base_filename, ".gpkg"),
    kml = paste0(base_filename, ".kml"),
    csv = paste0(base_filename, ".csv")
  )
  
  if ("shp" %in% formats) {
    if (!dir.exists(output_directory)) {
      dir.create(output_directory)
    }
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