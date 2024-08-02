# Function to save occurrence data in specified formats
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