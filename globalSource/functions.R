# Function to fetch data from GBIF for a specific continent, year, and observation type
fetch_data <- function(continent, year = NULL, observation_type) {
  start <- 0  # Initialize start as an integer
  continent_data <- list()
  repeat {
    result <- occ_search(
      continent = continent, 
      hasCoordinate = TRUE, 
      taxonKey = taxon_key, 
      basisOfRecord = observation_type, 
      fields = fields, 
      limit = limit, 
      start = start, 
      year = year
    )
    if (is.null(result$data) || nrow(result$data) == 0) break
    
    # Ensure all required columns are present
    missing_cols <- setdiff(fields, names(result$data))
    for (col in missing_cols) {
      result$data[[col]] <- NA
    }
    
    continent_data <- append(continent_data, list(result$data))
    start <- start + limit  # Increment start correctly
  }
  
  if (length(continent_data) > 0) {
    do.call(rbind, continent_data)
  } else {
    data.frame()  # Return an empty data frame if no data was fetched
  }
}

# Function to capitalize the first letter of each string in a vector
capitalize <- function(x) {
  sapply(x, function(y) paste0(toupper(substring(y, 1, 1)), substring(y, 2)), USE.NAMES = FALSE)
}

# Function to make unique names
make_unique_names <- function(names) {
  make.unique(names, sep = " ")
}

# Function to extract the data and create a data frame
extract_data <- function(data) {
  # Extract and modify the taxa vector
  taxa <- unlist(lapply(data, function(x) x$taxa))
  taxa <- sub(",.*", "", taxa)
  
  ids <- unlist(lapply(data, function(x) rep(x$layer$properties$`_leaflet_id`, length(x$taxa))))
  feature_types <- unlist(lapply(data, function(x) rep(x$layer$properties$feature_type, length(x$taxa))))
  geometries <- unlist(lapply(data, function(x) {
    coords <- x$layer$geometry$coordinates[[1]]
    replicate(length(x$taxa), st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE))), simplify = FALSE)
  }), recursive = FALSE)
  
  df <- data.frame(
    leaflet_id = ids,
    feature_type = feature_types,
    stringsAsFactors = FALSE
  )
  
  df <- st_sf(df, geometry = st_sfc(geometries))
  rownames(df) <- make_unique_names(capitalize(as.character(taxa)))
  
  # Set the CRS for the sf object
  st_crs(df) <- 4326  # CRS WGS 84
  
  return(df)
}

# Function to remove trailing numbers from names
remove_trailing_numbers <- function(name) {
  gsub(" [0-9]+$", "", name)
}


# Function to save occurrence data in specified formats
save_occurrence_data <- function(data, base_filename, formats = c("shp", "geojson", "gpkg", "kml", "csv")) {
  # Define the output directory
  current_directory <- getwd()
  output_directory <- file.path(current_directory, "SHP")
  if (!dir.exists(output_directory)) {
    dir.create(output_directory)
  }
  
  # Define output file paths based on the desired formats
  output_paths <- list(
    shp = file.path(output_directory, paste0(base_filename, ".shp")),
    geojson = paste0(base_filename, ".geojson"),
    gpkg = paste0(base_filename, ".gpkg"),
    kml = paste0(base_filename, ".kml"),
    csv = paste0(base_filename, ".csv")
  )
  
  # Save in the specified formats
  if ("shp" %in% formats) {
    st_write(data, output_paths$shp)
  }
  if ("geojson" %in% formats) {
    st_write(data, output_paths$geojson)
  }
  if ("gpkg" %in% formats) {
    st_write(data, output_paths$gpkg)
  }
  if ("kml" %in% formats) {
    st_write(data, output_paths$kml)
  }
  if ("csv" %in% formats) {
    output_csv <- st_drop_geometry(data)
    write.csv(output_csv, output_paths$csv, row.names = FALSE)
  }
  
  cat("Data saved in formats:", paste(formats, collapse = ", "), "\n")
}
