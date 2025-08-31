
# Load Global Functions
source("globalSource/functions.R")

# List of packages to ensure are installed and loaded
pack <- c('tibble', 'rgbif', 'sf', 'concaveman', 'ggplot2', 'rnaturalearth','rnaturalearthdata','leaflet',
          'mapedit', 'leaflet.extras2', 'dplyr', 'RColorBrewer', 'leaflet.extras','shiny', 'htmlwidgets',
          'tidyr', 'retry', 'openxlsx', 'httr', 'jsonlite','bdc','tools','countrycode','data.table','stringr')

# Check for packages that are not installed
vars <- pack[!(pack %in% installed.packages()[, "Package"])]

# Install any packages that are not already installed
if (length(vars) != 0) {
  install.packages(vars, dependencies = TRUE)
}

# Load all the packages
sapply(pack, require, character.only = TRUE)

####################################################

################ Script Starts Here ################



data <- data %>%
  mutate(taxa = if_else(!is.na(species), species, 
                        if_else(!is.na(genus), genus, 
                                if_else(!is.na(family), family, 
                                        if_else(!is.na(order), order, 
                                                if_else(!is.na(class), class, kingdom))))))

data <- data %>%
  mutate(identification_level = case_when(
    !is.na(species) ~ "species",
    !is.na(genus) ~ "genus",
    !is.na(family) ~ "family",
    !is.na(order) ~ "order",
    !is.na(class) ~ "class",
    !is.na(phylum) ~ "phylum",
    !is.na(kingdom) ~ "kingdom",
    TRUE ~ "unknown"
  ))

# Convert data to sf
sf_data <- st_as_sf(data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

source("mapSource/mapEdit.R")

shinyApp(ui, server)

# Convert to an sf object
selected_sf <- extract_data(drawnFeaturesGlobal)

# Initialize lists to store the search results for each row
all_search_results_inside <- list()

# Loop over each row of selected_sf
for (i in 1:nrow(selected_sf)) {
  # Extract the name from the current row of selected_sf
  name_to_search <- rownames(selected_sf[i, ])
  
  # Remove trailing numbers from the name
  name_to_search <- remove_trailing_numbers(name_to_search)
  
  # Extract the polygon from the current row of selected_sf
  current_polygon <- selected_sf[i, 3]
  
  # Initialize an empty data frame to store the search results
  search_results <- data.frame()
  
  # First search in the 'species' column
  search_results <- sf_data[sf_data$taxa == name_to_search, ]
  
  # Check if the points are within the polygon
  if (nrow(search_results) > 0) {
    points_sf <- st_as_sf(search_results, coords = c("longitude", "latitude"), crs = st_crs(selected_sf))
    within_polygon <- st_intersects(points_sf, current_polygon, sparse = FALSE)
    
    # Separate points that are within and outside the polygon
    search_results_inside <- search_results[within_polygon, ]
  } else {
    search_results_inside <- data.frame()
  }
  
  # Store the search results for the current row
  all_search_results_inside[[i]] <- search_results_inside
}

# Filter out sublists with 0 rows
all_search_results_inside <- Filter(function(x) nrow(x) > 0, all_search_results_inside)

# Combine all search results into single data frames (optional)
excluded_occurrence_data <- do.call(rbind, all_search_results_inside) ; rm(all_search_results_inside)

# Add WKT column for geometry comparison
sf_data$wkt <- st_as_text(sf_data$geometry)
excluded_occurrence_data$wkt <- st_as_text(excluded_occurrence_data$geometry)

# Perform anti-join to find unique rows in sf_data
unique_rows <- anti_join(as.data.frame(sf_data), as.data.frame(excluded_occurrence_data), 
                         by = c("gbifID","basisOfRecord", "species", "genus", "family", "order", "class", "phylum", "kingdom", "country", "wkt"))

# Convert back to sf and remove WKT column
selected_occurrence_data <- st_as_sf(unique_rows, wkt = "wkt", crs = st_crs(sf_data)) %>% select(-wkt)

# Separate the geometry coordinates into latitude and longitude columns
coords <- sf::st_coordinates(selected_occurrence_data)
selected_occurrence_data$decimalLongitude <- coords[, "X"]
selected_occurrence_data$decimalLatitude <- coords[, "Y"]

source("mapSource/mapVisualization.R")

shinyApp(ui, server)


########### Outputs ###########

# Example usage of the function for selected_occurrence_data
save_occurrence_data(selected_occurrence_data, "selected_occurrence_data", formats = c("shp", "geojson", "csv"))

# Example usage of the function for excluded_occurrence_data
save_occurrence_data(excluded_occurrence_data, "excluded_occurrence_data", formats = c("gpkg", "kml"))




