
library(rgbif)
library(sf)
library(concaveman)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(mapedit)
library(leaflet.extras2)
library(dplyr)
library(RColorBrewer)
library(leaflet.extras)
library(shiny)
library(htmlwidgets)
library(tidyr)
library(tibble)
library(retry)
library(openxlsx)
library(httr)
library(jsonlite)


#results <- occ_search(scientificName = "Gymnotus carapo", continent = "south_america", hasCoordinate = TRUE, basisOfRecord = "PRESERVED_SPECIMEN", limit = 100, fields = c("country", "name", "species", "genus", "family", "decimalLongitude", "decimalLatitude", "basisOfRecord"))

# Define the taxon and get the taxon key
taxa <- "Potamotrygonidae"
taxon_key <- name_suggest(q = taxa, rank = "family") ; taxon_key <- taxon_key[["data"]][["key"]][1]

# Define the search parameters
continents <- c("south_america")  # Vector of continents
fields <- c("country", "name", "species", "genus", "family", "decimalLongitude", "decimalLatitude", "basisOfRecord", "occurrenceID")
limit <- 400  # Number of records per request
observation_types <- c("PRESERVED_SPECIMEN", "HUMAN_OBSERVATION")  # Vector of observation types
all_data <- list()

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

# Fetch data for each continent, year, and observation type, and combine into a single data frame
all_data <- list()
years <- 1970:2024  # You can adjust the range of years as needed
for (continent in continents) {
  for (observation_type in observation_types) {
    for (year in years) {
      cat("Fetching data for", continent, "in year", year, "with observation type", observation_type, "\n")
      data <- fetch_data(continent, year, observation_type)
      if (!is.null(data) && nrow(data) > 0) {
        all_data <- append(all_data, list(data))
      }
    }
    
    # Fetch data without year information
    cat("Fetching data for", continent, "without year information with observation type", observation_type, "\n")
    data_no_year <- fetch_data(continent, observation_type = observation_type)
    if (!is.null(data_no_year) && nrow(data_no_year) > 0) {
      all_data <- append(all_data, list(data_no_year))
    }
  }
}

# Combine all data into a single data frame
data <- do.call(rbind, all_data)

# Exclude rows with missing coordinates and where the "species" column has NA values
data <- data[!is.na(data$decimalLongitude) & !is.na(data$decimalLatitude) & !is.na(data$species), ]

# Convert data to sf
sf_data <- st_as_sf(data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Exclude rows with missing coordinates and where the "species" column has NA values
data <- data[!is.na(data$decimalLongitude) & !is.na(data$decimalLatitude) & !is.na(data$species), ]

# Define a color palette for the species
#qual_palette <- colorFactor(palette = "Dark2", domain = sf_data$species)
qual_palette <- colorFactor(palette = brewer.pal(9, "Set1"), domain = sf_data$species)


scrollable_legend_css <- "
.info.legend {
  max-height: calc(100vh - 100px); /* Adjust the height as needed */
  overflow-y: auto;
}
"

# Leaflet map
map_within_sa <- leaflet(data) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addCircleMarkers(
    lng = ~decimalLongitude, lat = ~decimalLatitude,
    radius = 3,
    color = ~qual_palette(species),
    label = ~species,
    popup = ~species,
    group = ~species,
    layerId = ~paste0(decimalLongitude, decimalLatitude, species)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = qual_palette,
    values = ~species,
    title = "Species",
    opacity = 1,
    labFormat = function(type, cuts, p) {
      sapply(cuts, function(cut) {
        paste0("<span style='font-style:italic;'>", htmltools::htmlEscape(cut), "</span>")
      })
    }
  ) %>%
  addDrawToolbar(
    targetGroup = 'drawn',
    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
    polylineOptions = FALSE  # Excludes the draw a line functionality
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      var originalLayers = {};
      var selectedSpecies = [];

      myMap.eachLayer(function(layer) {
        if (layer.options && layer.options.group) {
          originalLayers[layer.options.layerId] = layer;
        }
      });

      var searchControl = L.control({position: 'bottomleft'});

      searchControl.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'info legend');
        div.innerHTML = '<h4>Search Species</h4>';
        div.innerHTML += '<input type=\"text\" id=\"search-box\" placeholder=\"Search for species...\"><br>';
        div.innerHTML += '<button id=\"search-button\">Search</button>';

        div.querySelector('#search-box').onfocus = function() {
          myMap.dragging.disable();
          myMap.touchZoom.disable();
          myMap.doubleClickZoom.disable();
          myMap.scrollWheelZoom.disable();
        };

        div.querySelector('#search-box').onblur = function() {
          myMap.dragging.enable();
          myMap.touchZoom.enable();
          myMap.doubleClickZoom.enable();
          myMap.scrollWheelZoom.enable();
        };

        div.querySelector('#search-button').onclick = function() {
          var searchValue = document.getElementById('search-box').value.toLowerCase();

          if (searchValue !== '') {
            var speciesArray = searchValue.split(',').map(function(species) {
              return species.trim();
            });
            selectedSpecies = speciesArray;
            Shiny.setInputValue('selected_species', speciesArray, {priority: 'event'});
          }

          Object.values(originalLayers).forEach(function(layer) {
            myMap.removeLayer(layer);
          });

          if (searchValue === '') {
            Object.values(originalLayers).forEach(function(layer) {
              myMap.addLayer(layer);
            });
          } else {
            Object.entries(originalLayers).forEach(function([key, layer]) {
              var layerSpecies = layer.options.group.toLowerCase();
              if (speciesArray.some(function(species) { return layerSpecies.includes(species); })) {
                myMap.addLayer(layer);
              }
            });
          }

          console.log('Selected species:', speciesArray);
        };

        return div;
      };

      searchControl.addTo(myMap);

      myMap.on('draw:created', function(e) {
        if (selectedSpecies.length > 0) {
          var layer = e.layer;
          var layerType = e.layerType;
          Shiny.setInputValue('drawn_feature', {
            type: layerType,
            layer: layer.toGeoJSON(),
            species: selectedSpecies
          });
        }
      });

      // JavaScript to dynamically adjust the legend width
      var legendItems = document.querySelectorAll('.leaflet-legend .legend-labels span');
      var maxWidth = 0;

      legendItems.forEach(function(item) {
        var width = item.clientWidth;
        if (width > maxWidth) {
          maxWidth = width;
        }
      });

      var legend = document.querySelector('.leaflet-legend');
      if (legend) {
        legend.style.width = (maxWidth + 50) + 'px'; // Add some padding
      }
    }
  ")

ui <- fillPage(
  tags$style(type = "text/css", "#map {height: calc(100vh - 20px) !important;}"),
  tags$style(HTML(scrollable_legend_css)),  # Add the custom CSS for scrollable legend
  leafletOutput("map"),
  verbatimTextOutput("searchedValuesOutput"),
  verbatimTextOutput("drawnFeaturesOutput")
)

server <- function(input, output, session) {
  
  searchedValues <- reactiveVal(character(0))
  drawnFeatures <- reactiveVal(list())
  
  output$map <- renderLeaflet(map_within_sa)
  
  observeEvent(input$selected_species, {
    newSearch <- input$selected_species
    currentSearches <- searchedValues()
    updatedSearches <- unique(c(currentSearches, unlist(newSearch)))
    searchedValues(updatedSearches)
    assign("searchedValuesGlobal", updatedSearches, envir = .GlobalEnv)
  })
  
  observeEvent(input$drawn_feature, {
    feature <- input$drawn_feature
    species <- feature$species
    print(paste("New feature drawn for species:", species))
    
    # Add the species and the drawn polygon to the list
    currentFeatures <- drawnFeatures()
    updatedFeatures <- append(currentFeatures, list(feature))
    drawnFeatures(updatedFeatures)
    
    # Update the global object with the drawn species
    speciesList <- lapply(updatedFeatures, function(f) f$species)
    assign("searchedValuesGlobal", speciesList, envir = .GlobalEnv)
    assign("drawnFeaturesGlobal", updatedFeatures, envir = .GlobalEnv)
  })
  
  output$searchedValuesOutput <- renderPrint({
    searchedValues()
  })
  
  output$drawnFeaturesOutput <- renderPrint({
    drawnFeatures()
  })
}

shinyApp(ui, server)


# Function to capitalize the first character of a string
capitalize <- function(x) {
  sapply(x, function(y) {
    paste(toupper(substring(y, 1, 1)), substring(y, 2), sep = "")
  })
}

# Function to ensure unique row names
make_unique_names <- function(names) {
  make.unique(names, sep = " ")
}

# Function to extract the data and create a data frame
extract_data <- function(data) {
  ids <- unlist(lapply(data, function(x) rep(x$layer$properties$`_leaflet_id`, length(x$species))))
  feature_types <- unlist(lapply(data, function(x) rep(x$layer$properties$feature_type, length(x$species))))
  geometries <- unlist(lapply(data, function(x) {
    coords <- x$layer$geometry$coordinates[[1]]
    replicate(length(x$species), st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE))), simplify = FALSE)
  }), recursive = FALSE)
  species <- unlist(lapply(data, function(x) x$species))
  
  df <- data.frame(
    leaflet_id = ids,
    feature_type = feature_types,
    stringsAsFactors = FALSE
  )
  
  df <- st_sf(df, geometry = st_sfc(geometries))
  rownames(df) <- make_unique_names(capitalize(species))
  
  # Set the CRS for the sf object
  st_crs(df) <- 4326  # CRS WGS 84
  
  return(df)
}

# Convert to an sf object
selected_sf <- extract_data(drawnFeaturesGlobal)

# Function to remove trailing numbers and whitespace from names
remove_trailing_numbers <- function(name) {
  gsub(" [0-9]+$", "", name)
}

# Initialize sf_data_final with the original dataset
sf_data_final <- sf_data

# Loop over each species in the list
for (i in seq_along(row.names(selected_sf))) {
  # Check which species is being viewed
  selected_species <- row.names(selected_sf)[i]
  
  # Remove trailing numbers and whitespace from the name
  cleaned_species <- remove_trailing_numbers(selected_species)
  
  # Filter the data for the selected species
  sf_data_selected_species <- sf_data %>% filter(species == cleaned_species)
  
  # Initialize a variable to accumulate intersections
  all_intersections <- rep(FALSE, nrow(sf_data_selected_species))
  
  # Loop over all polygons with the same cleaned species name
  for (j in seq_along(row.names(selected_sf))) {
    if (remove_trailing_numbers(row.names(selected_sf)[j]) == cleaned_species) {
      # Check intersections within the filtered dataset
      intersections <- st_intersects(sf_data_selected_species, selected_sf[j,], sparse = FALSE)
      
      # Accumulate the intersections
      all_intersections <- all_intersections | apply(intersections, 1, any)
    }
  }
  
  # Check for intersections and apply filter
  if (any(all_intersections)) {
    sf_data_cleaned <- sf_data_selected_species[!all_intersections, ]
  } else {
    sf_data_cleaned <- sf_data_selected_species
  }
  
  # Combine the cleaned data of the selected species with the data of other species
  sf_data_final <- bind_rows(sf_data_final %>% filter(species != cleaned_species), sf_data_cleaned)
}


# Add WKT column for geometry comparison
sf_data$wkt <- st_as_text(sf_data$geometry)
sf_data_final$wkt <- st_as_text(sf_data_final$geometry)

# Perform anti-join to find unique rows in sf_data
unique_rows <- anti_join(as.data.frame(sf_data), as.data.frame(sf_data_final), 
                         by = c("basisOfRecord", "family", "genus", "species", "country", "occurrenceID", "name", "wkt"))

# Convert back to sf and remove WKT column
excluded_sf_data <- st_as_sf(unique_rows, wkt = "wkt", crs = st_crs(sf_data)) %>% select(-wkt)


##########################################################################################


# Define a function to check and write the file
write_if_exists <- function(file_path, obj, driver = NULL) {
  if (is.null(driver)) {
    st_write(obj, file_path, delete_layer = TRUE)
  } else {
    st_write(obj, file_path, driver = driver, delete_layer = TRUE)
  }
}

# Shapefile

# Get the current working directory
current_directory <- getwd()

# Define the full path to the subdirectory
output_directory <- file.path(current_directory, "SHP")  ########## ATTENTION HERE TO CHANGE FOR LINUX TOO

# Check if the directory exists and create if necessary
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Suppress warnings when saving files
suppressWarnings(write_if_exists(file.path(output_directory, "occurrence_excluded.shp"), excluded_sf_data))

# GeoJSON
suppressWarnings(write_if_exists("occurrence_excluded.geojson", excluded_sf_data, driver = "GeoJSON"))

# GeoPackage
suppressWarnings(write_if_exists("occurrence_excluded.gpkg", excluded_sf_data))

# KML
suppressWarnings(write_if_exists("occurrence_excluded.kml", excluded_sf_data, driver = "KML"))


# Assume sf_data_final is your sf object
sf_excluded_data_to_export <- st_as_sf(excluded_sf_data)

# Convert the geometry to WKT
sf_excluded_data_to_export$geometry <- st_as_text(sf_excluded_data_to_export$geometry)

# Remove the sf class from the object to avoid errors
sf_excluded_data_to_export <- as.data.frame(sf_excluded_data_to_export)

# CSV
write.csv(sf_excluded_data_to_export, "occurrence_excluded.csv", row.names = FALSE)


##########################################################################################



##########################################################################################

# Remove WKT column from original sf objects
sf_data <- sf_data %>% select(-wkt)
sf_data_final <- sf_data_final %>% select(-wkt)

# Get the current working directory
current_directory <- getwd()

# Define the full path to the subdirectory
output_directory <- file.path(current_directory, "SHP")  ########## ATTENTION HERE TO CHANGE FOR LINUX TOO

# Check if the directory exists and create if necessary
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Suppress warnings when saving files
suppressWarnings(write_if_exists(file.path(output_directory, "occurrence_filtered.shp"), sf_data_final))

# GeoJSON
suppressWarnings(write_if_exists("occurrence_filtered.geojson", sf_data_final, driver = "GeoJSON"))

# GeoPackage
suppressWarnings(write_if_exists("occurrence_filtered.gpkg", sf_data_final))

# KML
suppressWarnings(write_if_exists("occurrence_filtered.kml", sf_data_final, driver = "KML"))


# Assume sf_data_final is your sf object
sf_data_to_export <- st_as_sf(sf_data_final)

# Convert the geometry to WKT
sf_data_to_export$geometry <- st_as_text(sf_data_to_export$geometry)

# Remove the sf class from the object to avoid errors
sf_data_to_export <- as.data.frame(sf_data_to_export)

# CSV
write.csv(sf_data_to_export, "occurrence_filtered.csv", row.names = FALSE)

##########################################################################################



# Separate the geometry coordinates into latitude and longitude columns
coords <- sf::st_coordinates(sf_data_final)
sf_data_final$decimalLongitude <- coords[, "X"]
sf_data_final$decimalLatitude <- coords[, "Y"]


# Leaflet map
map_within_sa_ <- leaflet(sf_data_final) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addCircleMarkers(
    lng = ~decimalLongitude, lat = ~decimalLatitude,
    radius = 3,
    color = ~qual_palette(species),
    label = ~species,
    popup = ~species,
    group = ~species,
    layerId = ~paste0(decimalLongitude, decimalLatitude, species)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = qual_palette,
    values = ~species,
    title = "Species",
    opacity = 1,
    labFormat = function(type, cuts, p) {
      sapply(cuts, function(cut) {
        paste0("<span style='font-style:italic;'>", htmltools::htmlEscape(cut), "</span>")
      })
    }
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      var originalLayers = {};

      myMap.eachLayer(function(layer) {
        if (layer.options && layer.options.group) {
          originalLayers[layer.options.layerId] = layer;
        }
      });

      var searchControl = L.control({position: 'bottomleft'});

      searchControl.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'info legend');
        div.innerHTML = '<h4>Search Species</h4>';
        div.innerHTML += '<input type=\"text\" id=\"search-box\" placeholder=\"Search for species...\"><br>';
        div.innerHTML += '<button id=\"search-button\">Search</button>';

        div.querySelector('#search-box').onfocus = function() {
          myMap.dragging.disable();
          myMap.touchZoom.disable();
          myMap.doubleClickZoom.disable();
          myMap.scrollWheelZoom.disable();
        };

        div.querySelector('#search-box').onblur = function() {
          myMap.dragging.enable();
          myMap.touchZoom.enable();
          myMap.doubleClickZoom.enable();
          myMap.scrollWheelZoom.enable();
        };

        div.querySelector('#search-button').onclick = function() {
          var searchValue = document.getElementById('search-box').value.toLowerCase();

          // Process the search for multiple species separated by commas
          var speciesArray = searchValue.split(',').map(function(species) {
            return species.trim();
          });

          Object.values(originalLayers).forEach(function(layer) {
            myMap.removeLayer(layer);
          });

          if (searchValue === '') {
            Object.values(originalLayers).forEach(function(layer) {
              myMap.addLayer(layer);
            });
          } else {
            Object.entries(originalLayers).forEach(function([key, layer]) {
              var layerSpecies = layer.options.group.toLowerCase();
              if (speciesArray.some(function(species) { return layerSpecies.includes(species); })) {
                myMap.addLayer(layer);
              }
            });
          }

          console.log('Selected species:', speciesArray);
        };

        return div;
      };

      searchControl.addTo(myMap);

      // JavaScript to dynamically adjust the legend width
      var legendItems = document.querySelectorAll('.leaflet-legend .legend-labels span');
      var maxWidth = 0;

      legendItems.forEach(function(item) {
        var width = item.clientWidth;
        if (width > maxWidth) {
          maxWidth = width;
        }
      });

      var legend = document.querySelector('.leaflet-legend');
      if (legend) {
        legend.style.width = (maxWidth + 50) + 'px'; // Add some padding
      }
    }
  ")

ui <- fillPage(
  tags$style(type = "text/css", "#map {height: calc(100vh - 20px) !important;}"),
  tags$style(HTML(scrollable_legend_css)),  # Add the custom CSS for scrollable legend
  leafletOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet(map_within_sa_)
}

shinyApp(ui, server)
