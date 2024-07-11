# List of packages to ensure are installed and loaded
pack <- c('tibble', 'rgbif', 'sf', 'concaveman', 'ggplot2', 'rnaturalearth',
          'rnaturalearthdata', 'leaflet', 'mapedit', 'leaflet.extras2', 
          'dplyr', 'RColorBrewer', 'leaflet.extras', 'shiny', 'htmlwidgets', 
          'tidyr', 'retry', 'openxlsx', 'httr', 'jsonlite','bdc')

# Check for packages that are not installed
vars <- pack[!(pack %in% installed.packages()[, "Package"])]

# Install any packages that are not already installed
if (length(vars) != 0) {
  install.packages(vars, dependencies = TRUE)
}

# Load all the packages
sapply(pack, require, character.only = TRUE)


#results <- occ_search(scientificName = "Gymnotus carapo", continent = "south_america", hasCoordinate = TRUE, basisOfRecord = "PRESERVED_SPECIMEN", limit = 100, fields = c("country", "name", "species", "genus", "family", "decimalLongitude", "decimalLatitude", "basisOfRecord"))

# Define the taxon and get the taxon key
taxa <- "Mollusca"
taxon_key <- name_suggest(q = taxa) ; taxon_key <- taxon_key[["data"]][["key"]][1]

# Define the search parameters
continents <- c("south_america")  # Vector of continents
fields <- c("gbifID","scientificName", "country", "stateProvince", "county", "locality", "species", "genus", "family", "order", "class", "phylum", "kingdom", "decimalLongitude", "decimalLatitude", "basisOfRecord", "verbatimEventDate")
limit <- 400  # Number of records per request
observation_types <- c("PRESERVED_SPECIMEN")  # Vector of observation types
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
years <- 2018:2024  # You can adjust the range of years as needed
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
    popup = ~paste("Species:", species, "<br>Family:", family),
    group = ~paste(species, family, sep = ","),  # Include both species and family in group
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
      var uniqueTaxa = new Set(); // Use a Set to store unique taxa

      myMap.eachLayer(function(layer) {
        if (layer.options && layer.options.group) {
          originalLayers[layer.options.layerId] = layer;
          uniqueTaxa.add(layer.options.group); // Add the unique taxa
        }
      });

      // Select all unique layers by default
      var selectedTaxa = Array.from(uniqueTaxa);
      selectedTaxa.forEach(function(taxon) {
        Object.values(originalLayers).forEach(function(layer) {
          if (layer.options.group === taxon) {
            myMap.addLayer(layer);
          }
        });
      });

      Shiny.setInputValue('selected_taxa', selectedTaxa, {priority: 'event'});

      var searchControl = L.control({position: 'bottomleft'});

      searchControl.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'info legend');
        div.innerHTML = '<h4>Search Taxa</h4>';
        div.innerHTML += '<input type=\"text\" id=\"search-box\" placeholder=\"Search for species or family...\"><br>';
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
          var taxaArray = searchValue.split(',').map(function(taxon) {
            return taxon.trim().toLowerCase();
          });

          Object.values(originalLayers).forEach(function(layer) {
            myMap.removeLayer(layer);
          });

          if (searchValue === '') {
            selectedTaxa.forEach(function(taxon) {
              Object.values(originalLayers).forEach(function(layer) {
                if (layer.options.group.toLowerCase() === taxon.toLowerCase()) {
                  myMap.addLayer(layer);
                }
              });
            });
          } else {
            var foundLayers = new Set();
            taxaArray.forEach(function(taxon) {
              Object.values(originalLayers).forEach(function(layer) {
                var layerGroup = layer.options.group.toLowerCase();
                if (layerGroup.includes(taxon)) {
                  myMap.addLayer(layer);
                  foundLayers.add(layer.options.group);
                }
              });
            });
            selectedTaxa = Array.from(foundLayers);
          }

          Shiny.setInputValue('selected_taxa', selectedTaxa, {priority: 'event'});
        };

        return div;
      };

      searchControl.addTo(myMap);

      myMap.on('draw:created', function(e) {
        if (selectedTaxa.length > 0) {
          var layer = e.layer;
          var layerType = e.layerType;
          Shiny.setInputValue('drawn_feature', {
            type: layerType,
            layer: layer.toGeoJSON(),
            taxa: selectedTaxa
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
  
  observeEvent(input$selected_taxa, {
    newSearch <- input$selected_taxa
    currentSearches <- searchedValues()
    updatedSearches <- unique(c(currentSearches, unlist(newSearch)))
    searchedValues(updatedSearches)
    assign("searchedValuesGlobal", updatedSearches, envir = .GlobalEnv)
  })
  
  observeEvent(input$drawn_feature, {
    feature <- input$drawn_feature
    taxa <- feature$taxa
    print(paste("New feature drawn for taxa:", taxa))
    
    # Add the taxa and the drawn polygon to the list
    currentFeatures <- drawnFeatures()
    updatedFeatures <- append(currentFeatures, list(feature))
    drawnFeatures(updatedFeatures)
    
    # Update the global object with the drawn taxa
    taxaList <- lapply(updatedFeatures, function(f) f$taxa)
    assign("searchedValuesGlobal", taxaList, envir = .GlobalEnv)
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

# Convert to an sf object
selected_sf <- extract_data(drawnFeaturesGlobal)

#rownames(selected_sf) <- sub(",.*", "", rownames(selected_sf))

remove_trailing_numbers <- function(name) {
  gsub(" [0-9]+$", "", name)
}

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
  search_results <- sf_data[sf_data$species == name_to_search, ]
  
  # If not found in 'species', search in 'genus'
  if (nrow(search_results) == 0) {
    search_results <- sf_data[sf_data$genus == name_to_search, ]
  }
  
  # If not found in 'genus', search in 'family'
  if (nrow(search_results) == 0) {
    search_results <- sf_data[sf_data$family == name_to_search, ]
  }
  
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
final_results_inside <- do.call(rbind, all_search_results_inside)

# Add WKT column for geometry comparison
sf_data$wkt <- st_as_text(sf_data$geometry)
final_results_inside$wkt <- st_as_text(final_results_inside$geometry)

# Perform anti-join to find unique rows in sf_data
unique_rows <- anti_join(as.data.frame(sf_data), as.data.frame(final_results_inside), 
                         by = c("basisOfRecord", "species", "genus", "family", "order", "class", "phylum", "kingdom", "country", "wkt"))

# Convert back to sf and remove WKT column
final_results_outside <- st_as_sf(unique_rows, wkt = "wkt", crs = st_crs(sf_data)) %>% select(-wkt)


# Separate the geometry coordinates into latitude and longitude columns
coords <- sf::st_coordinates(final_results_outside)
final_results_outside$decimalLongitude <- coords[, "X"]
final_results_outside$decimalLatitude <- coords[, "Y"]


# Leaflet map
map_within_sa_ <- leaflet(final_results_outside) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addCircleMarkers(
    lng = ~decimalLongitude, lat = ~decimalLatitude,
    radius = 3,
    color = ~qual_palette(species),
    label = ~species,
    popup = ~paste("Species:", species,"<br>Genus:", genus, "<br>Family:", family),
    group = ~paste(species, genus, family, sep = ","),  # Include both species and family in group
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
      var uniqueTaxa = new Set(); // Use a Set to store unique taxa

      myMap.eachLayer(function(layer) {
        if (layer.options && layer.options.group) {
          originalLayers[layer.options.layerId] = layer;
          uniqueTaxa.add(layer.options.group); // Add the unique taxa
        }
      });

      // Select all unique layers by default
      var selectedTaxa = Array.from(uniqueTaxa);
      selectedTaxa.forEach(function(taxon) {
        Object.values(originalLayers).forEach(function(layer) {
          if (layer.options.group === taxon) {
            myMap.addLayer(layer);
          }
        });
      });

      Shiny.setInputValue('selected_taxa', selectedTaxa, {priority: 'event'});

      var searchControl = L.control({position: 'bottomleft'});

      searchControl.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'info legend');
        div.innerHTML = '<h4>Search Taxa</h4>';
        div.innerHTML += '<input type=\"text\" id=\"search-box\" placeholder=\"Search for taxa...\"><br>';
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
          var taxaArray = searchValue.split(',').map(function(taxon) {
            return taxon.trim().toLowerCase();
          });

          Object.values(originalLayers).forEach(function(layer) {
            myMap.removeLayer(layer);
          });

          if (searchValue === '') {
            selectedTaxa.forEach(function(taxon) {
              Object.values(originalLayers).forEach(function(layer) {
                if (layer.options.group.toLowerCase() === taxon.toLowerCase()) {
                  myMap.addLayer(layer);
                }
              });
            });
          } else {
            var foundLayers = new Set();
            taxaArray.forEach(function(taxon) {
              Object.values(originalLayers).forEach(function(layer) {
                var layerGroup = layer.options.group.toLowerCase();
                if (layerGroup.includes(taxon)) {
                  myMap.addLayer(layer);
                  foundLayers.add(layer.options.group);
                }
              });
            });
            selectedTaxa = Array.from(foundLayers);
          }

          Shiny.setInputValue('selected_taxa', selectedTaxa, {priority: 'event'});
        };

        return div;
      };

      searchControl.addTo(myMap);

      myMap.on('draw:created', function(e) {
        if (selectedTaxa.length > 0) {
          var layer = e.layer;
          var layerType = e.layerType;
          Shiny.setInputValue('drawn_feature', {
            type: layerType,
            layer: layer.toGeoJSON(),
            taxa: selectedTaxa
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
  leafletOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet(map_within_sa_)
}

shinyApp(ui, server)
