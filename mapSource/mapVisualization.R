
scrollable_legend_css <- "
.info.legend {
  max-height: calc(100vh - 100px); /* Adjust the height as needed */
  overflow-y: auto;
}
"

# Define a color palette for the species
qual_palette <- colorFactor(palette = brewer.pal(9, "Set1"), domain = selected_occurrence_data$taxa)

# Leaflet map
map_within_sa <- leaflet(selected_occurrence_data) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addCircleMarkers(
    lng = ~decimalLongitude, lat = ~decimalLatitude,
    radius = 3,
    color = ~qual_palette(taxa),
    label = ~taxa,
    popup = ~paste("Taxa:", taxa, "<br>Genus:", genus, "<br>Family:", family, "<br>Order:", order, "<br>Class:", class, "<br>Phylum:", phylum, "<br>Level:", toTitleCase(identification_level)),
    group = ~paste(taxa, genus, family, order, class, phylum, identification_level, sep = ","),  # Include identification level in group
    layerId = ~paste0(decimalLongitude, decimalLatitude, taxa, genus, family, order, class, phylum, identification_level)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = qual_palette,
    values = ~taxa,
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
        div.innerHTML += '<input type=\"text\" id=\"search-box\" placeholder=\"Search for species, genus, family, order, class, or phylum...\"><br>';
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
          var searchValue = document.getElementById('search-box').value;
          var taxaArray = searchValue.split(',').map(function(taxon) {
            return taxon.trim();
          });

          Object.values(originalLayers).forEach(function(layer) {
            myMap.removeLayer(layer);
          });

          if (searchValue === 'all' || searchValue === '') {
            // Select all unique layers if search value is 'all' or empty
            selectedTaxa = Array.from(uniqueTaxa);
            selectedTaxa.forEach(function(taxon) {
              Object.values(originalLayers).forEach(function(layer) {
                if (layer.options.group === taxon) {
                  myMap.addLayer(layer);
                }
              });
            });
          } else {
            var foundLayers = new Set();
            taxaArray.forEach(function(taxon) {
              Object.values(originalLayers).forEach(function(layer) {
                var layerGroup = layer.options.group;
                var isGenusSearch = taxon.endsWith(' (genus)');
                var isFamilySearch = taxon.endsWith(' (family)');
                var isOrderSearch = taxon.endsWith(' (order)');
                var isClassSearch = taxon.endsWith(' (class)');
                var isPhylumSearch = taxon.endsWith(' (phylum)');
                var taxonWithoutSuffix = taxon.replace(' (genus)', '').replace(' (family)', '').replace(' (order)', '').replace(' (class)', '').replace(' (phylum)', '');

                if (isGenusSearch) {
                  if (layerGroup.includes(taxonWithoutSuffix) && layerGroup.includes('genus')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.group);
                  }
                } else if (isFamilySearch) {
                  if (layerGroup.includes(taxonWithoutSuffix) && layerGroup.includes('family')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.group);
                  }
                } else if (isOrderSearch) {
                  if (layerGroup.includes(taxonWithoutSuffix) && layerGroup.includes('order')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.group);
                  }
                } else if (isClassSearch) {
                  if (layerGroup.includes(taxonWithoutSuffix) && layerGroup.includes('class')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.group);
                  }
                } else if (isPhylumSearch) {
                  if (layerGroup.includes(taxonWithoutSuffix) && layerGroup.includes('phylum')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.group);
                  }
                } else {
                  if (layerGroup.includes(taxonWithoutSuffix)) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.group);
                  }
                }
              });
            });
            selectedTaxa = Array.from(foundLayers);
          }

          Shiny.setInputValue('selected_taxa', selectedTaxa, {priority: 'event'});
          Shiny.setInputValue('search_box', searchValue, {priority: 'event'});
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
  leafletOutput("map"),
  verbatimTextOutput("searchedValuesOutput")
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
  
  observeEvent(input$search_box, {
    searchValue <- input$search_box
    currentSearches <- searchedValues()
    updatedSearches <- unique(c(currentSearches, searchValue))
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
}

shinyApp(ui, server)
