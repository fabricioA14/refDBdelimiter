
function(el, x) {
  var myMap = this;
  var originalLayers = {};
  var selectedSpecies = null;

  // Store original layers with group information
  myMap.eachLayer(function(layer) {
    if (layer.options && layer.options.group) {
      originalLayers[layer.options.layerId] = layer;
    }
  });

  var searchedValues = [];

  var searchControl = L.control({position: 'bottomleft'});

  // Add search control to the map
  searchControl.onAdd = function(map) {
    var div = L.DomUtil.create('div', 'info legend');
    div.innerHTML = '<h4>Search Species</h4>';
    div.innerHTML += '<input type="text" id="search-box" placeholder="Search for species..."><br>';
    div.innerHTML += '<button id="search-button">Search</button>';

    // Disable map interactions when the search box is focused
    div.querySelector('#search-box').onfocus = function() {
      myMap.dragging.disable();
      myMap.touchZoom.disable();
      myMap.doubleClickZoom.disable();
      myMap.scrollWheelZoom.disable();
    };

    // Enable map interactions when the search box is blurred
    div.querySelector('#search-box').onblur = function() {
      myMap.dragging.enable();
      myMap.touchZoom.enable();
      myMap.doubleClickZoom.enable();
      myMap.scrollWheelZoom.enable();
    };

    // Search button functionality
    div.querySelector('#search-button').onclick = function() {
      var searchValue = document.getElementById('search-box').value.toLowerCase();

      if (searchValue !== '') {
        selectedSpecies = searchValue;
        Shiny.setInputValue('selected_species', selectedSpecies, {priority: 'event'});
      }

      // Remove all layers
      Object.values(originalLayers).forEach(function(layer) {
        myMap.removeLayer(layer);
      });

      // Add layers back based on search value
      if (searchValue === '') {
        Object.values(originalLayers).forEach(function(layer) {
          myMap.addLayer(layer);
        });
      } else {
        Object.entries(originalLayers).forEach(function([key, layer]) {
          if (layer.options.group.toLowerCase().includes(searchValue)) {
            myMap.addLayer(layer);
          }
        });
      }

      console.log('Selected species:', selectedSpecies);
    };

    return div;
  };

  searchControl.addTo(myMap);

  // Event handler for when a new feature is drawn
  myMap.on('draw:created', function(e) {
    if (selectedSpecies) {
      var layer = e.layer;
      var layerType = e.layerType;
      Shiny.setInputValue('drawn_feature', {
        type: layerType,
        layer: layer.toGeoJSON(),
        species: selectedSpecies
      });
    }
  });

  // Adjust the legend width dynamically
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
