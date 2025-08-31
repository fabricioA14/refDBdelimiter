

# Define the path to the output directory for Shapefile
current_directory <- getwd()
output_directory <- file.path(current_directory, "SHP")
shapefile_path <- file.path(output_directory, "occurrence_excluded.shp")

# Read the Shapefile
imported_shapefile <- st_read(shapefile_path)

# Define the path to the GeoJSON file
geojson_path <- "occurrence_excluded.geojson"

# Read the GeoJSON file
imported_geojson <- st_read(geojson_path)

# Define the path to the GeoPackage file
geopackage_path <- "occurrence_excluded.gpkg"

# Read the GeoPackage file
imported_geopackage <- st_read(geopackage_path)

# Define the path to the KML file
kml_path <- "occurrence_excluded.kml"

# Read the KML file
imported_kml <- st_read(kml_path)

# Define the path to the CSV file
csv_path <- "occurrence_excluded.csv"

# Read the CSV file
imported_csv <- read.csv(csv_path)

# Convert the geometry column from WKT to sf
imported_csv$geometry <- st_as_sfc(imported_csv$geometry, crs = st_crs(4326))  # Assuming WGS 84 CRS
imported_sf <- st_as_sf(imported_csv)
