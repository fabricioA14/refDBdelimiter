
cleaning_ncbi_database <- function(raw_database, min_sequence_length) {
  
  # Create the output filename by appending "_cleaned" to the input filename
  database_cleaned <- sub("(.*)\\..*", "\\1_cleaned.fasta", raw_database)
  
  pattern <- "UNVERIFIED"
  
  filter_command <- paste0(
    "wsl awk",
    " -v pattern='" , pattern, "'",
    " -v output_file='" , database_cleaned, "'",
    " '/^>/ { if ($0 !~ pattern) { if (header) { print header; print sequence } header = $0; sequence = \"\" } next } { sequence = sequence $0 } END { if (header) { print header; print sequence } }' ",
    raw_database,
    ">",
    database_cleaned
  )
  
  system(filter_command)
  
  awk_command <- paste0("wsl awk '/^>/ {print substr($0, 1, 50); next} 1' ", database_cleaned, " > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned)
  
  system(awk_command)
  
  command <- paste0('wsl temp_database_cleaned=$(mktemp)', 
                    ' && awk -v min_length=', min_sequence_length, 
                    ' -v RS=">" -v ORS="" \'{',
                    ' if (NR > 1) {',
                    '     header = ">" substr($0, 1, index($0, "\\n"));',
                    '     sequence = substr($0, index($0, "\\n")+1);',
                    '     gsub(/\\n/, "", sequence);',
                    '     if (length(sequence) >= min_length) {',
                    '         print header;',
                    '         print sequence;',
                    '         print "\\n";',
                    '     }',
                    ' }',
                    '}\' "', database_cleaned, '" > $temp_database_cleaned && ',
                    'mv $temp_database_cleaned "', database_cleaned, '"')
  
  system(command)
  
  system(paste0("wsl tr -d '[]\"' < ", database_cleaned, " > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned))
}


# Loading ...


library(rgbif)
library(sf)
library(concaveman)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(mapedit)

# Configurar a busca no GBIF
taxa <- "Pristis pristis"
continent <- c('north_america')
fields <- c("name", "species", "genus", "family", "decimalLongitude", "decimalLatitude", "basisOfRecord")
limit <- 400 # Número de registros por solicitação
start <- 0
all_data <- list()

observation_type <- "PRESERVED_SPECIMEN" # Filtro para observações de museu

# Buscar dados no GBIF
repeat {
  result <- occ_search(continent = continent, hasCoordinate = TRUE, scientificName = taxa, basisOfRecord = observation_type, fields = fields, limit = limit, start = start)
  if (nrow(result$data) == 0) break
  all_data <- append(all_data, list(result$data))
  start <- start + limit
}

# Combinar todos os dados em um único data frame
data <- do.call(rbind, all_data)

# Excluir linhas com coordenadas ausentes e onde a coluna "species" tem valores NA
data <- data[!is.na(data$decimalLongitude) & !is.na(data$decimalLatitude) & !is.na(data$species), ]

# Converter dados para sf
sf_data <- st_as_sf(data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
polygon <- concaveman(sf_data)

# Obter o mapa da América do Sul
south_america <- ne_countries(scale = "medium", continent = "South America", returnclass = "sf")

# Filtrar pontos dentro e fora da área terrestre da América do Sul
within_sa <- st_within(sf_data, south_america, sparse = FALSE)
within_sa_indices <- apply(within_sa, 1, any)
sf_data_within_sa <- sf_data[within_sa_indices, ]
sf_data_outside_sa <- sf_data[!within_sa_indices, ]

# Ensure the species column is a character vector
sf_data_within_sa$species <- as.character(sf_data_within_sa$species)

# Criar mapas interativos para seleção manual de pontos
map_within_sa <- leaflet(sf_data_within_sa) %>%
  addProviderTiles(providers$Esri) %>%
  addCircleMarkers(radius = 2, color = "blue", label = ~species)

map_outside_sa <- leaflet(sf_data_outside_sa) %>%
  addProviderTiles(providers$Esri) %>%
  addCircleMarkers(radius = 2, color = "red", label = ~species)

# Exibir mapas interativos
map_within_sa
map_outside_sa

# Selecionar manualmente pontos a serem excluídos
selected_within_sa <- editMap(map_within_sa, title = "Selecione os pontos a serem excluídos dentro da área terrestre")
selected_outside_sa <- editMap(map_outside_sa, title = "Selecione os pontos a serem excluídos fora da área terrestre")

# Converter os pontos selecionados em um objeto sf para manipulação
selected_within_sa_sf <- st_as_sf(selected_within_sa$finished)
selected_outside_sa_sf <- st_as_sf(selected_outside_sa$finished)

# Excluir pontos selecionados
sf_data_within_sa <- sf_data_within_sa[!st_intersects(sf_data_within_sa, selected_within_sa_sf, sparse = FALSE), ]
sf_data_outside_sa <- sf_data_outside_sa[!st_intersects(sf_data_outside_sa, selected_outside_sa_sf, sparse = FALSE), ]

# Criar mapas interativos para seleção manual de pontos
map_within_sa_ <- leaflet(sf_data_within_sa) %>%
  addProviderTiles(providers$Esri) %>%
  addCircleMarkers(radius = 2, color = "blue", label = ~species)

map_within_sa_

map_outside_sa_ <- leaflet(sf_data_outside_sa) %>%
  addProviderTiles(providers$Esri) %>%
  addCircleMarkers(radius = 2, color = "red", label = ~species)

