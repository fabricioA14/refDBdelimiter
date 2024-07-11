

# Example occurrence ID
occurrence_id <- data$gbifID

fetch_occurrence_data <- function(occurrence_id) {
  message("Fetching data for gbif ID: ", occurrence_id)
  result <- tryCatch({
    retry(
      expr = {
        occ_search(gbifId = occurrence_id, limit = 3000)
      },
      when = "error",
      max_tries = 5,
      interval = 2
    )
  }, error = function(e) {
    message("Error fetching data for ID: ", occurrence_id, " - ", e$message)
    return(NULL)
  })
  return(result)
}


excluded_occurrence_data_list <- list()

for (id in occurrence_id) {
  result <- fetch_occurrence_data(id)
  if (!is.null(result)) {
    excluded_occurrence_data_list <- append(excluded_occurrence_data_list, list(result))
  } else {
    message("No data returned for occurrence ID: ", id)
  }
}

# Adding the "species" information as a column in the "data" object
#initial_occurrence_data <- as.data.frame(
#  mutate(excluded_occurrence_data_list[1][[1]][[3]], 
#         query = excluded_occurrence_data_list[1][[1]][[4]][1][[1]][[1]][[3]])
#)


# Define the column names
column_names <- c(
  "key", "scientificName", "decimalLatitude", "decimalLongitude", "issues", "datasetKey", 
  "publishingOrgKey", "installationKey", "hostingOrganizationKey", "publishingCountry", 
  "protocol", "lastCrawled", "lastParsed", "crawlId", "basisOfRecord", "occurrenceStatus", 
  "taxonKey", "kingdomKey", "phylumKey", "orderKey", "familyKey", "genusKey", "speciesKey", 
  "acceptedTaxonKey", "acceptedScientificName", "kingdom", "phylum", "order", "family", 
  "genus", "species", "genericName", "specificEpithet", "taxonRank", "taxonomicStatus", 
  "iucnRedListCategory", "dateIdentified", "elevation", "elevationAccuracy", "continent", 
  "stateProvince", "higherGeography", "georeferencedBy", "year", "month", "day", "eventDate", 
  "startDayOfYear", "endDayOfYear", "lastInterpreted", "license", "isSequenced", "identifier", 
  "facts", "relations", "institutionKey", "collectionKey", "isInCluster", "recordedBy", 
  "identifiedBy", "preparations", "geodeticDatum", "countryCode", "recordedByIDs", 
  "identifiedByIDs", "gbifRegion", "country", "publishedByGbifRegion", "rightsHolder", 
  "georeferencedDate", "identifier.1", "institutionID", "georeferenceProtocol", 
  "georeferenceRemarks", "locality", "county", "collectionCode", "verbatimLocality", "gbifID", 
  "occurrenceID", "catalogNumber", "previousIdentifications", "institutionCode", "locationID", 
  "accessRights", "verbatimElevation", "collectionID", "georeferenceSources", "name", "query"
)

# Create an empty data frame with the specified column names
final_occurrence_data <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Define the types for final_occurrence_data columns (all as character for simplicity here)
final_occurrence_data[] <- lapply(final_occurrence_data, function(x) character(0))

# Loop through each element in excluded_occurrence_data_list
for (i in seq_along(excluded_occurrence_data_list)) {
  # Extract the current element
  current_element <- excluded_occurrence_data_list[i]
  
  # Add the "species" information as a column in the "data" object
  data_with_species <- as.data.frame(
    mutate(current_element[[1]][[3]], 
           query = current_element[[1]][[4]][1][[1]][[1]][["species"]])
  )
  
  # Convert all columns to character
  data_with_species[] <- lapply(data_with_species, as.character)
  
  # Combine data_with_species into final_occurrence_data
  final_occurrence_data <- bind_rows(final_occurrence_data, data_with_species)
}

# Ensure all columns are present, filling missing ones with NA
final_occurrence_data_ <- final_occurrence_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), NA, .)))

data <- final_occurrence_data_

#aaa <- subset(final_occurrence_data, country == "unknown or invalid")




















