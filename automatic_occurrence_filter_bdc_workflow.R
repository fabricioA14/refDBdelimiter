library(cowplot)
library(rworldmap)
library(hexbin)

## %#######################################################################%##
## %##      IMPORTANT: # The results of the VALIDATION test              ##%##
## %##         used to flag data quality are appended in                 ##%##
## %##          separate columns in this database and                    ##%##
## %##     retrieved as TRUE (ok) or FALSE (check carefully).            ##%##
## %#######################################################################%##


# 1 - Records with missing species names
final_occurrence_data_ <-
  bdc_scientificName_empty(
    data = data,
    sci_name = "scientificName"
  )

final_occurrence_data_$.scientificName_empty %>% table() # this is the new column for missing species names (TRUE or FALSE)

# 2 - Records lacking information on geographic coordinates
# This is a VALIDATION. Flag records missing partial or complete information on geographic coordinates
# will be flagged as FALSE.
final_occurrence_data_ <- bdc_coordinates_empty(
  data = final_occurrence_data_,
  lat = "decimalLatitude",
  lon = "decimalLongitude"
)


final_occurrence_data_$.coordinates_empty %>% table() # this is the new column for flagging incomplete/missing coordinates (TRUE or FALSE)

# 3 - Records with out-of-range coordinates
# This is a VALIDATION. This test flags records with out-of-range coordinates:
# latitude > 90 or -90; longitude >180 or -180 (i.e. geographically impossible coordinates)
final_occurrence_data_ <- bdc_coordinates_outOfRange(
  data = final_occurrence_data_,
  lat = "decimalLatitude",
  lon = "decimalLongitude"
)

final_occurrence_data_$.coordinates_outOfRange %>% table()


# 4 - Records from doubtful sources
# This is a VALIDATION. This test flags records from doubtful source. For example, records from
# drawings, photographs, or multimedia objects, fossil records, among others.
final_occurrence_data_ <- bdc_basisOfRecords_notStandard(
  data = final_occurrence_data_,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all"
)

final_occurrence_data_$.basisOfRecords_notStandard %>% table()

final_occurrence_data_ %>% # if we explore the frequency of different sources we get this
  dplyr::group_by(basisOfRecord) %>%
  dplyr::summarise(n = dplyr::n())

# 5 - Getting country names from valid coordinates
# This is an ENRICHMENT function because it improves the database by deriving country names
# based on coordinates for records with missing country names.

final_occurrence_data_ <- bdc_country_from_coordinates(
  data = final_occurrence_data_,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country"
)


final_occurrence_data_ <- bdc_country_from_coordinates(
  data = final_occurrence_data_,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country"
)

final_occurrence_data_$country %>% unique()

# 6 - Standardizing country names and getting country code information
# This is an ENRICHMENT function because the country names are standardized against
# a list of country names in several languages retrieved from Wikipedia.
# Selecionar linhas únicas com base na coluna 'country'
# Function to remove characters after a specific character
remove_after <- function(x, char) {
  sub(paste0(char, ".*"), "", x)
}

# Apply the function to remove characters after "," or "("
final_occurrence_data_ <- final_occurrence_data_ %>%
  mutate(
    country = remove_after(country, ","),
    country = remove_after(country, "\\(") # Need to escape "(" with "\\"
  )

#final_occurrence_data_ <- bdc_country_standardized(
#  data = final_occurrence_data_,
#  country = "country"
#)

# original messy country names
final_occurrence_data_$country %>%
  unique() %>%
  sort()
# corrected and standardized country names
final_occurrence_data_$country %>%
  unique() %>%
  sort()
# It is good to check the names to be sure that the corrected country names were correctly matched
ctrn <- final_occurrence_data_ %>%
  dplyr::select(country, country) %>%
  unique() %>%
  arrange(country)

# 7 - Correcting latitude and longitude that have been "transposed"
# The mismatch between the country listed and coordinates can be the result of transposed
# signs (+ -) or coordinates. Once a mismatch is detected, different coordinate transformations are made
# to correct the country and coordinates mismatch. Verbatim coordinates are then replaced by the
# rectified ones in the returned database (a database containing verbatim and corrected
# coordinates can be created in the Output folder if save_outputs = TRUE). Records near
# countries coastline are not tested to avoid false positives.

final_occurrence_data_ <- final_occurrence_data_ %>% mutate(database_id = "gbif")  #######ESSE PROCESSO ESTÁ GERANDO CERTOS ERROS

final_occurrence_data_ <-
  bdc_coordinates_transposed(
    data = final_occurrence_data_,
    id = "database_id",
    sci_names = "scientificName",
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    country = "country",
    countryCode = "countryCode",
    border_buffer = 0.1, # in decimal degrees (~11 km at the equator)
    save_outputs = FALSE
  )

final_occurrence_data_$coordinates_transposed %>% table()  #######ESSE PROCESSO ESTÁ GERANDO CERTOS ERROS

# 8 - Records outside a region of interest
# This is a VALIDATION function because records outside one or multiple reference countries
# (i.e., records in other countries) or at an odd distance from the coast
# (e.g., in the ocean). This last step avoids flagging records close to country
# limits (e.g., records of coast or marshland species) as invalid.

# bdc_coordinates_country_inconsistent needs a vector with countries to be tested.
# In this case all countries; however, we can test only for countries where our species are native
# for instance, using Argentina, Brazil, Paraguay, and Bolivia.

cntr <- final_occurrence_data_$country %>%
  unique() %>%
  na.omit() %>%
  c()
cntr

# Just for this example lets test only those countries were both species used here naturally distribute
#cntr <- c(
#  "Brazil",
#  "Ecuador",
#  "Paraguay",
#  "Peru",
#  "Colombia",
#  "Guyana",
#  "French Guiana",
#  "Venezuela",
#  "Suriname",
#  "Bolivia"
#)

# Note that in this step we can test
final_occurrence_data_ <- bdc_coordinates_country_inconsistent(
  data = final_occurrence_data_,
  country = "country",
  country_name = cntr,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  dist = 0.1 # in decimal degrees (~5 km at the equator)
)

# 9 - Identifying records not geo-referenced but containing locality information
# ENRICHMENT. Coordinates can be derived from a detailed description of the locality associated with
# records in a process called retrospective geo-referencing.
xyFromLocality <- bdc_coordinates_from_locality(
  data = final_occurrence_data_,
  locality = "locality",
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  save_outputs = TRUE # This is for saving the output
)
xyFromLocality[, c("country", "stateProvince", "county", "locality")]
# Note that this create a new database with coordinates for records that were geo-referenced


final_occurrence_data_ <- bdc_summary_col(data = final_occurrence_data_) #######ESSE PROCESSO ESTÁ GERANDO CERTOS ERROS

# Creating a report summarizing the results of all tests.
report <-
  bdc_create_report(
    data = final_occurrence_data_,
    database_id = "gbif",
    workflow_step = "prefilter",
    save_report = TRUE # For saving report
  )

report

# Filtering the database
# We can remove records flagged as erroneous or suspect to obtain a cleaner database.
# We can use the column .summary to filter valid records
# passing in all tests (i.e., flagged as TRUE). Next, we use
# the bdc_filter_out_flags function to remove all tests columns (that is, those starting with '.').

pre_filtered_data <-
  final_occurrence_data_ %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all")


## %######################################################%##
#                                                          #
####                      TAXONOMY                      ####
#                                                          #
## %######################################################%##

##### 1) Clean and parse species names

# In case a large database is being used with many species could be more efficient
# use a vector with unique elements (i.e., species names)
# and then merge it with the original database.
# This step is important for helping update specie names (see next function)
parse_names <-
  bdc_clean_names(
    sci_names = unique(pre_filtered_data$scientificName), # unique of species names
    save_outputs = FALSE
  )
parse_names # note that we have names_clean column

# scientificName: original names supplied
# .uncer_terms: indicates the presence of taxonomic uncertainty terms (e.g., sp., aff., affin.)
# .infraesp_names: indicates the presence of infraspecific terms
# name_clean: scientific names resulting from the cleaning and parsing processes
# quality: an index indicating the quality of parsing process. It ranges from 0 to 4,
# being 1 no problem detected, 4 serious problems detected; a value of
# 0 indicates no interpretable name that was not parsed).

check_taxonomy <- dplyr::left_join(pre_filtered_data, parse_names, by = "scientificName")
check_taxonomy

check_taxonomy$scientificName %>% unique() # raw names
check_taxonomy$names_clean %>% unique() # now the names are cleaned and prepare to be checked
# and updated in a taxonomic authority database
# Note that despite species names being corrected, several names could be or not
# synonyms


##### 2) Names standardization
# See the taxonomic database available until now in bdc in:
# https://brunobrr.github.io/bdc/articles/taxonomy.html
# In this case we will use GBIF as taxonomic authority
#?bdc_query_names_taxadb

spl <- unique(check_taxonomy$names_clean) %>% sort() # list of raw species names

#filtered_vec <- spl[sapply(strsplit(spl, " "), length) > 1]

# If you have trouble with "bdc_query_names_taxadb" function see this issue about "duckbd"
# https://github.com/brunobrr/bdc/issues/233
# Run fs::dir_delete(taxadb:::taxadb_dir())  and the try use the function again

query_names <- bdc_query_names_taxadb(
  sci_name            = spl,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Apteronotidae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = FALSE, # should parallel processing be used?
  ncores              = 2, # number of cores to be used in the palatalization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)

View(query_names)

# This step is crucial, so it is recommended to check all names
# and update it by hand in case you need it. For instance, we can save the
# query_names database and manipulate it in a spreadsheet. For example, you could
# update the following columns by hand, if necessary:
# scientificName, taxonomicStatus, family, genus, specificEpithet, and infraspecificEpithet

# Additionally, there are three important worldwide taxonomic databases for plants that
# were not implemented in bdc

#### LCVP -Leipzig catalogue of vascular plants-
# data available at: https://idata.idiv.de/ddm/Data/ShowData/1806
# R package: https://idiv-biodiversity.github.io/lcvplants/index.html

#### WFO -World Flora Online-
# Website: http://www.worldfloraonline.org
# R package: https://pubmed.ncbi.nlm.nih.gov/33014632/
# WFO database: http://www.worldfloraonline.org/downloadData

#### POWO -Plants of the World Online-
# Website: https://powo.science.kew.org

# Defining a taxonomic authority is essential because it will determine the names
# of your species and consequently, the number of species (e.g., in case you
# are working with an entire family), and the analysis derived of these points
# (e.g., points used for SDM or for calculating Extent of Occurrence). In case
# you are working only with occurrences and species within a given country,
# you can decide to use the official taxonomic authority of this country
# (for instance, Flora do Brazil 2020 - http://floradobrasil.jbrj.gov.br/ - or United States Department of Agriculture -https://plants.usda.gov/home -).
# And remember to be explicit in your manuscript which taxonomic authority you used!

# In this example we will add the accepted names for the three names not found in GBIF
# Another way is to save the "query_names" database and and edit it in an Excel,
# read it in R, and integrate with your original database.

# We can see that for two names ("Chorisia chodatii", "Peltophorum adnatum",
# "Peltophorum berteroanum") GBIF database did not find their accepted names.
# However, if we check those names in Plant of The World Online we see that they
# are existing species names
# https://powo.science.kew.org/results?q=Chorisia%20chodatii
# https://powo.science.kew.org/results?q=Peltophorum%20adnatum
# https://powo.science.kew.org/results?q=%20Peltophorum%20berteroanum


#query_names[query_names$original_search == "Chorisia chodatii", "scientificName"] <- "Ceiba chodatii"
#query_names[query_names$original_search == "Peltophorum adnatum", "scientificName"] <- "Peltophorum dubium"
#query_names[query_names$original_search == "Peltophorum berteroanum", "scientificName"] <- "Peltophorum dubium"

##### 3) Merging results of accepted names with the original database
# Merging results of the taxonomy standardization process with the original database.
# See bdc package tutorial how merging from bdc_query_names_taxadb output


# Create a named vector with unique values
nome_map <- setNames(query_names$scientificName, query_names$original_search)

# Replace the values in the 'scientificName' column in check_taxonomy
check_taxonomy$scientificName_test <- nome_map[check_taxonomy$names_clean]

# Remove columns with only NA values
taxonomy_cleaned <- check_taxonomy %>% select_if(~ !all(is.na(.)))


## %######################################################%##
#                                                          #
####                       SPACE                        ####
#                                                          #
## %######################################################%##

#db <- readr::read_csv(file.path(getwd(), "Output", "Intermediate", "01_prefilter_database.csv"))

# 1) Flagging common spatial issues
# This function identifies records with a coordinate precision below a specified number
# of decimal places. For example, the precision of a coordinate with 1 decimal place is
# 11.132 km at the equator, i.e., the scale of a large city.

# The precision depends on the use of the data, for instance, in the
# context of species distribution models a higher precision is needed if
# you are using high resolution environmental variables

check_space <-
  bdc_coordinates_precision(
    data = taxonomy_cleaned,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    ndec = 3 # number of decimals to be tested
  )

#table(check_space$.rou) # 740 records have < 3 decimals

check_space %>%
  dplyr::filter(!.rou) %>%
  dplyr::select(starts_with("decimal"))


# 2) flag common spatial issues using functions of the package CoordinateCleaner.
?clean_coordinates

# the process of spatial cleaning is processed by species, therefor it is important
# the database has a column with cleaned and updated species names
# and is important define the taxonomic level desired to work (i.e. species or subspecies)
check_space <-
  CoordinateCleaner::clean_coordinates(
    x = check_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName", # Species names with genus and species
    countries = ,
    tests = c(
      "capitals", # records within 3km of capitals and province centroids
      "centroids", # records within 5km around country centroids
      "duplicates", # duplicated records
      "equal", # records with equal coordinates
      "gbif", # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 100m of zoo and herbaria
      # "outliers",     # outliers
      "zeros" # records with coordinates 0,0
      # "urban"         # records within urban areas
    ),
    capitals_rad = 3000,
    centroids_rad = 10000, # Be careful if species distribute in small countries
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 100, # remove zoo and herbaria within 100m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 1000,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>%
  dplyr::tibble()



# The '.summary' column summing up the results of all tests. Here, we update it
# to integrate results of bdc_coordinates_precision and clean_coordinates
check_space$.summary %>% table()
check_space <- bdc_summary_col(data = check_space)
check_space$.summary %>% table()

colSums(!dplyr::select(check_space, starts_with(".")))

# 4) Filtering database
space_cleaned <-
  check_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all")

# Remove columns with only NA values
space_cleaned <- space_cleaned %>% select_if(~ !all(is.na(.)))

#nrow(check_space)
#nrow(check_space_final)

## %######################################################%##
#                                                          #
####                        TIME                        ####
#                                                          #
## %######################################################%##

# time cleaning will depend of the use of the database. For instance,
# in species distribution model is not recommended use data from all time periods (i.e., <1950)
# because they could have low geographical precision

space_cleaned

space_cleaned$year %>% hist() # seems to be very old records in the database
space_cleaned$year %>% range(., na.rm = T)


# 1) Records lacking event date information
# VALIDATION. This  function flags records lacking event date information (e.g., empty or NA).

check_time <-
  bdc_eventDate_empty(data = space_cleaned, eventDate = "verbatimEventDate") %>% tibble()

# 2) Records with out-of-range collecting year
# VALIDATION. This function identifies records with illegitimate or potentially imprecise # collecting years. The year provided can be out-of-range (e.g., in the future) or
# collected before a specified year supplied by the user (e.g., 1950).
check_time <-
  bdc_year_outOfRange(
    data = check_time,
    eventDate = "year",
    year_threshold = 1950
  )


# Report
check_time <- bdc_summary_col(data = check_time)
report <-
  bdc_create_report(
    data = check_time,
    database_id = "database_id",
    workflow_step = "time",
    save_report = FALSE
  )

report

time_cleaned <-
  check_time %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all") %>%
  dplyr::tibble()


