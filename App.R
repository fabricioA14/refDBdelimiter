# List of packages to ensure are installed and loaded
pack <- c('tibble', 'rgbif', 'sf', 'concaveman', 'ggplot2', 'rnaturalearth','rnaturalearthdata','leaflet',
          'mapedit', 'leaflet.extras2', 'dplyr', 'RColorBrewer', 'leaflet.extras','shiny', 'htmlwidgets',
          'tidyr', 'retry', 'openxlsx', 'httr', 'jsonlite','bdc','tools','countrycode','data.table','stringr',
          'plotly','shinyFiles','shinyjs', 'plyr', 'taxize', 'taxizedb')

# Check for packages that are not installed
vars <- pack[!(pack %in% installed.packages()[, "Package"])]

# Install any packages that are not already installed
if (length(vars) != 0) {
  install.packages(vars, dependencies = TRUE)
}

# Load all the packages
sapply(pack, require, character.only = TRUE)

# Limit to process 20 GB (or adjust as needed)
#options(shiny.maxRequestSize = 20000 * 1024^2)

# Function to delete intermediate files
delete_intermediate_files <- function(files) {
  sapply(files, function(file) {
    if (file.exists(file)) {
      file.remove(file)
    }
  })
}

scrollable_legend_css <- "
.info.legend {
  max-height: calc(93vh - 93px); /* Adjust the height as needed */
  overflow-y: auto;
}
"

# Function to create interactive stacked bar plot
create_stacked_bar <- function(data, taxonomic_level, title) {
  data %>%
    group_by(year, !!sym(taxonomic_level)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    plot_ly(x = ~year, y = ~count, type = 'bar', color = as.formula(paste0("~", taxonomic_level)), colors = "Set3") %>%
    layout(title = title, barmode = 'stack', xaxis = list(title = 'Year'), yaxis = list(title = 'Count'))
}

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Comfortaa:wght@700&display=swap"),
    tags$style(HTML("
      body, .container-fluid {
        background-color: white;
        color: black;
        font-family: 'Comfortaa', sans-serif;
        font-weight: bold;
      }
      .well {
        background-color: white;
        border-color: black;
      }
      .nav-tabs > li > a {
        color: black;
        font-family: 'Comfortaa', sans-serif;
        font-weight: bold;
      }
      .nav-tabs > li > a:hover, .nav-tabs > li > a:focus, .nav-tabs > .active > a, .nav-tabs > .active > a:hover, .nav-tabs > .active > a:focus {
        background-color: white;
        color: black;
        font-family: 'Comfortaa', sans-serif;
        font-weight: bold;
      }
      h3, p {
        font-family: 'Comfortaa', sans-serif;
        font-weight: bold;
      }
      .title-panel {
        text-align: center;
        width: 100%;
      }
      .sidebar {
        background-color: white;
        border-color: black;
        height: 90vh;
        overflow-y: auto;
      }
      .btn-file, .btn-primary {
        background-color: #337ab7;
        color: white;
        border-color: #2e6da4;
      }
      .btn-file.selected, #run.selected, #run_tax.selected, #run_space.selected, #run_time.selected, #run_map.selected, #run_edit_map.selected, #save_data.selected {
        background-color: #337ab7;
        color: white;
        border-color: #2e6da4;
      }
      .shiny-input-container {
        margin-top: 15px;
      }
    ")),
    tags$style(type = "text/css", "#map {height: calc(90vh - 20px) !important;}"),
    tags$style(type = "text/css", "#edit_map {height: calc(90vh - 20px) !important;}"),
    tags$style(HTML(scrollable_legend_css))
  ),
  
  div(class = "title-panel", 
      titlePanel(
        tags$div(
          "Data Cleaning Processes",
          style = "display: inline-block; vertical-align: middle;"
        )
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Pre-Treatment Process",
                           shinyFilesButton("file1", "Choose CSV File", "Please select a file", multiple = FALSE, buttonType = "primary"),
                           numericInput("nrows", "Number of rows to read:", value = Inf, min = 1),
                           textInput("fields", "Columns to import (comma-separated):", 
                                     value = "gbifID,kingdom,phylum,class,order,family,genus,species,scientificName,countryCode,stateProvince,locality,decimalLongitude,decimalLatitude,basisOfRecord,year"),
                           selectInput("taxonomic_level_pre", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           textInput("save_path_pre", "Save Path for Pre-Treatment:", "1_bdc_PreProcess_cleaned"),
                           sliderInput("dist", "Distance for Inconsistent Coordinates (decimal degrees):",
                                       min = 0.01, max = 1.0, value = 0.1, step = 0.01),
                           checkboxInput("save_outputs", "Save Coordinates Output", TRUE),
                           checkboxGroupInput("formats", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("shp", "geojson", "gpkg", "kml", "csv")),
                           actionButton("run", "Run Pre-Treatment", class = "btn-primary")
                  ),
                  tabPanel("Taxonomy Process",
                           checkboxInput("replace_synonyms", "Replace synonyms by accepted names", TRUE),
                           checkboxInput("suggest_names", "Suggest names for misspelled names", TRUE),
                           sliderInput("suggestion_distance", "Distance between the searched and suggested names:", min = 0, max = 1, value = 0.9, step = 0.01),
                           textInput("db", "Taxonomic Database:", value = "gbif"),
                           textInput("rank_name", "Taxonomic Rank Name:", value = "Chordata"),
                           textInput("rank", "Taxonomic Rank:", value = "Phylum"),
                           selectInput("taxonomic_level_tax", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           checkboxInput("parallel", "Use parallel processing?", FALSE),
                           numericInput("ncores", "Number of cores:", value = 2, min = 1),
                           checkboxInput("export_accepted", "Export accepted names?", FALSE),
                           textInput("save_path_tax", "Save Path for Taxonomy:", "2_bdc_taxonomy_cleaned"),
                           checkboxGroupInput("formats_tax", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("shp", "geojson", "gpkg", "kml", "csv")),
                           actionButton("run_tax", "Run Taxonomy Process", class = "btn-primary")
                  ),
                  tabPanel("Space Process",
                           numericInput("ndec", "Number of decimals to be tested:", value = 3, min = 1),
                           selectInput("clustering_level", "Clustering Based on:",
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "class"),
                           checkboxGroupInput("tests", "Select spatial tests:",
                                              choices = list(
                                                "capitals" = "capitals",
                                                "centroids" = "centroids",
                                                "duplicates" = "duplicates",
                                                "equal" = "equal",
                                                "gbif" = "gbif",
                                                "institutions" = "institutions",
                                                "outliers" = "outliers",
                                                "zeros" = "zeros",
                                                "urban" = "urban"
                                              ),
                                              selected = c("capitals", "centroids", "duplicates", "equal", "gbif", "institutions", "zeros")),
                           numericInput("capitals_rad", "Radius for capitals (meters):", value = 3000, min = 0),
                           numericInput("centroids_rad", "Radius for centroids (meters):", value = 10000, min = 0),
                           selectInput("centroids_detail", "Detail level for centroids:", choices = c("both", "country", "province"), selected = "both"),
                           numericInput("inst_rad", "Radius around biodiversity institutions coordinates (meters):", value = 100, min = 0),
                           numericInput("outliers_mtp", "Multiplicative factor for outliers:", value = 5, min = 0),
                           numericInput("outliers_td", "Minimum distance of a record to all other records of a species to be identified as outlier (Km):", value = 1000, min = 0),
                           numericInput("outliers_size", "Minimum number of records in a dataset to run the taxon-specific outlier test:", value = 10, min = 0),
                           numericInput("range_rad", "Range radius:", value = 0, min = 0),
                           numericInput("zeros_rad", "Radius for zeros (decimal degrees):", value = 0.5, min = 0),
                           selectInput("taxonomic_level_space", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           textInput("save_path_space", "Save Path for Space:", "3_bdc_space_cleaned"),
                           checkboxGroupInput("formats_space", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("shp", "geojson", "gpkg", "kml", "csv")),
                           actionButton("run_space", "Run Space Process", class = "btn-primary")
                  ),
                  tabPanel("Time Process",
                           numericInput("year_threshold", "Year threshold:", value = 1950, min = 0),
                           selectInput("taxonomic_level_time", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           textInput("save_path_time", "Save Path for Time:", "4_bdc_time_cleaned"),
                           checkboxGroupInput("formats_time", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("shp", "geojson", "gpkg", "kml", "csv")),
                           actionButton("run_time", "Run Time Process", class = "btn-primary")
                  ),
                  tabPanel("Edit Map",
                           actionButton("run_edit_map", "Generate Edit Map", class = "btn-primary"),
                           leafletOutput("edit_map"),
                           verbatimTextOutput("searchedValuesOutput_edit")
                  ),
                  tabPanel("Save Data",
                           radioButtons("save_option", "Save Data:",
                                        choices = c("selected_occurrence_data", "excluded_occurrence_data", "both")),
                           textInput("save_path_selected", "Save Path for Selected Data:", "selected_data"),
                           textInput("save_path_excluded", "Save Path for Excluded Data:", "excluded_data"),
                           checkboxGroupInput("formats_edit", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("shp", "geojson", "gpkg", "kml", "csv")),
                           actionButton("save_data", "Save Data", class = "btn-primary")
                  ),
                  tabPanel("Make Database",
                           textInput("raw_database", "Raw Database:", value = "ncbiChordata.fasta"),
                           textInput("gbif_database", "GBIF Database:", value = "gbif_taxa_dataset.txt"),
                           textInput("final_output_database", "Final Output Database:", value = "Chordata_Ncbi_Gbif.fasta"),
                           numericInput("min_sequence_length", "Minimum Sequence Length:", value = 100, min = 1),
                           textInput("pattern", "Pattern:", value = "UNVERIFIED"),
                           checkboxInput("parse_seqids", "Parse SeqIDs:", TRUE),
                           selectInput("database_type", "Database Type:", choices = c("nucl", "prot"), selected = "nucl"),
                           textInput("title", "Title:", value = "local_database"),
                           textInput("out", "Database Name (out)", value = ""),
                           checkboxInput("hash_index", "Hash Index", FALSE),
                           textInput("mask_data", "Mask Data", value = ""),
                           textInput("mask_id", "Mask ID", value = ""),
                           textInput("mask_desc", "Mask Description", value = ""),
                           checkboxInput("gi_mask", "GI Mask", FALSE),
                           textInput("gi_mask_name", "GI Mask Name", value = ""),
                           #numericInput("blastdb_version", "BLAST DB Version", value = NULL, min = 1),
                           textInput("max_file_sz", "Max File Size", value = ""),
                           textInput("logfile", "Log File", value = ""),
                           textInput("taxid", "TaxID", value = ""),
                           textInput("taxid_map", "TaxID Map File", value = ""),
                           #checkboxInput("version", "Version", FALSE),
                           actionButton("run_make_database", "Run Make Database", class = "btn-primary")
                  ),
                  tabPanel("Taxonomic Assignment",
                           textInput("directory", "Directory:", value = ""),
                           textInput("database_file", "Database File:", value = ""),
                           numericInput("max_target_seqs", "Max Target Seqs:", value = 50, min = 1),
                           numericInput("perc_identity", "Percentage Identity:", value = 95, min = 0, max = 100, step = 1),
                           numericInput("qcov_hsp_perc", "Query Coverage HSP Percentage:", value = 95, min = 0, max = 100, step = 1),
                           numericInput("num_threads", "Number of Threads:", value = 6, min = 1),
                           numericInput("specie_threshold", "Specie Threshold:", value = 99, min = 0, max = 100, step = 1),
                           numericInput("genus_threshold", "Genus Threshold:", value = 97, min = 0, max = 100, step = 1),
                           numericInput("family_threshold", "Family Threshold:", value = 95, min = 0, max = 100, step = 1),
                           actionButton("run_blast", "Run BLAST", class = "btn-primary")
                  )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pre-Treatment Output",
                 plotlyOutput("pre_input_plot"),
                 plotlyOutput("pre_output_plot")
        ),
        tabPanel("Taxonomy Output",
                 plotlyOutput("tax_input_plot"),
                 plotlyOutput("tax_output_plot")
        ),
        tabPanel("Space Output",
                 plotlyOutput("space_input_plot"),
                 plotlyOutput("space_output_plot")
        ),
        tabPanel("Time Output",
                 plotlyOutput("time_input_plot"),
                 plotlyOutput("time_output_plot")
        ),
        tabPanel("Edit Map Output",
                 leafletOutput("edit_map")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive variables to store processed data
  pre_filtered_data <- reactiveVal(NULL)
  taxonomy_cleaned <- reactiveVal(NULL)
  space_cleaned <- reactiveVal(NULL)
  time_cleaned <- reactiveVal(NULL)
  selected_occurrence_data <- reactiveVal(NULL)
  excluded_occurrence_data <- reactiveVal(NULL)
  drawn_features <- reactiveVal(NULL)  # Reactive variable to store drawn features
  
  # Pre-Treatment Process
  shinyFileChoose(input, "file1", roots = c(wd = getwd()), session = session)
  
  file_path <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      file_selected <- parseFilePaths(c(wd = getwd()), input$file1)
      file_path(as.character(file_selected$datapath[1]))
      runjs("$('#file1').addClass('selected');")
    }
  })
  
  observeEvent(input$run, {
    if (is.null(file_path()) || file_path() == "")
      return(NULL)
    
    runjs("$('#run').addClass('selected');")
    
    nrows <- if (input$nrows == Inf) Inf else input$nrows
    fields <- strsplit(input$fields, ",")[[1]]
    
    data <- fread(file_path(), select = fields, nrows = nrows)
    
    dataPreProcess <- bdc_scientificName_empty(data, "scientificName") %>%
      bdc_coordinates_empty(lat = "decimalLatitude", lon = "decimalLongitude") %>%
      bdc_coordinates_outOfRange(lat = "decimalLatitude", lon = "decimalLongitude") %>%
      bdc_basisOfRecords_notStandard(basisOfRecord = "basisOfRecord", names_to_keep = "all")
    
    unique_country_codes <- unique(dataPreProcess$countryCode)
    country_names <- countrycode(unique_country_codes, origin = "iso2c", destination = "country.name")
    nome_map <- setNames(country_names, unique_country_codes)
    
    dataPreProcess <- dataPreProcess %>%
      mutate(country = nome_map[countryCode]) %>%
      select(gbifID, scientificName, countryCode, country, everything()) %>%
      bdc_country_from_coordinates(lat = "decimalLatitude", lon = "decimalLongitude", country = "country")
    
    cntr <- dataPreProcess$country %>%
      unique() %>%
      na.omit() %>%
      c()
    
    dataPreProcess <- bdc_coordinates_country_inconsistent(
      data = dataPreProcess,
      country = "country",
      country_name = cntr,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      dist = input$dist
    )
    
    xyFromLocality <- bdc_coordinates_from_locality(
      data = dataPreProcess,
      locality = "locality",
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      save_outputs = input$save_outputs
    )
    
    xyFromLocality[, c("country", "stateProvince", "locality")]
    
    dataPreProcess <- bdc_summary_col(data = dataPreProcess)
    
    pre_filtered <- dataPreProcess %>%
      dplyr::filter(.summary == TRUE) %>%
      bdc_filter_out_flags(data = ., col_to_remove = "all")
    
    pre_filtered_data(pre_filtered)  # Update reactive variable
    
    save_occurrence_data(pre_filtered, input$save_path_pre, formats = input$formats)
    
    output$pre_input_plot <- renderPlotly({
      create_stacked_bar(data, input$taxonomic_level_pre, "Raw Dataset by Year and Taxonomic Level")
    })
    
    output$pre_output_plot <- renderPlotly({
      create_stacked_bar(pre_filtered, input$taxonomic_level_pre, "Pre-Treated Dataset by Year and Taxonomic Level")
    })
  })
  
  rm(data, dataPreProcess, xyFromLocality)
  gc()
  
  # Taxonomy Process
  observeEvent(input$run_tax, {
    pre_filtered <- pre_filtered_data()
    if (is.null(pre_filtered))
      return(NULL)
    
    runjs("$('#run_tax').addClass('selected');")
    
    rank_lower <- tolower(input$rank)
    
    parse_names <- bdc_clean_names(
      sci_names = unique(pre_filtered$scientificName),
      save_outputs = FALSE
    )
    
    check_taxonomy <- dplyr::left_join(pre_filtered, parse_names, by = "scientificName")
    
    spl <- unique(check_taxonomy$names_clean) %>% sort()
    
    query_names <- bdc_query_names_taxadb(
      sci_name = spl,
      replace_synonyms = input$replace_synonyms,
      suggest_names = input$suggest_names,
      suggestion_distance = input$suggestion_distance,
      db = input$db,
      rank_name = input$rank_name,
      rank = rank_lower,
      parallel = input$parallel,
      ncores = input$ncores,
      export_accepted = input$export_accepted
    )
    
    nome_map <- setNames(query_names$scientificName, query_names$original_search)
    
    check_taxonomy$scientificName_updated <- nome_map[check_taxonomy$names_clean]
    
    check_taxonomy <- check_taxonomy %>%
      mutate(scientificName_updated = if_else(is.na(scientificName_updated), names_clean, scientificName_updated))
    
    taxonomy_cleaned_data <- check_taxonomy %>% select_if(~ !all(is.na(.)))
    
    taxonomy_cleaned(taxonomy_cleaned_data)  # Update reactive variable
    
    save_occurrence_data(taxonomy_cleaned_data, input$save_path_tax, formats = input$formats_tax)
    
    output$tax_input_plot <- renderPlotly({
      create_stacked_bar(pre_filtered, input$taxonomic_level_tax, "Pre-Treated Dataset by Year and Taxonomic Level")
    })
    
    output$tax_output_plot <- renderPlotly({
      create_stacked_bar(taxonomy_cleaned_data, input$taxonomic_level_tax, "Taxonomy-Cleaned Dataset by Year and Taxonomic Level")
    })
  })
  
  rm(pre_filtered)
  gc()
  
  # Space Process
  observeEvent(input$run_space, {
    taxonomy_cleaned_data <- taxonomy_cleaned()
    if (is.null(taxonomy_cleaned_data))
      return(NULL)
    
    runjs("$('#run_space').addClass('selected');")
    
    check_space <- bdc_coordinates_precision(
      data = taxonomy_cleaned_data,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      ndec = input$ndec
    )
    
    check_space <- CoordinateCleaner::clean_coordinates(
      x = check_space,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      species = input$clustering_level,
      tests = input$tests,
      capitals_rad = input$capitals_rad,
      centroids_rad = input$centroids_rad,
      centroids_detail = input$centroids_detail,
      inst_rad = input$inst_rad,
      outliers_mtp = input$outliers_mtp,
      outliers_td = input$outliers_td,
      outliers_size = input$outliers_size,
      range_rad = input$range_rad,
      zeros_rad = input$zeros_rad,
      value = "spatialvalid"
    ) %>%
      dplyr::tibble()
    
    check_space <- bdc_summary_col(data = check_space)
    
    space_cleaned_data <- check_space %>%
      dplyr::filter(.summary == TRUE) %>%
      bdc_filter_out_flags(data = ., col_to_remove = "all")
    
    space_cleaned_data <- space_cleaned_data %>%
      filter(!is.na(species) & species != "")
    
    space_cleaned(space_cleaned_data)  # Update reactive variable
    
    save_occurrence_data(space_cleaned_data, input$save_path_space, formats = input$formats_space)
    
    output$space_input_plot <- renderPlotly({
      create_stacked_bar(taxonomy_cleaned_data, input$taxonomic_level_space, "Taxonomy-Cleaned Dataset by Year and Taxonomic Level")
    })
    
    output$space_output_plot <- renderPlotly({
      create_stacked_bar(space_cleaned_data, input$taxonomic_level_space, "Spatially-Cleaned Dataset by Year and Taxonomic Level")
    })
  })
  
  rm(taxonomy_cleaned_data)
  gc()
  
  # Time Process
  observeEvent(input$run_time, {
    space_cleaned_data <- space_cleaned()
    if (is.null(space_cleaned_data))
      return(NULL)
    
    runjs("$('#run_time').addClass('selected');")
    
    year_threshold <- input$year_threshold
    save_path_time <- input$save_path_time
    formats_time <- input$formats_time
    
    check_time <- bdc_year_outOfRange(
      data = space_cleaned_data,
      eventDate = "year",
      year_threshold = year_threshold
    )
    
    check_time <- bdc_summary_col(data = check_time)
    
    time_cleaned_data <- check_time %>%
      dplyr::filter(.summary == TRUE) %>%
      bdc_filter_out_flags(data = ., col_to_remove = "all") %>%
      dplyr::tibble() 
    
    time_cleaned_data <- time_cleaned_data %>%
      select(-scientificName, -countryCode, -stateProvince, -locality, -basisOfRecord, -names_clean)
    
    
    time_cleaned(time_cleaned_data)  # Update reactive variable
    
    save_occurrence_data(time_cleaned_data, input$save_path_time, formats = input$formats_time)
    
    output$time_input_plot <- renderPlotly({
      create_stacked_bar(space_cleaned_data, input$taxonomic_level_time, "Spatially-Cleaned Dataset by Year and Taxonomic Level")
    })
    
    output$time_output_plot <- renderPlotly({
      create_stacked_bar(time_cleaned_data, input$taxonomic_level_time, "Time-Cleaned Dataset by Year and Taxonomic Level")
    })
  })
  
  # Edit Map Process
  observeEvent(input$run_edit_map, {
    runjs("$('#run_edit_map').addClass('selected');")
    time_cleaned_data <- time_cleaned()
    if (is.null(time_cleaned_data)) {
      showNotification("No data available for mapping. Please run the previous processes first.", type = "error")
      return(NULL)
    }
    
    visualization <- time_cleaned_data %>%
      mutate(identification_level = case_when(
        !is.na(species) & str_trim(species) != "" ~ "species",
        !is.na(genus) & str_trim(genus) != "" ~ "genus",
        !is.na(family) & str_trim(family) != "" ~ "family",
        !is.na(order) & str_trim(order) != "" ~ "order",
        !is.na(class) & str_trim(class) != "" ~ "class",
        !is.na(phylum) & str_trim(phylum) != "" ~ "phylum",
        !is.na(kingdom) & str_trim(kingdom) != "" ~ "kingdom",
        TRUE ~ "unknown"
      ))
    
    visualization <- visualization %>% rename(taxa = scientificName_updated)
    
    # Convert data to sf
    sf_data <- st_as_sf(visualization, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    # Define a color palette for the species
    qual_palette <- colorFactor(palette = brewer.pal(9, "Set1"), domain = visualization$taxa)
    
    # Leaflet map with drawing functionality
    map_within_sa_edit <- leaflet(visualization) %>%
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
    
            var searchBox = div.querySelector('#search-box');
    
            searchBox.onfocus = function() {
              myMap.dragging.disable();
              myMap.touchZoom.disable();
              myMap.doubleClickZoom.disable();
              myMap.scrollWheelZoom.disable();
            };
    
            searchBox.onblur = function() {
              myMap.dragging.enable();
              myMap.touchZoom.enable();
              myMap.doubleClickZoom.enable();
              myMap.scrollWheelZoom.enable();
            };
    
            div.querySelector('#search-button').onclick = function() {
              var searchValue = searchBox.value;
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
    
              // Use requestAnimationFrame for setting the cursor at the end
              requestAnimationFrame(function() {
                searchBox.setSelectionRange(searchBox.value.length, searchBox.value.length);
                searchBox.scrollLeft = searchBox.scrollWidth;
              });
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
              
              // Update reactive variable with drawn features
              Shiny.onInputChange('drawnFeaturesGlobal', layer.toGeoJSON());
              drawn_features(layer.toGeoJSON());
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
    
    output$edit_map <- renderLeaflet(map_within_sa_edit)
  })
  
  rm(visualization, sf_data)
  gc()
  
  # Save Data Process
  observeEvent(input$save_data, {
    runjs("$('#save_data').addClass('selected');")
    save_option <- input$save_option
    
    # Insert the logic for the Save Data input here
    time_cleaned_data <- time_cleaned()
    if (is.null(time_cleaned_data)) {
      showNotification("No data available for mapping. Please run the previous processes first.", type = "error")
      return(NULL)
    }
    
    visualization <- time_cleaned_data %>%
      mutate(identification_level = case_when(
        !is.na(species) & str_trim(species) != "" ~ "species",
        !is.na(genus) & str_trim(genus) != "" ~ "genus",
        !is.na(family) & str_trim(family) != "" ~ "family",
        !is.na(order) & str_trim(order) != "" ~ "order",
        !is.na(class) & str_trim(class) != "" ~ "class",
        !is.na(phylum) & str_trim(phylum) != "" ~ "phylum",
        !is.na(kingdom) & str_trim(kingdom) != "" ~ "kingdom",
        TRUE ~ "unknown"
      ))
    
    visualization <- visualization %>% rename(taxa = scientificName_updated)
    
    # Convert data to sf
    sf_data <- st_as_sf(visualization, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    # Check if the updatedFeatures.RData file exists in the current working directory
    if (!file.exists("updatedFeatures.RData")) {
      # Get unique species names and collapse into a string
      unique_taxa <- unique(visualization$taxa)
      collapsed_string <- paste(unique_taxa, collapse = "|")
      
      # Write collapsed string to file
      writeLines(collapsed_string, "gbif_taxa_dataset.txt")
      # Save all data from the visualization object
      save_occurrence_data(sf_data, input$save_path_selected, formats = input$formats_edit)
      showNotification("All data from the visualization object has been saved.", type = "message")
      return(NULL)
    }
    
    # Use the reactive variable to obtain the drawn data
    load("updatedFeatures.RData")
    if (is.null(updatedFeatures)) {
      showNotification("No drawn features found.", type = "error")
      return(NULL)
    }
    
    selected_sf <- extract_data(updatedFeatures)
    
    all_search_results_inside <- list()
    
    for (i in 1:nrow(selected_sf)) {
      name_to_search <- rownames(selected_sf[i, ])
      name_to_search <- remove_trailing_numbers(name_to_search)
      current_polygon <- selected_sf[i, 3]
      search_results <- sf_data[sf_data$taxa == name_to_search, ]
      if (nrow(search_results) > 0) {
        points_sf <- st_as_sf(search_results, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(selected_sf))
        within_polygon <- st_intersects(points_sf, current_polygon, sparse = FALSE)
        search_results_inside <- search_results[within_polygon, ]
      } else {
        search_results_inside <- data.frame()
      }
      all_search_results_inside[[i]] <- search_results_inside
    }
    
    all_search_results_inside <- Filter(function(x) nrow(x) > 0, all_search_results_inside)
    excluded_occurrence_data <- do.call(rbind, all_search_results_inside)
    
    sf_data$wkt <- st_as_text(sf_data$geometry)
    excluded_occurrence_data$wkt <- st_as_text(excluded_occurrence_data$geometry)
    coords <- sf::st_coordinates(excluded_occurrence_data)
    excluded_occurrence_data$decimalLongitude <- coords[, "X"]
    excluded_occurrence_data$decimalLatitude <- coords[, "Y"]
    
    unique_rows <- anti_join(as.data.frame(sf_data), as.data.frame(excluded_occurrence_data), 
                             by = c("gbifID", "species", "genus", "family", "order", "class", "phylum", "kingdom")) %>% select(-geometry)
    
    excluded_occurrence_data <- excluded_occurrence_data %>% select(-wkt) %>% st_drop_geometry()
    selected_occurrence_data <- st_as_sf(unique_rows, wkt = "wkt", crs = st_crs(sf_data))
    coords <- sf::st_coordinates(selected_occurrence_data)
    selected_occurrence_data$decimalLongitude <- coords[, "X"]
    selected_occurrence_data$decimalLatitude <- coords[, "Y"]
    selected_occurrence_data <- selected_occurrence_data %>% st_drop_geometry()
    
    # Get unique species names and collapse into a string
    unique_taxa <- unique(selected_occurrence_data$taxa)
    collapsed_string <- paste(unique_taxa, collapse = "|")
    
    # Write collapsed string to file
    writeLines(collapsed_string, "gbif_taxa_dataset.txt")
    
    if (save_option == "selected_occurrence_data" || save_option == "both") {
      save_occurrence_data(selected_occurrence_data, input$save_path_selected, formats = input$formats_edit)
    }
    if (save_option == "excluded_occurrence_data" || save_option == "both") {
      save_occurrence_data(excluded_occurrence_data, input$save_path_excluded, formats = input$formats_edit)
    }
  })
  
  # Make Database Process
  observeEvent(input$run_make_database, {
    runjs("$('#run_make_database').addClass('selected');")
    raw_database <- input$raw_database
    gbif_database <- input$gbif_database
    final_output_database <- input$final_output_database
    min_sequence_length <- input$min_sequence_length
    pattern <- input$pattern
    parse_seqids <- input$parse_seqids
    database_type <- input$database_type
    title <- input$title
    out <- input$out
    hash_index <- input$hash_index
    mask_data <- input$mask_data
    mask_id <- input$mask_id
    mask_desc <- input$mask_desc
    gi_mask <- input$gi_mask
    gi_mask_name <- input$gi_mask_name
    max_file_sz <- input$max_file_sz 
    logfile <- input$logfile
    taxid <- input$taxid
    taxid_map <- input$taxid_map
    
    # Define intermediate parameters
    database_cleaned <- "ncbiChordataToGbif.fasta"
    #cleaned_ncbi_database <- "ncbi_cleaned.fasta"
    
    # Call the function format_ncbi_database
    format_ncbi_database(raw_database, database_cleaned, min_sequence_length, pattern)
    # Delete intermediate file
    #delete_intermediate_files(c(raw_database))
    
    # Call the function subset_ncbi_based_on_gbif
    subset_ncbi_based_on_gbif(gbif_database, database_cleaned, final_output_database)
    # Delete intermediate file
    delete_intermediate_files(c(database_cleaned))
    
    # Call the function create_blast_db
    create_blast_db(final_output_database, parse_seqids, database_type, title, out, hash_index, mask_data, mask_id, mask_desc, gi_mask, gi_mask_name, max_file_sz, logfile, taxid, taxid_map)
    # Delete intermediate file
    #delete_intermediate_files(c(cleaned_ncbi_database))
  })
  
  # Adding reactives and observers for "Edit Map"
  searchedValues <- reactiveVal(character(0))
  drawnFeatures <- reactiveVal(list())
  
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
    save(updatedFeatures, file = "updatedFeatures.RData")
  })
  
  output$searchedValuesOutput_edit <- renderPrint({
    searchedValues()
  })
  
  observeEvent(input$run_blast, {
    Directory <- input$directory
    Database_File <- input$database_file
    max_target_seqs <- input$max_target_seqs
    perc_identity <- input$perc_identity
    qcov_hsp_perc <- input$qcov_hsp_perc
    num_threads <- input$num_threads
    Specie_Threshold <- input$specie_threshold
    Genus_Threshold <- input$genus_threshold
    Family_Threshold <- input$family_threshold
    
    blast_gibi(Directory, Database_File, max_target_seqs, perc_identity, qcov_hsp_perc, num_threads, Specie_Threshold, Genus_Threshold, Family_Threshold)
  })
}

shinyApp(ui, server)

