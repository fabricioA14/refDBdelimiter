#' refDBdelimiter
#'
#' This function launches a Shiny app for preprocessing, analyzing, and visualizing biodiversity data.
#'
#' @param run_in_browser Logical. If TRUE, the app will run in the default web browser. If FALSE, the app will run in the RStudio viewer or the R default browser.
#'
#'
#' @examples{
#' \dontrun{
#' refDBdelimiter()
#' )}
#' }
#' @importFrom dplyr if_else select_if anti_join bind_cols
#' @importFrom shiny shinyApp fluidPage sidebarLayout sidebarPanel mainPanel tabsetPanel tabPanel textInput selectInput numericInput actionButton
#' @importFrom leaflet leaflet colorFactor leafletOutput renderLeaflet
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions selectedPathOptions
#' @importFrom shinyjs useShinyjs
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom sf st_polygon st_crs sf_use_s2
#' @importFrom tools toTitleCase
#' @importFrom bdc bdc_coordinates_precision bdc_query_names_taxadb bdc_scientificName_empty bdc_coordinates_empty bdc_coordinates_outOfRange bdc_clean_names bdc_basisOfRecords_notStandard bdc_country_from_coordinates bdc_coordinates_country_inconsistent bdc_coordinates_from_locality bdc_summary_col bdc_filter_out_flags bdc_year_outOfRange
#' @importFrom countrycode countrycode
#' @importFrom data.table fread
#' @export
refDBdelimiter <- function(run_in_browser = FALSE) {
  
  scrollable_legend_css <- "
.info.legend {
  max-height: calc(93vh - 93px); /* Adjust the height as needed */
  overflow-y: auto;
}
"

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
        position: relative;
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
      .output-tab {
        position: relative;
      }
      .output-tab::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center;
        opacity: 0.15;
        z-index: -1;
      }
    ")),
    tags$style(type = "text/css", "#map {height: calc(90vh - 20px) !important;}"),
    tags$style(type = "text/css", "#edit_map {height: calc(90vh - 20px) !important;}"),
    tags$style(HTML(scrollable_legend_css)),
  ),
  
  div(class = "title-panel", 
      tags$div(
        tags$img(src = "data:image/png;base64,", height = "80px", style = "opacity: 0.12; display: inline-block; vertical-align: middle;", id = "image_base64")
      ),
      tags$div(
        "refDBdelimiter",
        style = "position: absolute; top: 20px; left: 50%; transform: translateX(-50%); font-size: 30px; font-weight: bold; color: black; z-index: 1000; background-color: rgba(255, 255, 255, 0.5); padding: 5px; border-radius: 5px;"
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Preprocess",
                           textInput("input_file", "Input File Path:", value = ""),
                           selectInput("nrows_select", "Number of rows to read:", 
                                       choices = c("All" = "All", "Specific number" = "Specific"), 
                                       selected = "All"),
                           conditionalPanel(
                             condition = "input.nrows_select == 'Specific'",
                             numericInput("nrows", "Specify number of rows:", value = 1000, min = 1)
                           ),
                           selectInput("additional_fields", "Additional Columns to import:",
                                       choices = c("datasetKey", "occurrenceID", "infraspecificEpithet", "taxonRank", 
                                                   "verbatimScientificName", "verbatimScientificNameAuthorship", "occurrenceStatus", 
                                                   "individualCount", "publishingOrgKey", "coordinateUncertaintyInMeters", 
                                                   "coordinatePrecision", "elevation", "elevationAccuracy", "depth", "depthAccuracy", 
                                                   "eventDate", "day", "month", "taxonKey", "speciesKey", "institutionCode", 
                                                   "collectionCode", "catalogNumber", "recordNumber", "identifiedBy", "dateIdentified", 
                                                   "license", "rightsHolder", "recordedBy", "typeStatus", "establishmentMeans", 
                                                   "lastInterpreted", "mediaType", "issue"),
                                       multiple = TRUE),
                           selectInput("taxonomic_level_pre", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           textInput("save_path_pre", "Save Path for Preprocess:", "1_PreProcess_cleaned"),
                           sliderInput("dist", HTML('Distance for Inconsistent Coordinates <a id="dist_link" href="#">(dist)</a>:'), 
                                       min = 0.01, max = 1.0, value = 0.1, step = 0.01),
                           tags$div("Decimal Degrees", style = "margin-top: -10px; font-size: 11px; color: #999999;"),
                           tags$hr(style = "border: none; border-top: 1px solid #D8D8D8; margin-top: 0px;"),
                           checkboxInput("save_outputs", "Save Coordinates Output", FALSE),
                           checkboxGroupInput("formats", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("")),
                           actionButton("run", "Run Preprocess", class = "btn-primary")
                  ),
                  tabPanel("Taxonomic Filters",
                           checkboxInput("replace_synonyms", HTML('Replace synonyms by accepted names <a id="replace_synonyms_link" href="#">(replace_synonyms)</a>'), TRUE),
                           checkboxInput("suggest_names", HTML('Suggest names for misspelled names <a id="suggest_names_link" href="#">(suggest_names)</a>'), TRUE),
                           sliderInput("suggestion_distance", HTML('Distance between the searched and suggested names <a id="suggestion_distance_link" href="#">(suggestion_distance)</a>:'), min = 0, max = 1, value = 0.9, step = 0.01),
                           textInput("db", HTML('Taxonomic Database <a id="db_link" href="#">(db)</a>:'), value = "gbif"),
                           textInput("rank_name", HTML('Taxonomic Rank Name <a id="rank_name_link" href="#">(rank_name)</a>:'), value = "Chordata"),
                           selectInput("rank", HTML('Taxonomic Rank <a id="rank_link" href="#">(rank)</a>:'), 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "phylum"),
                           selectInput("taxonomic_level_tax", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           checkboxInput("parallel", HTML('Use parallel processing <a id="parallel_link" href="#">(parallel)</a>?'), TRUE),
                           numericInput("ncores", HTML('Number of cores <a id="ncores_link" href="#">(ncores)</a>:'), value = 4, min = 1),
                           checkboxInput("export_accepted", HTML('Export accepted names <a id="export_accepted_link" href="#">(export_accepted)</a>?'), FALSE),
                           textInput("save_path_tax", "Save Path for Taxonomy (output_file):", "2_taxonomy_cleaned"),
                           checkboxGroupInput("formats_tax", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("")),
                           actionButton("run_tax", "Run Taxonomic Filters", class = "btn-primary")
                  ),
                  tabPanel("Spatial Filters",
                           numericInput("ndec", HTML('Number of decimals to be tested <a id="ndec_link" href="#">(ndec)</a>:'), value = 3, min = 1),
                           selectInput("clustering_level", HTML('Clustering Based on <a id="clustering_level_link" href="#">(species)</a>:'), 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "species"),
                           checkboxGroupInput("tests", HTML('Select spatial tests <a id="tests_link" href="#">(tests)</a>:'), 
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
                                              selected = c("capitals", "centroids", "duplicates", "equal", "gbif", "institutions", "outliers", "zeros", "urban")),
                           numericInput("capitals_rad", HTML('Radius for capitals <a id="capitals_rad_link" href="#">(capitals_rad)</a>:'), value = 3000, min = 0),
                           tags$div("Meters", style = "margin-top: -10px; font-size: 11px; color: #999999;"),
                           tags$hr(style = "border: none; border-top: 1px solid #D8D8D8; margin-top: 0px;"),
                           numericInput("centroids_rad", HTML('Radius for centroids <a id="centroids_rad_link" href="#">(centroids_rad)</a>:'), value = 10000, min = 0),
                           tags$div("Meters", style = "margin-top: -10px; font-size: 11px; color: #999999;"),
                           tags$hr(style = "border: none; border-top: 1px solid #D8D8D8; margin-top: 0px;"),
                           selectInput("centroids_detail", HTML('Detail level for centroids <a id="centroids_detail_link" href="#">(centroids_detail)</a>:'), choices = c("both", "country", "province"), selected = "both"),
                           numericInput("inst_rad", HTML('Radius around biodiversity institutions coordinates (meters) <a id="inst_rad_link" href="#">(inst_rad)</a>:'), value = 100, min = 0),
                           tags$div("Meters", style = "margin-top: -10px; font-size: 11px; color: #999999;"),
                           tags$hr(style = "border: none; border-top: 1px solid #D8D8D8; margin-top: 0px;"),
                           numericInput("outliers_mtp", HTML('Multiplicative factor for outliers <a id="outliers_mtp_link" href="#">(outliers_mtp)</a>:'), value = 5, min = 0),
                           numericInput("outliers_td", HTML('Minimum distance of a record to all other records of a species to be identified as outlier <a id="outliers_td_link" href="#">(outliers_td)</a>:'), value = 1000, min = 0),
                           tags$div("Kilometers", style = "margin-top: -10px; font-size: 11px; color: #999999;"),
                           tags$hr(style = "border: none; border-top: 1px solid #D8D8D8; margin-top: 0px;"),
                           numericInput("outliers_size", HTML('Minimum number of records in a dataset to run the taxon-specific outlier test <a id="outliers_size_link" href="#">(outliers_size)</a>:'), value = 10, min = 0),
                           numericInput("range_rad", HTML('Range radius <a id="range_rad_link" href="#">(range_rad)</a>:'), value = 0, min = 0),
                           tags$div("Decimal degrees", style = "margin-top: -10px; font-size: 11px; color: #999999;"),
                           tags$hr(style = "border: none; border-top: 1px solid #D8D8D8; margin-top: 0px;"),
                           numericInput("zeros_rad", HTML('Radius for zeros <a id="zeros_rad_link" href="#">(zeros_rad)</a>:'), value = 0.5, min = 0),
                           tags$div("Decimal degrees", style = "margin-top: -10px; font-size: 11px; color: #999999;"),
                           tags$hr(style = "border: none; border-top: 1px solid #D8D8D8; margin-top: 0px;"),
                           selectInput("taxonomic_level_space", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           textInput("save_path_space", "Save Path for Space:", "3_space_cleaned"),
                           checkboxGroupInput("formats_space", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("")),
                           actionButton("run_space", "Run Spatial Filters", class = "btn-primary")
                  ),
                  tabPanel("Temporal Filters",
                           numericInput("year_threshold", HTML('Year threshold <a id="year_threshold_link" href="#">(year_threshold)</a>:'), value = 1950, min = 0),
                           selectInput("taxonomic_level_time", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           textInput("save_path_time", "Save Path for Time:", "4_time_cleaned"),
                           checkboxGroupInput("formats_time", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("")),
                           actionButton("run_time", "Run Temporal Filters", class = "btn-primary")
                  ),
                  tabPanel("Interactive Map",
                           checkboxInput("database_condition", "Are you building a Metabarcoding Database?", TRUE),
                           textInput("shp_path", "Enter Shapefile Path", value = ""),
                           actionButton("load_shp", "Load Shapefile", class = "btn-primary"),
                           actionButton("run_edit_map", "Generate Map", class = "btn-primary"),
                           leafletOutput("edit_map"),
                           verbatimTextOutput("searchedValuesOutput_edit")
                  ),
                  tabPanel("Save Data",
                           checkboxInput("genus_flexibility", "Genus Flexibility:", TRUE),
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
                                              selected = c("")),
                           actionButton("save_data", "Save Data", class = "btn-primary")
                  ),
                  tabPanel("Make Database",
                           checkboxInput("genus_flexibility", "Genus Flexibility:", TRUE),
                           textInput("raw_database", "Raw Database", value = "ncbi_dataset.fasta"),
                           textInput("gbif_database", "GBIF Database", value = ""),
                           textInput("final_output_database", "Output Database", value = "ncbi_gbif_db.fasta"),
                           checkboxInput("pattern_unverified", "Exclude UNVERIFIED Sequences", value = TRUE),
                           numericInput("min_sequence_length", "Minimum Sequence Length", value = 100, min = 1),
                           checkboxInput("parse_seqids", HTML('Parse SeqIDs <a id="parse_seqids_link" href="#">(parse_seqids)</a>'), TRUE),
                           selectInput("database_type", HTML('Database Type <a id="database_type_link" href="#">(dbtype)</a>'), choices = c("nucl", "prot"), selected = "nucl"),
                           textInput("title", HTML('Title <a id="title_link" href="#">(title)</a>'), value = "local_database"),
                           textInput("out", HTML('Database Name <a id="out_link" href="#">(out)</a>'), value = "local_database"),
                           checkboxInput("hash_index", HTML('Hash Index <a id="hash_index_link" href="#">(hash_index)</a>'), FALSE),
                           textInput("mask_data", HTML('Mask Data <a id="mask_data_link" href="#">(mask_data)</a>'), value = ""),
                           textInput("mask_id", HTML('Mask ID <a id="mask_id_link" href="#">(mask_id)</a>'), value = ""),
                           textInput("mask_desc", HTML('Mask Description <a id="mask_desc_link" href="#">(mask_desc)</a>'), value = ""),
                           checkboxInput("gi_mask", HTML('GI Mask <a id="gi_mask_link" href="#">(gi_mask)</a>'), FALSE),
                           textInput("gi_mask_name", HTML('GI Mask Name <a id="gi_mask_name_link" href="#">(gi_mask_name)</a>'), value = ""),
                           textInput("max_file_sz", HTML('Max File Size <a id="max_file_sz_link" href="#">(max_file_sz)</a>'), value = ""),
                           textInput("logfile", HTML('Log File <a id="logfile_link" href="#">(logfile)</a>'), value = "logfile.log"),
                           textInput("taxid", HTML('TaxID <a id="taxid_link" href="#">(taxid)</a>'), value = ""),
                           textInput("taxid_map", HTML('TaxID Map File <a id="taxid_map_link" href="#">(taxid_map)</a>'), value = ""),
                           actionButton("run_make_database", "Run Make Database", class = "btn-primary")
                  ),
                  tabPanel("Taxonomic Assignment",
                           textInput("directory", "Directory", value = ""),
                           actionButton("browse_dir", "Browse Directory", style = "margin-bottom: 0px;"),
                           textInput("database_file", HTML('Database File <a id="database_file_link" href="#">(db)</a>:'), value = ""),
                           textInput("query", HTML('Query File <a id="query_link" href="#">(query)</a>:'), value = "otus.fasta"),
                           textInput("otu_table", "OTU Table", value = "otu_table.txt"),   
                           #textInput("task", HTML('Task <a id="task_link" href="#">(task)</a>:'), value = "megablast"),          
                           textInput("out", HTML('Output File <a id="out_link" href="#">(out)</a>:'), value = "blast.txt"),    
                           numericInput("max_target_seqs", HTML('Max Target Seqs <a id="max_target_seqs_link" href="#">(max_target_seqs)</a>:'), value = 50, min = 1),
                           sliderInput("perc_identity", HTML('Percentage Identity <a id="perc_identity_link" href="#">(perc_identity)</a>:'), min = 0, max = 100, value = 95, step = 0.5),
                           sliderInput("qcov_hsp_perc", HTML('Query Coverage HSP Percentage <a id="qcov_hsp_perc_link" href="#">(qcov_hsp_perc)</a>:'), min = 0, max = 100, value = 95, step = 0.5),
                           sliderInput("specie_threshold", "Species Threshold:", min = 0, max = 100, value = 99, step = 0.5),
                           sliderInput("genus_threshold", "Genus Threshold:", min = 0, max = 100, value = 97, step = 0.5),
                           sliderInput("family_threshold", "Family Threshold:", min = 0, max = 100, value = 95, step = 0.5),
                           numericInput("num_threads", HTML('Number of Threads <a id="num_threads_link" href="#">(num_threads)</a>:'), value = 4, min = 1),
                           numericInput("penalty", HTML('Penalty <a id="penalty_link" href="#">(penalty)</a>:'), value = NA, min = -100, max = 0, step = 1), 
                           numericInput("reward", HTML('Reward <a id="reward_link" href="#">(reward)</a>:'), value = NA, min = 0, max = 100, step = 1),   
                           numericInput("evalue", HTML('E-value <a id="evalue_link" href="#">(evalue)</a>:'), value = NA, min = 0),                      
                           numericInput("word_size", HTML('Word Size <a id="word_size_link" href="#">(word_size)</a>:'), value = NA, min = 1),                 
                           numericInput("gapopen", HTML('Gap Open Penalty <a id="gapopen_link" href="#">(gapopen)</a>:'), value = NA, min = 0),             
                           numericInput("gapextend", HTML('Gap Extend Penalty <a id="gapextend_link" href="#">(gapextend)</a>:'), value = NA, min = 0),         
                           numericInput("max_hsps", HTML('Max HSPs <a id="max_hsps_link" href="#">(max_hsps)</a>:'), value = NA, min = 1),                    
                           numericInput("xdrop_ungap", HTML('Xdrop Ungap <a id="xdrop_ungap_link" href="#">(xdrop_ungap)</a>:'), value = NA, min = 0),             
                           numericInput("xdrop_gap", HTML('Xdrop Gap <a id="xdrop_gap_link" href="#">(xdrop_gap)</a>:'), value = NA, min = 0),                  
                           numericInput("xdrop_gap_final", HTML('Xdrop Gap Final <a id="xdrop_gap_final_link" href="#">(xdrop_gap_final)</a>:'), value = NA, min = 0),      
                           numericInput("searchsp", HTML('Search Space <a id="searchsp_link" href="#">(searchsp)</a>:'), value = NA, min = 0),                
                           numericInput("sum_stats", HTML('Sum Stats <a id="sum_stats_link" href="#">(sum_stats)</a>:'), value = NA),                          
                           numericInput("no_greedy", HTML('No Greedy <a id="no_greedy_link" href="#">(no_greedy)</a>:'), value = NA),                          
                           numericInput("min_raw_gapped_score", HTML('Min Raw Gapped Score <a id="min_raw_gapped_score_link" href="#">(min_raw_gapped_score)</a>:'), value = NA, min = 0), 
                           textInput("template_type", HTML('Template Type <a id="template_type_link" href="#">(template_type)</a>:'), value = ""),                      
                           numericInput("template_length", HTML('Template Length <a id="template_length_link" href="#">(template_length)</a>:'), value = NA, min = 0),      
                           textInput("dust", HTML('DUST Options <a id="dust_link" href="#">(dust)</a>:'), value = ""),                               
                           textInput("filtering_db", HTML('Filtering DB <a id="filtering_db_link" href="#">(filtering_db)</a>:'), value = ""),                        
                           textInput("window_masker_taxid", HTML('Window Masker Taxid <a id="window_masker_taxid_link" href="#">(window_masker_taxid)</a>:'), value = ""),          
                           textInput("window_masker_db", HTML('Window Masker DB <a id="window_masker_db_link" href="#">(window_masker_db)</a>:'), value = ""),                
                           numericInput("soft_masking", HTML('Soft Masking <a id="soft_masking_link" href="#">(soft_masking)</a>:'), value = NA),                     
                           numericInput("ungapped", HTML('Ungapped <a id="ungapped_link" href="#">(ungapped)</a>:'), value = NA),                             
                           numericInput("culling_limit", HTML('Culling Limit <a id="culling_limit_link" href="#">(culling_limit)</a>:'), value = NA, min = 1),          
                           numericInput("best_hit_overhang", HTML('Best Hit Overhang <a id="best_hit_overhang_link" href="#">(best_hit_overhang)</a>:'), value = NA, min = 0),  
                           numericInput("best_hit_score_edge", HTML('Best Hit Score Edge <a id="best_hit_score_edge_link" href="#">(best_hit_score_edge)</a>:'), value = NA, min = 0), 
                           numericInput("subject_besthit", HTML('Subject Besthit <a id="subject_besthit_link" href="#">(subject_besthit)</a>:'), value = NA),               
                           numericInput("window_size", HTML('Window Size <a id="window_size_link" href="#">(window_size)</a>:'), value = NA, min = 1),              
                           numericInput("off_diagonal_range", HTML('Off Diagonal Range <a id="off_diagonal_range_link" href="#">(off_diagonal_range)</a>:'), value = NA, min = 0),
                           numericInput("use_index", HTML('Use Index <a id="use_index_link" href="#">(use_index)</a>:'), value = NA),                           
                           textInput("index_name", HTML('Index Name <a id="index_name_link" href="#">(index_name)</a>:'), value = ""),                            
                           numericInput("lcase_masking", HTML('Lcase Masking <a id="lcase_masking_link" href="#">(lcase_masking)</a>:'), value = NA),                   
                           textInput("query_loc", HTML('Query Loc <a id="query_loc_link" href="#">(query_loc)</a>:'), value = ""),                              
                           textInput("strand", HTML('Strand <a id="strand_link" href="#">(strand)</a>:'), value = ""),                                   
                           numericInput("parse_deflines", HTML('Parse Deflines <a id="parse_deflines_link" href="#">(parse_deflines)</a>:'), value = NA),                
                           #numericInput("outfmt", "Output Format", value = 6),                           
                           numericInput("show_gis", HTML('Show GIS <a id="show_gis_link" href="#">(show_gis)</a>:'), value = NA),                            
                           numericInput("num_descriptions", HTML('Num Descriptions <a id="num_descriptions_link" href="#">(num_descriptions)</a>:'), value = NA, min = 1),    
                           numericInput("num_alignments", HTML('Num Alignments <a id="num_alignments_link" href="#">(num_alignments)</a>:'), value = NA, min = 1),       
                           numericInput("line_length", HTML('Line Length <a id="line_length_link" href="#">(line_length)</a>:'), value = NA, min = 1),              
                           numericInput("html", HTML('HTML <a id="html_link" href="#">(html)</a>:'), value = NA),                                   
                           textInput("sorthits", HTML('Sort Hits <a id="sorthits_link" href="#">(sorthits)</a>:'), value = ""),                               
                           textInput("sorthsps", HTML('Sort HSPs <a id="sorthsps_link" href="#">(sorthsps)</a>:'), value = ""),                              
                           numericInput("mt_mode", HTML('MT Mode <a id="mt_mode_link" href="#">(mt_mode)</a>:'), value = NA, min = 0),                   
                           numericInput("remote", HTML('Remote <a id="remote_link" href="#">(remote)</a>:'), value = NA),                                 
                           actionButton("run_blast", "Run BLAST", class = "btn-primary")                                 
                  ),
                  tabPanel("GBIF Submission",
                           DT::dataTableOutput("editable_table"),
                           actionButton("save_table", "Save Table"))
                  
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Preprocess",
                 div(class = "output-tab",
                     plotlyOutput("pre_input_plot"),
                     plotlyOutput("pre_output_plot")
                 )
        ),
        tabPanel("Taxonomic",
                 div(class = "output-tab",
                     plotlyOutput("tax_input_plot"),
                     plotlyOutput("tax_output_plot")
                 )
        ),
        tabPanel("Spatial",
                 div(class = "output-tab",
                     plotlyOutput("space_input_plot"),
                     plotlyOutput("space_output_plot")
                 )
        ),
        tabPanel("Temporal",
                 div(class = "output-tab",
                     plotlyOutput("time_input_plot"),
                     plotlyOutput("time_output_plot")
                 )
        ),
        tabPanel("Map",
                 div(class = "output-tab",
                     leafletOutput("edit_map")
                 )
        ),
        tabPanel("GBIF Submission", 
                 tabsetPanel(
                   tabPanel("Otu Table",
                            div(class = "output-tab",
                                fileInput("file_upload", "Upload Otu Table", accept = c(".csv", ".xlsx")),
                                actionButton("load_data", "Load Data"),
                                dataTableOutput("otu_table"),
                                actionButton("save_otu_table", "Save Otu Table")
                            )
                   ),
                   tabPanel("Taxonomic Information",
                            fluidRow(
                              column(5, 
                                     fileInput("taxonomic_assignment_file", "Upload Taxonomic Assignment", accept = c(".txt", ".csv", ".xlsx"))
                              ),
                              column(5, 
                                     fileInput("otus_file", "Upload Otus", accept = c(".fasta", ".fas"))
                              )
                            ),
                            actionButton("load_taxonomic_data", "Load Data"),
                            div(class = "output-tab",
                                dataTableOutput("taxonomic_table")
                            ),
                            actionButton("save_tax_data", "Save Taxonomic Data", class = "btn-primary")
                   ),
                   tabPanel("Samples",
                            div(class = "output-tab",
                                h4("Optional Fields"),
                                selectInput("optional_fields", "Select Optional Fields:",
                                            choices = c("gbifID", "datasetKey", "occurrenceID", "countryCode", "locality", "stateProvince",
                                                        "occurrenceStatus", "individualCount", "publishingOrgKey", "decimalLatitude", "decimalLongitude",
                                                        "coordinateUncertaintyInMeters", "coordinatePrecision", "elevation", "elevationAccuracy", "depth",
                                                        "depthAccuracy", "day", "month", "year", "basisOfRecord", "institutionCode", "collectionCode",
                                                        "catalogNumber", "recordNumber", "identifiedBy", "dateIdentified", "license", "rightsHolder",
                                                        "recordedBy", "typeStatus", "establishmentMeans", "lastInterpreted", "mediaType", "issue")
                                            ,
                                            multiple = TRUE),
                                actionButton("add_columns", "Add Selected Columns"),
                                dataTableOutput("samples_table"),
                                actionButton("save_samples", "Save Samples Data")
                            )
                   ),
                   tabPanel("Default Values",
                            div(class = "output-tab",
                                dataTableOutput("editable_table"),
                                actionButton("save_default_values", "Save Table", class = "btn-primary")
                            )
                   )
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Reactive variables to store processed data
  pre_filtered_data <- reactiveVal(NULL)
  taxonomy_cleaned <- reactiveVal(NULL)
  space_cleaned <- reactiveVal(NULL)
  time_cleaned <- reactiveVal(NULL)
  selected_occurrence_data <- reactiveVal(NULL)
  excluded_occurrence_data <- reactiveVal(NULL)
  drawn_features <- reactiveVal(NULL)
  polygon_data <- reactiveVal(NULL)
  shapefile_path_state <- reactiveVal(NULL)
  edited_map <- reactiveVal(NULL)
  
  onclick("replace_synonyms_link", {
    browseURL("https://brunobrr.github.io/bdc/reference/bdc_query_names_taxadb.html")
  })
  
  ids <- c("replace_synonyms_link", "suggest_names_link", "suggestion_distance_link", "db_link", "rank_name_link", "rank_link", 
           "parallel_link", "ncores_link", "export_accepted_link")
  
  lapply(ids, function(id) {
    onclick(id, {
      browseURL("https://brunobrr.github.io/bdc/reference/bdc_query_names_taxadb.html")
    })
  })
  
  onclick("ndec_link", {
    browseURL("https://brunobrr.github.io/bdc/reference/bdc_coordinates_precision.html")
  })
  
  ids1 <- c("clustering_level_link", "tests_link", "capitals_rad_link", "centroids_rad_link",
            "centroids_detail_link", "inst_rad_link", "outliers_mtp_link", "outliers_td_link",
            "outliers_size_link", "range_rad_link", "zeros_rad_link")
  
  lapply(ids1, function(id) {
    onclick(id, {
      browseURL("https://www.rdocumentation.org/packages/CoordinateCleaner/versions/3.0.1/topics/clean_coordinates")
    })
  })
  
  onclick("year_threshold_link", {
    browseURL("https://brunobrr.github.io/bdc/reference/bdc_year_outOfRange.html")
  })
  
  ids2 <- c("parse_seqids_link", "database_type_link", "title_link", "out_link", "hash_index_link",
            "mask_data_link", "mask_id_link", "mask_desc_link", "gi_mask_link", "gi_mask_name_link",
            "max_file_sz_link", "logfile_link", "taxid_link", "taxid_map_link")
  
  lapply(ids2, function(id) {
    onclick(id, {
      browseURL("https://www.ncbi.nlm.nih.gov/books/NBK279684/table/appendices.T.makeblastdb_application_opt/")
    })
  })
  
  ids3 <- c("database_file_link", "query_link", "task_link", "out_link",
            "max_target_seqs_link", "perc_identity_link", "qcov_hsp_perc_link",
            "num_threads_link", "penalty_link", "reward_link",
            "evalue_link", "word_size_link", "gapopen_link", "gapextend_link", "max_hsps_link", "xdrop_ungap_link",
            "xdrop_gap_link", "xdrop_gap_final_link", "searchsp_link", "sum_stats_link", "no_greedy_link",
            "min_raw_gapped_score_link", "template_type_link", "template_length_link", "dust_link", "filtering_db_link",
            "window_masker_taxid_link", "window_masker_db_link", "soft_masking_link", "ungapped_link",
            "culling_limit_link", "best_hit_overhang_link", "best_hit_score_edge_link", "subject_besthit_link",
            "window_size_link", "off_diagonal_range_link", "use_index_link", "index_name_link", "lcase_masking_link",
            "query_loc_link", "strand_link", "parse_deflines_link", "show_gis_link", "num_descriptions_link",
            "num_alignments_link", "line_length_link", "html_link", "sorthits_link", "sorthsps_link", "mt_mode_link",
            "remote_link")
  
  lapply(ids3, function(id) {
    onclick(id, {
      browseURL("https://www.ncbi.nlm.nih.gov/books/NBK279684/table/appendices.T.blastn_application_options/")
    })
  })
  
  observe({
    #img_path <- "C:/Users/fabricio/Desktop/fabricioA14/refDBdelimiter/www/refdb.png"
    img_path <- system.file("www", "refdb.png", package = "refDBdelimiter")
    if (file.exists(img_path)) {
      img_base64 <- base64enc::dataURI(file = img_path, mime = "image/png")
      runjs(sprintf('document.getElementById("image_base64").src = "%s";', img_base64))
    } else {
      showNotification("Image not found", type = "error")
    }
  })
  
  # Pre-Treatment Process
  observeEvent(input$run, {
    if (input$input_file == "")
      return(NULL)
    
    runjs("$('#run').addClass('selected');")
    
    nrows <- if (input$nrows_select == "All") Inf else as.numeric(input$nrows)
    default_fields <- c("gbifID", "kingdom", "phylum", "class", "order", "family", "genus", "species", 
                        "scientificName", "countryCode", "stateProvince", "locality", "decimalLongitude", 
                        "decimalLatitude", "basisOfRecord", "year")
    additional_fields <- input$additional_fields
    fields <- c(default_fields, additional_fields)
    
    data <- fread(input$input_file, select = fields, nrows = nrows)
    
    sf_use_s2(FALSE)
    
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
    
    if (!is.null(input$formats) && length(input$formats) > 0) {
      refDB_SaveOccurrenceData(pre_filtered, input$save_path_pre, formats = input$formats)
      if (file.exists(input$save_path_pre)) {
        showNotification("Pre-Treatment data saved successfully.", type = "message")
      } else {
        showNotification("Error saving Pre-Treatment data.", type = "error")
      }
    } else {
      showNotification("No save format selected. Data not saved.", type = "warning")
    }
    
    output$pre_input_plot <- renderPlotly({
      refDB_CreateStackedBar(data, input$taxonomic_level_pre, "Raw Dataset by Year and Taxonomic Level", input$taxonomic_level_pre)
    })
    
    output$pre_output_plot <- renderPlotly({
      refDB_CreateStackedBar(pre_filtered, input$taxonomic_level_pre, "Pre-Treated Dataset by Year and Taxonomic Level", input$taxonomic_level_pre)
    })
    
    # Automatically switch to the "Preprocess" tab in the main panel
    updateTabsetPanel(session, "main_tabs", selected = "Preprocess")
    
  })
  
  suppressWarnings({
    rm(data, dataPreProcess, xyFromLocality)
  })
  #invisible(capture.output(gc()))
  
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
    
    #taxonomy_cleaned_data <- check_taxonomy %>% select_if(~ !all(is.na(.)))
    
    columns_to_keep <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
    
    check_taxonomy <- check_taxonomy %>%
      select(all_of(columns_to_keep)) %>%
      bind_cols(check_taxonomy %>%
                  select(-all_of(columns_to_keep)) %>%
                  select_if(~ !all(is.na(.))))
    
    taxonomy_cleaned_data <- check_taxonomy[,c(8:11,1:7,12:ncol(check_taxonomy))]
    
    rm(check_taxonomy)
    
    taxonomy_cleaned(taxonomy_cleaned_data)  # Update reactive variable
    
    if (!is.null(input$formats_tax) && length(input$formats_tax) > 0) {
      refDB_SaveOccurrenceData(taxonomy_cleaned_data, input$save_path_tax, formats = input$formats_tax)
      if (file.exists(input$save_path_tax)) {
        showNotification("Taxonomy data saved successfully.", type = "message")
      } else {
        showNotification("Error saving Taxonomy data.", type = "error")
      }
    } else {
      showNotification("No save format selected. Data not saved.", type = "warning")
    }
    
    output$tax_input_plot <- renderPlotly({
      refDB_CreateStackedBar(pre_filtered, input$taxonomic_level_tax, "Pre-Treated Dataset by Year and Taxonomic Level", input$taxonomic_level_tax)
    })
    
    output$tax_output_plot <- renderPlotly({
      refDB_CreateStackedBar(taxonomy_cleaned_data, input$taxonomic_level_tax, "Taxonomy-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_tax)
    })
    
    # Automatically switch to the "Taxonomic" tab in the main panel
    updateTabsetPanel(session, "main_tabs", selected = "Taxonomic")
    
  })
  
  suppressWarnings({
    rm(pre_filtered)
  })
  #invisible(capture.output(gc()))
  
  
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
    
    if (!is.null(input$formats_space) && length(input$formats_space) > 0) {
      refDB_SaveOccurrenceData(space_cleaned_data, input$save_path_space, formats = input$formats_space)
      if (file.exists(input$save_path_space)) {
        showNotification("Space data saved successfully.", type = "message")
      } else {
        showNotification("Error saving Space data.", type = "error")
      }
    } else {
      showNotification("No save format selected. Data not saved.", type = "warning")
    }
    
    output$space_input_plot <- renderPlotly({
      refDB_CreateStackedBar(taxonomy_cleaned_data, input$taxonomic_level_space, "Taxonomy-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_space)
    })
    
    output$space_output_plot <- renderPlotly({
      refDB_CreateStackedBar(space_cleaned_data, input$taxonomic_level_space, "Spatially-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_space)
    })
    
    # Automatically switch to the "Spatial" tab in the main panel
    updateTabsetPanel(session, "main_tabs", selected = "Spatial")
    
  })
  
  suppressWarnings({
    rm(taxonomy_cleaned_data)
  })
  #invisible(capture.output(gc()))
  
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
    
    time_cleaned_data <- time_cleaned_data %>%
      mutate(scientificName_updated = case_when(
        !is.na(scientificName_updated) ~ scientificName_updated, 
        !is.na(species) & species != "" ~ species,               
        !is.na(genus) & genus != "" ~ genus,                     
        !is.na(family) & family != "" ~ family,                  
        !is.na(order) & order != "" ~ order,                     
        !is.na(class) & class != "" ~ class,                     
        !is.na(phylum) & phylum != "" ~ phylum,                  
        !is.na(kingdom) & kingdom != "" ~ kingdom,               
        TRUE ~ scientificName_updated                            
      ))
    
    time_cleaned(time_cleaned_data)  # Update reactive variable
    
    if (!is.null(formats_time) && length(formats_time) > 0) {
      refDB_SaveOccurrenceData(time_cleaned_data, save_path_time, formats = formats_time)
      if (file.exists(save_path_time)) {
        showNotification("Time data saved successfully.", type = "message")
      } else {
        showNotification("Error saving Time data.", type = "error")
      }
    } else {
      showNotification("No save format selected. Data not saved.", type = "warning")
    }
    
    output$time_input_plot <- renderPlotly({
      refDB_CreateStackedBar(space_cleaned_data, input$taxonomic_level_time, "Spatially-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_time)
    })
    
    output$time_output_plot <- renderPlotly({
      refDB_CreateStackedBar(time_cleaned_data, input$taxonomic_level_time, "Time-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_time)
    })
    
    # Automatically switch to the "Temporal" tab in the main panel
    updateTabsetPanel(session, "main_tabs", selected = "Temporal")
    
  })
  
  # Load Shapefile Process
  observeEvent(input$load_shp, {
    shapefile_path <- input$shp_path
    if (shapefile_path == "") {
      polygon_data(NULL)
      shapefile_path_state(NULL)
      #showNotification("No shapefile path provided. Previous shapefile cleared.", type = "warning")
    } else {
      # Clear any previously loaded shapefile
      polygon_data(NULL)
      shapefile_path_state(NULL)
      if (file.exists(shapefile_path)) {
        polygon <- tryCatch({
          st_read(shapefile_path)
        }, error = function(e) {
          showNotification("Error reading shapefile", type = "error")
          NULL
        })
        if (!is.null(polygon)) {
          polygon_data(st_transform(polygon, 4326))  # Transform the CRS of the polygon to match the points
          shapefile_path_state(shapefile_path)
          showNotification("Shapefile loaded successfully.", type = "message")
        }
      } else {
        showNotification("Shapefile not found.", type = "error")
      }
    }
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
    
    # Ensure scientificName_updated column exists
    if ("scientificName_updated" %in% colnames(visualization)) {
      visualization <- visualization %>% dplyr::rename(taxa = scientificName_updated)
    }
    
    # Convert data to sf
    sf_data <- st_as_sf(visualization, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    # Filter points within the polygon, if a polygon is loaded
    if (!is.null(polygon_data())) {
      sf_data <- sf_data[st_intersects(sf_data, polygon_data(), sparse = FALSE), ]
    }
    
    # Check if any points remain after filtering
    if (!is.null(polygon_data()) && nrow(sf_data) == 0) {
      showNotification("No point is inside the loaded polygon.", type = "error")
      return()
    }
    
    # Convert the filtered sf_data back to a data.frame with separate longitude and latitude columns
    visualization <- sf_data %>%
      st_drop_geometry() %>%               # Remove the geometry column
      mutate(
        decimalLongitude = st_coordinates(sf_data)[, 1],  # Extract longitude
        decimalLatitude = st_coordinates(sf_data)[, 2]    # Extract latitude
      )
    
    if (input$database_condition == TRUE) {
      visualization <- visualization %>%
        distinct(taxa, .keep_all = TRUE)
    }
    
    # Define a color palette for the species
    qual_palette <- colorFactor(palette = brewer.pal(9, "Set1"), domain = visualization$taxa)
    
    # Initialize the map
    map_within_sa_edit <- leaflet(visualization) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
    
    # Add polygon
    if (!is.null(polygon_data())) {
      map_within_sa_edit <- map_within_sa_edit %>%
        addPolygons(data = polygon_data(), color = "blue", weight = 2, fillOpacity = 0.2)
    }
    
    # Map
    map_within_sa_edit <- map_within_sa_edit %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addCircleMarkers(
        lng = ~decimalLongitude, lat = ~decimalLatitude,
        radius = 3,
        color = ~qual_palette(taxa),
        label = ~taxa,
        popup = ~paste("Taxa:", taxa, "<br>Genus:", genus, "<br>Family:", family, "<br>Order:", order, "<br>Class:", class, "<br>Phylum:", phylum, "<br>Level:", toTitleCase(identification_level)),
        layerId = ~paste0(taxa, "_", decimalLongitude, "_", decimalLatitude)
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
      htmlwidgets::onRender("
function(el, x) {
  var myMap = this;
  var originalLayers = {};
  var uniqueTaxa = new Set();

  myMap.eachLayer(function(layer) {
    if (layer.options && layer.options.layerId) {
      originalLayers[layer.options.layerId] = layer;
      uniqueTaxa.add(layer.options.layerId);
    }
  });

  var selectedTaxa = Array.from(uniqueTaxa);
  selectedTaxa.forEach(function(taxon) {
    Object.values(originalLayers).forEach(function(layer) {
      if (layer.options.layerId === taxon) {
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

    // Adjust the search box position directly with inline style
    div.style.marginBottom = '20px'; // Adjust as needed
    div.style.padding = '10px';
    div.style.backgroundColor = '#ffffff';
    div.style.border = '2px solid #ddd';
    div.style.borderRadius = '5px';

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
        selectedTaxa = Array.from(uniqueTaxa);
        selectedTaxa.forEach(function(taxon) {
          Object.values(originalLayers).forEach(function(layer) {
            if (layer.options.layerId === taxon) {
              myMap.addLayer(layer);
            }
          });
        });
      } else {
        var foundLayers = new Set();
        taxaArray.forEach(function(taxon) {
          Object.values(originalLayers).forEach(function(layer) {
            var layerGroup = layer.options.layerId;
            var isGenusSearch = taxon.endsWith(' (genus)');
            var isFamilySearch = taxon.endsWith(' (family)');
            var isOrderSearch = taxon.endsWith(' (order)');
            var isClassSearch = taxon.endsWith(' (class)');
            var isPhylumSearch = taxon.endsWith(' (phylum)');
            var taxonWithoutSuffix = taxon.replace(' (genus)', '')
                                          .replace(' (family)', '')
                                          .replace(' (order)', '')
                                          .replace(' (class)', '')
                                          .replace(' (phylum)', '');

            if (isGenusSearch) {
              if (layer.options.popup.includes('Genus: ' + taxonWithoutSuffix + ' ')) {
                myMap.addLayer(layer);
                foundLayers.add(layer.options.layerId);
              }
            } else if (isFamilySearch) {
              if (layer.options.popup.includes('Family: ' + taxonWithoutSuffix + ' ')) {
                myMap.addLayer(layer);
                foundLayers.add(layer.options.layerId);
              }
            } else if (isOrderSearch) {
              if (layer.options.popup.includes('Order: ' + taxonWithoutSuffix + ' ')) {
                myMap.addLayer(layer);
                foundLayers.add(layer.options.layerId);
              }
            } else if (isClassSearch) {
              if (layer.options.popup.includes('Class: ' + taxonWithoutSuffix + ' ')) {
                myMap.addLayer(layer);
                foundLayers.add(layer.options.layerId);
              }
            } else if (isPhylumSearch) {
              if (layer.options.popup.includes('Phylum: ' + taxonWithoutSuffix + ' ')) {
                myMap.addLayer(layer);
                foundLayers.add(layer.options.layerId);
              }
            } else {
              if (layerGroup.includes(taxonWithoutSuffix)) {
                myMap.addLayer(layer);
                foundLayers.add(layer.options.layerId);
              }
            }
          });
        });
        selectedTaxa = Array.from(foundLayers);
      }

      Shiny.setInputValue('selected_taxa', selectedTaxa, {priority: 'event'});
      Shiny.setInputValue('search_box', searchValue, {priority: 'event'});

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
      
      Shiny.onInputChange('drawnFeaturesGlobal', layer.toGeoJSON());
      drawn_features(layer.toGeoJSON());
    }
  });

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
    legend.style.width = (maxWidth + 50) + 'px';
  }
}
")%>%
      addDrawToolbar(
        targetGroup = 'drawn',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polylineOptions = FALSE
      )
    
    output$edit_map <- renderLeaflet(map_within_sa_edit)
    
    #time_cleaned(visualization)
    
    edited_map(visualization)
    
  })
  
  # Save Data Process
  observeEvent(input$save_data, {
    runjs("$('#save_data').addClass('selected');")
    save_option <- input$save_option
    genus_flexibility <- input$genus_flexibility
    
    # Insert the logic for the Save Data input here
    time_cleaned_data <- edited_map()
    if (is.null(time_cleaned_data)) {
      showNotification("No data available for saving. Please run the previous processes first.", type = "error")
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
    
    # Convert data to sf
    sf_data <- st_as_sf(visualization, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    # Check if the updatedFeatures.RData file exists in the current working directory
    if (!file.exists("updatedFeatures.RData")) {
      if (genus_flexibility) {
        species_chunks <- unique(visualization$taxa[str_detect(visualization$taxa, "\\s")])
        genus_chunks <- unique(str_extract(species_chunks, "^[^\\s]+"))
        collapsed_string <- paste0(paste(genus_chunks, collapse = "|"), "|")
      } else {
        species_chunks <- unique(visualization$taxa[str_detect(visualization$taxa, "\\s")])
        species_chunks <- gsub("\\s", "_", species_chunks)
        collapsed_string <- paste0(paste(species_chunks, collapse = "|"), "|")
      }
      
      # Write collapsed string to file
      writeLines(collapsed_string, "gbif_taxa_dataset.txt")
      
      if ("scientificName_updated" %in% colnames(sf_data)) {
      sf_data <- sf_data %>% dplyr::rename(taxa = scientificName_updated)
      }
      
      # Save all data from the visualization object
      if (!is.null(input$formats_edit) && length(input$formats_edit) > 0) {
        refDB_SaveOccurrenceData(sf_data, input$save_path_selected, formats = input$formats_edit)
        showNotification("All data from the visualization object has been saved.", type = "message")
      } else {
        showNotification("No save format selected. Data not saved.", type = "warning")
      }
      return(NULL)
    }
    
    # Use the reactive variable to obtain the drawn data
    load("updatedFeatures.RData")
    if (is.null(updatedFeatures)) {
      showNotification("No drawn features found.", type = "error")
      return(NULL)
    }
    
    selected_sf <- refDB_ExtractData(updatedFeatures)
    
    all_search_results_inside <- list()
    
    if ("scientificName_updated" %in% colnames(sf_data)) {
    sf_data <- sf_data %>% dplyr::rename(taxa = scientificName_updated)
    }
    
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
    
    if (genus_flexibility) {
      species_chunks <- unique(selected_occurrence_data$taxa[str_detect(selected_occurrence_data$taxa, "\\s")])
      genus_chunks <- unique(str_extract(species_chunks, "^[^\\s]+"))
      collapsed_string <- paste0(paste(genus_chunks, collapse = "|"), "|")
    } else {
      species_chunks <- unique(selected_occurrence_data$taxa[str_detect(selected_occurrence_data$taxa, "\\s")])
      species_chunks <- gsub("\\s", "_", species_chunks)
      collapsed_string <- paste0(paste(species_chunks, collapse = "|"), "|")
    }
    
    # Write collapsed string to file
    writeLines(collapsed_string, "gbif_taxa_dataset.txt")
    
    if (save_option == "selected_occurrence_data" || save_option == "both") {
      if (!is.null(input$formats_edit) && length(input$formats_edit) > 0) {
        refDB_SaveOccurrenceData(selected_occurrence_data, input$save_path_selected, formats = input$formats_edit)
        Sys.sleep(5)
        if (file.exists(input$save_path_selected)) {
          #showNotification("Selected occurrence data saved successfully.", type = "message")
        } else {
          #showNotification("Error saving selected occurrence data.", type = "error")
        }
      } else {
        showNotification("No save format selected for selected occurrence data. Data not saved.", type = "warning")
      }
    }
    if (save_option == "excluded_occurrence_data" || save_option == "both") {
      if (!is.null(input$formats_edit) && length(input$formats_edit) > 0) {
        refDB_SaveOccurrenceData(excluded_occurrence_data, input$save_path_excluded, formats = input$formats_edit)
        Sys.sleep(5)
        if (file.exists(input$save_path_excluded)) {
          #showNotification("Excluded occurrence data saved successfully.", type = "message")
        } else {
          #showNotification("Error saving excluded occurrence data.", type = "error")
        }
      } else {
        showNotification("No save format selected for excluded occurrence data. Data not saved.", type = "warning")
      }
    }
  })
  
  # Cleanup global variables
  rm(list = c("searchedValuesGlobal", "drawnFeaturesGlobal"), envir = .GlobalEnv)
  
  # Make Database Process
  observeEvent(input$run_make_database, {
    runjs("$('#run_make_database').addClass('selected');")
    raw_database <- input$raw_database
    gbif_database <- input$gbif_database
    final_output_database <- input$final_output_database
    min_sequence_length <- input$min_sequence_length
    pattern <- if (input$pattern_unverified) "UNVERIFIED" else ""
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
    genus_flexibility <- input$genus_flexibility
    
    # Define intermediate parameters
    #database_cleaned <- "ncbiChordataToGbif.fasta"
    
    # Call the function refDB_FormatNcbiDatabase
    refDB_FormatNcbiDatabase(raw_database, database_cleaned, min_sequence_length, pattern)
    
    if (file.exists(database_cleaned)) {
      showNotification("NCBI Database formatted successfully.", type = "message")
    } else {
      showNotification("Error formatting NCBI Database.", type = "error")
    }
    
    # Conditionally call the appropriate function
    if (gbif_database != "") {
      refDB_SubsetNcbiGbif(gbif_database, database_cleaned, final_output_database, genus_flexibility)
    } else {
      refDB_ncbiToMakeblastdb(database_cleaned, final_output_database)
    }
    
    if (file.exists(final_output_database)) {
      showNotification("NCBI Database created successfully.", type = "message")
    } else {
      showNotification("Error creating final database.", type = "error")
    }
    
    # Call the function refDB_CreateBlastDB
    refDB_CreateBlastDB(final_output_database, parse_seqids, database_type, title, out, hash_index, mask_data, mask_id, mask_desc, gi_mask, gi_mask_name, max_file_sz, logfile, taxid, taxid_map)
    
    if (file.exists(out)) {
      showNotification("BLAST Database created successfully.", type = "message")
    } else {
      showNotification("Error creating BLAST database.", type = "error")
    }
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
    
    # Cleanup global variables
    rm(list = c("searchedValuesGlobal", "drawnFeaturesGlobal"), envir = .GlobalEnv)
  })
  
  output$searchedValuesOutput_edit <- renderPrint({
    searchedValues()
    
    suppressWarnings({
      rm(searchedValuesGlobal, drawnFeaturesGlobal)
    })
    
  })
  
  #Taxonomic Assignment
  
  # To browse the directory
  observeEvent(input$browse_dir, {
  dir <- rstudioapi::selectDirectory()
  if(!is.na(dir)) {
    updateTextInput(session, "directory", value = dir)
  }
  })
  
# To update the Family, Genus, and Species sliders reactively
rv <- reactiveValues(prev_f = NULL, prev_g = NULL, prev_s = NULL, busy = FALSE)

# Initialize previous values (only once at startup)
observe({
  if (is.null(rv$prev_f)) {
    rv$prev_f <- input$family_threshold
    rv$prev_g <- input$genus_threshold
    rv$prev_s <- input$specie_threshold
  }
})

# Observe any change in the three sliders
observeEvent(
  list(input$family_threshold, input$genus_threshold, input$specie_threshold),
  ignoreInit = TRUE,
  {
    # Prevent infinite loops while updating
    if (rv$busy) return()
    rv$busy <- TRUE

    # Current slider values
    f <- input$family_threshold
    g <- input$genus_threshold
    s <- input$specie_threshold

    # Start with the assumption that new values = current values
    new_f <- f; new_g <- g; new_s <- s

    # Detect which slider the user moved (compare with previous values)
    if (!is.null(rv$prev_s) && s != rv$prev_s) {
      # User moved Species slider
      if (s < g) {
        new_g <- s            # pull Genus down
        if (new_f > new_g) new_f <- new_g  # pull Family down if needed
      }
      # If s > g, no adjustment required
    } else if (!is.null(rv$prev_g) && g != rv$prev_g) {
      # User moved Genus slider
      if (g < f) {
        new_f <- g            # pull Family down
      }
      if (g > s) {
        new_s <- g            # push Species up if Genus > Species
      }
    } else if (!is.null(rv$prev_f) && f != rv$prev_f) {
      # User moved Family slider
      if (f > g) {
        new_g <- f            # push Genus up if Family > Genus
        if (new_g > s) new_s <- new_g  # push Species up if needed
      }
    }

    # Ensure values stay within [0, 100]
    new_f <- max(0, min(100, new_f))
    new_g <- max(0, min(100, new_g))
    new_s <- max(0, min(100, new_s))

    # Update sliders only if values have changed
    if (!identical(new_f, f)) {
      updateSliderInput(session, "family_threshold", value = new_f)
    }
    if (!identical(new_g, g)) {
      updateSliderInput(session, "genus_threshold", value = new_g)
    }
    if (!identical(new_s, s)) {
      updateSliderInput(session, "specie_threshold", value = new_s)
    }

    # Save current values as "previous" for the next change detection
    rv$prev_f <- new_f
    rv$prev_g <- new_g
    rv$prev_s <- new_s

    rv$busy <- FALSE
  }
)
  
  observeEvent(input$run_blast, {
    Directory <- input$directory
    Database_File <- input$database_file
    query <- input$query
    otu_table <- input$otu_table
    task <- "megablast"
    out <- input$out
    max_target_seqs <- input$max_target_seqs
    perc_identity <- input$perc_identity
    qcov_hsp_perc <- input$qcov_hsp_perc
    num_threads <- input$num_threads
    Family_Threshold <- input$family_threshold
    Genus_Threshold <- max(input$genus_threshold, Family_Threshold)
    Specie_Threshold <- max(input$specie_threshold, Genus_Threshold)
    #Specie_Threshold <- input$specie_threshold
    #Genus_Threshold <- input$genus_threshold
    #Family_Threshold <- input$family_threshold
    penalty <- if (is.na(input$penalty)) NULL else input$penalty
    reward <- if (is.na(input$reward)) NULL else input$reward
    evalue <- if (is.na(input$evalue)) NULL else input$evalue
    word_size <- if (is.na(input$word_size)) NULL else input$word_size
    gapopen <- if (is.na(input$gapopen)) NULL else input$gapopen
    gapextend <- if (is.na(input$gapextend)) NULL else input$gapextend
    max_hsps <- if (is.na(input$max_hsps)) NULL else input$max_hsps
    xdrop_ungap <- if (is.na(input$xdrop_ungap)) NULL else input$xdrop_ungap
    xdrop_gap <- if (is.na(input$xdrop_gap)) NULL else input$xdrop_gap
    xdrop_gap_final <- if (is.na(input$xdrop_gap_final)) NULL else input$xdrop_gap_final
    searchsp <- if (is.na(input$searchsp)) NULL else input$searchsp
    sum_stats <- if (is.na(input$sum_stats)) NULL else input$sum_stats
    no_greedy <- if (is.na(input$no_greedy)) NULL else input$no_greedy
    min_raw_gapped_score <- if (is.na(input$min_raw_gapped_score)) NULL else input$min_raw_gapped_score
    template_type <- if (input$template_type == "") NULL else input$template_type
    template_length <- if (is.na(input$template_length)) NULL else input$template_length
    dust <- if (input$dust == "") NULL else input$dust
    filtering_db <- if (input$filtering_db == "") NULL else input$filtering_db
    window_masker_taxid <- if (input$window_masker_taxid == "") NULL else input$window_masker_taxid
    window_masker_db <- if (input$window_masker_db == "") NULL else input$window_masker_db
    soft_masking <- if (is.na(input$soft_masking)) NULL else input$soft_masking
    ungapped <- if (is.na(input$ungapped)) NULL else input$ungapped
    culling_limit <- if (is.na(input$culling_limit)) NULL else input$culling_limit
    best_hit_overhang <- if (is.na(input$best_hit_overhang)) NULL else input$best_hit_overhang
    best_hit_score_edge <- if (is.na(input$best_hit_score_edge)) NULL else input$best_hit_score_edge
    subject_besthit <- if (is.na(input$subject_besthit)) NULL else input$subject_besthit
    window_size <- if (is.na(input$window_size)) NULL else input$window_size
    off_diagonal_range <- if (is.na(input$off_diagonal_range)) NULL else input$off_diagonal_range
    use_index <- if (is.na(input$use_index)) NULL else input$use_index
    index_name <- if (input$index_name == "") NULL else input$index_name
    lcase_masking <- if (is.na(input$lcase_masking)) NULL else input$lcase_masking
    query_loc <- if (input$query_loc == "") NULL else input$query_loc
    strand <- if (input$strand == "") NULL else input$strand
    parse_deflines <- if (is.na(input$parse_deflines)) NULL else input$parse_deflines
    #outfmt <- input$outfmt
    outfmt <- 6 
    show_gis <- if (is.na(input$show_gis)) NULL else input$show_gis
    num_descriptions <- if (is.na(input$num_descriptions)) NULL else input$num_descriptions
    num_alignments <- if (is.na(input$num_alignments)) NULL else input$num_alignments
    line_length <- if (is.na(input$line_length)) NULL else input$line_length
    html <- if (is.na(input$html)) NULL else input$html
    sorthits <- if (input$sorthits == "") NULL else input$sorthits
    sorthsps <- if (input$sorthsps == "") NULL else input$sorthsps
    mt_mode <- if (is.na(input$mt_mode)) NULL else input$mt_mode
    remote <- if (is.na(input$remote)) NULL else input$remote
    
    refDB_Blast(Directory, Database_File, otu_table, query, task, out, max_target_seqs, perc_identity, qcov_hsp_perc, num_threads, 
                Specie_Threshold, Genus_Threshold, Family_Threshold, penalty, reward, evalue, word_size, gapopen, 
                gapextend, max_hsps, xdrop_ungap, xdrop_gap, xdrop_gap_final, searchsp, sum_stats, no_greedy, 
                min_raw_gapped_score, template_type, template_length, dust, filtering_db, window_masker_taxid, 
                window_masker_db, soft_masking, ungapped, culling_limit, best_hit_overhang, best_hit_score_edge, 
                subject_besthit, window_size, off_diagonal_range, use_index, index_name, lcase_masking, query_loc, 
                strand, parse_deflines, outfmt, show_gis, num_descriptions, num_alignments, line_length, html, 
                sorthits, sorthsps, mt_mode, remote)
    
    if (file.exists(out)) {
      #showNotification("BLAST run completed successfully.", type = "message")
    } else {
      #showNotification("Error running BLAST.", type = "error")
    }
  })
  
  # Placeholder for the Otu Table with no default data
  otu_values <- reactiveVal(data.frame())
  
  # Reactive value to store the samples data
  samples_data <- reactiveVal(data.frame())
  
  # Render the editable Otu Table
  output$otu_table <- renderDT({
    datatable(otu_values(), 
              selection = list(
                mode = "multiple",     # Allow multiple cell selection
                target = "cell",       # Target cell selection
                selected = NULL,       # No initial selection
                class = "selected"     # Class applied to selected cells
              ),
              editable = list(
                target = 'cell', 
                disable = list(
                  columns = c(0)   # Disable editing for the first column
                )
              ),
              options = list(
                dom = 'fti',            # Multiple values for DOM: length, filter, table, info, pagination, processing
                ordering = FALSE,          # Disable column ordering
                rownames = FALSE,          # Disable row names
                pageLength = 15,           # Show 15 rows per page
                lengthMenu = c(10, 15, 20),# Options for number of rows per page
                autoWidth = FALSE,         # Disable auto width for columns
                scrollX = TRUE,            # Enable horizontal scrolling
                scrollY = "400px",         # Vertical scroll with height of 400 pixels
                searching = TRUE,          # Enable search functionality
                info = TRUE,               # Show table information
                paging = TRUE,             # Enable pagination
                columnDefs = list(
                  list(targets = 0, className = "dt-left unselectable"), # Apply CSS class to the first column
                  list(width = '200px', targets = "_all") # Define a minimum width for all columns
                )
              )
    )
  }, server = FALSE)
  
  # Load data from file input
  observeEvent(input$load_data, {
    req(input$file_upload)
    file <- input$file_upload
    
    # Read data based on file type
    if (grepl("\\.csv$", file$name)) {
      otu_data <- read.csv(file$datapath)
    } else if (grepl("\\.xlsx$", file$name)) {
      otu_data <- readxl::read_xlsx(file$datapath)
    } else if (grepl("\\.txt$", file$name)) {
      otu_data <- read.delim(file$datapath)
    }
    
    # Remove the name of the first column
    colnames(otu_data)[1] <- " "
    
    otu_values(otu_data)
    
    # Initialize the Samples Table with mandatory columns and row names from OTU Table
    sample_cols <- c("ID", "Event Date")
    sample_data <- data.frame(matrix(ncol = length(sample_cols), nrow = ncol(otu_data) - 1))
    colnames(sample_data) <- sample_cols
    sample_data$ID <- colnames(otu_data)[-1]
    samples_data(sample_data)
  })
  
  # Capture the edited Otu Table
  observeEvent(input$otu_table_cell_edit, {
    info <- input$otu_table_cell_edit
    str(info)  # For debugging purposes
    if (!is.null(info)) {
      new_otu_data <- otu_values()
      if (info$row <= nrow(new_otu_data) && info$col <= ncol(new_otu_data)) {
        new_otu_data[info$row, info$col] <- info$value  # Adjusting for 0-based index
        otu_values(new_otu_data)
        cat("Updated cell at row:", info$row, "column:", info$col, "with value:", info$value, "\n")
      } else {
        cat("Index out of bounds: row:", info$row, "column:", info$col, "\n")
      }
    }
  })
  
  # Save the Otu Table
  observeEvent(input$save_otu_table, {
    saved_otu_data <- otu_values()
    write.csv(saved_otu_data, file = "otu_table.csv",sep = "\t", row.names = FALSE)
    # Perform save operation (e.g., write to a file or database)
    showNotification("Otu Table saved successfully.")
  })
  
  # Placeholder for the Taxonomic Information data
  taxonomic_values <- reactiveVal(data.frame())
  
  output$taxonomic_table <- renderDT({
    datatable(taxonomic_values(), 
              selection = list(mode = "multiple", target = "cell"),
              editable = list(target = 'cell', disable = list(columns = c(0))),
              options = list(dom = 'fti', ordering = FALSE, rownames = FALSE, columnDefs = list(
                list(targets = 0, className = "dt-left unselectable")
              ))
    )
  })
  
  observeEvent(input$load_taxonomic_data, {
    req(input$taxonomic_assignment_file, input$otus_file)
    
    tax_file <- input$taxonomic_assignment_file
    otu_fasta_file <- input$otus_file
    
    taxonomic_assignment <- NULL
    
    if (grepl("\\.csv$", tax_file$name)) {
      taxonomic_assignment <- read.csv(tax_file$datapath)
    } else if (grepl("\\.xlsx$", tax_file$name)) {
      taxonomic_assignment <- readxl::read_xlsx(tax_file$datapath)
    } else if (grepl("\\.txt$", tax_file$name)) {
      taxonomic_assignment <- read.delim(tax_file$datapath)
    }
    
    # Separate tax table
    tax <- taxonomic_assignment %>% 
      select(qseqid, Kingdom, Phylum, Class, Order, Family, Genus, Species)
    
    colnames(tax) <-  c("id", "kingdom", "phylum", "class", "order", "family", "genus", "species")
    
    # Load the FASTA file
    fasta_file <- readDNAStringSet(otu_fasta_file$datapath)
    
    # Convert to a data frame
    raw_otus <- data.frame(
      id = names(fasta_file),
      sequence = as.character(fasta_file),
      stringsAsFactors = FALSE
    )
    
    tax <- merge(tax, raw_otus[, c("id", "sequence")], by = "id", all.x = TRUE)
    
    tax <- tax %>%
      mutate(ScientificName = case_when(
        !is.na(species) ~ species,
        !is.na(genus) ~ genus,
        !is.na(family) ~ family,
        !is.na(order) ~ order,
        !is.na(class) ~ class,
        !is.na(phylum) ~ phylum,
        !is.na(kingdom) ~ kingdom,
        TRUE ~ NA_character_
      ))
    
    tax <- tax[, c(names(tax)[1], "ScientificName", names(tax)[2:(ncol(tax)-1)])]
    
    taxonomic_values(tax)
  })
  
  # Save Taxonomic Data
  observeEvent(input$save_tax_data, {
    final_data <- taxonomic_values()
    write.csv(final_data, file = "tax.csv",sep = "\t", row.names = FALSE)
    showNotification("Taxonomic Data saved successfully as tax.csv.")
  })
  
  # Placeholder for the Samples Table with initial mandatory columns
  samples_data <- reactiveVal(data.frame(ID = character(0), `Event Date` = character(0), stringsAsFactors = FALSE))
  
  # Add or remove selected columns in the Samples Table
  observeEvent(input$add_columns, {
    req(samples_data())
    current_data <- samples_data()
    new_cols <- input$optional_fields
    
    # Add new columns
    for (col in new_cols) {
      if (!col %in% colnames(current_data)) {
        current_data[[col]] <- NA
      }
    }
    
    # Remove not selected columns
    all_cols <- c("ID", "Event Date", new_cols)
    current_data <- current_data[, all_cols, drop = FALSE]
    
    # Remove rows with all NA, "", or " " values
    clean_data <- function(df) {
      df[!apply(df, 1, function(row) all(is.na(row) | row == "" | row == " ")), ]
    }
    cleaned_current_data <- clean_data(current_data)
    rownames(cleaned_current_data) <- seq_len(nrow(cleaned_current_data))
    samples_data(cleaned_current_data)
  })
  
  # Render the editable Samples Table
  output$samples_table <- renderDT({
    datatable(samples_data(), 
              selection = list(
                mode = "multiple",     # Allow multiple cell selection
                target = "cell",       # Target cell selection
                selected = NULL,       # No initial selection
                class = "selected"     # Class applied to selected cells
              ),
              editable = list(
                target = 'cell', 
                disable = list(
                  columns = c(0)
                )
              ),
              options = list(
                dom = 'fti',            # Show filter, table, and information elements
                ordering = FALSE,          # Disable column ordering
                rownames = TRUE,           # Enable row names
                pageLength = 15,           # Show 15 rows per page
                lengthMenu = c(10, 15, 20),# Options for number of rows per page
                autoWidth = FALSE,         # Disable auto width for columns
                scrollX = TRUE,            # Enable horizontal scrolling
                scrollY = "400px",         # Vertical scroll with height of 400 pixels
                searching = TRUE,          # Enable search functionality
                info = TRUE,               # Show table information
                paging = TRUE,             # Enable pagination
                columnDefs = list(
                  #list(className = "dt-bold", targets = 0),
                  list(width = '8px', targets = 0), # Set a specific width for the first column (ID)
                  list(width = '40px', targets = 1), # Set a specific width for the row number column
                  list(width = '200px', targets = "_all") # Define a minimum width for all other columns
                )
              ),
    )
  }, server = FALSE)
  
  # Reactive value to store temporary edits
  temp_samples_data <- reactiveVal(NULL)
  
  # Capture the edited Samples Table
  observeEvent(input$samples_table_cell_edit, {
    info <- input$samples_table_cell_edit
    
    # Get the current state of the samples data
    new_samples_data <- temp_samples_data() %||% samples_data()
    
    # Apply the edit to the temporary data
    if (!is.null(info) && info$row <= nrow(new_samples_data) && info$col <= ncol(new_samples_data)) {
      new_samples_data[info$row, info$col] <- info$value
      temp_samples_data(new_samples_data)
    }
  })
  
  # Save the Samples Table when Save Data is clicked
  observeEvent(input$save_samples, {
    final_data <- temp_samples_data() %||% samples_data()
    
    # Update the main samples_data with the final edits
    samples_data(final_data)
    
    # Optionally, clear temp_samples_data to avoid confusion
    temp_samples_data(NULL)
    
    # Save the final data as a CSV file
    write.csv(final_data, file = "samples.csv",sep = "\t", row.names = FALSE)
    
    # Perform save operation (e.g., write to a file or database)
    showNotification("Samples Data saved successfully.")
  })
  
  # Define the initial data for the Default Values table
  initial_data <- data.frame(
    term = c("env_medium", "target_gene", "target_subfragment", "pcr_primer_forward", "pcr_primer_name_forward", 
             "pcr_primer_reverse", "pcr_primer_name_reverse", "sop", "seq_meth", "otu_db", "lib_layout", 
             "otu_class_appr", "otu_seq_comp_appr", "pcr_primer_reference"),
    value = c("soil [ENVO:00001998]", "ITS2", "ITS2", "GTGAATCATCGAATCTTTG", "gITS7", "TCCCTCCGCTTATTGATATGC", "ITS4",
              "https://www.protocols.io/view/emp-its-illumina-amplicon-protocol-14eqnypg5dy/v1", "Illumina MiSeq", 
              "UNITE v9.3", "paired", "dada2;ASV", "ncbi; blastn; min 98% identity; min 97% query coverage; accepting some competing names", 
              "S2F: https://doi.org/10.1371/journal.pone.0080613 | ITS4: White, T. J., Bruns, T., Lee, S. J. W. T., & Taylor, J. (1990).")
  )
  
  values <- reactiveVal(initial_data)
  
  # Render the editable Default Values table
  output$editable_table <- renderDT({
    datatable(values(), 
              selection = list(
                mode = "multiple",     # Allow multiple cell selection
                target = "cell",       # Target cell selection
                selected = NULL,       # No initial selection
                class = "selected"     # Class applied to selected cells
              ),
              editable = list(
                target = 'cell', 
                disable = list(
                  columns = c(0, 1)   # Disable editing for the first and second columns
                )
              ),
              options = list(
                dom = 'fti',            # Multiple values for DOM: length, filter, table, info, pagination, processing
                ordering = FALSE,          # Disable column ordering
                rownames = FALSE,          # Disable row names
                pageLength = 15,           # Show 15 rows per page
                lengthMenu = c(10, 15, 20),# Options for number of rows per page
                autoWidth = TRUE,          # Enable auto width for columns
                scrollX = TRUE,            # Enable horizontal scrolling
                scrollY = "400px",         # Vertical scroll with height of 400 pixels
                searching = TRUE,          # Enable search functionality
                info = TRUE,               # Show table information
                paging = TRUE,             # Enable pagination
                columnDefs = list(
                  list(targets = 0, className = "dt-left unselectable") # Apply CSS class to the first column
                )
              )
    )
  }, server = FALSE)
  
  # Capture the edited Default Values table
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    str(info)  # For debugging purposes
    if (!is.null(info)) {
      new_data <- values()
      new_data[info$row + 1, info$col + 1] <- info$value  # Adjusting for 0-based index
      values(new_data)
    }
  })
  
  # Save the Default Values table
  observeEvent(input$save_default_values, {
    saved_data <- values()
    # Save dataset in "DefaultValues.csv"
    write.csv(saved_data, file = "DefaultValues.csv",sep = "\t", row.names = FALSE)
    # Perform save operation (e.g., write to a file or database)
    showNotification("Table saved successfully.")
  })
  
}

if (run_in_browser) {
  shinyApp(ui, server, options = list(launch.browser = TRUE))
} else {
  shinyApp(ui, server)
}

}