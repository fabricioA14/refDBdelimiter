#' Extract Data and Create a Data Frame
#'
#' This function extracts data from a list of spatial features and creates a data frame.
#'
#' @param data A list of spatial features. Each feature should have a \code{taxa} vector, a \code{layer} list with \code{properties} and \code{geometry}.
#' @return An \code{sf} object (data frame) with extracted and modified data, including unique taxa names.
#' @examples{
#' \dontrun{
#' data <- list(
#'   list(taxa = c("Panthera leo, Africa", "Panthera onca, America"), layer = list(properties = list(`_leaflet_id` = 1, feature_type = "polygon"), geometry = list(coordinates = list(matrix(c(1, 2, 3, 4, 5, 6, 1, 2), ncol = 2, byrow = TRUE))))),
#'   list(taxa = c("Felis catus, Europe", "Canis lupus, North America"), layer = list(properties = list(`_leaflet_id` = 2, feature_type = "polygon"), geometry = list(coordinates = list(matrix(c(2, 3, 4, 5, 6, 7, 2, 3), ncol = 2, byrow = TRUE)))))
#' )
#' refDB_ExtractData(data)
#' )}
#' }
#' @importFrom dplyr mutate select
#' @importFrom utils head tail
#' @importFrom sf st_crs st_sf st_sfc st_as_sf st_geometry st_drop_geometry
#' @export
refDB_ExtractData <- function(data) {
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
