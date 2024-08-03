#' Create Interactive Stacked Bar Plot
#'
#' This function creates an interactive stacked bar plot using the input data.
#'
#' @param data A data frame containing the data to be plotted.
#' @param taxonomic_level The taxonomic level to be used for grouping the data.
#' @param title The title of the plot.
#' @param legend_title The title of the legend.
#' @return An interactive stacked bar plot created using plotly.
#' @examples{
#' \dontrun{
#' # Example data frame
#' example_data <- data.frame(
#'   year = c(2020, 2021, 2022, 2020, 2021, 2022),
#'   family = c("Felidae", "Canidae", "Felidae", "Canidae", "Felidae", "Canidae"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Create a stacked bar plot
#' refDB_CreateStackedBar(example_data, "family", "Example Stacked Bar Plot", "Family")
#' )}
#' }
#' @return A plotly object.
#' @importFrom dplyr group_by summarise n %>%
#' @importFrom plotly plot_ly layout
#' @importFrom rlang sym
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_to_title
#' @importFrom stats as.formula
#' @export
refDB_CreateStackedBar <- function(data, taxonomic_level, title, legend_title) {
  # Capitalize the first letter of the legend title
  legend_title <- str_to_title(legend_title)
  
  # Replace empty strings and NA in taxonomic_level with "Empty"
  data[[taxonomic_level]][data[[taxonomic_level]] == ""] <- "Empty"
  data[[taxonomic_level]][is.na(data[[taxonomic_level]])] <- "Empty"
  
  # Replace empty strings and NA in year with "Unknown"
  data$year[data$year == ""] <- "Unknown"
  data$year[is.na(data$year)] <- "Unknown"
  
  # Order the taxonomic levels, keeping "Empty" at the end
  levels_ordered <- c("Empty", rev(sort(unique(data[[taxonomic_level]][data[[taxonomic_level]] != "Empty"]))))
  data[[taxonomic_level]] <- factor(data[[taxonomic_level]], levels = levels_ordered)
  
  # Order the years, keeping "Unknown" at the start
  years_ordered <- c("Unknown", sort(unique(data$year[data$year != "Unknown"])))
  data$year <- factor(data$year, levels = years_ordered)
  
  # Define colors, assigning "grey60" to "Empty"
  colors <- c(RColorBrewer::brewer.pal(n = length(levels_ordered) - 1, name = "Set3"))
  
  data %>%
    group_by(year, !!sym(taxonomic_level)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    plot_ly(x = ~year, y = ~count, type = 'bar', color = as.formula(paste0("~", taxonomic_level)), colors = colors) %>%
    layout(
      title = list(text = title, font = list(family = 'Comfortaa', size = 20)),
      barmode = 'stack',
      xaxis = list(title = 'Year', titlefont = list(family = 'Comfortaa', size = 18), tickfont = list(family = 'Comfortaa')),
      yaxis = list(title = 'Count', titlefont = list(family = 'Comfortaa', size = 18), tickfont = list(family = 'Comfortaa')),
      legend = list(
        title = list(
          text = paste0("   ", legend_title),
          font = list(family = 'Comfortaa', size = 14)
        ),
        font = list(family = 'Comfortaa', size = 12),
        orientation = "v"
      ),
      margin = list(t = 80)  # Add top margin for the title
    )
}
