#' Create Interactive Stacked Bar Plot
#'
#' This function creates an interactive stacked bar plot.
#'
#' @param data A data frame containing the data to plot.
#' @param taxonomic_level The taxonomic level to use for the plot.
#' @param title The title of the plot.
#' @param legend_title The title of the legend.
#' @return A plotly object.
#' @examples
#' refDB_CreateStackedBar(data, "species", "Title", "Legend Title")
#' @export
refDB_CreateStackedBar <- function(data, taxonomic_level, title, legend_title) {
  legend_title <- str_to_title(legend_title)
  
  data[[taxonomic_level]][data[[taxonomic_level]] == ""] <- "Empty"
  data[[taxonomic_level]][is.na(data[[taxonomic_level]])] <- "Empty"
  
  data$year[data$year == ""] <- "Unknown"
  data$year[is.na(data$year)] <- "Unknown"
  
  levels_ordered <- c("Empty", rev(sort(unique(data[[taxonomic_level]][data[[taxonomic_level]] != "Empty"]))))
  data[[taxonomic_level]] <- factor(data[[taxonomic_level]], levels = levels_ordered)
  
  years_ordered <- c("Unknown", sort(unique(data$year[data$year != "Unknown"])))
  data$year <- factor(data$year, levels = years_ordered)
  
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
      margin = list(t = 80)
    )
}