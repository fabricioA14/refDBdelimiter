\name{refDB_CreateStackedBar}
\alias{refDB_CreateStackedBar}
\title{Create Interactive Stacked Bar Plot}
\description{
  This function creates an interactive stacked bar plot using the input data.
}
\usage{
refDB_CreateStackedBar(data, taxonomic_level, title, legend_title)
}
\arguments{
  \item{data}{A data frame containing the data to be plotted.}
  \item{taxonomic_level}{The taxonomic level to be used for grouping the data.}
  \item{title}{The title of the plot.}
  \item{legend_title}{The title of the legend.}
}
\value{
  An interactive stacked bar plot created using plotly.
}
\examples{
\dontrun{
# Example data frame
example_data <- data.frame(
  year = c(2020, 2021, 2022, 2020, 2021, 2022),
  family = c("Felidae", "Canidae", "Felidae", "Canidae", "Felidae", "Canidae"),
  stringsAsFactors = FALSE
)

# Create a stacked bar plot
refDB_CreateStackedBar(example_data, "family", "Example Stacked Bar Plot", "Family")
}
}