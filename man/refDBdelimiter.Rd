\name{refDBdelimiter}
\alias{refDBdelimiter}
\title{Shiny Application for refDBdelimiter}
\description{
This function launches a Shiny application for the refDBdelimiter tool. The application offers various processes, including automatic and manual filters for geographic data from the GBIF database, molecular database creation from NCBI structure, and taxonomic assignment.
}
\usage{
refDBdelimiter(run_in_browser = FALSE)
}
\arguments{
  \item{run_in_browser}{Logical value indicating whether to run the Shiny application in the default web browser. Default is \code{FALSE}.}
}
\value{
  None. This function launches a Shiny application.
}
\examples{
\dontrun{
# To run the application:
refDBdelimiter()
}
}