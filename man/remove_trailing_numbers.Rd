\name{remove_trailing_numbers}
\alias{remove_trailing_numbers}
\title{Remove Trailing Numbers from Names}
\description{
  This function removes trailing numbers from names.
}
\usage{
remove_trailing_numbers(name)
}
\arguments{
  \item{name}{A character vector of names from which trailing numbers should be removed.}
}
\value{
  A character vector with trailing numbers removed.
}
\examples{
names <- c("Homo sapiens 1", "Pan troglodytes 2", "Gorilla gorilla 3")
remove_trailing_numbers(names)
}