\name{make_unique_names}
\alias{make_unique_names}
\title{Make Unique Names}
\description{
  This function ensures that names in a character vector are unique by appending numbers where necessary.
}
\usage{
make_unique_names(names)
}
\arguments{
  \item{names}{A character vector of names to be made unique.}
}
\value{
  A character vector with unique names.
}
\examples{
make_unique_names(c("Panthera", "Panthera", "Felis", "Canis", "Canis"))
}