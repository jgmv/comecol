% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_species_range.R
\name{find_species_range}
\alias{find_species_range}
\title{Find Species' Distribution Ranges}
\usage{
find_species_range(x, lon = "lon", lat = "lat")
}
\arguments{
\item{x}{table with longitude and latitude coordinates of a species' occurrence.}

\item{lon}{name of the table column with longitude values}

\item{lat}{name of the table column with latitude values}
}
\value{
The species range in Km.
}
\description{
Finds the maximum distance in Km between the edges of a species' distribution from occurrence coordinates.
}
\details{
The tree calculated from the taxonomic data provided is performed
}
\examples{
find_species_range()
}
\keyword{distribution}
\keyword{range}
