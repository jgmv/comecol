#' Find Species' Distribution Ranges
#'
#' Finds the maximum distance in Km between the edges of a species' distribution from occurrence coordinates.
#' @param x table with longitude and latitude coordinates of a species' occurrence.
#' @param lon name of the table column with longitude values
#' @param lat name of the table column with latitude values
#' @details The tree calculated from the taxonomic data provided is performed  
#' @return The species range in Km.
#' @keywords distribution range
#' @export
#' @examples
#' find_species_range()
find_species_range <- function(x, lon = "lon", lat = "lat") {
  xy <- as.matrix(x[, c(lon, lat)])
  xy <- na.omit(xy)
  if(nrow(xy) > 0) {
    xy_dist <- sp::spDists(xy, longlat = T)
    result <- max(xy_dist)
  } else {
    result <- NA
  }
  return(result)  
}

