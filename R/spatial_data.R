#' Add Geographic Coordinates To Community Data
#'
#' Adds grographic coordinates as a separate table in a community data object.
#' @param com a community data object.
#' @param x name of the column in 'env' where the longitude coordinates are stored.
#' @param y name of the column in 'env' where the latitude coordinates are stored.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' spatial_data()
spatial_data <- function(com, x, y) {

  geo_coord <- as.matrix(com$env[, c(x, y)])
  rownames(geo_coord) <- rownames(com$env)
  com$distance <- geo_coord
  return(com)
  
}

