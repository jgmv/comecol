#' Add Soil Data To Community Data Object
#'
#' Adds soil characteristics to community data object.
#' @param com a community data object.
#' @param soil data frame with soil charactersitics to be added. Row names must be as in 'env'.
#' @param k the number of eigenvectors retained.
#' @details Eigenvectors are obtained from a Principal Coordinates Analysis calculated from the soil data.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' soil_data()
soil_data <- function(com, soil, k = 3) {

  if(!is.data.frame(soil)) stop("'soil' must be a data frame")
  
  if(!all(com$env$site %in% rownames(soil)) |
     !all(rownames(soil) %in% com$env$site)) {
    message("Some samples are not present everywhere. Aborting...")
    break
  }
  
  soil <- soil[com$env$site, ]
  rownames(soil) <- rownames(com$env)  
  com$soil <- soil
  soil_scaled  <- scale(soil)
  soil_dist    <- dist(soil_scaled)
  soil_pca     <- cmdscale(soil_dist, k = k, eig = T)
  soil_coord <- soil_pca$points
  rownames(soil_coord) <- rownames(com$env)
  colnames(soil_coord) <- paste0("soil_PC", 1:ncol(soil_coord))
  com$soil_eig <- soil_coord
  com$soil_pca <- soil_pca
  
  return(com)
  
}
