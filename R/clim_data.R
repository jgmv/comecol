#' Get Bioclimatic Data For Community Data
#'
#' Obtains bioclimatic variables from the "WorldClim" dataset for the samples in a community data object, using geographical coordinates provided in the environmental table.
#' @param com a community data object.
#' @param x name of the column in 'env' where the longitude coordinates are stored.
#' @param y name of the column in 'env' where the latitude coordinates are stored.
#' @param eigenvectors logical, whether eigenvectors should be provided instead of raw bioclimatic data.
#' @param k the number of eigenvectors retained.
#' @param keep_data logical, whether the downloaded bioclimatic database should be kept after extracting the data.
#' @details If eigenvectors = T, eigenvalues are obtained from a Principal Coordinates Analysis calculated from the bioclimatic data.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' clim_data()
clim_data <- function(com, x = "lon", y = "lat", eigenvectors = T, k = 3,
  keep_data = T) {
   
  bio_db <- raster::getData("worldclim", var = "bio", res = 10)
  bio <- raster::extract(bio_db, com$env[, c(x, y)])
  rownames(bio) <- rownames(com$env)
  if(!keep_data) unlink("wc10", recursive = T)
  
  if(eigenvectors) {
    bio_scaled <- scale(bio)
    bio_dist   <- dist(bio_scaled)
    bio_pca    <- cmdscale(bio_dist, k = k, eig = T)
    bio_coord <- vegan::scores(bio_pca)
    colnames(bio_coord) <- paste0("clim_PC", 1:ncol(bio_coord))
    com$climate <- bio_coord
  } else {
    com$climate <- bio
  }
  
  return(com)
  
}

