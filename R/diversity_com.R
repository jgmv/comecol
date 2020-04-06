#' Calculates Diversity Indices For Communitiy data
#'
#' Calculates abundance and various diversity indices per sample for a community data object. The diversity values are expressed in effective species numbers.
#' @param com a community data object.
#' @param file the name of the file where a table with the diversity values is exported.
#' @param ... further arguments to be passed to 'write.table'.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' diversity_com()
diversity_com <- function(com, file = "diversity_indices.csv", ...) {  
  
  # calculate indices
  abundance <- rowSums(com$cdm)
  richness <- apply(com$cdm, 1, function(x) sum(x > 0))
  shannon  <- exp(vegan::diversity(com$cdm, index = "shannon"))
  simpson  <- vegan::diversity(com$cdm, index = "invsimpson")
  
  # combine data
  com$diversity <- data.frame(abundance, richness, shannon, simpson)
      
  # save data
  write.table(com$diversity, file = file, col.names = NA, ...) 
 
  return(com)  
}

