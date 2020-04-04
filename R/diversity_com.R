#' Calculates Diversity Indices For Communitiy data
#'
#' Calculates abundance and various diversity indices per sample for a community data object. The diversity values are expressed in effective species numbers. Diversity results are also normalized by sampling depth using linear models.
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
  
  # account for sample depth in indices
  message("Normalizing richness by sampling depth...")
  lm_rich <- lm(richness ~ sqrt(abundance))
  print(anova(lm_rich))
  richness_norm <- lm_rich$residuals / sd(lm_rich$residuals)
  
  message("Normalizing Shannon by sampling depth...")
  lm_shan <- lm(shannon ~ sqrt(abundance))
  print(anova(lm_shan))
  shannon_norm <- lm_shan$residuals / sd(lm_shan$residuals)    
  message("Normalizing Simpson by sampling depth...")
  lm_simp <- lm(simpson ~ sqrt(abundance))
  print(anova(lm_simp))
  simpson_norm <- lm_simp$residuals / sd(lm_simp$residuals)
  
  # combine data
  com$diversity <- data.frame(abundance, richness, richness_norm, shannon,
    shannon_norm, simpson, simpson_norm)
      
  # save data
  write.table(com$diversity, file = file, col.names = NA, ...) 
 
  return(com)  
}

