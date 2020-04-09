#' Proportion of Taxa In Community Data
#'
#' Calculates the proportion of taxonomic categories across samples of a community data object.
#' @param com a community data object.
#' @param taxon name of the taxonomic level to calculate proportions. Must be a column name in table 'tax'.
#' @param var a factor to pool samples. Must be a column name in table 'env'. Optional.
#' @param fun function to use for pooling taxonomic data. Default is sum.
#' @return A data frame with proportions of taxa across samples.
#' @keywords community data
#' @export
#' @examples
#' tax_prop()
tax_prop <- function(com, taxon, var = NULL,
  n = length(levels(com$tax[, taxon])), fun = sum) {  
  if(!(taxon %in% colnames(com$tax))) {    
    stop(paste0(taxon, " not found among taxonomic levels. Cancelled."))       
  }
  tab <- t(apply(com$cdm, 1, function(x) tapply(x, com$tax[, taxon],
    FUN = fun)))
  if(!is.null(var)) tab <- apply(tab, 2, function(x) tapply(x, com$env[, var],
    sum))
  tab <- tab / rowSums(tab)
  tab <- tab[, names(sort(colSums(tab), decreasing = T))]
  if(n < length(levels(com$tax[, taxon]))) {
    tab <- cbind(tab[, 1:n], others = rowSums(tab[, (n + 1):ncol(tab)]))
  }
  return(tab)

}

