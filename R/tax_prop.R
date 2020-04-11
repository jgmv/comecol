#' Proportion of Taxa In Community Data
#'
#' Calculates the proportion of taxonomic categories across samples of a community data object.
#' @param com a community data object.
#' @param taxon name of the taxonomic level to calculate proportions. Must be a column name in table 'tax'.
#' @param var a factor to pool samples. Must be a column name in table 'env'. Optional.
#' @param fun function to use for pooling taxonomic data. Default is sum.
#' @param omit_unclassified omit unclassified taxa from the column sorting. Unclassified taxa are placed at the end.
#' @term if omit_unclassified = T, the term defining unclassified taxa.
#' @return A data frame with proportions of taxa across samples.
#' @keywords community data
#' @export
#' @examples
#' tax_prop()
tax_prop <- function(com, taxon, var = NULL,
  n = length(levels(com$tax[, taxon])), fun = sum, omit_unclassified = T,
  term = "unclassified") {  
  if(!(taxon %in% colnames(com$tax))) {    
    stop(paste0(taxon, " not found among taxonomic levels. Cancelled."))       
  }
  tab <- t(apply(com$cdm, 1, function(x) tapply(x, com$tax[, taxon],
    FUN = fun)))
  if(!is.null(var)) tab <- apply(tab, 2, function(x) tapply(x, com$env[, var],
    sum))
  tab <- tab / rowSums(tab)
  if(omit_unclassified) {
    unclassified <- colnames(tab)[grep(term, colnames(tab))]
    tab1 <- tab[, !(colnames(tab) %in% unclassified)]
    tab1 <- tab1[, names(sort(colSums(tab1), decreasing = T))]
    tab2 <- tab[, colnames(tab) %in% unclassified]
    tab2 <- tab2[, names(sort(colSums(tab2), decreasing = T))]
    tab <- cbind(tab1, tab2)
  } else {
    tab <- tab[, names(sort(colSums(tab), decreasing = T))]
  }
  if(n < length(levels(com$tax[, taxon]))) {
    tab <- cbind(tab[, 1:n], others = rowSums(tab[, (n + 1):ncol(tab)]))
  }
  return(tab)

}


