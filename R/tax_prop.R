#' Proportion of Taxa In Community Data
#'
#' Calculates the proportion of taxonomic categories across samples of a community data object.
#' @param com a community data object.
#' @param taxon name of the taxonomic level to calculate proportions. Must be a column name in table 'tax'.
#' @param var a factor to pool samples. Must be a column name in table 'env'. Optional.
#' @param sel selection of specific taxa to output. Overrides 'n'.
#' @param fun function to use for pooling taxonomic data. Default is sum.
#' @param omit_unclassified omit unclassified taxa from the column sorting. Unclassified taxa are placed at the end.
#' @param term if omit_unclassified = T, the term defining unclassified taxa.
#' @return A data frame with proportions of taxa across samples.
#' @keywords community data
#' @export
#' @examples
#' tax_prop()
tax_prop <- function(com, taxon, var = NULL, sel = NULL,
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
    if(!is.vector(tab2)) {
      tab2 <- tab2[, names(sort(colSums(tab2), decreasing = T))]
    }
    tab <- cbind(tab1, tab2)
    colnames(tab)[ncol(tab)] <- "unclassified"
  } else {
    tab <- tab[, names(sort(colSums(tab), decreasing = T))]
  }
  if(!is.null(sel)) {
    if(!is.vector(sel)) stop("'sel' must be a vector.")
    if(any(!(sel %in% colnames(tab)))) {
      missing <- sel[!(sel %in% colnames(tab))]
      message(sprintf("Taxa missing in dataset: %s",
        paste(missing, collapse = ", ")))
      tab <- cbind(tab, matrix(0, nrow = nrow(tab), ncol = length(missing),
        dimnames = list(rownames(tab), missing)))
    }
    if(sum(!(colnames(tab) %in% sel)) == 1) {
      tab <- cbind(tab[, sel],
        others = tab[, !(colnames(tab) %in% sel)]) 
    } else {
      tab <- cbind(tab[, sel],
        others = rowSums(tab[, !(colnames(tab) %in% sel)])) 
    }      
  } else if(n < length(levels(com$tax[, taxon]))) {
    tab <- cbind(tab[, 1:n], others = rowSums(tab[, (n + 1):ncol(tab)]))
  }
  return(tab)

}
