#' Taxonomy Table To Tree
#'
#' Generates a tree from the taxonomy table in a 'com' object.
#' @param tax a taxonomy data frame.
#' @param a first column to be included.
#' @param b last column to be included.
#' @return A phylogenetic tree in 'ape' format.
#' @keywords community data
#' @export
#' @examples
#' taxtotree()
taxtotree <- function(tax, a = 1, b = 7) {
  
  tax <- tax[, a:b]
  string <- tidyr::unite(cbind(tax, rownames(tax)), pathString, sep = "/")
  phy <- ape::as.phylo(data.tree::as.Node(string))
  return(phy)

}

