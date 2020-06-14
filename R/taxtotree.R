#' Taxonomy Table To Tree
#'
#' Generates a tree from the taxonomy table in a 'com' object.
#' @param tax a taxonomy data frame.
#' @return A phylogenetic tree in 'ape' format.
#' @keywords community data
#' @export
#' @examples
#' taxtotree()
taxtotree <- function(tax) {

  string <- tidyr::unite(cbind(tax, rownames(tax)), pathString, sep = "/")
  phy <- ape::as.phylo(data.tree::as.Node(string))
  return(phy)

}

