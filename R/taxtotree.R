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
  
  message("Building phylogenetic tree...")
  tax <- tax[, a:b]
  #string <- tidyr::unite(cbind(tax, rownames(tax)), pathString, sep = "/")
  string <- apply(cbind(all_pd[, 4:10], rownames(all_pd)), 1,
    function(x) paste(x, collapse = "/"))
  string <- as.data.frame(string)
  colnames(string) <- "pathString"
  phy <- ape::as.phylo(data.tree::as.Node(string))
  message("Done!")
  return(phy)

}

